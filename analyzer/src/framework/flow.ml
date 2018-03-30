(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Flows are handled continuations and are identified by a finite
   set of tokens. We use a partitioning abstraction to aggregate together
   the abstract environments of each flow token.
*)

open Top
open Lattice

let debug fmt = Debug.debug ~channel:"framework.flow" fmt

(*==========================================================================*)
                           (** {2 Tokens} *)
(*==========================================================================*)


type token = ..
(** Extensible type of flow tokens. *)

type token +=
  | TCur (** Flow of current executions. *)


let token_compare_chain : (token -> token -> int) ref = ref (fun tk1 tk2 ->
    match tk1, tk2 with
    | TCur, TCur -> 0
    | _ -> compare tk1 tk2
  )

let register_token_compare cmp =
  token_compare_chain := cmp !token_compare_chain

let compare_token tk = !token_compare_chain tk

let pp_token_chain : (Format.formatter -> token -> unit) ref = ref (fun fmt ->
    function
    | TCur -> Format.pp_print_string fmt "cur"
    | _ -> failwith "Pp: Unknown flow token"
)


let register_pp_token pp = pp_token_chain := pp !pp_token_chain

let pp_token fmt ft = !pp_token_chain fmt ft


(*==========================================================================*)
                           (** {2 Flows map} *)
(*==========================================================================*)


module Map = MapExt.Make(
  struct
    type t = token
    let compare tk1 tk2 = compare_token tk1 tk2
    let print fmt tk = pp_token fmt tk
  end
  )

type 'a flow = 'a Map.t with_top


(*==========================================================================*)
                           (** {2 Flows manager} *)
(*==========================================================================*)


type 'a flow_manager = {
  bottom : 'a flow;
  top : 'a flow;
  is_bottom : 'a flow -> bool;
  is_cur_bottom : 'a flow -> bool;
  is_top : 'a flow -> bool;
  leq : 'a flow -> 'a flow -> bool;
  join : 'a flow -> 'a flow -> 'a flow;
  meet : 'a flow -> 'a flow -> 'a flow;
  widening : Context.context -> 'a flow -> 'a flow -> 'a flow;
  print : Format.formatter -> 'a flow -> unit;
  get : token -> 'a flow -> 'a;
  set : token -> 'a -> 'a flow -> 'a flow;
  add : token -> 'a -> 'a flow -> 'a flow;
  remove : token -> 'a flow -> 'a flow;
  filter : ('a -> token -> bool) -> 'a flow -> 'a flow;
  map : 'b. ('a -> token -> 'b) -> 'a flow -> 'b flow;
  fold : 'b. ('b -> 'a -> token -> 'b) -> 'b -> 'a flow -> 'b;
  merge : (token -> 'a option -> 'a option -> 'a option) -> 'a flow -> 'a flow -> 'a flow;
}

let lift_lattice_manager (value: 'a lattice_manager) : ('a flow_manager) = {
  bottom = Nt Map.empty;
  top = TOP;
  is_bottom = (fun fabs ->
      top_dfl1 false (fun m ->
          Map.for_all (fun _ v -> value.is_bottom v) m
        ) fabs
    );

  is_cur_bottom = (fun fabs ->
      top_dfl1 false (fun m ->
          try Map.find TCur m |> value.is_bottom with Not_found -> true
        ) fabs
    );

  is_top = (fun fabs ->
      top_dfl1 true (fun _ -> false) fabs
    );

  leq = (fun fabs1 fabs2 ->
      top_included
        (Map.for_all2zo
           (fun _ v1 -> value.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
           (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
           (fun _ v1 v2 -> value.leq v1 v2)
        )
        fabs1 fabs2
    );

  join = (fun fabs1 fabs2 ->
      top_lift2
        (Map.map2zo
           (fun _ v1 -> v1)
           (fun _ v2 -> v2)
           (fun _ v1 v2 -> value.join v1 v2)
        )
        fabs1 fabs2
    );

  meet = (fun fabs1 fabs2 ->
      top_neutral2
        (fun b1 b2 ->
           (Map.map2zo
              (fun _ v1 -> value.bottom)
              (fun _ v2 -> value.bottom)
              (fun _ v1 v2 -> value.meet v1 v2)
              b1) b2
        )
        fabs1 fabs2
    );

  widening = (fun ctx fabs1 fabs2 ->
      top_lift2
        (Map.map2zo
           (fun _ v1 -> v1)
           (fun _ v2 -> v2)
           (fun _ v1 v2 -> value.widening ctx v1 v2)
        )
        fabs1 fabs2
    );

  print = (fun fmt a ->
      let open Format in
      top_fprint (fun fmt m ->
          if Map.is_empty m then
            pp_print_string fmt "⊥"
          else
            fprintf fmt "@[<v>%a@]"
              (pp_print_list
                 ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                 (fun fmt (k, v) -> fprintf fmt "⏵ %a ↦@\n@[<hov4>    %a@]" pp_token k value.print v)
              ) (Map.bindings m)
        ) fmt a
    );

  get = (fun tk fabs ->
      try
        let m = top_to_exn fabs in
        try Map.find tk m with Not_found -> value.bottom
      with Found_TOP -> value.top
    );

  set = (fun tk abs fabs ->
      top_lift1 (fun m ->
          if value.is_bottom abs then
            Map.remove tk m
          else
            Map.add tk abs m
        ) fabs
    );

  remove = (fun tk fabs ->
      top_lift1 (Map.remove tk) fabs
    );

  filter = (fun f fabs ->
      top_lift1 (Map.filter (fun tk a -> f a tk)) fabs
    );

  add = (fun tk a fabs ->
      top_lift1 (fun m ->
          if value.is_bottom a then
            m
          else
            let a' =
              try
                let old = Map.find tk m in
                value.join a old
              with Not_found ->
                a
            in
            Map.add tk a' m
        ) fabs
    );

  map = (
    let g : type b. ('a -> token -> b) -> 'a flow -> b flow =
      fun f fabs ->
        top_lift1 (Map.mapi (fun tk a -> f a tk)) fabs
    in
    g
  );


  fold = (
    let g : type b. (b -> 'a -> token -> b) -> b -> 'a flow -> b =
      fun f x fabs ->
        let m = top_to_exn fabs in
        Map.fold (fun tk a acc -> f acc a tk) m x
    in
    g
  );

  merge = (fun f flow1 flow2 ->
      top_lift2 (Map.merge f) flow1 flow2
    );
}
