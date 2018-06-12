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

let bottom : 'a flow = Nt Map.empty

let top : 'a flow = TOP

let is_bottom (flow: 'a flow) ~(is_value_bottom: 'a -> bool) : bool =
  top_dfl1 false (fun m ->
      Map.for_all (fun _ v -> is_value_bottom v) m
    ) flow


let is_top (flow: 'a flow) : bool =
  top_dfl1 true (fun _ -> false) flow


let leq (flow1: 'a flow) (flow2: 'a flow) ~(is_value_bottom: 'a -> bool) ~(value_leq: 'a -> 'a -> bool) : bool =
  top_included
    (Map.for_all2zo
       (fun _ v1 -> is_value_bottom v1) (* non-⊥ ⊈ ⊥ *)
       (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
       (fun _ v1 v2 -> value_leq v1 v2)
    )
    flow1 flow2

let join (flow1: 'a flow) (flow2: 'a flow) ~(value_join: 'a -> 'a -> 'a) : 'a flow =
  top_lift2
    (Map.map2zo
       (fun _ v1 -> v1)
       (fun _ v2 -> v2)
       (fun _ v1 v2 -> value_join v1 v2)
    )
    flow1 flow2

let meet (flow1: 'a flow) (flow2: 'a flow) ~(value_bottom: 'a) ~(value_meet: 'a -> 'a -> 'a) : 'a flow =
  top_neutral2
    (fun b1 b2 ->
       Map.map2zo
         (fun _ v1 -> value_bottom)
         (fun _ v2 -> value_bottom)
         (fun _ v1 v2 -> value_meet v1 v2)
         b1 b2
    )
    flow1 flow2

let widening (ctx: Context.context) (flow1: 'a flow) (flow2: 'a flow) ~(value_widening: Context.context -> 'a -> 'a -> 'a) : 'a flow =
  top_lift2
    (Map.map2zo
       (fun _ v1 -> v1)
       (fun _ v2 -> v2)
       (fun _ v1 v2 -> value_widening ctx v1 v2)
    )
    flow1 flow2

let print ~(value_print: Format.formatter -> 'a -> unit) fmt (flow : 'a flow) : unit=
  let open Format in
  top_fprint (fun fmt m ->
      if Map.is_empty m then pp_print_string fmt "⊥"
      else
        fprintf fmt "@[<v>%a@]"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt "@,")
             (fun fmt (k, v) -> fprintf fmt "⏵ %a ↦@\n@[<hov4>    %a@]" pp_token k value_print v)
          ) (Map.bindings m)
    ) fmt flow



let get (tk: token) (flow: 'a flow) ~(value_bottom: 'a) ~(value_top: 'a) : 'a =
  try
    let m = top_to_exn flow in
    try Map.find tk m with Not_found -> value_bottom
  with Found_TOP -> value_top


let set (tk: token) (a: 'a) (flow: 'a flow) ~(is_value_bottom: 'a -> bool) : 'a flow =
  top_lift1 (fun m ->
      if is_value_bottom a then Map.remove tk m
      else Map.add tk a m
    ) flow

let remove (tk: token) (flow: 'a flow) : 'a flow =
  top_lift1 (Map.remove tk) flow


let filter (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow =
  top_lift1 (Map.filter f) flow

let add (tk: token) (a: 'a) (flow: 'a flow) ~(is_value_bottom: 'a -> bool) ~(value_join: 'a -> 'a -> 'a) : 'a flow =
  top_lift1 (fun m ->
      if is_value_bottom a then m
      else
        let a' =
          try
            let old = Map.find tk m in
            value_join a old
          with Not_found ->
            a
        in
        Map.add tk a' m
    ) flow

let map (f: token -> 'a -> 'b) (flow: 'a flow) : 'b flow =
  top_lift1 (Map.mapi f) flow

let fold (f: token -> 'a -> 'b -> 'b) (flow: 'a flow) (x: 'b) : 'b =
  let m = top_to_exn flow in
  Map.fold f m x

let merge (f: token -> 'a option -> 'a option -> 'a option) (flow1: 'a flow) (flow2: 'a flow) ~(value_bottom: 'a) : 'a flow =
  let exec tk a b =
    match f tk a b with
    | None -> value_bottom
    | Some v -> v
  in
  top_lift2
    (Map.map2zo
       (fun tk v1 -> exec tk (Some v1) None)
       (fun tk v2 -> exec tk None (Some v2))
       (fun tk v1 v2 -> exec tk (Some v1) (Some v2))
    )
    flow1 flow2
