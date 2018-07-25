(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(*==========================================================================*)
(**                            {2 Flows}                                    *)
(*==========================================================================*)

type token = ..
(** Flow tokens are used to tag abstract elements when encountered in a
    relevant control point *)

type token += TCur
(** Token of current (active) execution flow *)

type token_info = {
  compare : (token -> token -> int) -> token -> token -> int;
  print   : (Format.formatter -> token -> unit) -> Format.formatter -> token -> unit;
}

let token_compare_chain : (token -> token -> int) ref = ref (fun tk1 tk2 ->
    match tk1, tk2 with
    | TCur, TCur -> 0
    | _ -> compare tk1 tk2
  )

let print_token_chain : (Format.formatter -> token -> unit) ref = ref (fun fmt ->
    function
    | TCur -> Format.pp_print_string fmt "cur"
    | _ -> failwith "Pp: Unknown flow token"
)


let regiter_token info =
  token_compare_chain := info.compare !token_compare_chain;
  print_token_chain := info.print !print_token_chain;
  ()

let compare_token tk = !token_compare_chain tk

let pp_token fmt ft = !print_token_chain fmt ft

module FlowMap =
struct
  include MapExt.Make(
    struct
      type t = token
      let compare = compare_token
      let print = pp_token
    end
    )

  let print pp_value fmt m =
    if is_empty m then Format.pp_print_string fmt "⊥"
    else
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt (k, v) -> Format.fprintf fmt "⏵ %a ↦@\n@[<hov4>    %a@]" pp_token k pp_value v)
        ) (bindings m)
end

type 'a fmap = 'a FlowMap.t Top.with_top

type 'a flow = {
  map   : 'a fmap;
  annot : 'a Annotation.t;
}

let set_annot annot flow = {flow with annot}

let get_annot flow = flow.annot

(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('a, 'e) evl_case = {
  exp : 'e option;
  flow: 'a flow;
  cleaners: Ast.stmt list;
}

type ('a, 'e) evl = ('a, 'e) evl_case Dnf.t


(*==========================================================================*)
                           (** {2 Analysis manager} *)
(*==========================================================================*)


type ('a, 't) man = {
  (* Functions on the global abstract element *)
  bottom    : 'a;
  top       : 'a;
  is_bottom : 'a -> bool;
  is_top    : 'a -> bool;
  leq       : 'a -> 'a -> bool;
  join      : 'a Annotation.t -> 'a -> 'a -> 'a;
  meet      : 'a Annotation.t -> 'a -> 'a -> 'a;
  widen     : 'a Annotation.t -> 'a -> 'a -> 'a;
  print     : Format.formatter -> 'a -> unit;

  (* Accessors to the domain's abstract element *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Transfer functions *)
  exec : ?zone:Zone.t -> Ast.stmt -> 'a flow -> 'a flow;
  eval : ?zpath:Zone.path -> Ast.expr -> 'a flow -> ('a, Ast.expr) evl;
  ask : 'r. ?zone:Zone.t -> 'r Query.query -> 'a flow -> 'r;
}
