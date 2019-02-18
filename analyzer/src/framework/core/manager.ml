(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

open Annotation

(*==========================================================================*)
(**                            {2 Flows}                                    *)
(*==========================================================================*)

type token = ..
(** Flow tokens are used to tag abstract elements when encountered in a
    relevant control point *)

type token += T_cur
(** Token of current (active) execution flow *)

type token_info = {
  compare : (token -> token -> int) -> token -> token -> int;
  print   : (Format.formatter -> token -> unit) -> Format.formatter -> token -> unit;
}

let token_compare_chain : (token -> token -> int) ref = ref (fun tk1 tk2 ->
    match tk1, tk2 with
    | T_cur, T_cur -> 0
    | _ -> compare tk1 tk2
  )

let print_token_chain : (Format.formatter -> token -> unit) ref = ref (fun fmt ->
    function
    | T_cur -> Format.pp_print_string fmt "cur"
    | _ -> failwith "Pp: Unknown flow token"
)


let register_token info =
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
  annot : 'a annot;
}

type 'a flow_callback = {
  flow: 'a flow;
  callbacks: ('a flow -> 'a flow) list;
}

(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('a, 'e) evl_case = {
  expr    : 'e option;
  flow    : 'a flow;
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
  subset    : 'a -> 'a -> bool;
  join      : 'a annot -> 'a -> 'a -> 'a;
  meet      : 'a annot -> 'a -> 'a -> 'a;
  widen     : 'a annot -> 'a -> 'a -> 'a;
  print     : Format.formatter -> 'a -> unit;

  (* Accessors to the domain's abstract element *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Transfer functions *)
  exec : ?zone:Zone.zone -> Ast.stmt -> 'a flow -> 'a flow;
  eval : ?zone:(Zone.zone * Zone.zone) -> ?via:Zone.zone -> Ast.expr -> 'a flow -> ('a, Ast.expr) evl;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;
}
