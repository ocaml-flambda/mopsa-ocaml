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

(** Extensible type of expressions. *)


open Location
open Typ
open Program
open Operator
open Constant
open Var
open Format

type expr_kind = ..

type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: Location.range;
  eprev: expr option;
}

type expr_kind +=
  | E_var of var * mode option
  | E_constant of constant
  | E_unop of operator * expr
  | E_binop of operator * expr * expr

let ekind (e: expr) = e.ekind
let etyp (e: expr) = e.etyp
let erange (e: expr) = e.erange

let expr_compare_chain = TypeExt.mk_compare_chain (fun e1 e2 ->
      match ekind e1, ekind e2 with
      | E_var(v1, m1), E_var(v2, m2) ->
        Compare.compose [
          (fun () -> compare_var v1 v2);
          (fun () -> Compare.option compare_mode m1 m2)
        ]
      | E_constant c1, E_constant c2 -> compare_constant c1 c2

      | _ -> Stdlib.compare (ekind e1) (ekind e2)
  )

let expr_pp_chain = TypeExt.mk_print_chain (fun fmt expr ->
    match ekind expr with
    | E_constant c -> pp_constant fmt c
    | E_var(v, None) -> pp_var fmt v
    | E_var(v, Some STRONG) -> Format.fprintf fmt "strong(%a)" pp_var v
    | E_var(v, Some WEAK) -> Format.fprintf fmt "weak(%a)" pp_var v
    | _ -> failwith "Pp: Unknown expression"
  )

let register_expr info = TypeExt.register info expr_compare_chain expr_pp_chain

let register_expr_compare cmp = TypeExt.register_compare cmp expr_compare_chain

let register_expr_pp pp = TypeExt.register_print pp expr_pp_chain

let compare_expr e1 e2 = TypeExt.compare expr_compare_chain e1 e2

let pp_expr fmt e = TypeExt.print expr_pp_chain fmt e

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_unop(op1, e1), E_unop(op2, e2) ->
          Compare.compose [
            (fun () -> compare_operator op1 op2);
            (fun () -> compare_expr e1 e2);
          ]

        | E_binop(op1, e1, e1'), E_binop(op2, e2, e2') ->
          Compare.compose [
            (fun () -> compare_operator op1 op2);
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_expr e1' e2');
          ]

        | _ -> next e1 e2
      );

    print = (fun next fmt e ->
        match ekind e with
        | E_unop(O_cast, ee) -> fprintf fmt "cast(%a, %a)" pp_typ e.etyp pp_expr ee
        | E_unop(op, e) -> fprintf fmt "%a(%a)" pp_operator op pp_expr e
        | E_binop(op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_operator op pp_expr e2
        | _ -> next fmt e
      );
  }


(** Utility functions *)

let mk_expr
    ?(etyp = T_any)
    ?(eprev = None)
    ekind
    erange
  =
  {ekind; etyp; erange; eprev}

let mk_var v ?(mode = None) erange =
  mk_expr ~etyp:v.vtyp (E_var(v, mode)) erange

let weaken_var_expr v =
  match ekind v with
  | E_var (vv, Some WEAK)                 -> v
  | E_var (vv, None) when vv.vmode = WEAK -> v
  | E_var (vv, _)                         -> { v with ekind = E_var (vv,Some WEAK) }
  | _ -> assert false

let strongify_var_expr v =
  match ekind v with
  | E_var (vv,Some STRONG)                 -> v
  | E_var (vv,None) when vv.vmode = STRONG -> v
  | E_var (vv, _)                          -> { v with ekind = E_var (vv,Some STRONG) }
  | _ -> assert false


let var_mode (v:var) (omode: mode option) : mode =
  match omode with
  | None   -> v.vmode
  | Some m -> m

let mk_binop ?(etyp = T_any) left op right erange =
  mk_expr (E_binop (op, left, right)) ~etyp erange

let mk_unop ?(etyp = T_any) op operand erange =
  mk_expr (E_unop (op, operand)) ~etyp erange

let mk_constant ?(etyp=T_any) c = mk_expr ~etyp (E_constant c)

let mk_top typ range = mk_constant (C_top typ) ~etyp:typ range

let mk_not e range = mk_unop O_log_not e ~etyp:e.etyp range

module ExprSet = SetExt.Make(struct
    type t = expr
    let compare = compare_expr
  end)

module ExprMap = MapExt.Make(struct
    type t = expr
    let compare = compare_expr
  end)
