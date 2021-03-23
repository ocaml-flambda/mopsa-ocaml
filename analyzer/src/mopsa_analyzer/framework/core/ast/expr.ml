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


open Mopsa_utils
open Location
open Typ
open Program
open Operator
open Constant
open Var
open Addr
open Format
open Semantic


type expr_kind = ..

type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: Location.range;
  etrans: expr SemanticMap.t;
  ehistory: expr list;
}


let ekind (e: expr) = e.ekind
let etyp (e: expr) = e.etyp
let erange (e: expr) = e.erange
let etrans (e:expr) = e.etrans
let ehistory (e:expr) = e.ehistory

type expr_kind +=
  | E_var of var * mode option
  | E_constant of constant
  | E_unop of operator * expr
  | E_binop of operator * expr * expr
  | E_addr of addr * mode option
  | E_alloc_addr of addr_kind * mode

let expr_compare_chain = TypeExt.mk_compare_chain (fun e1 e2 ->
      match ekind e1, ekind e2 with
      | E_var(v1, m1), E_var(v2, m2) ->
        Compare.compose [
          (fun () -> compare_var v1 v2);
          (fun () -> Compare.option compare_mode m1 m2)
        ]
      | E_constant c1, E_constant c2 -> compare_constant c1 c2

      | E_alloc_addr(ak1, m1), E_alloc_addr(ak2, m2) ->
        Compare.compose [
          (fun () -> compare_addr_kind ak1 ak2);
          (fun () -> compare_mode m1 m2);
        ]

      | E_addr(a1, om1), E_addr(a2, om2) ->
        Compare.compose
          [
            (fun () -> compare_addr a1 a2);
            (fun () -> Compare.option compare_mode om1 om2);
          ]

      | _ -> Stdlib.compare (ekind e1) (ekind e2)
  )

let expr_pp_chain = TypeExt.mk_print_chain (fun fmt expr ->
    match ekind expr with
    | E_constant c -> pp_constant fmt c
    | E_var(v, None) -> pp_var fmt v
    | E_var(v, Some STRONG) -> Format.fprintf fmt "strong(%a)" pp_var v
    | E_var(v, Some WEAK) -> Format.fprintf fmt "weak(%a)" pp_var v
    | E_alloc_addr(akind, mode) -> fprintf fmt "alloc(%a, %a)" pp_addr_kind akind pp_mode mode
    | E_addr (addr, None) -> fprintf fmt "%a" pp_addr addr
    | E_addr (addr, Some STRONG) -> fprintf fmt "strong(%a)" pp_addr addr
    | E_addr (addr, Some WEAK) -> fprintf fmt "weak(%a)" pp_addr addr
    | _ -> failwith "Pp: Unknown expression"
  )

let register_expr info = TypeExt.register info expr_compare_chain expr_pp_chain

let register_expr_compare cmp = TypeExt.register_compare cmp expr_compare_chain

let register_expr_pp pp = TypeExt.register_print pp expr_pp_chain

let compare_expr e1 e2 = TypeExt.compare expr_compare_chain e1 e2

let pp_expr fmt e = TypeExt.print expr_pp_chain fmt e

let add_expr_translation semantic t e =
  { e with etrans = SemanticMap.add semantic t e.etrans }

let get_expr_translations e = e.etrans

let get_expr_translation semantic e =
  if is_any_semantic semantic then e
  else
    try SemanticMap.find semantic e.etrans
    with Not_found ->
      (* XXX We assume that an expression is translated to itself if no
         translation exists. This is a temporary fix for situations where
         domains don't add appropriate entires in the translation table. *)
      e

let get_expr_history e = e.ehistory

let get_orig_expr e =
  match e.ehistory with
  | [] -> e
  | _  -> ListExt.last e.ehistory

let find_expr_ancestor f e =
  let rec iter = function
    | [] -> raise Not_found
    | hd::tl -> if f hd then hd else iter tl
  in
  iter e.ehistory

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
    ?(etrans = SemanticMap.empty)
    ?(ehistory = [])
    ekind erange
  =
  {ekind; etyp; erange; etrans; ehistory}

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

let mk_addr addr ?(etyp=T_addr) ?(mode=None) range = mk_expr ~etyp (E_addr (addr, mode)) range

let mk_alloc_addr ?(mode=STRONG) addr_kind range =
  mk_expr (E_alloc_addr (addr_kind, mode)) ~etyp:T_addr range

let weaken_addr_expr e =
  match ekind e with
  | E_addr ({addr_mode = WEAK}, _) -> e
  | E_addr (addr, om) -> {e with ekind = E_addr ({addr with addr_mode = WEAK}, om)}
  | _ -> assert false

let strongigy_addr_expr e =
  match ekind e with
  | E_addr ({addr_mode = STRONG}, _) -> e
  | E_addr (addr, om) -> {e with ekind = E_addr ({addr with addr_mode = STRONG}, om)}
  | _ -> assert false

let mk_binop ?(etyp = T_any) left op right erange =
  mk_expr (E_binop (op, left, right)) ~etyp erange

let mk_unop ?(etyp = T_any) op operand erange =
  mk_expr (E_unop (op, operand)) ~etyp erange

let mk_constant ?(etyp=T_any) c = mk_expr ~etyp (E_constant c)

let mk_top typ range = mk_constant (C_top typ) ~etyp:typ range

let mk_not e range = mk_unop O_log_not e ~etyp:e.etyp range

let rec negate_expr exp =
  match ekind exp with
  | E_unop(O_log_not,ee) -> ee
  | E_binop(op,e1,e2) when is_logic_op op ->
    mk_binop (negate_expr e1) (negate_logic_op op) (negate_expr e2) ~etyp:exp.etyp exp.erange
  | E_binop(op,e1,e2) when is_comparison_op op ->
    mk_binop e1 (negate_comparison_op op) e2 ~etyp:exp.etyp exp.erange
  | _ -> mk_not exp exp.erange


module ExprSet = SetExt.Make(struct
    type t = expr
    let compare = compare_expr
  end)

module ExprMap = MapExt.Make(struct
    type t = expr
    let compare = compare_expr
  end)
