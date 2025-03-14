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

(** Extensible type of statements *)

open Mopsa_utils
open Program
open Expr
open Format


type stmt_kind = ..

type stmt = {
  skind : stmt_kind;
  srange : Location.range;
}

type stmt_kind +=
  | S_program of program * string list option
  | S_assign of expr (** lhs *) * expr (** rhs *)
  | S_assume of expr (** condition *)
  | S_add of expr
  | S_remove of expr
  | S_invalidate of expr
  | S_rename of expr (** old *) * expr (** new *)
  | S_forget of expr
  | S_project of expr list
  | S_expand of expr * expr list
  | S_fold of expr * expr list
  | S_block of stmt list * Var.var list
  | S_breakpoint of string

type block = stmt list

let skind (stmt: stmt) = stmt.skind
let srange (stmt: stmt) = stmt.srange

let stmt_compare_chain = TypeExt.mk_compare_chain (fun s1 s2 ->
    match skind s1, skind s2 with
    | S_assign(x1, e1), S_assign(x2, e2) ->
      Compare.compose [
        (fun () -> compare_expr x1 x2);
        (fun () -> compare_expr e1 e2);
      ]

    | S_assume(e1), S_assume(e2) -> compare_expr e1 e2

    | S_rename(e1, e1'), S_rename(e2, e2') ->
      Compare.compose [
        (fun () -> compare_expr e1 e2);
        (fun () -> compare_expr e1' e2');
      ]

    | S_remove(e1), S_remove(e2) -> compare_expr e1 e2

    | S_invalidate(e1), S_invalidate(e2) -> compare_expr e1 e2

    | S_add(e1), S_add(e2) -> compare_expr e1 e2

    | S_project(el1), S_project(el2) -> Compare.list compare_expr el1 el2

    | S_expand(e, el), S_expand(e', el') ->
      Compare.compose [
        (fun () -> compare_expr e e');
        (fun () -> Compare.list compare_expr el el')
      ]

    | S_fold(e, el), S_fold(e', el') ->
      Compare.compose [
        (fun () -> compare_expr e e');
        (fun () -> Compare.list compare_expr el el')
      ]

    | S_breakpoint b1, S_breakpoint b2 -> String.compare b1 b2

    | _ -> Stdlib.compare s1 s2
  )

let compare_stmt s1 s2 =
  TypeExt.compare stmt_compare_chain s1 s2

let stmt_pp_chain = TypeExt.mk_print_chain (fun fmt stmt ->
    match skind stmt with
    | S_program (prog,args) -> pp_program fmt prog

    | S_remove(e) -> fprintf fmt "remove(%a)" pp_expr e

    | S_invalidate(e) -> fprintf fmt "invalidate(%a)" pp_expr e

    | S_add(e) -> fprintf fmt "add(%a)" pp_expr e

    | S_forget(e) -> fprintf fmt "forget(%a)" pp_expr e

    | S_project(el) ->
      fprintf fmt "project(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) el

    | S_rename(e, e') -> fprintf fmt "rename(%a, %a)" pp_expr e pp_expr e'

    | S_assign(x, e) -> fprintf fmt "%a = %a;" pp_expr x pp_expr e

    | S_assume(e) -> fprintf fmt "assume(%a)" pp_expr e

    | S_expand(e, el) ->
      fprintf fmt "expand(%a,{%a})"
        pp_expr e
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ")
           pp_expr) el

    | S_fold(e, el) ->
      fprintf fmt "fold(%a,{%a})"
        pp_expr e
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ")
           pp_expr) el

    | S_breakpoint b ->
      fprintf fmt "breakpoint(%s)" b

    | _ -> failwith "Pp: Unknown statement"
  )


let pp_stmt fmt stmt =
  Var.force_print_uniq_with_uid false (fun () -> TypeExt.print stmt_pp_chain fmt stmt)

let pp_stmt_with_range fmt stmt =
  Var.force_print_uniq_with_uid false (fun () -> Format.fprintf fmt "%a@%a" (TypeExt.print stmt_pp_chain) stmt Location.pp_range stmt.srange)

let pp_block fmt (block:block) =
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") pp_stmt fmt block;
  fprintf fmt "@]"

let register_stmt info =
  TypeExt.register info stmt_compare_chain stmt_pp_chain

let register_stmt_compare cmp = TypeExt.register_compare cmp stmt_compare_chain

let register_stmt_pp pp = TypeExt.register_print pp stmt_pp_chain

let () = register_stmt {
    print = (fun next fmt stmt ->
        match skind stmt with
        | S_block([],_) -> fprintf fmt "pass"
        | S_block([s],_) -> pp_stmt fmt s
        | S_block(l,_) ->
          fprintf fmt "@[<v>";
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            pp_stmt
            fmt l
          ;
          fprintf fmt "@]"
        | _ -> next fmt stmt);
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_block(sl1,vl1), S_block(sl2,vl2) ->
          Compare.pair (Compare.list compare_stmt) (Compare.list Var.compare_var) (sl1,vl1) (sl2,vl2)
        | _ -> next s1 s2
      )
  }

let mk_stmt skind srange = {skind; srange}

let mk_rename v v' =
  mk_stmt (S_rename (v, v'))

let mk_assign v e =
  mk_stmt (S_assign (v, e))

let mk_assume e =
  mk_stmt (S_assume e)

let mk_remove v = mk_stmt (S_remove v)

let mk_remove_var v range =
  mk_remove (mk_var v range) range

let mk_invalidate v = mk_stmt (S_invalidate v)

let mk_invalidate_var v range =
  mk_invalidate (mk_var v range) range

let mk_add v = mk_stmt (S_add v)

let mk_add_var v range =
  mk_add (mk_var v range) range

let mk_rename e e' range =
  mk_stmt (S_rename (e, e')) range

let mk_rename_var v v' range =
  mk_rename (mk_var v range) (mk_var v' range) range

let mk_project vars = mk_stmt (S_project vars)

let mk_project_vars vars range =
  mk_project (List.map (fun v -> mk_var v range) vars) range

let mk_forget e = mk_stmt (S_forget e)

let mk_forget_var v range = mk_forget (mk_var v range) range

let mk_expand v vl range =
  mk_stmt (S_expand(v, vl)) range

let mk_expand_var v vl range =
  mk_expand
    (mk_var v range)
    (List.map (fun v' -> mk_var v' range) vl)
    range

let mk_fold v vl range =
  mk_stmt (S_fold(v, vl)) range

let mk_fold_var v vl range =
  mk_fold
    (mk_var v range)
    (List.map (fun v' -> mk_var v' range) vl)
    range

let mk_breakpoint b range =
  mk_stmt (S_breakpoint b) range

module StmtSet = SetExt.Make(struct
    type t = stmt
    let compare = compare_stmt
  end)

module StmtMap = MapExt.Make(struct
    type t = stmt
    let compare = compare_stmt
  end)
