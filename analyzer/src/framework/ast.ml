(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Extensible Abstract Syntax Tree. *)

(*==========================================================================*)
(**                             {2 Types}                                   *)
(*==========================================================================*)

type typ = ..
(** Extensible type of expression types. *)

(** Basic types *)
type typ +=
  | T_any (** Generic unknown type. *)


let typ_compare_chain : (typ -> typ -> int) ref = ref (fun t1 t2 ->
    match t1, t2 with
    | T_any, T_any -> 0
    | _ -> compare t1 t2
  )

let register_typ_compare cmp =
  typ_compare_chain := cmp !typ_compare_chain

let compare_typ t1 t2 = !typ_compare_chain t1 t2




(*==========================================================================*)
(**                           {2 Variables}                                 *)
(*==========================================================================*)

type var = {
  vname : string;  (** original name of the variable. *)
  vuid  : int;     (** unique identifier. *)
  vtyp  : typ;     (** type of the variable. *)
}
(** variables *)

(** Compare two variables *)
let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
    (fun () -> compare_typ v1.vtyp v2.vtyp);
  ]

let tmp_counter = ref 100

(** Create a temporary variable with a unique name. *)
let mktmp ?(vtyp = T_any) () =
  incr tmp_counter;
  let vname = "$tmp" ^ (string_of_int !tmp_counter) in
  {vname; vuid = !tmp_counter; vtyp}





(*==========================================================================*)
(**                            {2 Expressions}                              *)
(*==========================================================================*)


type operator = ..
(** Extensible type of operators (unary, binary, etc.). *)

(** Basic operators *)
type operator +=
  | O_plus       (** + *)
  | O_minus      (** - *)
  | O_mult       (** * *)
  | O_div        (** / *)
  | O_mod        (** % *)

  | O_eq         (** == *)
  | O_ne         (** != *)
  | O_lt         (** < *)
  | O_le         (** <= *)
  | O_gt         (** > *)
  | O_ge         (** >= *)

  | O_log_not    (** Logical negation *)
  | O_log_or     (** || *)
  | O_log_and    (** && *)


type constant = ..
(** Extensible type of constants. *)

type constant +=
  | C_top of typ (** top value of a specific type *)

type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: Location.range;
}

(** Extensible type of expressions. *)
and expr_kind = ..

(** Some basic expressions *)
type expr_kind +=
  | E_var of var
  (** variables *)

  | E_constant of constant
  (** constants *)

  | E_unop of operator * expr
  (** unary operator expressions *)

  | E_binop of operator * expr * expr
  (** binary operator expressions *)



let ekind (e: expr) = e.ekind
let etyp (e: expr) = e.etyp
let erange (e: expr) = e.erange
let mk_expr
    ?(etyp = T_any)
    ekind
    erange
  =
  {ekind; etyp; erange}

let mk_var v erange =
  mk_expr ~etyp:v.vtyp (E_var v) erange

let mk_binop left op right ?(etyp = T_any) erange =
  mk_expr (E_binop (op, left, right)) ~etyp erange

let mk_unop op operand ?(etyp = T_any) erange =
  mk_expr (E_unop (op, operand)) ~etyp erange

let mk_constant ~etyp c = mk_expr ~etyp (E_constant c)

let mk_top typ range = mk_constant (C_top typ) ~etyp:typ range

let mk_not e = mk_unop O_log_not e ~etyp:e.etyp

(*==========================================================================*)
                        (** {2 Programs} *)
(*==========================================================================*)
type program_kind = ..
(** Extensible type for describing analyzed programs. *)

type program = {
  prog_kind : program_kind;
  prog_file : string;
}

(*==========================================================================*)
                     (**      {2 Statements}      *)
(*==========================================================================*)

type stmt_kind = ..
(** Extensible statements kinds. *)


(** Mode of an assignment *)
type assign_mode =
  | STRONG
  | WEAK
  | EXPAND


(** Basic statements *)
type stmt_kind +=
  | S_program of program
  (** Program to be analyzed *)

  | S_assign of expr (** lhs *) * expr (** rhs *) * assign_mode (** assignment mode *)
  (** Assignments *)

  | S_assume of expr (** condition *)

  | S_rename_var of var (** old *) * var (** new *)
  (** Rename a variable into another*)

  | S_remove_var of var
  (** Remove a variable from the abstract environments. *)

  | S_project_vars of var list
  (** Project the abstract environments on the given list of variables. *)


type stmt = {
  skind : stmt_kind; (** Kind of the statement. *)
  srange : Location.range; (** Location range of the statement. *)
}
(** Statements with their kind and range. *)

let skind (stmt: stmt) = stmt.skind
let srange (stmt: stmt) = stmt.srange
let mk_stmt skind srange = {skind; srange}

let mk_rename v v' =
  mk_stmt (S_rename_var (v, v'))

let mk_assign ?(mode = STRONG)v e =
  mk_stmt (S_assign (v, e, mode))

let mk_assume e =
  mk_stmt (S_assume e)

let mk_remove_var v = mk_stmt (S_remove_var v)

let mk_project_vars vars = mk_stmt (S_project_vars vars)





(*==========================================================================*)
(**                      {2 Pretty printers}                                *)
(*==========================================================================*)

open Format

let pp_var fmt v = fprintf fmt "%s@%d" v.vname v.vuid

(* Processing chain for the extensible type [Ast.expr] *)
let rec pp_expr_chain : (Format.formatter -> expr -> unit) ref =
  ref (fun fmt expr ->
      match ekind expr with
      | E_constant c -> pp_constant fmt c
      | E_var(v) -> pp_var fmt v
      | E_unop(op, e) -> fprintf fmt "%a (%a)" pp_operator op pp_expr e
      | E_binop(op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_operator op pp_expr e2
      | _ -> failwith "Pp: Unknown expression"
    )

(* Processing chain for the extensible type [Ast.stmt] *)
and pp_stmt_chain : (Format.formatter -> stmt -> unit) ref =
  ref (fun fmt stmt ->
      match skind stmt with
      | S_program prog -> pp_program fmt prog
      | _ -> failwith "Pp: Unknown statement"
    )

(* Processing chain for the extensible type [Ast.program] *)
and pp_program_chain : (Format.formatter -> program -> unit) ref =
  ref (fun fmt prg ->
      failwith "Pp: Unknown program"
    )

(* Processing chain for the extensible type [Ast.stmt] *)
and pp_typ_chain : (Format.formatter -> typ -> unit) ref =
  ref (fun fmt typ ->
      match typ with
      | T_any -> fprintf fmt "?"
      | _ -> failwith "Pp: Unknown type"
    )

(* Processing chain for the extensible type [Ast.operator] *)
and pp_operator_chain : (Format.formatter -> operator -> unit) ref =
  ref (fun fmt op ->
      failwith "Pp: Unknown operator"
    )

(* Processing chain for the extensible type [Ast.constant] *)
and pp_constant_chain : (Format.formatter -> constant -> unit) ref =
  ref (fun fmt c ->
      match c with
      | C_top T_any -> fprintf fmt "⊤"
      | C_top t -> fprintf fmt "⊤:%a" pp_typ t
      | _ -> failwith "Pp: Unknown constant"
    )



(* To register a new pp, we just give the previous chain as argument to pp
 * so that it can call the chain when it can not handle the given case
 *)

and register_pp_expr pp = pp_expr_chain := pp !pp_expr_chain
and register_pp_stmt pp = pp_stmt_chain := pp !pp_stmt_chain
and register_pp_program pp = pp_program_chain := pp !pp_program_chain
and register_pp_typ pp = pp_typ_chain := pp !pp_typ_chain
and register_pp_operator pp = pp_operator_chain := pp !pp_operator_chain
and register_pp_constant pp = pp_constant_chain := pp !pp_constant_chain


(* These functions start the chain processing *)
and pp_expr fmt expr = !pp_expr_chain fmt expr
and pp_stmt fmt stmt = !pp_stmt_chain fmt stmt
and pp_program fmt prg = !pp_program_chain fmt prg
and pp_typ fmt typ = !pp_typ_chain fmt typ
and pp_operator fmt op = !pp_operator_chain fmt op
and pp_constant fmt c = !pp_constant_chain fmt c
