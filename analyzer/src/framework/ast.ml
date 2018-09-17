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
  vname : string;      (** original name of the variable. *)
  vuid  : int;         (** unique identifier. *)
  vtyp  : typ;         (** type of the variable. *)
}
(** variables *)

(** Compare two variables *)
let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
    (fun () -> compare_typ v1.vtyp v2.vtyp);
  ]

let tmp_counter = ref 1

(** Create a temporary variable with a unique name. *)
let mk_tmp ?(vtyp = T_any) () =
  incr tmp_counter;
  let vname = "$tmp" ^ (string_of_int !tmp_counter) in
  {vname; vuid = !tmp_counter; vtyp}

let get_var_uniq_name v : string =
  v.vname ^ ":" ^ (string_of_int v.vuid)

(*==========================================================================*)
(**                            {2 Operators}                                *)
(*==========================================================================*)


type operator = ..
(** Extensible type of operators (unary, binary, etc.). *)

(** Basic operators *)
type operator +=
  | O_eq         (** == *)
  | O_ne         (** != *)
  | O_lt         (** < *)
  | O_le         (** <= *)
  | O_gt         (** > *)
  | O_ge         (** >= *)

  | O_log_not    (** Logical negation *)
  | O_log_or     (** || *)
  | O_log_and    (** && *)


let operator_compare_chain : (operator -> operator -> int) ref = ref Pervasives.compare
let register_operator_compare cmp = operator_compare_chain := cmp !operator_compare_chain
let compare_operator op1 op2 = !operator_compare_chain op1 op2


(*==========================================================================*)
(**                            {2 Constants}                                *)
(*==========================================================================*)


type constant = ..
(** Extensible type of constants. *)

type constant +=
  | C_top of typ (** top value of a specific type *)

let constant_compare_chain : (constant -> constant -> int) ref =
  ref (fun c1 c2 ->
      match c1, c2 with
      | C_top t1, C_top t2 -> compare_typ t1 t2
      | _ -> Pervasives.compare c1 c2
    )
let register_constant_compare cmp = constant_compare_chain := cmp !constant_compare_chain
let compare_constant op1 op2 = !constant_compare_chain op1 op2


(*==========================================================================*)
(**                           {2 Expressions}                               *)
(*==========================================================================*)



type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: Location.range;
}

(** Extensible type of expressions. *)
and expr_kind = ..


(** Strength of a variable*)
type strength =
  | STRONG
  | WEAK
let compare_strength = compare
let pp_strength fmt strength =
  match strength with
  | STRONG -> Format.fprintf fmt "STRONG"
  | WEAK   -> Format.fprintf fmt "WEAK"

(** Some basic expressions *)
type expr_kind +=
  | E_var of var * strength
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

let rec expr_compare_chain : (expr -> expr -> int) ref =
  ref (fun e1 e2 ->
      match ekind e1, ekind e2 with
      | E_var(v1, s1), E_var(v2, s2) ->
        Compare.compose [
          (fun () -> compare_var v1 v2);
          (fun () -> compare_strength s1 s2)
        ]
      | E_constant c1, E_constant c2 -> compare_constant c1 c2
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
      | _ -> Pervasives.compare e1 e2
    )

and register_expr_compare cmp = expr_compare_chain := cmp !expr_compare_chain
and compare_expr e1 e2 =
  if e1 == e2 then 0 else !expr_compare_chain e1 e2


let mk_expr
    ?(etyp = T_any)
    ekind
    erange
  =
  {ekind; etyp; erange}

let mk_var v ?(vstrength = STRONG) erange =
  mk_expr ~etyp:v.vtyp (E_var(v, vstrength)) erange

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

(** Basic statements *)
type stmt_kind +=
  | S_program of program
  (** Program to be analyzed *)

  | S_assign of expr (** lhs *) * expr (** rhs *)
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

let rec stmt_compare_chain : (stmt -> stmt -> int) ref =
  ref (fun s1 s2 ->
      match skind s1, skind s2 with
      | S_assign(x1, e1), S_assign(x2, e2) ->
        Compare.compose [
          (fun () -> compare_expr x1 x2);
          (fun () -> compare_expr e1 e2);
        ]

      | S_assume(e1), S_assume(e2) -> compare_expr e1 e2

      | S_rename_var(v1, v1'), S_rename_var(v2, v2') ->
        Compare.compose [
          (fun () -> compare_var v1 v2);
          (fun () -> compare_var v1' v2');
        ]

      | S_remove_var(v1), S_remove_var(v2) -> compare_var v1 v2

      | S_project_vars(vl1), S_project_vars(vl2) ->
        Compare.compose (
          (fun () -> Pervasives.compare (List.length vl1) (List.length vl2))
          ::
          (List.map (fun (v1, v2) -> (fun () -> compare_var v1 v2)) @@ List.combine vl1 vl2)
        )
      | _ -> Pervasives.compare s1 s2
    )

and register_stmt_compare cmp = stmt_compare_chain := cmp !stmt_compare_chain

and compare_stmt s1 s2 =
  if s1 == s2 then 0 else !stmt_compare_chain s1 s2


let mk_stmt skind srange = {skind; srange}

let mk_rename v v' =
  mk_stmt (S_rename_var (v, v'))

let mk_assign v e =
  mk_stmt (S_assign (v, e))

let mk_assume e =
  mk_stmt (S_assume e)

let mk_remove_var v = mk_stmt (S_remove_var v)

let mk_project_vars vars = mk_stmt (S_project_vars vars)


(*==========================================================================*)
(**                      {2 Pretty printers}                                *)
(*==========================================================================*)

open Format

let pp_var fmt v = fprintf fmt "%s" v.vname

(* Processing chain for the extensible type [Ast.expr] *)
let rec pp_expr_chain : (Format.formatter -> expr -> unit) ref =
  ref (fun fmt expr ->
      match ekind expr with
      | E_constant c -> pp_constant fmt c
      | E_var(v, STRONG) -> pp_var fmt v
      | E_var(v, WEAK) -> Format.fprintf fmt "_w_%a" pp_var v
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
      match op with
      | O_lt -> pp_print_string fmt "<"
      | O_le -> pp_print_string fmt "<="
      | O_gt -> pp_print_string fmt ">"
      | O_ge -> pp_print_string fmt ">="
      | O_eq -> pp_print_string fmt "=="
      | O_ne -> pp_print_string fmt "!="
      | O_log_or -> pp_print_string fmt "lor"
      | O_log_and -> pp_print_string fmt "land"
      | O_log_not -> pp_print_string fmt "lnot"
      | _ -> failwith "Pp: Unknown operator"
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
