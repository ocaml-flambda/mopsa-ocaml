(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Extensible Abstract Syntax Tree. *)


open Format


(** Information record on an AST construct (types, variables, ... etc) *)
type 'a info = {
  compare : ('a -> 'a -> int) -> 'a -> 'a -> int;
  print : (formatter -> 'a -> unit) -> formatter -> 'a -> unit;
}


(*==========================================================================*)
(**                           {2 Programs}                                  *)
(*==========================================================================*)

type program = ..

let program_pp_chain : (formatter -> program -> unit) ref =
  ref (fun fmt prg ->
      Exceptions.panic "program_pp_chain: unknown program"
    )

let program_compare_chain : (program -> program -> int) ref =
  ref (fun p1 p2 ->
      compare p1 p2
    )

let register_program (info: program info) =
  program_compare_chain := info.compare !program_compare_chain;
  program_pp_chain := info.print !program_pp_chain;
  ()

let register_program_pp pp =
  program_pp_chain := pp !program_pp_chain

let compare_program p1 p2 = !program_compare_chain p1 p2

let pp_program fmt program = !program_pp_chain fmt program


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

(* Processing chain for the extensible type [Ast.stmt] *)
let typ_pp_chain : (Format.formatter -> typ -> unit) ref =
  ref (fun fmt typ ->
      match typ with
      | T_any -> Format.pp_print_string fmt "?"
      | _ -> Exceptions.panic "typ_pp_chain: unknown type"
    )

let register_typ (info: typ info) : unit =
  typ_compare_chain := info.compare !typ_compare_chain;
  typ_pp_chain := info.print !typ_pp_chain;
  ()

let register_typ_compare cmp =
  typ_compare_chain := cmp !typ_compare_chain

let register_typ_pp pp =
  typ_pp_chain := pp !typ_pp_chain

let compare_typ t1 t2 = !typ_compare_chain t1 t2

let pp_typ fmt typ = !typ_pp_chain fmt typ


(*==========================================================================*)
(**                           {2 Variables}                                 *)
(*==========================================================================*)


(** variables *)
type var = {
  vname : string;
  vuid  : int;         (** unique identifier. *)
  vtyp  : typ;         (** type of the variable. *)
}

let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
    (fun () -> compare_typ v1.vtyp v2.vtyp);
  ]

let pp_var fmt v =
  if v.vname = "$tmp" then
    Format.fprintf fmt "%s_%d" v.vname v.vuid
  else
    pp_print_string fmt v.vname

let vtyp v = v.vtyp

let uniq_vname v = v.vname ^ ":" ^ (string_of_int v.vuid)

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


let operator_compare_chain : (operator -> operator -> int) ref = ref (fun o1 o2 ->
    compare o1 o2
  )

let operator_pp_chain : (Format.formatter -> operator -> unit) ref =
  ref (fun fmt op ->
      match op with
      | O_lt -> pp_print_string fmt "<"
      | O_le -> pp_print_string fmt "<="
      | O_gt -> pp_print_string fmt ">"
      | O_ge -> pp_print_string fmt ">="
      | O_eq -> pp_print_string fmt "=="
      | O_ne -> pp_print_string fmt "!="
      | O_log_or -> pp_print_string fmt "or"
      | O_log_and -> pp_print_string fmt "and"
      | O_log_not -> pp_print_string fmt "not"
      | _ -> Exceptions.panic "operator_pp_chain: unknown operator"
    )


let register_operator (info: operator info) : unit =
  operator_compare_chain := info.compare !operator_compare_chain;
  operator_pp_chain := info.print !operator_pp_chain;
  ()

let register_operator_compare cmp =
  operator_compare_chain := cmp !operator_compare_chain

let register_operator_pp pp =
  operator_pp_chain := pp !operator_pp_chain

let compare_operator o1 o2 = !operator_compare_chain o1 o2

let pp_operator fmt operator = !operator_pp_chain fmt operator


(*==========================================================================*)
(**                            {2 Constants}                                *)
(*==========================================================================*)


type constant = ..
(** Extensible type of constants. *)

type constant +=
  | C_top of typ (** top value of a specific type *)

let constant_compare_chain : (constant -> constant -> int) ref = ref (fun c1 c2 ->
    match c1, c2 with
    | C_top t1, C_top t2 -> compare_typ t1 t2
    | _ -> compare c1 c2
  )

let constant_pp_chain : (Format.formatter -> constant -> unit) ref =
  ref (fun fmt c ->
      match c with
      | C_top T_any -> fprintf fmt "⊤"
      | C_top t -> fprintf fmt "⊤:%a" pp_typ t
      | _ -> Exceptions.panic "constant_pp_chain: unknown constant"
    )


let register_constant (info: constant info) : unit =
  constant_compare_chain := info.compare !constant_compare_chain;
  constant_pp_chain := info.print !constant_pp_chain;
  ()

let register_constant_compare cmp =
  constant_compare_chain := cmp !constant_compare_chain

let register_constant_pp pp =
  constant_pp_chain := pp !constant_pp_chain


let compare_constant o1 o2 = !constant_compare_chain o1 o2

let pp_constant fmt constant = !constant_pp_chain fmt constant


(*==========================================================================*)
(**                           {2 Expressions}                               *)
(*==========================================================================*)

type expr_kind = ..

type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: Location.range;
}


(** Mode of a variable expression *)
type mode =
  | STRONG
  | WEAK

let compare_mode = compare

let pp_mode fmt mode =
  match mode with
  | STRONG -> Format.fprintf fmt "STRONG"
  | WEAK   -> Format.fprintf fmt "WEAK"

(** Some basic expressions *)
type expr_kind +=
  | E_var of var * mode
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
          (fun () -> compare_mode s1 s2)
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

and compare_expr e1 e2 =
  if e1 == e2 then 0 else !expr_compare_chain e1 e2

let rec expr_pp_chain : (Format.formatter -> expr -> unit) ref =
  ref (fun fmt expr ->
      match ekind expr with
      | E_constant c -> pp_constant fmt c
      | E_var(v, STRONG) -> pp_var fmt v
      | E_var(v, WEAK) -> Format.fprintf fmt "_w_%a" pp_var v
      | E_unop(op, e) -> fprintf fmt "%a (%a)" pp_operator op pp_expr e
      | E_binop(op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_operator op pp_expr e2
      | _ -> failwith "Pp: Unknown expression"
    )

and pp_expr fmt exp = !expr_pp_chain fmt exp

let register_expr_pp pp =
  expr_pp_chain := pp !expr_pp_chain

let register_expr_compare cmp =
  expr_compare_chain := cmp !expr_compare_chain


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

  | S_add_var of var
  (** Add a variable to the abstract environments. *)

  | S_remove_var of var
  (** Remove a variable from the abstract environments. *)

  | S_project_vars of var list
  (** Project the abstract environments on the given list of variables. *)

  | S_expand of var * var list
  (** Expands the first variable into the list of variables, the first
      variable is removed from the environment *)

  | S_fold of var * var list
  (** Folds the the list of variables into the first variable, the
      list of variable is then removed from the environment *)


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

      | S_add_var(v1), S_add_var(v2) -> compare_var v1 v2

      | S_project_vars(vl1), S_project_vars(vl2) ->
        Compare.compose (
          (fun () -> Pervasives.compare (List.length vl1) (List.length vl2))
          ::
          (List.map (fun (v1, v2) -> (fun () -> compare_var v1 v2)) @@ List.combine vl1 vl2)
        )
      | S_expand(v, vl), S_expand(v', vl') ->
        Compare.compose [
          (fun () -> compare_var v v');
          (fun () -> Compare.list compare_var vl vl')
        ]

      | S_fold(v, vl), S_fold(v', vl') ->
        Compare.compose [
          (fun () -> compare_var v v');
          (fun () -> Compare.list
              compare_var vl vl')
        ]

      | _ -> Pervasives.compare s1 s2
    )

and compare_stmt s1 s2 =
  if s1 == s2 then 0 else !stmt_compare_chain s1 s2

let stmt_pp_chain : (Format.formatter -> stmt -> unit) ref =
  ref (fun fmt stmt ->
      match skind stmt with
      | S_program prog -> pp_program fmt prog
      | S_expand(v, vl) ->
        fprintf fmt "expand(%a,{%a})"
          pp_var v
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",")
             pp_var) vl
      | S_fold(v, vl) ->
        fprintf fmt "fold(%a,{%a})"
          pp_var v
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ",")
             pp_var) vl

      | _ -> failwith "Pp: Unknown statement"
    )


let pp_stmt fmt stmt = !stmt_pp_chain fmt stmt

let register_stmt_pp pp =
  stmt_pp_chain := pp !stmt_pp_chain

let register_stmt_compare cmp =
  stmt_compare_chain := cmp !stmt_compare_chain


(*==========================================================================*)
(**                            {2 Visitors}                                 *)
(*==========================================================================*)


(** Parts are the direct sub-elements of an AST node *)
type parts = {
  exprs : expr list; (** child expressions *)
  stmts : stmt list; (** child statements *)
}

(** A structure of an extensible type ['a] is a tuple composed of two elements:
    the parts and a builder function.
*)
type 'a structure = parts * (parts -> 'a)

let leaf (x: 'a) : 'a structure =
  {exprs = []; stmts = []}, (fun _ -> x)



(** Information record of an AST construct with visitors *)
type 'a info2 = {
  compare : ('a -> 'a -> int) -> 'a -> 'a -> int;
  print   : (formatter -> 'a -> unit) -> formatter -> 'a -> unit;
  visit : ('a -> 'a structure) -> 'a -> 'a structure;
}


let expr_visit_chain = ref (fun exp ->
    match ekind exp with
    | E_var _ -> leaf exp
    | E_constant _ -> leaf exp
    | E_unop(unop, e) ->
      {exprs = [e]; stmts = []},
      (fun parts -> {exp with ekind = E_unop(unop, List.hd parts.exprs)})
    | E_binop(binop, e1, e2) ->
        {exprs = [e1; e2]; stmts = []},
        (fun parts -> {exp with ekind = E_binop(binop, List.hd parts.exprs, List.nth parts.exprs 1)})
    | _ ->
      Exceptions.panic "Unknown expression %a" pp_expr exp
  )

let register_expr (info: expr info2) : unit =
  expr_compare_chain := info.compare !expr_compare_chain;
  expr_pp_chain := info.print !expr_pp_chain;
  expr_visit_chain := info.visit !expr_visit_chain;
  ()

let register_expr_visitor visitor =
  expr_visit_chain := visitor !expr_visit_chain

let stmt_visit_chain : (stmt -> stmt structure) ref =
  ref (fun stmt ->
      match skind stmt with
      | S_program _ -> Exceptions.panic "visitor of S_program not supported"

      | S_assign(lhs, rhs) -> {
          exprs = [lhs; rhs];
          stmts = []
        } , (
            function
            | { exprs = [lhs; rhs] } -> { stmt with skind = S_assign(lhs, rhs) }
            | _ -> assert false
          )

      | S_assume cond ->  {
          exprs = [cond];
          stmts = []
        } , (
            function
            | { exprs = [cond] } -> { stmt with skind = S_assume(cond) }
            | _ -> assert false
          )

      | S_rename_var _
      | S_add_var _
      | S_remove_var _
      | S_project_vars _
      | S_expand _
      | S_fold _ -> leaf stmt

      | _ -> Exceptions.panic "stmt_visit_chain: unknown statement"
    )

let register_stmt (info: stmt info2) : unit =
  stmt_compare_chain := info.compare !stmt_compare_chain;
  stmt_pp_chain := info.print !stmt_pp_chain;
  stmt_visit_chain := info.visit !stmt_visit_chain;
  ()

let register_stmt_visitor visitor =
  stmt_visit_chain := visitor !stmt_visit_chain



(*==========================================================================*)
(**                       {2 Utility functions}                             *)
(*==========================================================================*)

let mkv vname vuid vtyp =
  {vname; vuid; vtyp}

let vcounter = ref 0

let mkfresh f vtyp () =
  incr vcounter;
  mkv (f !vcounter) !vcounter vtyp

let mktmp vtyp () =
  mkfresh (fun _ -> "$tmp") vtyp ()

(** deprecated function; use mktmp instead *)
let mk_tmp ?(vtyp=T_any) () =
  mktmp vtyp ()

let mk_expr
    ?(etyp = T_any)
    ekind
    erange
  =
  {ekind; etyp; erange}

let mk_var v ?(mode = STRONG) erange =
  mk_expr ~etyp:v.vtyp (E_var(v, mode)) erange

let mk_binop left op right ?(etyp = T_any) erange =
  mk_expr (E_binop (op, left, right)) ~etyp erange

let mk_unop op operand ?(etyp = T_any) erange =
  mk_expr (E_unop (op, operand)) ~etyp erange

let mk_constant ~etyp c = mk_expr ~etyp (E_constant c)

let mk_top typ range = mk_constant (C_top typ) ~etyp:typ range

let mk_not e = mk_unop O_log_not e ~etyp:e.etyp

let mk_stmt skind srange = {skind; srange}

let mk_rename v v' =
  mk_stmt (S_rename_var (v, v'))

let mk_assign v e =
  mk_stmt (S_assign (v, e))

let mk_expand v vl range =
  mk_stmt (S_expand(v, vl)) range

let mk_assume e =
  mk_stmt (S_assume e)

let mk_remove_var v = mk_stmt (S_remove_var v)

let mk_add_var v = mk_stmt (S_add_var v)

let mk_project_vars vars = mk_stmt (S_project_vars vars)

(* Utility to negate the comparisons in framework *)
let negate_comparison = function
  | O_eq -> O_ne
  | O_ne -> O_eq
  | O_lt -> O_ge
  | O_le -> O_gt
  | O_gt -> O_le
  | O_ge -> O_lt
  | op -> Exceptions.panic "don't know how to negate operator %a" pp_operator op
