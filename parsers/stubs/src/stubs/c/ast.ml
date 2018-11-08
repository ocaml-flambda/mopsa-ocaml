(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type loc = {
    file: string;
    line: int;
    col: int;
  }

type range = loc * loc

type 'a with_range = 'a * range

type stub =
  | S_simple of simple_stub
  | S_multi of multi_stub

and simple_stub = {
  simple_predicates : predicate with_range list;
  simple_requires   : requires with_range list;
  simple_assigns    : assigns with_range list;
  simple_local      : local with_range list;
  simple_ensures    : ensures with_range list;
}

and multi_stub = {
  multi_predicates : predicate with_range list;
  multi_requires   : requires with_range list;
  multi_cases      : case with_range list;
}

and requires = formula with_range
and ensures = formula with_range
and assumes = formula with_range

and local = {
    local_var : var;
    local_value : local_value;
  }

and local_value =
  | Local_new           of resource
  | Local_function_call of var (** function *) * expr with_range list (* arguments *)

and assigns = {
    assigns_target : expr with_range;
    assigns_range  : (expr with_range * expr with_range) option;
  }

and case = {
    case_label: string;
    case_assumes: assumes with_range list;
    case_requires : requires with_range list;
    case_assigns  : assigns with_range list;
    case_local    : local with_range list;
    case_ensures  : ensures with_range list;
  }

and formula =
  | F_expr   of expr with_range
  | F_bool   of bool
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of var * set
  | F_free   of expr with_range

and expr =
  | E_int       of Z.t
  | E_float     of float
  | E_string    of string
  | E_char      of char

  | E_var       of var

  | E_unop      of unop  * expr with_range
  | E_binop     of binop * expr with_range * expr with_range

  | E_addr_of   of expr with_range
  | E_deref     of expr with_range
  | E_cast      of typ * expr with_range

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_return

and typ =
  | T_char
  | T_int
  | T_long
  | T_float
  | T_double
  | T_pointer of typ
  | T_signed of typ
  | T_unsigned of typ
  | T_const of typ
  | T_user of var
  | T_struct of var
  | T_union of var
  | T_predicate
  | T_unknown

and predicate = {
  predicate_var : var;
  predicate_body: formula with_range;
}

and log_binop =
  | AND
  | OR
  | IMPLIES

and binop =
  | ADD     (* + *)
  | SUB     (* - *)
  | MUL     (* * *)
  | DIV     (* / *)
  | MOD     (* % *)
  | RSHIFT  (* >> *)
  | LSHIFT  (* << *)
  | LOR     (* || *)
  | LAND    (* && *)
  | LT      (* < *)
  | LE      (* <= *)
  | GT      (* > *)
  | GE      (* >= *)
  | EQ      (* == *)
  | NEQ     (* != *)
  | BOR     (* | *)
  | BAND    (* & *)
  | BXOR    (* ^ *)

and unop =
  | PLUS    (* + *)
  | MINUS   (* - *)
  | LNOT    (* ! *)
  | BNOT    (* ~ *)

and set =
  | S_interval of expr with_range * expr with_range
  | S_resource of resource

and resource = string

and var = {
    var_name: string;
    var_uid: int;
    var_typ: typ;
  }

and builtin =
  | OLD
  | SIZE
  | OFFSET
  | BASE

let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.var_name v2.var_name);
    (fun () -> compare v1.var_uid v2.var_uid);
  ]

let without_range (a: 'a with_range) (f: 'a -> 'b) : 'b =
  f (fst a)

let bind_range (a: 'a with_range) (f: 'a -> 'b) : 'b with_range =
  let x, range = a in
  let y = f x in
  (y, range)
