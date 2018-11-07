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

type stub = {
    stub_requires : requires with_range list;
    stub_local    : local with_range list;
    stub_predicates : predicate with_range list;
    stub_assigns  : assigns with_range list;
    stub_case     : case with_range list;
    stub_ensures  : ensures with_range list;
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
    assign_target : expr with_range;
    assign_range  : (expr with_range * expr with_range) option;
  }

and case = {
    case_label: string;
    case_assumes: assumes with_range list;
    case_requires : requires with_range list;
    case_local    : local with_range list;
    case_assigns  : assigns with_range list;
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

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_return

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

and var = string

and builtin =
  | OLD
  | SIZE
  | OFFSET
  | BASE

let map_kind (a: 'a with_range) (f: 'a -> 'b) : 'b =
  f (fst a)

let kind (a: 'a with_range) : 'a = fst a
