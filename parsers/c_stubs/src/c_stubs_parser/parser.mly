(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Parser of C stubs *)

%{
    open Location
    open Cst

    let debug fmt = Debug.debug ~channel:"c_stub.parser" fmt

%}

(* Constants *)
%token <Z.t> INT_CONST
%token <float> FLOAT_CONST
%token <char> CHAR_CONST
%token <string> STRING_CONST

(* Identifiers *)
%token <string> IDENT

(* Logical operators *)
%token AND OR IMPLIES NOT

(* Assignments (in locals only) *)
%token ASSIGN

(* C operators *)
%token PLUS MINUS DIV MOD
%token LAND LOR
%token BAND BOR
%token LNOT BNOT
%token ARROW STAR
%token GT GE LT LE EQ NEQ
%token RSHIFT LSHIFT BXOR

(* Delimiters *)
%token LPAR RPAR
%token LBRACK RBRACK
%token COLON SEMICOL DOT COMMA
%token PRIME
%token BEGIN END
%token EOF

(* Keywords *)
%token REQUIRES LOCAL ASSIGNS CASE ASSUMES ENSURES PREDICATE
%token TRUE FALSE
%token FORALL EXISTS IN NEW
%token FREE PRIMED RETURN SIZE OFFSET BASE
%token FLOAT_VALID FLOAT_INF FLOAT_NAN

(* Deprecated *)
%token OLD

(* Types *)
%token VOID CHAR INT LONG FLOAT DOUBLE SHORT
%token SIGNED UNSIGNED CONST
%token STRUCT UNION ENUM

(* Priorities of logical operators *)
%left IMPLIES
%left OR
%left AND
%nonassoc FORALL EXISTS
%nonassoc NOT

(* Priorities of C operators *)
%left LOR
%left LAND
%left BOR
%left BXOR
%left BAND
%left EQ NEQ
%left LT GT LE GE
%left RSHIFT LSHIFT

%left PLUS MINUS
%left STAR DIV MOD
%nonassoc CAST
%left LBRACK
%nonassoc UNARY
%left DOT ARROW COLON
%right PRIME

%start stub

%type <Cst.stub Location.with_range option> stub

%%

stub:
  | BEGIN predicate_list requires_list assigns_list local_list ensures_list END EOF
    {
      Some (
          with_range
            (S_simple {
                 simple_stub_predicates = $2;
                 simple_stub_requires   = $3;
                 simple_stub_assigns    = $4;
                 simple_stub_local      = $5;
                 simple_stub_ensures    = $6;
            })
            (from_lexing_range $startpos $endpos)
        )
    }

  | BEGIN predicate_list requires_list case_list END EOF
    {
      Some (
          with_range
            (S_case {
                 case_stub_predicates = $2;
                 case_stub_requires   = $3;
                 case_stub_cases      = $4;
            })
            (from_lexing_range $startpos $endpos)
        )
    }

  | EOF { None }

(* Requirements section *)
requires_list:
  | { [] }
  | with_range(requires) requires_list { $1 :: $2 }

requires:
  | REQUIRES COLON with_range(formula) SEMICOL { $3 }


(* Locals section *)
local_list:
  | { [] }
  | with_range(local) local_list { $1 :: $2 }

local:
  | LOCAL COLON c_qual_typ var ASSIGN local_value SEMICOL
    {
      {
        lvar = $4;
        ltyp = $3;
        lval = $6;
      }
    }

local_value:
  | NEW resource { L_new $2 }
  | with_range(var) LPAR args RPAR { L_call ($1, $3) }

(* Predicates section *)
predicate_list:
  | { [] }
  | with_range(predicate) predicate_list { $1 :: $2 }

predicate:
  | PREDICATE var COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = $2;
        predicate_args = [];
        predicate_body = $4;
      }
    }

  | PREDICATE var LPAR var_list COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = $2;
        predicate_args = $4;
        predicate_body = $6;
      }
    }

(* Assignments section *)
assigns_list:
  | { [] }
  | with_range(assigns) assigns_list { $1 :: $2 }

assigns:
  | ASSIGNS COLON with_range(expr) SEMICOL
    {
      {
	assign_target = $3;
	assign_offset = None;
      }
    }

  | ASSIGNS COLON with_range(expr) assigns_offset_list SEMICOL
    {
      {
	assign_target = $3;
	assign_offset = Some $4;
      }
    }

assigns_offset_list:
  | { [] }
  | LBRACK with_range(expr) COMMA with_range(expr) RBRACK assigns_offset_list
    {
      ($2, $4) :: $6
    }

(* Cases section *)
case_list:
  | with_range(case) { [ $1 ] }
  | with_range(case) case_list { $1 :: $2 }

case:
  | CASE STRING_CONST COLON assumes_list requires_list local_list assigns_list ensures_list
    {
      {
        case_label    = $2;
        case_assumes  = $4;
        case_requires = $5;
        case_local    = $6;
        case_assigns  = $7;
        case_ensures  = $8;
      }
    }

(* Assumptions section *)
assumes_list:
  | { [] }
  | with_range(assumes) assumes_list { $1 :: $2 }

assumes:
  | ASSUMES COLON with_range(formula) SEMICOL { $3 }


(* Ensures section *)
ensures_list:
  | { [] }
  | with_range(ensures) ensures_list { $1 :: $2 }

ensures:
  | ENSURES COLON with_range(formula) SEMICOL { $3 }


(* Logic formula *)
formula:
  | RPAR formula RPAR                                 { $2 }
  | with_range(TRUE)                                  { F_bool true }
  | with_range(FALSE)                                 { F_bool false }
  | with_range(expr)                                  { F_expr $1 }
  | with_range(formula) log_binop with_range(formula) { F_binop ($2, $1, $3) }
  | NOT with_range(formula)                           { F_not $2 }
  | FORALL c_qual_typ var IN set COLON with_range(formula) { F_forall ($3, $2, $5, $7) } %prec FORALL
  | EXISTS c_qual_typ var IN set COLON with_range(formula) { F_exists ($3, $2, $5, $7) } %prec EXISTS
  | var IN set                                        { F_in ($1, $3) }
  | FREE with_range(expr)                             { F_free $2 }
  | LPAR formula RPAR                                 { $2 }

(* C expressions *)
expr:
  | LPAR c_qual_typ RPAR with_range(expr)             { E_cast ($2, $4) } %prec CAST
  | LPAR expr RPAR                                    { $2 } (* FIXME: conflict between `(id) e` and `(id + e) *)
  | INT_CONST                                         { E_int $1 }
  | STRING_CONST                                      { E_string $1}
  | FLOAT_CONST                                       { E_float $1 }
  | CHAR_CONST                                        { E_char $1 }
  | var                                               { E_var $1 }
  | unop with_range(expr)                             { E_unop ($1, $2) } %prec UNARY
  | with_range(expr) binop with_range(expr)           { E_binop ($2, $1, $3) }
  | BAND with_range(expr)                             { E_addr_of $2 } %prec UNARY
  | STAR with_range(expr)                             { E_deref $2 } %prec UNARY
  | with_range(expr) LBRACK with_range(expr) RBRACK   { E_subscript ($1, $3) }
  | with_range(expr) DOT IDENT                        { E_member ($1, $3) }
  | with_range(expr) COLON IDENT                      { E_attribute ($1, $3) }
  | with_range(expr) ARROW IDENT                      { E_arrow ($1, $3) }
  | RETURN                                            { E_return }
  | builtin LPAR with_range(expr) RPAR                { E_builtin_call ($1, $3) }
  | with_range(expr) PRIME                            { E_builtin_call (PRIMED, $1) }


(* C types *)
c_qual_typ:
  | c_qual_typ STAR        { (T_pointer $1, false) }
  | c_qual_typ CONST STAR  { (T_pointer $1, true) }
  | c_qual_typ_no_ptr      { $1 }

c_qual_typ_no_ptr:
  | CONST c_typ { ($2, true) }
  | c_typ       { ($1, false) }

c_typ:
  | VOID               { T_void }
  | CHAR               { T_char }
  | UNSIGNED CHAR      { T_unsigned_char }
  | SIGNED CHAR        { T_signed_char }
  | SHORT              { T_signed_short }
  | UNSIGNED SHORT     { T_unsigned_short }
  | SIGNED SHORT       { T_signed_short }
  | INT                { T_signed_int }
  | UNSIGNED INT       { T_unsigned_int }
  | SIGNED INT         { T_signed_int }
  | LONG               { T_signed_long }
  | UNSIGNED LONG      { T_unsigned_long }
  | SIGNED LONG        { T_signed_long }
  | LONG LONG          { T_signed_long_long }
  | SIGNED LONG LONG   { T_signed_long_long }
  | UNSIGNED LONG LONG { T_unsigned_long_long }
  | FLOAT              { T_float }
  | DOUBLE             { T_double }
  | LONG DOUBLE        { T_long_double }
  | STRUCT var         { T_struct($2) }
  | UNION var          { T_union($2) }
  | ENUM var           { T_enum($2) }
  | var                { T_typedef($1) }

(* Operators *)
%inline binop:
  | PLUS   { ADD }
  | MINUS  { SUB }
  | STAR   { MUL }
  | DIV    { DIV }
  | MOD    { MOD }
  | LOR    { LOR }
  | LAND   { LAND }
  | BOR    { BOR }
  | BXOR   { BXOR }
  | BAND   { BAND }
  | EQ     { EQ }
  | NEQ    { NEQ }
  | LT     { LT }
  | GT     { GT }
  | LE     { LE }
  | GE     { GE }
  | RSHIFT { RSHIFT }
  | LSHIFT { LSHIFT }

%inline unop:
  | MINUS { MINUS }
  | LNOT  { LNOT }
  | BNOT  { BNOT }

set:
  | LBRACK with_range(expr) COMMA with_range(expr) RBRACK  { S_interval ($2, $4) }
  | resource                                               { S_resource $1 }

%inline log_binop:
  | AND     { AND }
  | OR      { OR }
  | IMPLIES { IMPLIES }

%inline builtin:
  | SIZE   { SIZE }
  | OFFSET { OFFSET }
  | BASE   { BASE }
  | PRIMED { PRIMED }
  | FLOAT_VALID { FLOAT_VALID }
  | FLOAT_INF   { FLOAT_INF }
  | FLOAT_NAN   { FLOAT_NAN }
  (* Deprecated *)
  | OLD    { OLD }

args:
  |                             { [] }
  | with_range(expr) COMMA args { $1 :: $3 }
  | with_range(expr)            { [ $1 ] }

var_list:
  |                    { [] }
  | var COLON var_list { $1 :: $3 }


resource:
  | IDENT { $1 }

var:
  | IDENT {
	{
	  vname = $1;
	  vuid = 0;
	  vtyp = no_qual T_unknown;
	  vlocal = false;
	  vrange = from_lexing_range $startpos $endpos;
	}
      }


// adds range information to rule
%inline with_range(X):
  | x=X { with_range x (from_lexing_range $startpos $endpos) }
