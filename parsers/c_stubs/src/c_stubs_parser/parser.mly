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

(** Parser of C stubs *)

%{
    open Location
    open Cst

    let debug fmt = Debug.debug ~channel:"c_stub.parser" fmt

%}

(* Constants *)
%token TOP
%token <Z.t * Cst.int_suffix> INT_CONST
%token <float> FLOAT_CONST
%token <int> CHAR_CONST
%token <string> STRING_CONST
%token INVALID

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
%token LBRACE RBRACE
%token COLON SEMICOL DOT COMMA
%token PRIME
%token BEGIN END
%token EOF

(* Keywords *)
%token REQUIRES LOCAL ASSIGNS CASE ASSUMES ENSURES PREDICATE WARN ALIAS
%token TRUE FALSE
%token FORALL EXISTS IN NEW
%token FREE PRIMED RETURN SIZE BYTES SIZEOF_TYPE SIZEOF_EXPR OFFSET BASE VALID_PTR
%token VALID_FLOAT FLOAT_INF FLOAT_NAN


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

%start parse_stub
%start parse_expr
%start parse_type

%type <Cst.stub option> parse_stub
%type <Cst.expr> parse_expr
%type <Cst.c_qual_typ> parse_type

%%

parse_stub:
  | BEGIN with_range(section_list) END EOF { Some $2 }
  | EOF { None }

parse_expr:
  | expr EOF { $1 }

parse_type:
  | c_qual_typ EOF { $1 }

(* Sections *)
section_list:
  | { [] }
  | section section_list { $1 :: $2 }

section:
  | with_range(case_section) { S_case $1 }
  | leaf_section             { S_leaf $1 }
  | with_range(predicate)    { S_predicate $1 }
  | with_range(alias)        { S_alias $1 }

(* Case section *)
case_section:
  | CASE STRING_CONST LBRACE leaf_section_list RBRACE
    {
      {
        case_label = $2;
        case_body  = $4;
      }
    }

(* Leaf sections *)
leaf_section_list:
  | { [] }
  | leaf_section leaf_section_list { $1 :: $2 }

leaf_section:
  | with_range(local)      { S_local $1 }
  | with_range(assumes)    { S_assumes $1 }
  | with_range(requires)   { S_requires $1 }
  | with_range(assigns)    { S_assigns $1 }
  | with_range(ensures)    { S_ensures $1 }
  | with_range(free)       { S_free $1 }
  | with_range(warn)       { S_warn $1 }


(* Requirement section *)
requires:
  | REQUIRES COLON with_range(formula) SEMICOL { $3 }


(* Local section *)
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

(* Predicate section *)
predicate:
  | PREDICATE var COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = $2;
        predicate_args = [];
        predicate_body = $4;
      }
    }

  | PREDICATE var LPAR var_list RPAR COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = $2;
        predicate_args = $4;
        predicate_body = $7;
      }
    }


(* Alias section *)
alias:
  | ALIAS COLON IDENT SEMICOL { $3 }

(* Assignment section *)
assigns:
  | ASSIGNS COLON with_range(expr) assigns_offset_list SEMICOL
    {
      {
	assign_target = $3;
	assign_offset = $4;
      }
    }

assigns_offset_list:
  | { [] }
  | LBRACK with_range(expr) COMMA with_range(expr) RBRACK assigns_offset_list
    { ($2, $4) :: $6 }

(* Free section *)
free:
  | FREE COLON with_range(expr) SEMICOL { $3 }


(* Assumption section *)
assumes:
  | ASSUMES COLON with_range(formula) SEMICOL { $3 }


(* Ensures section *)
ensures:
  | ENSURES COLON with_range(formula) SEMICOL { $3 }

(* Warning section *)
warn:
  | WARN COLON STRING_CONST SEMICOL { $3 }


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
  | with_range(expr) IN set                           { F_in ($1, $3) }
  | LPAR formula RPAR                                 { $2 }
  | var LPAR args RPAR                                { F_predicate ($1, $3) }

(* C expressions *)
expr:
  | LPAR c_qual_typ RPAR with_range(expr)             { E_cast ($2, $4) } %prec CAST
  | LPAR expr RPAR                                    { $2 } (* FIXME: conflict between `(id) e` and `(id + e) *)
  | TOP LPAR c_qual_typ RPAR                          { E_top $3 }
  | INT_CONST                                         { E_int (fst $1, snd $1) }
  | STRING_CONST                                      { E_string $1}
  | FLOAT_CONST                                       { E_float $1 }
  | CHAR_CONST                                        { E_char $1 }
  | INVALID                                           { E_invalid }
  | var                                               { E_var $1 }
  | unop with_range(expr)                             { E_unop ($1, $2) } %prec UNARY
  | with_range(expr) binop with_range(expr)           { E_binop ($2, $1, $3) }
  | BAND with_range(expr)                             { E_addr_of $2 } %prec UNARY
  | STAR with_range(expr)                             { E_deref $2 } %prec UNARY
  | with_range(expr) LBRACK with_range(expr) RBRACK   { E_subscript ($1, $3) }
  | with_range(expr) DOT IDENT                        { E_member ($1, $3) }
  | with_range(expr) ARROW IDENT                      { E_arrow ($1, $3) }
  | RETURN                                            { E_return }
  | SIZEOF_TYPE LPAR with_range(c_qual_typ) RPAR           { E_sizeof_type $3 }
  | SIZEOF_EXPR LPAR with_range(expr) RPAR                 { E_sizeof_expr $3 }
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
  | BYTES  { BYTES }
  | OFFSET { OFFSET }
  | BASE   { BASE }
  | PRIMED { PRIMED }
  | VALID_PTR { VALID_PTR }
  | VALID_FLOAT { VALID_FLOAT }
  | FLOAT_INF   { FLOAT_INF }
  | FLOAT_NAN   { FLOAT_NAN }

args:
  |                             { [] }
  | with_range(expr) COMMA args { $1 :: $3 }
  | with_range(expr)            { [ $1 ] }

var_list:
  |                    { [] }
  | var                { [$1] }
  | var COMMA var_list { $1 :: $3 }


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
