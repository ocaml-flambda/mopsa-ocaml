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
    open Mopsa_utils
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
%token REQUIRES LOCAL ASSIGNS CASE ASSUMES ENSURES PREDICATE WARN ALARM UNSOUND ALIAS
%token TRUE FALSE
%token FORALL EXISTS IN NEW
%token FREE PRIMED RETURN LENGTH BYTES SIZEOF_TYPE SIZEOF_EXPR INDEX OFFSET BASE VALID_PTR CAST
%token VALID_FLOAT FLOAT_INF FLOAT_NAN ALIVE


(* Types *)
%token VOID CHAR INT LONG FLOAT DOUBLE SHORT
%token SIGNED UNSIGNED CONST VOLATILE RESTRICT
%token STRUCT UNION ENUM

(* Priorities of logical operators *)
%right IMPLIES
%left OR
%left AND
%nonassoc FORALL EXISTS
%nonassoc NOT


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
  | type_name EOF { $1 }

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
  | with_range(message)    { S_message $1 }


(* Requirement section *)
requires:
  | REQUIRES COLON with_range(formula) SEMICOL { $3 }


(* Local section *)
local:
  | LOCAL COLON type_name var ASSIGN local_value SEMICOL
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
  | ASSIGNS COLON with_range(postfix_expr) assigns_offset_list SEMICOL
    {
      {
	assign_target = $3;
	assign_offset = $4;
      }
    }

  | ASSIGNS COLON with_range(expr) SEMICOL
    {
      {
	assign_target = $3;
	assign_offset = [];
      }
    }

assigns_offset_list:
  | interval                     { [$1] }
  | interval assigns_offset_list { $1 :: $2 }

(* Free section *)
free:
  | FREE COLON with_range(expr) SEMICOL { $3 }


(* Assumption section *)
assumes:
  | ASSUMES COLON with_range(formula) SEMICOL { $3 }


(* Ensures section *)
ensures:
  | ENSURES COLON with_range(formula) SEMICOL { $3 }

(* Message sections *)
message:
  | message_kind COLON STRING_CONST SEMICOL { { message_kind = $1;
						message_body = $3; } }

message_kind:
  | WARN    { WARN }
  | UNSOUND { UNSOUND }
  | ALARM   { ALARM }


(* Logic formula *)
primary_formula:
  | with_range(TRUE)                                  { F_bool true }
  | with_range(FALSE)                                 { F_bool false }
  | with_range(expr)                                  { F_expr $1 }
  | with_range(expr) IN set                           { F_in ($1, $3) }
  | var LPAR args RPAR                                { F_predicate ($1, $3) }

composed_formula:
  | with_range(formula) log_binop with_range(formula) { F_binop ($2, $1, $3) }
  | NOT with_range(formula)                           { F_not $2 }
  | FORALL type_name var IN set COLON with_range(formula) { F_forall ($3, $2, $5, $7) } %prec FORALL
  | EXISTS type_name var IN set COLON with_range(formula) { F_exists ($3, $2, $5, $7) } %prec EXISTS

formula:
  | LPAR composed_formula RPAR { $2 }
  | primary_formula            { $1 }
  | composed_formula           { $1 }


(* C expressions *)
primary_expr:
  | var                        { E_var $1 }
  | const                      { $1 }
  | RETURN                     { E_return }
  | LPAR expr RPAR             { $2 }

postfix_expr:
  | primary_expr                                 { $1 }
  | with_range(postfix_expr) LBRACK with_range(expr) RBRACK  { E_subscript ($1, $3) }
  | with_range(postfix_expr) PRIME               { E_builtin_call (PRIMED, $1) } 
  | builtin LPAR with_range(expr) RPAR           { E_builtin_call ($1, $3) }
  | with_range(postfix_expr) DOT IDENT           { E_member ($1, $3) }
  | with_range(postfix_expr) ARROW IDENT         { E_arrow ($1, $3) }

unary_expr:
  | postfix_expr { $1 }
  | SIZEOF_TYPE LPAR with_range(type_name) RPAR { E_sizeof_type $3 }
  | SIZEOF_EXPR LPAR with_range(unary_expr) RPAR { E_sizeof_expr $3 }
  | unop with_range(cast_expr)                   { E_unop ($1, $2) }
  | STAR with_range(cast_expr)                   { E_deref $2 }
  | BAND with_range(cast_expr)                   { E_addr_of $2 }

cast_expr:
  | unary_expr                                               { $1 }
  | LPAR type_name_except_typedef RPAR with_range(cast_expr) { E_cast ($2, $4) }
  | CAST LPAR type_name COMMA with_range(cast_expr) RPAR     { E_cast ($3, $5) }

multiplicative_expr:
  | cast_expr { $1 }
  | with_range(multiplicative_expr) multiplicative_binop with_range(cast_expr)  { E_binop ($2, $1, $3) }

additive_expr:
  | multiplicative_expr { $1 }
  | with_range(additive_expr) additive_binop with_range(multiplicative_expr)    { E_binop ($2, $1, $3) }

shift_expr:
  | additive_expr { $1 }
  | with_range(shift_expr) shift_binop with_range(additive_expr)                { E_binop ($2, $1, $3) }

relational_expr:
  | shift_expr { $1 }
  | with_range(relational_expr) relational_binop with_range(shift_expr)         { E_binop ($2, $1, $3) }

equality_expr:
  | relational_expr { $1 }
  | with_range(equality_expr) equality_binop with_range(relational_expr)        { E_binop ($2, $1, $3) }

band_expr:
  | equality_expr { $1 }
  | with_range(band_expr) BAND with_range(equality_expr)   { E_binop (BAND, $1, $3) }

bxor_expr:
  | band_expr { $1 }
  | with_range(bxor_expr) BXOR with_range(band_expr)       { E_binop (BXOR, $1, $3) }

bor_expr:
  | bxor_expr { $1 }
  | with_range(bor_expr) BOR with_range(bxor_expr)         { E_binop (BOR, $1, $3) }

land_expr:
  | bor_expr { $1 }
  | with_range(land_expr) LAND with_range(bor_expr)       { E_binop (LAND, $1, $3) }

lor_expr:
  | land_expr { $1 }
  | with_range(lor_expr) LOR with_range(land_expr)        { E_binop (LOR, $1, $3) }

expr:
  | lor_expr { $1 }


 
(* Types *)
type_specifier_except_typedef:
  | VOID               { T_void }
  | CHAR               { T_char }
  | UNSIGNED CHAR      { T_unsigned_char }
  | SIGNED CHAR        { T_signed_char }
  | SHORT              { T_signed_short }
  | SHORT INT          { T_signed_short }
  | UNSIGNED SHORT     { T_unsigned_short }
  | UNSIGNED SHORT INT { T_unsigned_short }
  | SIGNED SHORT       { T_signed_short }
  | SIGNED SHORT INT   { T_signed_short }
  | INT                { T_signed_int }
  | UNSIGNED           { T_unsigned_int }
  | UNSIGNED INT       { T_unsigned_int }
  | SIGNED             { T_signed_int }
  | SIGNED INT         { T_signed_int }
  | LONG               { T_signed_long }
  | LONG INT           { T_signed_long }
  | UNSIGNED LONG      { T_unsigned_long }
  | UNSIGNED LONG INT  { T_unsigned_long }
  | SIGNED LONG        { T_signed_long }
  | SIGNED LONG INT    { T_signed_long }
  | LONG LONG          { T_signed_long_long }
  | LONG LONG INT      { T_signed_long_long }
  | SIGNED LONG LONG   { T_signed_long_long }
  | SIGNED LONG LONG INT  { T_signed_long_long }
  | UNSIGNED LONG LONG { T_unsigned_long_long }
  | UNSIGNED LONG LONG INT { T_unsigned_long_long }
  | FLOAT              { T_float }
  | DOUBLE             { T_double }
  | LONG DOUBLE        { T_long_double }
  | STRUCT var         { T_struct($2) }
  | UNION var          { T_union($2) }
  | ENUM var           { T_enum($2) }

type_specifier:
  | type_specifier_except_typedef { $1 }
  | var                           { T_typedef($1) }

type_qualifier:
  | CONST    { const }
  | VOLATILE { volatile }
  | RESTRICT { restrict }

specifier_qualifier_list:
  | type_specifier                          { $1, no_c_qual }
  | type_qualifier specifier_qualifier_list { let t,q = $2 in t, merge_c_qual $1 q }

type_qualifier_list:
  |                                    { no_c_qual }
  | type_qualifier_list type_qualifier { merge_c_qual $1 $2 }

pointer:
  | specifier_qualifier_list STAR type_qualifier_list { T_pointer $1, $3 }
  | pointer STAR type_qualifier_list                  { T_pointer $1, $3 }

type_name:
  | pointer                  { $1 }
  | specifier_qualifier_list { $1 }

type_name_except_typedef:
  | type_specifier_except_typedef           { $1, no_c_qual }
  | type_name_except_typedef STAR { T_pointer $1, no_c_qual }


(* Operators *)
additive_binop:
  | PLUS   { ADD }
  | MINUS  { SUB }

multiplicative_binop:
  | STAR   { MUL }
  | DIV    { DIV }
  | MOD    { MOD }

equality_binop:
  | EQ     { EQ }
  | NEQ    { NEQ }

relational_binop:
  | LT     { LT }
  | GT     { GT }
  | LE     { LE }
  | GE     { GE }

shift_binop:
  | RSHIFT { RSHIFT }
  | LSHIFT { LSHIFT }

unop:
  | PLUS  { PLUS }
  | MINUS { MINUS }
  | LNOT  { LNOT }
  | BNOT  { BNOT }

set:
  | interval  { S_interval ($1) }
  | resource  { S_resource $1 }

interval:
  | LBRACK with_range(expr) COMMA with_range(expr) RBRACK
    { { itv_lb=$2;
	itv_open_lb=false;
	itv_ub=$4;
	itv_open_ub=false; } }

  | LPAR with_range(expr) COMMA with_range(expr) RBRACK
    { { itv_lb=$2;
  	itv_open_lb=true;
  	itv_ub=$4;
  	itv_open_ub=false; } }

  | LBRACK with_range(expr) COMMA with_range(expr) RPAR
    { { itv_lb=$2;
  	itv_open_lb=false;
  	itv_ub=$4;
  	itv_open_ub=true; } }

  | LPAR with_range(expr) COMMA with_range(expr) RPAR
    { { itv_lb=$2;
  	itv_open_lb=true;
  	itv_ub=$4;
  	itv_open_ub=true; } }

%inline log_binop:
  | AND     { AND }
  | OR      { OR }
  | IMPLIES { IMPLIES }

%inline builtin:
  | LENGTH      { LENGTH }
  | INDEX       { INDEX }
  | BYTES       { BYTES }
  | OFFSET      { OFFSET }
  | BASE        { BASE }
  | PRIMED      { PRIMED }
  | VALID_PTR   { VALID_PTR }
  | VALID_FLOAT { VALID_FLOAT }
  | FLOAT_INF   { FLOAT_INF }
  | FLOAT_NAN   { FLOAT_NAN }
  | ALIVE       { ALIVE }

args:
  |                             { [] }
  | with_range(expr) COMMA args { $1 :: $3 }
  | with_range(expr)            { [ $1 ] }

var_list:
  |                    { [] }
  | var                { [$1] }
  | var COMMA var_list { $1 :: $3 }


resource:
  | var { $1 }

var:
  | IDENT {
	{
	  vname = $1;
	  vuid = 0;
	  vtyp = T_unknown, no_c_qual;
	  vlocal = false;
	  vrange = from_lexing_range $startpos $endpos;
	}
      }

const:
  | TOP LPAR type_name RPAR { E_top $3 }
  | INT_CONST               { E_int (fst $1, snd $1) }
  | STRING_CONST            { E_string $1}
  | FLOAT_CONST             { E_float $1 }
  | CHAR_CONST              { E_char $1 }
  | INVALID                 { E_invalid }



// adds range information to rule
%inline with_range(X):
  | x=X { with_range x (from_lexing_range $startpos $endpos) }
