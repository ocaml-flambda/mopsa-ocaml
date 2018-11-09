(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

%{
    open Ast

    let pos_to_loc pos =
      let open Lexing in
      {
	file = pos.pos_fname;
	line = pos.pos_lnum;
	col = pos.pos_cnum - pos.pos_bol + 1;
      }

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
%token ARROW ADDROF STAR
%token GT GE LT LE EQ NEQ
%token RSHIFT LSHIFT BXOR

(* Delimiters *)
%token LPAR RPAR
%token LBRACK RBRACK
%token COLON SEMICOL DOT
%token EOF

(* Keywords *)
%token REQUIRES LOCAL ASSIGNS CASE ASSUMES ENSURES PREDICATE
%token TRUE FALSE
%token FORALL EXISTS IN NEW
%token FREE OLD RETURN SIZE OFFSET BASE

(* Types *)
%token CHAR INT LONG FLOAT DOUBLE SHORT
%token SIGNED UNSIGNED CONST

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
%left DOT ARROW

%start stub

%type <Ast.stub> stub

%type <Ast.requires> requires

%%

stub:
  | predicate_list requires_list assigns_list local_list ensures_list EOF
    {
      S_simple {
        simple_predicates = $1;
        simple_requires = $2;
        simple_assigns  = $3;
        simple_local    = $4;
        simple_ensures  = $5;
      }
    }

  | predicate_list requires_list case_list EOF
    {
      S_multi {
        multi_predicates = $1;
        multi_requires = $2;
        multi_cases     = $3;
      }
    }

requires_list:
  | { [] }
  | with_range(requires) requires_list { $1 :: $2 }

requires:
  | REQUIRES COLON with_range(formula) SEMICOL { $3 }

local_list:
  | { [] }
  | with_range(local) local_list { $1 :: $2 }

local:
  | LOCAL COLON typ var ASSIGN local_value SEMICOL
    {
      {
        local_var = { $4 with var_typ = $3 };
        local_value = $6;
      }
    }

local_value:
  | NEW resource { Local_new $2 }
  | var LPAR args RPAR { Local_function_call ($1, $3) }

predicate_list:
  | { [] }
  | with_range(predicate) predicate_list { $1 :: $2 }

predicate:
  | PREDICATE var COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = { $2 with var_typ = T_predicate };
        predicate_body = $4;
      }
    }

assigns_list:
  | { [] }
  | with_range(assigns) assigns_list { $1 :: $2 }

assigns:
  | ASSIGNS COLON with_range(expr) SEMICOL
    {
      {
	assigns_target = $3;
	assigns_range = None;
      }
    }

  | ASSIGNS COLON with_range(expr) LBRACK with_range(expr) DOT DOT with_range(expr) RBRACK
    {
      {
	assigns_target = $3;
	assigns_range = Some ($5, $8);
      }
    }

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

assumes_list:
  | { [] }
  | with_range(assumes) assumes_list { $1 :: $2 }

assumes:
  | ASSUMES COLON with_range(formula) SEMICOL { $3 }

ensures_list:
  | { [] }
  | with_range(ensures) ensures_list { $1 :: $2 }

ensures:
  | ENSURES COLON with_range(formula) SEMICOL { $3 }

formula:
  | RPAR formula RPAR         { $2 }
  | with_range(TRUE)                      { F_bool true }
  | with_range(FALSE)                     { F_bool false }
  | with_range(expr)                      { F_expr $1 }
  | with_range(formula) log_binop with_range(formula) { F_binop ($2, $1, $3) }
  | NOT with_range(formula)               { F_not $2 }
  | FORALL typ var IN set COLON with_range(formula) { F_forall ({ $3 with var_typ = $2 }, $5, $7) } %prec FORALL
  | EXISTS typ var IN set COLON with_range(formula) { F_exists ({ $3 with var_typ = $2 }, $5, $7) } %prec EXISTS
  | var IN set                { F_in ($1, $3) }
  | FREE with_range(expr)                 { F_free $2 }

expr:
  | LPAR typ RPAR with_range(expr) { E_cast ($2, $4) }            %prec CAST
  | LPAR expr RPAR { $2 }
  | PLUS expr { $2 }
  | INT_CONST                     { E_int $1 }
  | STRING_CONST                  { E_string $1}
  | FLOAT_CONST                   { E_float $1 }
  | CHAR_CONST                    { E_char $1 }
  | var                               { E_var $1 }
  | unop with_range(expr)               { E_unop ($1, $2) }       %prec UNARY
  | with_range(expr) binop with_range(expr)         { E_binop ($2, $1, $3) }
  | ADDROF with_range(expr)             { E_addr_of $2 }          %prec UNARY
  | STAR with_range(expr)               { E_deref $2 }            %prec UNARY
  | with_range(expr) LBRACK with_range(expr) RBRACK { E_subscript ($1, $3) }
  | with_range(expr) DOT IDENT          { E_member ($1, $3) }
  | with_range(expr) ARROW IDENT        { E_arrow ($1, $3) }
  | RETURN                  { E_return }
  | builtin LPAR with_range(expr) RPAR  { E_builtin_call ($1, $3) }

typ:
  | c_typ { T_c $1 }

c_typ:
  | CONST typ_spec { ($2, C_AST.{ qual_is_const = true}) }
  | typ_spec { ($1, C_AST.{ qual_is_const = false}) }

typ_spec:
  | int_typ { C_AST.T_integer $1 }
  | float_typ { C_AST.T_float $1 }
  | c_typ STAR { C_AST.T_pointer $1 }

int_typ:
  | CHAR          { C_AST.(Char SIGNED) } (* FIXME: signeness should be defined by the platform. *)
  | UNSIGNED CHAR { C_AST.UNSIGNED_CHAR }
  | SIGNED CHAR { C_AST.SIGNED_CHAR }

  | SHORT    { C_AST.SIGNED_SHORT }
  | UNSIGNED SHORT { C_AST.UNSIGNED_SHORT }
  | SIGNED SHORT { C_AST.SIGNED_SHORT }

  | INT    { C_AST.SIGNED_INT }
  | UNSIGNED INT { C_AST.UNSIGNED_INT }
  | SIGNED INT { C_AST.SIGNED_INT }

  | LONG    { C_AST.SIGNED_LONG }
  | UNSIGNED LONG { C_AST.UNSIGNED_LONG }
  | SIGNED LONG { C_AST.SIGNED_LONG }

float_typ:
  | FLOAT { C_AST.FLOAT }
  | DOUBLE { C_AST.DOUBLE }

%inline binop:
  | PLUS { ADD }
  | MINUS { SUB }
  | STAR { MUL }
  | DIV { DIV }
  | MOD { MOD }
  | LOR { LOR }
  | LAND { LAND }
  | BOR { BOR }
  | BXOR { BXOR }
  | BAND { BAND }
  | EQ { EQ }
  | NEQ { NEQ }
  | LT { LT }
  | GT { GT }
  | LE { LE }
  | GE { GE }
  | RSHIFT { RSHIFT }
  | LSHIFT { LSHIFT }

%inline unop:
  | MINUS { MINUS }
  | LNOT { LNOT }
  | BNOT { BNOT }

set:
  | LBRACK with_range(expr) DOT DOT with_range(expr) RBRACK { S_interval ($2, $5) }
  | resource { S_resource $1 }

%inline log_binop:
  | AND { AND }
  | OR { OR }
  | IMPLIES { IMPLIES }

%inline builtin:
  | SIZE { SIZE }
  | OFFSET { OFFSET }
  | BASE { BASE }
  | OLD { OLD }

args:
  | { [] }
  | with_range(expr) COLON args { $1 :: $3 }
  | with_range(expr) { [ $1 ] }

resource:
  | IDENT { $1 }

var:
  | IDENT { { var_name = $1; var_uid = 0; var_typ = T_unknown; } }

// adds range information to rule
%inline with_range(X):
  | x=X { x, (pos_to_loc $startpos, pos_to_loc $endpos) }
