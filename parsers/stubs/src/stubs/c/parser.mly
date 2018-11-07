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
%token <Z.t> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING

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
%nonassoc LBRACK
%nonassoc UNARY
%left DOT ARROW

(* Priorities of logical operators *)
%left IMPLIES
%left OR
%left AND
%nonassoc FORALL EXISTS
%nonassoc NOT

%start stub

%type <Ast.stub> stub

%type <Ast.requires> requires

%%

stub:
  | local_list predicate_list requires_list assigns_list EOF
    {
      {
        stub_local    = $1;
        stub_predicates = $2;
        stub_requires = $3;
        stub_assigns  = $4;
        stub_case     = [];
        stub_ensures  = [];
      }
    }

  | local_list predicate_list requires_list assigns_list case_list EOF
    {
      {
        stub_local    = $1;
        stub_predicates = $2;
        stub_requires = $3;
        stub_assigns  = $4;
        stub_case     = $5;
        stub_ensures  = [];
      }
    }

  | local_list predicate_list requires_list assigns_list ensures_list EOF
    {
      {
        stub_local    = $1;
        stub_predicates = $2;
        stub_requires = $3;
        stub_assigns  = $4;
        stub_case     = [];
        stub_ensures  = $5;
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
  | LOCAL COLON IDENT ASSIGN local_value SEMICOL
    {
      {
        local_var = $3;
        local_value = $5;
      }
    }

local_value:
  | NEW resource { Local_new $2 }
  | IDENT LPAR args RPAR { Local_function_call ($1, $3) }

predicate_list:
  | { [] }
  | with_range(predicate) predicate_list { $1 :: $2 }

predicate:
  | PREDICATE IDENT COLON with_range(formula) SEMICOL
    {
      {
        predicate_var = $2;
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
	assign_target = $3;
	assign_range = None;
      }
    }

  | ASSIGNS COLON with_range(expr) LBRACK with_range(expr) DOT DOT with_range(expr) RBRACK
    {
      {
	assign_target = $3;
	assign_range = Some ($5, $8);
      }
    }

case_list:
  | with_range(case) { [ $1 ] }
  | with_range(case) case_list { $1 :: $2 }

case:
  | CASE STRING COLON assumes_list requires_list local_list assigns_list ensures_list
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
  | with_range(ensures) { [ $1 ] }
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
  | FORALL IDENT IN set COLON with_range(formula) { F_forall ($2, $4, $6) } %prec FORALL
  | EXISTS IDENT IN set COLON with_range(formula) { F_exists ($2, $4, $6) } %prec EXISTS
  | IDENT IN set                { F_in ($1, $3) }
  | FREE with_range(expr)                 { F_free $2 }

expr:
  | LPAR expr RPAR { $2 }
  | LPAR typ RPAR with_range(expr) { E_cast ($2, $4) }
  | PLUS expr { $2 }
  | INT                     { E_int $1 }
  | STRING                  { E_string $1}
  | FLOAT                   { E_float $1 }
  | CHAR                    { E_char $1 }
  | IDENT                               { E_var $1 }
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
  | IDENT { $1 }

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

// adds range information to rule
%inline with_range(X):
  | x=X { x, (pos_to_loc $startpos, pos_to_loc $endpos) }
