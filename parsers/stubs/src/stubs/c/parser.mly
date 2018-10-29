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

    let with_range (e: 'a) pos1 pos2 : 'a with_range = (e, (pos_to_loc pos1, pos_to_loc pos2))
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
%token LBRACE RBRACE
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
%nonassoc POSTFIX
%left DOT ARROW

(* Priorities of logical operators *)
%left IMPLIES
%left OR
%left AND
%nonassoc FORALL EXISTS
%nonassoc NOT

%start stub

%type <Ast.stub Ast.with_range> stub

%%

stub:
  | local_list predicate_list requires_list assigns_list EOF
    {
      with_range {
          stub_local    = $1;
          stub_predicates = $2;
          stub_requires = $3;
          stub_assigns  = $4;
          stub_case     = [];
          stub_ensures  = [];
        }
      $startpos $endpos
    }

  | local_list predicate_list requires_list assigns_list case_list EOF
    {
      with_range {
          stub_local    = $1;
          stub_predicates = $2;
          stub_requires = $3;
          stub_assigns  = $4;
          stub_case     = $5;
          stub_ensures  = [];
        }
      $startpos $endpos
    }

  | local_list predicate_list requires_list assigns_list ensures_list EOF
    {
      with_range {
          stub_local    = $1;
          stub_predicates = $2;
          stub_requires = $3;
          stub_assigns  = $4;
          stub_case     = [];
          stub_ensures  = $5;
	}
      $startpos $endpos
    }

requires_list:
  | { [] }
  | requires requires_list { $1 :: $2 }

requires:
  | REQUIRES COLON formula SEMICOL { $3 }

local_list:
  | { [] }
  | local local_list { $1 :: $2 }

local:
  | LOCAL COLON IDENT ASSIGN local_value SEMICOL
    {
      with_range {
          local_var = $3;
          local_value = $5;
        }
      $startpos $endpos
    }

local_value:
  | NEW resource { LV_new $2 }
  | IDENT LPAR args RPAR { LV_call ($1, $3) }

predicate_list:
  | { [] }
  | predicate predicate_list { $1 :: $2 }

predicate:
  | PREDICATE IDENT COLON formula SEMICOL
    {
      with_range {
          predicate_var = $2;
          predicate_body = $4;
        }
      $startpos $endpos
    }

assigns_list:
  | { [] }
  | assigns assigns_list { $1 :: $2 }

assigns:
  | ASSIGNS COLON expr SEMICOL
    {
      with_range {
	  assign_target = $3;
	  assign_range = None;
	}
      $startpos $endpos
    }

  | ASSIGNS COLON expr LBRACE expr DOT DOT expr RBRACE
    {
      with_range {
	  assign_target = $3;
	  assign_range = Some ($5, $8);
	}
      $startpos $endpos
    }

case_list:
  | case { [ $1 ] }
  | case case_list { $1 :: $2 }

case:
  | CASE STRING COLON assumes_list requires_list local_list assigns_list ensures_list
    {
      with_range {
          case_label    = $2;
          case_assumes  = $4;
          case_requires = $5;
          case_local    = $6;
          case_assigns  = $7;
          case_ensures  = $8;
	}
      $startpos $endpos
    }

assumes_list:
  | { [] }
  | assumes assumes_list { $1 :: $2 }

assumes:
  | ASSUMES COLON formula SEMICOL { $3 }

ensures_list:
  | ensures { [ $1 ] }
  | ensures ensures_list { $1 :: $2 }

ensures:
  | ENSURES COLON formula SEMICOL { $3 }

formula:
  | RPAR formula RPAR         { $2 }
  | formula_kind { with_range $1 $startpos $endpos }

formula_kind:
  | TRUE                      { F_bool true }
  | FALSE                     { F_bool false }
  | expr                      { F_expr $1 }
  | formula log_binop formula { F_binop ($2, $1, $3) }
  | NOT formula               { F_not $2 }
  | FORALL IDENT IN set COLON formula { F_forall ($2, $4, $6) } %prec FORALL
  | EXISTS IDENT IN set COLON formula { F_exists ($2, $4, $6) } %prec EXISTS
  | IDENT IN set                { F_in ($1, $3) }
  | FREE expr                 { F_free $2 }

expr:
  | LPAR expr RPAR { $2 }
  | PLUS expr { $2 }
  | expr_kind { with_range $1 $startpos $endpos }

expr_kind:
  | INT                     { E_int $1 }
  | STRING                  { E_string $1}
  | FLOAT                   { E_float $1 }
  | CHAR                    { E_char $1 }
  | IDENT                   { E_var $1 }
  | unop expr               { E_unop ($1, $2) }       %prec UNARY
  | expr binop expr         { E_binop ($2, $1, $3) }
  | ADDROF expr             { E_addr_of $2 }          %prec UNARY
  | STAR expr               { E_deref $2 }            %prec UNARY
  | expr LBRACK expr RBRACK { E_subscript ($1, $3) }
  | expr DOT expr           { E_member ($1, $3) }     %prec POSTFIX
  | expr ARROW expr         { E_arrow ($1, $3) }      %prec POSTFIX
  | RETURN                  { E_return }
  | builtin LPAR expr RPAR  { E_builtin_call ($1, $3) }

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
  | LBRACE expr DOT DOT expr RBRACE { S_interval ($2, $5) }
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
  | expr COLON args { $1 :: $3 }
  | expr { [ $1 ] }

resource:
  | IDENT { $1 }
