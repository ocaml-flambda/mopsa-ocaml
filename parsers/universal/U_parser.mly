(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

(**
  Parser for a very simple C-like "curly bracket" language.

  The supported portion of the Universal grammar is documented in the [GRAMMAR.md] file.
 *)

%{
    open Mopsa_utils
    open U_ast
    let add_declaration decl prog =
      {prog with gvars = decl :: prog.gvars}
    let add_fundec fundec prog =
      {prog with funs = fundec :: prog.funs}
%}

/* tokens */
/**********/

%token TOK_VOID
%token TOK_INT
%token TOK_REAL
%token TOK_CHAR
%token TOK_STRING
%token TOK_ARRAY

%token TOK_TRUE
%token TOK_FALSE
%token TOK_WHILE
%token TOK_FOR
%token TOK_IF
%token TOK_ELSE
%token TOK_RETURN
%token TOK_BREAK
%token TOK_CONTINUE
%token TOK_RAND
%token TOK_RANDF
%token TOK_ROUND
%token TOK_ASSERT
%token TOK_ASSUME
%token TOK_PRINT

%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LCURLY
%token TOK_RCURLY
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_STAR
%token TOK_PLUS
%token TOK_MINUS
%token TOK_EXCLAIM
%token TOK_DIVIDE
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_AND_AND
%token TOK_BAR_BAR
%token TOK_BAR
%token TOK_SEMICOLON
%token TOK_COMMA
%token TOK_EQUAL
%token TOK_CONCAT

%token <string> TOK_id
%token <string> TOK_int
%token <string> TOK_real
%token <string> TOK_string
%token <char> TOK_char


%token TOK_EOF

/* priorities of operators (lowest to highest) */
%left TOK_BAR_BAR
%left TOK_AND_AND
%left TOK_EXCLAIM
%left TOK_NOT_EQUAL TOK_LESS TOK_LESS_EQUAL TOK_GREATER_EQUAL TOK_EQUAL_EQUAL TOK_GREATER
%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_DIVIDE
%left TOK_CONCAT
%left TOK_LBRACKET



%nonassoc lowPrec1
%nonassoc TOK_SEMICOLON
%nonassoc TOK_ELSE

/* entry-point */
/****************/

%start<U_ast.prog> file


/* entry-points for AST fragments (useful for REPL) */
/****************************************************/

%start<U_ast.declaration> declaration_eof
%start<U_ast.fundec> fundec_eof
%start<U_ast.stat> stat_eof
%start<U_ast.expr> expr_eof

%type<bool> boolean_constant
%type<string> integer_constant real_constant string_constant
%type<char> char_constant
%type<(expr ext) array> array_constant
%type<var> var
%type<expr> expr
%type<string> sign_int_literal
%type<typ * var> tvar
%type<declaration> declaration
%type<typ> typ
%type<typ option> topt
%type<stat> block_no_curly stat
%type<unit> ender
%type<fundec> fundec
%type<prog> prog

%type<declaration ext list> list(ext(declaration))
%type<stat ext list> list(ext(stat))
%type<expr ext list> loption(separated_nonempty_list(TOK_COMMA,ext(expr)))
%type<(typ * var) ext list> loption(separated_nonempty_list(TOK_COMMA,ext(tvar)))
%type<expr ext list> loption(separated_nonempty_list(TOK_SEMICOLON,ext(expr)))
%type<expr ext list> separated_nonempty_list(TOK_COMMA,ext(expr))
%type<(typ * var) ext list> separated_nonempty_list(TOK_COMMA,ext(tvar))
%type<expr ext list> separated_nonempty_list(TOK_SEMICOLON,ext(expr))


%%


/* toplevel */
/************/

file: t=prog TOK_EOF { t }


/* expressions */
/***************/


// unary operators
%inline unary_op:
| TOK_PLUS           { AST_UNARY_PLUS }
| TOK_MINUS          { AST_UNARY_MINUS }
| TOK_EXCLAIM        { AST_NOT }
| TOK_ROUND          { AST_ROUND }


// binary operators
%inline binary_op:
| TOK_STAR           { AST_MULTIPLY }
| TOK_DIVIDE         { AST_DIVIDE }
| TOK_PLUS           { AST_PLUS }
| TOK_MINUS          { AST_MINUS }
| TOK_LESS           { AST_LESS }
| TOK_GREATER        { AST_GREATER }
| TOK_LESS_EQUAL     { AST_LESS_EQUAL }
| TOK_GREATER_EQUAL  { AST_GREATER_EQUAL }
| TOK_EQUAL_EQUAL    { AST_EQUAL }
| TOK_NOT_EQUAL      { AST_NOT_EQUAL }
| TOK_CONCAT         { AST_CONCAT }
| TOK_AND_AND        { AST_AND }
| TOK_BAR_BAR        { AST_OR }

boolean_constant:
| TOK_TRUE {true}
| TOK_FALSE {false}

integer_constant:
| v=TOK_int {v}

real_constant:
| v=TOK_real {v}

string_constant:
| v=TOK_string {v}

char_constant:
| c=TOK_char {c}

array_constant:
| TOK_LBRACKET l=separated_list(TOK_SEMICOLON, ext(expr)) TOK_RBRACKET
    {Array.of_list l}

var:
| v=TOK_id
    {v}

expr:
| TOK_LPAREN TOK_RPAREN
    { AST_unit_const }
| TOK_LPAREN e=expr TOK_RPAREN
    { e }
| ext(integer_constant)
    { AST_int_const $1}
| ext(real_constant)
    { AST_real_const $1}
| ext(string_constant)
    { AST_string_const $1}
| ext(char_constant)
    { AST_char_const $1}
| ext(array_constant)
    { AST_array_const $1}

| ext(boolean_constant)
     {AST_bool_const $1}
| ext(var)
    { AST_identifier $1}

| o=unary_op e=ext(expr)
    { AST_unary (o,e) }

| e1=ext(expr) o=binary_op e2=ext(expr)
    { AST_binary (o,e1,e2) }

| TOK_RAND TOK_LPAREN TOK_RPAREN
    { AST_rand_string }

| TOK_RAND TOK_LPAREN e1=ext(sign_int_literal) TOK_COMMA e2=ext(sign_int_literal) TOK_RPAREN
    { AST_rand (e1, e2) }

| TOK_RANDF TOK_LPAREN e1=ext(real_constant) TOK_COMMA e2=ext(real_constant) TOK_RPAREN
    { AST_randf (e1, e2) }

| TOK_BAR e=ext(expr) TOK_BAR
    { AST_len (e)}

| e1=ext(expr) TOK_LBRACKET e2=ext(expr) TOK_RBRACKET
   { AST_array_access(e1, e2) }

| va=ext(var) TOK_LPAREN args=separated_list(TOK_COMMA, ext(expr)) TOK_RPAREN
   { AST_fun_call(va, args)}


sign_int_literal:
| i=TOK_int            { i }
| TOK_PLUS i=TOK_int   { i }
| TOK_MINUS i=TOK_int  { "-"^i }



/* statements */
/**************/

// a declaration, simply "int x"
tvar:
| t=typ v=TOK_id { t, v }

declaration:
| tv=ext(tvar) TOK_SEMICOLON
    {(tv, None)}
| tv=ext(tvar) TOK_EQUAL e=ext(expr) TOK_SEMICOLON
    {(tv, Some e)}

typ:
| TOK_INT { AST_INT }
| TOK_REAL { AST_REAL }
| t=typ TOK_ARRAY { AST_ARRAY t }
| TOK_STRING { AST_STRING }
| TOK_CHAR { AST_CHAR }

separated_ended_list(X, Y):
| l=separated_list(X, Y) X {l}
block_no_curly:
| l=list(ext(stat))
      {AST_block l}

    // statements

%inline topt:
| TOK_VOID { None }
| t=typ { Some t }

stat:
| TOK_LCURLY l=block_no_curly TOK_RCURLY
  { l }

| e=ext(expr) TOK_EQUAL f=ext(expr) TOK_SEMICOLON
  { AST_assign (e, f) }

| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) ender
  { AST_if (e, s, None) }

| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) TOK_ELSE t=ext(stat) ender
  { AST_if (e, s, Some t) }

| TOK_WHILE TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) ender
  { AST_while (e, s) }

| TOK_ASSERT TOK_LPAREN e=ext(expr) TOK_RPAREN TOK_SEMICOLON
  { AST_assert e }

| TOK_ASSUME TOK_LPAREN e=ext(expr) TOK_RPAREN TOK_SEMICOLON
  { AST_assume e }

| TOK_PRINT TOK_LPAREN TOK_RPAREN TOK_SEMICOLON
  { AST_print }

| TOK_RETURN e=ext(expr) TOK_SEMICOLON
   { AST_return (Some e) }

| TOK_RETURN TOK_SEMICOLON
   { AST_return None }

| TOK_BREAK TOK_SEMICOLON
   { AST_break }

| TOK_CONTINUE TOK_SEMICOLON
   { AST_continue }

| TOK_FOR TOK_LPAREN v=ext(var) TOK_COMMA e1=ext(expr) TOK_COMMA e2=ext(expr) TOK_RPAREN st=ext(stat) ender
   { AST_for(v, e1, e2, st) }

| e=ext(expr) TOK_SEMICOLON
      { AST_expr e }

ender:
| TOK_SEMICOLON {}
| %prec lowPrec1 {}

fundec:
| t=topt f=var TOK_LPAREN args=separated_list(TOK_COMMA, ext(tvar)) TOK_RPAREN TOK_LCURLY
   ldec=list(ext(declaration))
   st=ext(block_no_curly)
   TOK_RCURLY
{{funname = f; parameters = args; body=st ; locvars = ldec; return_type = t; range = Location.from_lexing_range $startpos $endpos}}

prog:
| st=ext(block_no_curly)
       {{gvars = []; funs = [] ; main = st}}
| d=ext(declaration) prg=prog {add_declaration d prg}
| d=ext(declaration) TOK_SEMICOLON prg=prog {add_declaration d prg}
| d=ext(fundec) prg=prog {add_fundec d prg}
| d=ext(fundec) TOK_SEMICOLON prg=prog {add_fundec d prg}

/* utilities */
/*************/

// adds extent information to rule
%inline ext(X):
| x=X { x, Location.from_lexing_range $startpos $endpos }


/* rules for AST fragments */
/***************************/

expr_eof:
| expr TOK_EOF { $1 }

stat_eof:
| stat TOK_EOF { $1 }

declaration_eof:
| declaration TOK_EOF { $1 }

fundec_eof:
| fundec TOK_EOF { $1 }

%%
