%{
    open Ast

    let int_type = assert false

    let pos_to_loc pos = assert false

    let with_range (e: 'a) pos1 pos2 : 'a with_range = (e, (pos_to_loc pos1, pos_to_loc pos2))
%}

(* Constants *)
%token <Z.t> INT
%token <float> FLOAT
%token <string> STRING

(* Identifiers *)
%token <string> IDENT

(* Operators *)
%token ADD SUB MUL DIV MOD
%token LOGAND LOGOR
%token LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT
%token LT GT LE GE EQ NEQ EQUAL

(* Delimiters *)
%token LPAR RPAR
%token LBRACE RBRACE
%token LBRACK RBRACK
%token COLON COMMA SEMICOL DOT
%token EOF

(* Keywords *)
%token REQUIRES LOCAL ASSIGNS CASE ASSUMES ENSURES
%token TRUE FALSE RETURN
%token FORALL EXISTS IN NEW FREE
%token AND OR IMPLIES

%start stub

%type <Ast.stub Ast.with_range> stub

%%

stub:
  | requires_list local_list assigns_list EOF
    {
      with_range {
          stub_requires = $1;
          stub_local    = $2;
          stub_assigns  = $3;
          stub_case     = [];
          stub_ensures  = [];
        }
      $startpos $endpos
    }

  | requires_list local_list assigns_list case_list EOF
    {
      with_range {
        stub_requires = $1;
        stub_local    = $2;
        stub_assigns  = $3;
        stub_case     = $4;
        stub_ensures  = [];
        }
      $startpos $endpos
    }

  | requires_list local_list assigns_list ensures_list EOF
    {
      with_range {
          stub_requires = $1;
          stub_local    = $2;
          stub_assigns  = $3;
          stub_case     = [];
          stub_ensures  = $4;
        }
      $startpos $endpos
    }

requires_list:
  | { [] }
  | requires requires_list { $1 :: $2 }

requires:
  | REQUIRES COLON formula SEMICOL { with_range $3 $startpos $endpos }

local_list:
  | { [] }
  | local local_list { $1 :: $2 }

local:
  | LOCAL COLON var EQUAL local_value SEMICOL
    {
      with_range {
          local_var = $3;
          local_value = $5;
        }
      $startpos $endpos
    }

local_value:
  | NEW resource { LV_new $2 }
  | var LPAR args RPAR { LV_call ($1, $3) }

assigns_list:
  | { [] }
  | assigns assigns_list { $1 :: $2 }

assigns:
  | ASSIGNS COLON expr SEMICOL
    { {assign_target = $3; assign_range = None;} }

  | ASSIGNS COLON expr LBRACE expr DOT DOT expr RBRACE
    { {assign_target = $3; assign_range = Some ($5, $8);} }

case_list:
  | case { [ $1 ] }
  | case case_list { $1 :: $2 }

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
  | assumes assumes_list { $1 :: $2 }

assumes:
  | ASSUMES COLON formula SEMICOL { $3 }

ensures_list:
  | ensures { [ $1 ] }
  | ensures ensures_list { $1 :: $2 }

ensures:
  | ENSURES COLON formula SEMICOL { $3 }

formula:
  | TRUE  { F_bool true }
  | FALSE { F_bool false }

expr:
  | INT { E_int $1 }

args:
  | { [] }
  | expr COLON args { $1 :: $3 }
  | expr { [ $1 ] }

var:
  | IDENT { $1 }

resource:
  | IDENT { $1 }
