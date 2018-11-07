(*
   Definition of the abstract syntax trees output by the parser.
*)


open Lexing


(* position in the source file, we use ocamllex's default type *)
type position = Lexing.position
let position_unknown = Lexing.dummy_pos


(* extents are pairs of positions *)
type extent = position * position (* start/end *)
let extent_unknown = (position_unknown, position_unknown)


(* Many parts of the syntax are tagged with an extent indicating which
   part of the parser-file corresponds to the sub-tree.
   This is very useful for interesting error reporting!
 *)
type 'a ext = 'a * extent

(* variable identifiers, just strings *)
(* local variables and scoping would require using UNIQUE IDENTIFIERS
   to handle the case where several variables have the same name
 *)
type var = string

(* types: only integers (mathematical integers, in Z) *)
type typ =
    AST_INT
  | AST_REAL
  | AST_ARRAY of typ
  | AST_STRING
  | AST_CHAR
  | AST_TREE
  | AST_UNIT

(* unary expression operators *)
type unary_op =
  | AST_UNARY_PLUS     (* +e *)
  | AST_UNARY_MINUS    (* -e *)
  | AST_NOT            (* !e logical negation *)

(* binary expression operators *)
type binary_op =
  | AST_PLUS          (* e + e *)
  | AST_MINUS         (* e - e *)
  | AST_MULTIPLY      (* e * e *)
  | AST_DIVIDE        (* e / e *)
  | AST_EQUAL         (* e == e *)
  | AST_NOT_EQUAL     (* e != e *)
  | AST_LESS          (* e < e *)
  | AST_LESS_EQUAL    (* e <= e *)
  | AST_GREATER       (* e > e *)
  | AST_GREATER_EQUAL (* e >= e *)
  | AST_AND           (* e && e *)
  | AST_OR            (* e || e *)
  | AST_CONCAT        (* e @ e *)

(* tree constructor *)
type tree_constructor =
  | Symbol of expr ext * expr ext list
  | Int of expr ext

(* expressions *)
and expr =
  (* unary operation *)
  | AST_unary of unary_op * (expr ext)

  (* length of an array or a string *)
  | AST_len of (expr ext)

  (* binary operation *)
  | AST_binary of binary_op * (expr ext) * (expr ext)

  (* variable use *)
  | AST_identifier of var ext

  (* integer constants (integers are still in their string representation) *)
  | AST_int_const of string ext

  (* unit constant *)
  | AST_unit_const

  (* boolean constants *)
  | AST_bool_const of bool ext

  (* real constants (reals are still in their string representation) *)
  | AST_real_const of string ext

  (* string constants *)
  | AST_string_const of string ext

  (* char constants *)
  | AST_char_const of char ext

  (* array construction *)
  | AST_array_const of (expr ext) array ext

  (* non-deterministic choice between two integers *)
  | AST_rand of (string ext) (* lower bound *) *
                (string ext) (* upper bound *)

  (* array accesses *)
  | AST_array_access of (expr ext * expr ext)

  (* function call *)
  | AST_fun_call of var ext * expr ext list

  (* tree constructor *)
  | AST_tree of tree_constructor


type typed_var =
  (typ * var) ext

type declaration =
  (typed_var * expr ext option)

(* statements *)
type stat =
  | AST_block of (stat ext) list

  (* assignment of integer expression: e1 = e2 *)
  | AST_assign of (expr ext) * (expr ext)

  (* if-then-else, with boolean condition;
     the else branch is optional
   *)
  | AST_if of (expr ext) (* condition *) *
              (stat ext) (* then branch *) *
              (stat ext option) (* optional else *)

  (* while loop, with boolean condition *)
  | AST_while of (expr ext) (* condition *) *
                 (stat ext) (* body *)

  | AST_for of (var ext * expr ext * expr ext * stat ext)

  | AST_return of (expr ext) option

  | AST_break

  | AST_continue

  (* assertion: exit if the boolean expression does not hold *)
  | AST_assert of expr ext

  (* evaluates expression (useful for function calls) *)
  | AST_expr of expr ext
                
  | AST_print

type fundec =
  {
    funname : var;
    parameters : typed_var list;
    body : stat ext;
    locvars : declaration ext list;
    return_type : typ option;
  }

(* a program is a list of statements preceded by function declaration *)
type prog =
  { gvars : declaration ext list;
    funs  : fundec ext list;
    main  : stat ext
  }
