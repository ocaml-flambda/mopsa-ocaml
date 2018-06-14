(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract Syntax Tree extension for the simple Universal language. *)

open Framework.Ast

(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)


type typ +=
  | T_int (** Mathematical integers with arbitrary precision. *)
  | T_float (** Floating-point real numbers. *)
  | T_string (** Strings. *)
  | T_addr (** Heap addresses. *)


(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)


type constant +=
  | C_int of Z.t (** Integer numbers, with arbitrary precision. *)
  | C_float of float (** Floating-point numbers. *)
  | C_string of string (** String constants. *)
  | C_int_interval of Z.t * Z.t (** Integer ranges. *)
  | C_float_interval of float * float (** Float ranges. *)
(** Constants. *)


(*==========================================================================*)
                           (** {2 Operators} *)
(*==========================================================================*)


type operator +=
  (** Unary operators *)
  | O_log_not      (** Logical negation *)
  | O_sqrt         (** Square root *)
  | O_bit_invert   (** bitwise ~ *)


  (** Binary operators *)
  | O_plus       (** + *)
  | O_minus      (** - *)
  | O_mult       (** * *)
  | O_div        (** / *)
  | O_mod        (** % *)
  | O_pow        (** power *)

  | O_eq         (** == *)
  | O_ne         (** != *)
  | O_lt         (** < *)
  | O_le         (** <= *)
  | O_gt         (** > *)
  | O_ge         (** >= *)

  | O_log_or     (** || *)
  | O_log_and    (** && *)

  | O_bit_and    (** & *)
  | O_bit_or     (** | *)
  | O_bit_xor    (** ^ *)
  | O_bit_rshift (** >> *)
  | O_bit_lshift (** << *)

  | O_wrap of Z.t * Z.t (** wrap *)


let is_int_type = function
  | T_int -> true
  | _ -> false
(*==========================================================================*)
                           (** {2 Heap addresses} *)
(*==========================================================================*)

(** Kind of heap addresses, may be used to store extra information. *)
type addr_kind = ..

(** Heap addresses. *)
type addr = {
  addr_kind : addr_kind; (** Kind of a heap address. *)
  addr_range : Framework.Utils.Location.range; (** Range of the allocation site. *)
  addr_uid : int; (** Unique identifier. *)
}

let addr_kind_compare_chain : (addr_kind -> addr_kind -> int) ref = ref (fun ak1 ak2 ->
    compare ak1 ak2
  )

let register_addr_kind_compare cmp =
  addr_kind_compare_chain := cmp !addr_kind_compare_chain

let compare_addr a1 a2 =
  Framework.Utils.Compare.compose [
    (fun () -> Framework.Utils.Location.compare_range a1.addr_range a2.addr_range);
    (fun () -> compare a1.addr_uid a2.addr_uid);
    (fun () -> !addr_kind_compare_chain a1.addr_kind a2.addr_kind);
  ]

(*==========================================================================*)
                           (** {2 Functions} *)
(*==========================================================================*)


(** Function definition *)
type fundec = {
  fun_name: string; (** unique name of the function *)
  fun_parameters: var list; (** list of parameters *)
  fun_locvars : var list; (** list of local variables *)
  fun_body: stmt; (** body of the function *)
  fun_return_type: typ; (** return type *)
}



(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)


type expr_kind +=
  (** Function expression *)
  | E_function of fundec

  (** Function calls *)
  | E_call of expr (** Function expression *) * expr list (** List of arguments *)

  (** Array value as a list of expressions *)

  | E_array of expr list

  (** Subscript access to an indexed object (arrays) *)
  | E_subscript of expr * expr

  (** Allocation of an address on the heap *)
  | E_alloc_addr of addr_kind * Framework.Utils.Location.range

  (** Head address. *)
  | E_addr of addr

let mk_not e = mk_unop O_log_not e

let mk_int i erange =
  mk_constant ~etyp:T_int (C_int (Z.of_int i)) erange

let mk_z i erange =
  mk_constant ~etyp:T_int (C_int i) erange

let mk_float f erange =
  mk_constant ~etyp:T_float (C_float f) erange

let mk_int_interval a b range =
  mk_constant ~etyp:T_int (C_int_interval (Z.of_int a, Z.of_int b)) range

let mk_z_interval a b range =
  mk_constant ~etyp:T_int (C_int_interval (a, b)) range

let mk_float_interval a b range =
  mk_constant ~etyp:T_float (C_float_interval (a, b)) range

let mk_string s =
  mk_constant ~etyp:T_string (C_string s)

let mk_in ?(strict = false) ?(left_strict = false) ?(right_strict = false) v e1 e2 erange =
  match strict, left_strict, right_strict with
  | true, _, _
  | false, true, true ->
    mk_binop
      (mk_binop e1 O_lt v erange)
      O_log_and
      (mk_binop v O_lt e2 erange)
      erange

  | false, true, false ->
    mk_binop
      (mk_binop e1 O_lt v erange)
      O_log_and
      (mk_binop v O_le e2 erange)
      erange

  | false, false, true ->
    mk_binop
      (mk_binop e1 O_le v erange)
      O_log_and
      (mk_binop v O_lt e2 erange)
      erange

  | false, false, false ->
    mk_binop
      (mk_binop e1 O_le v erange)
      O_log_and
      (mk_binop v O_le e2 erange)
      erange

let mk_zero = mk_int 0
let mk_one = mk_int 1


let mk_addr addr range = mk_expr ~etyp:T_addr (E_addr addr) range

let mk_alloc_addr addr_kind addr_range range =
  mk_expr (E_alloc_addr (addr_kind, addr_range)) ~etyp:T_addr range



(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)

type stmt_kind +=
  | S_expression of expr
  (** Expression statement, useful for calling functions without a return value *)

  | S_if of expr (** condition *) * stmt (** then branch *) * stmt (** else branch *)

  | S_block of stmt list (** Sequence block of statements *)

  | S_return of expr option (** Function return with an optional return expression *)

  | S_while of expr (** loop condition *) *
             stmt (** loop body *)
  (** While loops *)

  | S_break (** Loop break *)

  | S_continue (** Loop continue *)

  | S_rebase_addr of addr (** old *) * addr (** new *) * assign_mode
  (** Change the address of a previously allocated object *)

  | S_unit_tests of string (** test file *) * (string * stmt) list (** list of unit tests and their names *)
  (** Unit tests suite *)

  | S_simple_assert of expr * bool * bool
  (** Unit tests simple assertions : S_simple_assert(e,b,b') = b
     is_bottom(assume(b' cond)) where b exp is understood as exp if b
      = true and not exp otherwise *)

  | S_assert of expr
  (** Unit tests assertions *)

let mk_assert e range =
  mk_stmt (S_assert e) range

let mk_simple_assert e b1 b2 range =
  mk_stmt (S_simple_assert (e, b1, b2)) range

let mk_assert_reachable range =
  mk_simple_assert (mk_one range) true false range

let mk_assert_unreachable range =
  mk_simple_assert (mk_one range) true true range

let mk_block block = mk_stmt (S_block block)

let mk_if cond body orelse range =
  mk_stmt (S_if (cond, body, orelse)) range

let mk_while cond body range =
  mk_stmt (S_while (cond, body)) range

let mk_rebase_addr old recent mode range =
  mk_stmt (S_rebase_addr (old, recent, mode)) range

let mk_call fundec args range =
  mk_expr (E_call (
      mk_expr (E_function fundec) range,
      args
    )) range

let kind_of_addr addr = addr.addr_kind

let range_of_addr addr = addr.addr_range
