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
                           (** {2 Variables} *)
(*==========================================================================*)


type var = {
  unname : string; (** Unique name. *)
  orgname : string; (** Original name. *)
  vtyp: typ; (** Type. *)
}
(** Program variable. *)

let compare_var v1 v2 = compare v1.unname v2.unname


let unname v = v.unname
let orgname v = v.orgname

let tmp_counter = ref 0

(** Create a temporary variable with a unique name. *)
let mktmp ?(vtyp = T_any) () =
  incr tmp_counter;
  let name = "$tmp" ^ (string_of_int !tmp_counter) in
  {orgname = name; unname = name; vtyp}


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)


type typ +=
  | T_int (** Mathematical integers with arbitrary precision. *)
  | T_float (** Floating-point real numbers. *)
  | T_string (** Strings. *)
  | T_bool (** Booleans. *)
  | T_addr (** Heap addresses. *)
  | T_empty (** Value type of empty arrays *)



(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)


type constant +=
  | C_int of Z.t (** Integer numbers, with arbitrary precision. *)
  | C_float of float (** Floating-point numbers. *)
  | C_string of string (** String constants. *)
  | C_int_range of Z.t * Z.t (** Integer ranges. *)
  | C_float_range of float * float (** Float ranges. *)
  | C_true (** Boolean true value. *)
  | C_false (** Boolean false value. *)
  | C_empty (** Empty value, useful for empty arrays abstraction. *)
(** Constants. *)


(*==========================================================================*)
                           (** {2 Operators} *)
(*==========================================================================*)


type operator +=
  (** Unary operators *)
  | O_log_not (** Logical negation *)
  | O_sqrt (** Square root *)
  | O_invert (** bitwise ~ *)
  | O_abs (** absolute value *)

  (** Binary operators *)
  | O_plus (** + *)
  | O_minus (** - *)
  | O_mult (** * *)
  | O_div (** / *)
  | O_mod (** % *)
  | O_pow (** power *)

  | O_eq (** == *)
  | O_ne (** != *)
  | O_lt (** < *)
  | O_le (** <= *)
  | O_gt (** > *)
  | O_ge (** >= *)

  | O_log_or (** || *)
  | O_log_and (** && *)

  | O_bit_and (** & *)
  | O_bit_or (** | *)
  | O_bit_xor (** ^ *)
  | O_bit_rshift (** >> *)
  | O_bit_lshift (** << *)


(*==========================================================================*)
                           (** {2 Heap addresses} *)
(*==========================================================================*)

(** Kind of heap addresses, may be used to store extra information. *)
type addr_kind = ..

(** Heap addresses. *)
type addr = {
  addr_kind : addr_kind; (** Kind of a heap address. *)
  addr_range : Framework.Ast.range; (** Range of the allocation site. *)
  addr_uid : int; (** Unique identifier. *)
}

let compare_addr a1 a2 =
  Framework.Ast.compare_composer [
    (fun () -> compare_range a1.addr_range a2.addr_range);
    (fun () -> compare a1.addr_kind a2.addr_kind);
    (fun () -> compare a1.addr_uid a2.addr_uid)
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
}



(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)


type expr_kind +=
  (** Variable *)
  | E_var of var

  (** Constant *)

  | E_constant of constant

  (** Unary expressions *)
  | E_unop of operator * expr

  (** Binary expressions *)
  | E_binop of operator * expr * expr

  (** Function expression *)
  | E_function of fundec

  (** Function calls *)
  | E_call of expr (** Function expression *) * expr list (** List of arguments *)

  (** Array value as a list of expressions *)

  | E_array of expr list

  (** Subscript access to an indexed object (arrays) *)
  | E_subscript of expr * expr

  (** Allocation of an address on the heap *)
  | E_alloc_addr of addr_kind * range

  (** Head address. *)
  | E_addr of addr

  (** Access to an attribute of a heap address *)
  | E_addr_attribute of addr * string


let mk_var v erange =
  mk_expr ~etyp:v.vtyp (E_var v) erange

let mk_binop left op right erange =
  mk_expr (E_binop (op, left, right)) erange

let mk_unop op operand erange =
  mk_expr (E_unop (op, operand)) erange

let mk_neg e = mk_unop O_minus e

let mk_not e = mk_unop O_log_not e

let mk_constant ~etyp c = mk_expr ~etyp (E_constant c)

let mk_int i erange =
  mk_constant ~etyp:T_int (C_int (Z.of_int i)) erange

let mk_z i erange =
  mk_expr ~etyp:T_int (E_constant (C_int i)) erange

let mk_float f erange =
  mk_expr ~etyp:T_float (E_constant (C_float f)) erange

let mk_string s =
  mk_constant ~etyp:T_string (C_string s)

let mk_in ?(strict = false) ?(left_strict = false) ?(right_strict = false) v e1 e2 erange =
  match strict, left_strict, right_strict with
  | true, _, _
  | false, true, true ->
    mk_binop
      (mk_binop e1 O_lt v (tag_range erange "in1"))
      O_log_and
      (mk_binop v O_lt e2 (tag_range erange "in2"))
      erange

  | false, true, false ->
    mk_binop
      (mk_binop e1 O_lt v (tag_range erange "in1"))
      O_log_and
      (mk_binop v O_le e2 (tag_range erange "in2"))
      erange

  | false, false, true ->
    mk_binop
      (mk_binop e1 O_le v (tag_range erange "in1"))
      O_log_and
      (mk_binop v O_lt e2 (tag_range erange "in2"))
      erange

  | false, false, false ->
    mk_binop
      (mk_binop e1 O_le v (tag_range erange "in1"))
      O_log_and
      (mk_binop v O_le e2 (tag_range erange "in2"))
      erange

let mk_zero = mk_int 0
let mk_one = mk_int 1

let mk_bool b = mk_constant ~etyp:T_bool (if b then C_true else C_false)
let mk_true = mk_bool true
let mk_false = mk_bool false


let mk_addr addr range = mk_expr ~etyp:T_addr (E_addr addr) range

let mk_addr_attribute addr attr range = mk_expr (E_addr_attribute (addr, attr)) range

(** Extract variables from an expression *)
let acc_vars acc e =
  match ekind e with
  | E_var(v) -> v :: acc
  | _ -> acc

let expr_vars (e: expr) : var list =
  Framework.Visitor.fold_expr
    acc_vars
    (fun acc s -> acc)
    [] e


let rec expr_to_int (e: expr) : int option =
  match ekind e with
  | E_constant (C_int n) -> Some (Z.to_int n)
  | E_unop (O_minus, e') ->
    begin
      match expr_to_int e' with
      | None -> None
      | Some n -> Some (-n)
    end
  | E_binop(op, e1, e2) ->
    begin
      match expr_to_int e1, expr_to_int e2 with
      | Some n1, Some n2 ->
        begin
          match op with
          | O_plus -> Some (n1 + n2)
          | O_minus -> Some (n1 - n2)
          | O_mult -> Some (n1 * n2)
          | O_div -> if n2 = 0 then None else Some (n1 / n2)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None




(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


type stmt_kind +=
  | S_assign of expr (** lvalue *) * expr (** rvalue *)
  (** Assignments. *)

  | S_assume of expr
  (** Filter statements, with condition expressions *)

  | S_expression of expr
  (** Expression statement, useful for calling functions without a return value *)

  | S_expand of expr * expr
  (** Perform an assignment, but without creating a relation between the
      rhs and the lhs.
  *)


  | S_if of expr (** condition *) * stmt (** then branch *) * stmt (** else branch *)

  | S_block of stmt list (** Sequence block of statements *)

  | S_return of expr option (** Function return with an optional return expression *)

  | S_while of expr (** loop condition *) *
             stmt (** loop body *)
  (** While loops *)

  | S_break (** Loop break *)

  | S_continue (** Loop continue *)

  | S_rename_var of var * var
  (** Rename a variable into another*)

  | S_remove_var of var
  (** Remove a variable from the abstract environments. *)

  | S_project_vars of var list
  (** Project the abstract environments on the given list of variables. *)
        
  | S_rebase_addr of addr (** old *) * addr (** new *)
  (** Change the address of a previously allocated object *)

  | S_unit_tests of string (** test file *) * (string * stmt) list (** list of unit tests and their names *)
  (** Unit tests suite *)

  | S_assert of expr
  (** Unit tests assertions *)

let mk_rename v v' =
  mk_stmt (S_rename_var (v, v'))

let mk_assign v e =
  mk_stmt (S_assign (v, e))

let mk_assume e =
  mk_stmt (S_assume e)

let mk_block block = mk_stmt (S_block block)

let mk_nop = mk_block []

let mk_remove_var v = mk_stmt (S_remove_var v)

let stmt_vars (s: stmt) : var list =
  Framework.Visitor.fold_stmt
    acc_vars
    (fun acc s -> acc)
    [] s

let mk_if cond body orelse range =
  mk_stmt (S_if (cond, body, orelse)) range

let mk_rebase_addr old recent range =
  mk_stmt (S_rebase_addr (old, recent)) range

let mk_call fundec args range =
  mk_expr (E_call (
      mk_expr (E_function fundec) (tag_range range "fun"),
      args
    )) range
