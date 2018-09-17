(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract Syntax Tree extension for the simple Universal language. *)

open Framework.Essentials


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)


type typ +=
  | T_bool (** Boolean *)
  | T_int (** Mathematical integers with arbitrary precision. *)
  | T_float (** Floating-point real numbers. *)
  | T_string (** Strings. *)
  | T_addr (** Heap addresses. *)
  | T_array of typ (** Array of [typ] *)
  | T_char

let () =
  register_typ_compare (fun next t1 t2 ->
      match t1, t2 with
      | T_array t1, T_array t2 -> compare_typ t1 t2
      | _ -> next t1 t2
    )

(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)


type constant +=
  | C_bool of bool
  | C_int of Z.t (** Integer numbers, with arbitrary precision. *)
  | C_float of float (** Floating-point numbers. *)
  | C_string of string (** String constants. *)
  | C_int_interval of Z.t * Z.t (** Integer ranges. *)
  | C_float_interval of float * float (** Float ranges. *)
(** Constants. *)

let () =
  register_constant_compare (fun next c1 c2 ->
      match c1, c2 with
      | C_int z1, C_int z2 -> Z.compare z1 z2
      | C_float f1, C_float f2 -> Pervasives.compare f1 f2
      | C_string s1, C_string s2 -> Pervasives.compare s1 s2
      | C_int_interval(z1, z1'), C_int_interval(z2, z2') ->
        Compare.compose [
          (fun () -> Z.compare z1 z2);
          (fun () -> Z.compare z1' z2')
        ]
      | C_float_interval(f1, f1'), C_float_interval(f2, f2') ->
        Compare.compose [
          (fun () -> Pervasives.compare f1 f2);
          (fun () -> Pervasives.compare f1' f2')
        ]
      | _ -> next c1 c2
    )

(*==========================================================================*)
                           (** {2 Operators} *)
(*==========================================================================*)


type operator +=
  (* Unary operators *)
  | O_sqrt         (** Square root *)
  | O_bit_invert   (** bitwise ~ *)
  | O_wrap of Z.t * Z.t (** wrap *)

  (* Binary operators *)
  | O_plus       (** + *)
  | O_minus      (** - *)
  | O_mult       (** * *)
  | O_div        (** / *)
  | O_mod        (** % *)
  | O_pow        (** power *)
  | O_bit_and    (** & *)
  | O_bit_or     (** | *)
  | O_bit_xor    (** ^ *)
  | O_bit_rshift (** >> *)
  | O_bit_lshift (** << *)
  | O_concat     (** concatenation of arrays and strings *)

let () =
  register_operator_compare (fun next op1 op2 ->
      match op1, op2 with
      | O_wrap(l1, u1), O_wrap(l2, u2) ->
        Compare.compose [
          (fun () -> Z.compare l1 l2);
          (fun () -> Z.compare u1 u2)
        ]
      | _ -> next op1 op2
    )

(*==========================================================================*)
                         (** {2 Heap addresses} *)
(*==========================================================================*)

(** Kind of heap addresses, may be used to store extra information. *)
type addr_kind = ..

(** Heap addresses. *)
type addr = {
  addr_uid  : int;       (** Unique identifier. *)
  addr_kind : addr_kind; (** Kind de l'adresse. *)
}

type addr_info = {
  compare : (addr -> addr -> int) -> addr -> addr -> int;
  print   : (Format.formatter -> addr -> unit) -> Format.formatter -> addr -> unit;
}

let addr_compare_chain : (addr -> addr -> int) ref =
  ref (fun a1 a2 -> compare a1 a2)

let addr_pp_chain : (Format.formatter -> addr -> unit) ref =
  ref (fun fmt a -> failwith "Pp: Unknown address")

let pp_addr fmt a = !addr_pp_chain fmt a

let compare_addr a b = !addr_compare_chain a b

let register_addr info =
  addr_compare_chain := info.compare !addr_compare_chain;
  addr_pp_chain := info.print !addr_pp_chain;
  ()


(*==========================================================================*)
                           (** {2 Functions} *)
(*==========================================================================*)


(** Function definition *)
type fundec = {
  fun_name: string; (** unique name of the function *)
  fun_parameters: var list; (** list of parameters *)
  fun_locvars : var list; (** list of local variables *)
  mutable fun_body: stmt; (** body of the function *)
  fun_return_type: typ; (** return type *)
}

(*==========================================================================*)
                           (** {2 Programs} *)
(*==========================================================================*)

type program_kind +=
  | P_universal of {
      universal_gvars   : var list;
      universal_fundecs : fundec list;
      universal_main    : stmt;
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
  | E_alloc_addr of addr_kind

  (** Head address. *)
  | E_addr of addr

  (** Length of array or string *)
  | E_len of expr

let () =
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_function(f1), E_function(f2) -> Pervasives.compare f1.fun_name f2.fun_name

      | E_call(f1, args1), E_call(f2, args2) ->
        Compare.compose [
          (fun () -> compare_expr f1 f2);
          (fun () -> Compare.list_compare compare_expr args1 args2)
        ]

      | E_array(el1), E_array(el2) ->
        Compare.list_compare compare_expr el1 el2

      | E_subscript(a1, i1), E_subscript(a2, i2) ->
        Compare.compose [
          (fun () -> compare_expr a1 a2);
          (fun () -> compare_expr i1 i2);
        ]

      | E_alloc_addr(a1), E_alloc_addr(a2) ->
        compare_addr { addr_kind = a1; addr_uid = 0} {addr_kind = a2; addr_uid = 0}

      | E_addr(a1), E_addr(a2) -> compare_addr a1 a2

      | E_len(a1), E_len(a2) -> compare_expr a1 a2

      | _ -> next e1 e2
    )

(*==========================================================================*)
                           (** {2 Utility functions} *)
(*==========================================================================*)

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

let mk_alloc_addr addr_kind range =
  mk_expr (E_alloc_addr addr_kind) ~etyp:T_addr range

let is_int_type = function
  | T_int -> true
  | _ -> false

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

  | S_rebase_addr of addr (** old *) * addr (** new *) * strength
  (** Change the address of a previously allocated object *)

  | S_unit_tests of string (** test file *) * (string * stmt) list (** list of unit tests and their names *)
  (** Unit tests suite *)

  | S_simple_assert of expr * bool * bool
  (** Unit tests simple assertions : S_simple_assert(e,b,b') = b
     is_bottom(assume(b' cond)) where b exp is understood as exp if b
      = true and not exp otherwise *)

  | S_assert of expr
  (** Unit tests assertions *)

  | S_print
  (** Print the abstract flow map at current location *)

let () =
  register_stmt_compare (fun next s1 s2 ->
      match skind s1, skind s2 with
      | S_expression(e1), S_expression(e2) -> compare_expr e1 e2

      | S_if(c1,then1,else1), S_if(c2,then2,else2) ->
        Compare.compose [
          (fun () -> compare_expr c1 c2);
          (fun () -> compare_stmt then1 then2);
          (fun () -> compare_stmt else1 else2);
        ]

      | S_block(sl1), S_block(sl2) -> Compare.list_compare compare_stmt sl1 sl2

      | S_return(e1), S_return(e2) -> Compare.option_compare compare_expr e1 e2

      | S_while(c1, body1), S_while(c2, body2) ->
        Compare.compose [
          (fun () -> compare_expr c1 c2);
          (fun () -> compare_stmt body1 body2)
        ]

      | S_rebase_addr(a1,a1',m1), S_rebase_addr(a2,a2',m2) ->
        Compare.compose [
          (fun () -> compare_addr a1 a2);
          (fun () -> compare_addr a1' a2');
          (fun () -> Pervasives.compare m1 m2);
        ]

      | S_unit_tests(f1, _), S_unit_tests(f2, _) -> Pervasives.compare f1 f2

      | S_simple_assert(e1,b1,b1'), S_simple_assert(e2,b2,b2') ->
        Compare.compose [
          (fun () -> compare_expr e1 e2);
          (fun () -> Pervasives.compare b1 b2);
          (fun () -> Pervasives.compare b1' b2');
        ]

      | S_assert(e1), S_assert(e2) -> compare_expr e1 e2

      | _ -> next s1 s2
    )

let mk_assert e range =
  mk_stmt (S_assert e) range

let mk_simple_assert e b1 b2 range =
  mk_stmt (S_simple_assert (e, b1, b2)) range

let mk_assert_reachable range =
  mk_simple_assert (mk_one range) true false range

let mk_assert_unreachable range =
  mk_simple_assert (mk_one range) true true range

let mk_block block = mk_stmt (S_block block)

let mk_nop range = mk_block [] range

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
