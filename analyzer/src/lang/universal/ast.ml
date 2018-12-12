(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract Syntax Tree extension for the simple Universal language. *)

open Mopsa


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

type float_prec =
  | F_SINGLE      (** IEEE single-precision 32-bit *)
  | F_DOUBLE      (** IEEE double-precision 64-bit *)
  | F_LONG_DOUBLE (** extended precision, abstracted as double *)
  | F_REAL        (** no rounding, abstracted as double *)

type typ +=
  | T_bool (** Boolean *)
  | T_int (** Mathematical integers with arbitrary precision. *)
  | T_float of float_prec (** Floating-point real numbers. *)
  | T_string (** Strings. *)
  | T_addr (** Heap addresses. *)
  | T_tree (** Tree type *)
  | T_array of typ (** Array of [typ] *)
  | T_unit (** Unit type *)
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
  | C_unit
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
  | O_cast         (** Cast *)

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
  fun_range: range; (** function range *)
  fun_parameters: var list; (** list of parameters *)
  fun_locvars : var list; (** list of local variables *)
  mutable fun_body: stmt; (** body of the function *)
  fun_return_type: typ option; (** return type *)
  fun_return_var: var; (** variable storing the return value *)
}

type fun_builtin =
  { name: string;
    args: typ option list;
    output: typ
  }

type fun_expr =
  | User_defined of fundec
  | Builtin of fun_builtin

let compare_fun_expr x y = match x, y with
  | User_defined a, User_defined b -> Pervasives.compare a.fun_name b.fun_name
  | Builtin a, Builtin b -> Pervasives.compare a b
  | _ -> 1

(*==========================================================================*)
                           (** {2 Programs} *)
(*==========================================================================*)

type program +=
  | P_universal of {
      universal_gvars   : var list;
      universal_fundecs : fundec list;
      universal_main    : stmt;
    }

(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)
type tc =
  | TC_int of expr
  | TC_symbol of expr * expr list

type expr_kind +=
  (** Function expression *)
  | E_function of fun_expr

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

  (** Tree construction *)
  | E_tree of tc

let () =
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_function(f1), E_function(f2) -> compare_fun_expr f1 f2

      | E_call(f1, args1), E_call(f2, args2) ->
        Compare.compose [
          (fun () -> compare_expr f1 f2);
          (fun () -> Compare.list compare_expr args1 args2)
        ]

      | E_array(el1), E_array(el2) ->
        Compare.list compare_expr el1 el2

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

let mk_float ?(prec=F_DOUBLE) f erange =
  mk_constant ~etyp:(T_float prec) (C_float f) erange

let mk_int_interval a b range =
  mk_constant ~etyp:T_int (C_int_interval (Z.of_int a, Z.of_int b)) range

let mk_z_interval a b range =
  mk_constant ~etyp:T_int (C_int_interval (a, b)) range

let mk_float_interval ?(prec=F_DOUBLE) a b range =
  mk_constant ~etyp:(T_float prec) (C_float_interval (a, b)) range

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

let mk_bool b range = mk_constant ~etyp:T_bool (C_bool b) range
let mk_true = mk_bool true
let mk_false = mk_bool false

let mk_addr addr range = mk_expr ~etyp:T_addr (E_addr addr) range

let mk_alloc_addr addr_kind range =
  mk_expr (E_alloc_addr addr_kind) ~etyp:T_addr range

let is_int_type = function
  | T_int -> true
  | _ -> false

let is_float_type = function
  | T_float _ -> true
  | _ -> false

let is_numeric_type = function
  | T_int | T_float _ -> true
  | _ -> false

let is_math_type = function
  | T_int | T_float _ | T_bool -> true
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

  | S_rebase_addr of addr (** old *) * addr (** new *) * mode
  (** Change the address of a previously allocated object *)

  | S_unit_tests of (string * stmt) list (** list of unit tests and their names *)
  (** Unit tests suite *)

  | S_simple_assert of expr * bool * bool
  (** Unit tests simple assertions : S_simple_assert(e,b,b') = b
     is_bottom(assume(b' cond)) where b exp is understood as exp if b
      = true and not exp otherwise *)

  | S_assert of expr
  (** Unit tests assertions *)

  | S_print
  (** Print the abstract flow map at current location *)

  | S_forget of var
  (** Forgets variable *)


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

      | S_block(sl1), S_block(sl2) -> Compare.list compare_stmt sl1 sl2

      | S_return(e1), S_return(e2) -> Compare.option compare_expr e1 e2

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

      | S_unit_tests(tl1), S_unit_tests(tl2) ->
        Compare.list (fun (t1, _) (t2, _) -> Pervasives.compare t1 t2) tl1 tl2

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
      mk_expr (E_function (User_defined fundec)) range,
      args
    )) range

let mk_expr_stmt e =
  mk_stmt (S_expression e)

let rec expr_to_z (e: expr) : Z.t option =
  match ekind e with
  | E_constant (C_int n) -> Some n
  | E_unop (O_minus, e') ->
    begin
      match expr_to_z e' with
      | None -> None
      | Some n -> Some (Z.neg n)
    end
  | E_binop(op, e1, e2) ->
    begin
      match expr_to_z e1, expr_to_z e2 with
      | Some n1, Some n2 ->
        begin
          match op with
          | O_plus -> Some (Z.add n1 n2)
          | O_minus -> Some (Z.sub n1 n2)
          | O_mult -> Some (Z.mul n1 n2)
          | O_div -> if Z.equal n2 Z.zero then None else Some (Z.div n1 n2)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None
