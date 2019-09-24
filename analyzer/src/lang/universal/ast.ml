(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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

(** Abstract Syntax Tree extension for the simple Universal language. *)

open Mopsa
open Format


(** {2 Universal types} *)
(*  ******************* *)

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
  | T_array of typ (** Array of [typ] *)
  | T_unit (** Unit type *)
  | T_char

let pp_float_prec fmt = function
  | F_SINGLE      -> pp_print_string fmt "float"
  | F_DOUBLE      -> pp_print_string fmt "double"
  | F_LONG_DOUBLE -> pp_print_string fmt "long double"
  | F_REAL        -> pp_print_string fmt "real"

let pp_float_op opreal opfloat fmt = function
  | F_SINGLE      -> fprintf fmt "%sf" opfloat
  | F_DOUBLE      -> fprintf fmt "%sd" opfloat
  | F_LONG_DOUBLE -> fprintf fmt "%sl" opreal
  | F_REAL        -> pp_print_string fmt opreal

let () =
  register_typ {
    compare = (fun next t1 t2 ->
        match t1, t2 with
        | T_array t1, T_array t2 -> compare_typ t1 t2
        | _ -> next t1 t2
      );

    print = (fun default fmt typ ->
        match typ with
        | T_unit -> pp_print_string fmt "unit"
        | T_bool -> pp_print_string fmt "bool"
        | T_int -> pp_print_string fmt "int"
        | T_float p -> pp_float_prec fmt p
        | T_string -> pp_print_string fmt "string"
        | T_addr -> pp_print_string fmt "addr"
        | T_char -> pp_print_string fmt "char"
        | T_array t -> Format.fprintf fmt "[%a]" pp_typ t
        | _ -> default fmt typ
      );
  }


(** {2 Universal constants} *)
(*  *********************** *)

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
  register_constant {
    compare = (fun next c1 c2 ->
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
      );

    print = (fun default fmt -> function
        | C_unit -> fprintf fmt "()"
        | C_bool(b) -> fprintf fmt "%a" Format.pp_print_bool b
        | C_string(s) -> fprintf fmt "\"%s\"" s
        | C_int(n) -> Z.pp_print fmt n
        | C_float(f) -> pp_print_float fmt f
        | C_int_interval(a,b) -> fprintf fmt "[%a,%a]" Z.pp_print a Z.pp_print b
        | C_float_interval(a,b) -> fprintf fmt "[%a,%a]" pp_print_float a pp_print_float b
        | c -> default fmt c
      );
  }


(** {2 Universal operators} *)
(*  *********************** *)

type operator +=
  (* Unary operators *)
  | O_sqrt              (** Square root *)
  | O_bit_invert        (** bitwise ~ *)
  | O_wrap of Z.t * Z.t (** wrap *)
  | O_cast of typ * typ (** Cast *)

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
  register_operator {
    compare = (fun next op1 op2 ->
        match op1, op2 with
        | O_wrap(l1, u1), O_wrap(l2, u2) ->
          Compare.compose [
            (fun () -> Z.compare l1 l2);
            (fun () -> Z.compare u1 u2)
          ]
        | _ -> next op1 op2
      );
    print = (fun default fmt op ->
        match op with
        | O_plus       -> pp_print_string fmt "+"
        | O_minus      -> pp_print_string fmt "-"
        | O_mult       -> pp_print_string fmt "*"
        | O_div        -> pp_print_string fmt "/"
        | O_mod        -> pp_print_string fmt "%"
        | O_pow        -> pp_print_string fmt "**"
        | O_sqrt       -> pp_print_string fmt "sqrt"
        | O_bit_invert -> pp_print_string fmt "~"
        | O_wrap(l,u)  -> fprintf fmt "wrap(%a, %a)" Z.pp_print l Z.pp_print u
        | O_concat     -> pp_print_string fmt "@"
        | O_bit_and    -> pp_print_string fmt "&"
        | O_bit_or     -> pp_print_string fmt "|"
        | O_bit_xor    -> pp_print_string fmt "^"
        | O_bit_rshift -> pp_print_string fmt ">>"
        | O_bit_lshift -> pp_print_string fmt "<<"
        | O_cast(t1,t2)    -> fprintf fmt "cast[%a->%a]" pp_typ t1 pp_typ t2
        | op           -> default fmt op
      );
  }



(** {2 Universal heap addresses} *)
(*  **************************** *)

(** Kind of heap addresses, used to store extra information. *)
type addr_kind = ..

let addr_kind_compare_chain : (addr_kind -> addr_kind -> int) ref =
  ref (fun a1 a2 -> compare a1 a2)

let addr_kind_pp_chain : (Format.formatter -> addr_kind -> unit) ref =
  ref (fun fmt a -> panic "addr_kind_pp_chain: unknown address")

let pp_addr_kind fmt ak =
  !addr_kind_pp_chain fmt ak

let compare_addr_kind ak1 ak2 =
  if ak1 == ak2 then 0 else
  !addr_kind_compare_chain ak1 ak2

let register_addr_kind (info: addr_kind info) =
  addr_kind_compare_chain := info.compare !addr_kind_compare_chain;
  addr_kind_pp_chain := info.print !addr_kind_pp_chain;
  ()


(** Addresses are grouped by static criteria to make them finite *)
type addr_group = ..

type addr_group +=
  | G_all (** Group all addresses into one *)

let addr_group_compare_chain : (addr_group -> addr_group -> int) ref =
  ref (fun a1 a2 -> compare a1 a2)

let addr_group_pp_chain : (Format.formatter -> addr_group -> unit) ref =
  ref (fun fmt g ->
      match g with
      | G_all -> Format.pp_print_string fmt "*"
      | _ -> panic "addr_group_pp_chain: unknown address"
    )

let pp_addr_group fmt ak =
  !addr_group_pp_chain fmt ak

let compare_addr_group a1 a2 =
  if a1 == a2 then 0 else !addr_group_compare_chain a1 a2

let register_addr_group (info: addr_group info) =
  addr_group_compare_chain := info.compare !addr_group_compare_chain;
  addr_group_pp_chain := info.print !addr_group_pp_chain;
  ()


(** Heap addresses. *)
type addr = {
  addr_kind : addr_kind;   (** Kind of the address. *)
  addr_group : addr_group; (** Group of the address *)
  addr_mode : mode;        (** Assignment mode of address (string or weak) *)
}


let akind addr = addr.addr_kind

let pp_addr fmt a =
  if a.addr_group = G_all then
    fprintf fmt "@@%a:*:%s"
      pp_addr_kind a.addr_kind
      (match a.addr_mode with WEAK -> "w" | STRONG -> "s")
  else
    fprintf fmt "@@%a:%xd:%s"
      pp_addr_kind a.addr_kind
      (* Using Hashtbl.hash leads to collisions. Hashtbl.hash is
         equivalent to Hashtbl.hash_param 10 100. By increasing the
         number of meaningful nodes to encounter, collisions are less
         likely to happen.
      *)
      (Hashtbl.hash_param 30 100 a.addr_group)
      (match a.addr_mode with WEAK -> "w" | STRONG -> "s")



let compare_addr a b =
  if a == b then 0
  else Compare.compose [
      (fun () -> compare_addr_kind a.addr_kind b.addr_kind);
      (fun () -> compare_addr_group a.addr_group b.addr_group);
      (fun () -> compare_mode a.addr_mode b.addr_mode);
    ]


(** Address variables *)
type var_kind +=
  | V_addr_attr of addr * string

let () =
  register_var {
    compare = (fun next v1 v2 ->
        match vkind v1, vkind v2 with
        | V_addr_attr (a1,attr1), V_addr_attr (a2,attr2) ->
          Compare.compose [
            (fun () -> compare attr1 attr2);
            (fun () -> compare_addr a1 a2)
          ]
        | _ -> next v1 v2
      );
    print = (fun next fmt v ->
        match vkind v with
        | V_addr_attr (addr, attr) -> Format.fprintf fmt "%a.%s" pp_addr addr attr
        | _ -> next fmt v
      )
  }

let mk_addr_attr addr attr typ =
  let name =
    let () = Format.fprintf Format.str_formatter "%a.%s" pp_addr addr attr in
    Format.flush_str_formatter ()
  in
  mkv name (V_addr_attr (addr,attr)) typ



(** {2 Universal functions} *)
(*  *********************** *)


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


(** {2 Universal program} *)
(*  ********************* *)

type prog_kind +=
  | P_universal of {
      universal_gvars   : var list;
      universal_fundecs : fundec list;
      universal_main    : stmt;
    }

let () =
  register_program {
    compare = (fun next -> next);
    print   = (fun default fmt prg ->
        match prg.prog_kind with
        | P_universal (u_prog) ->
          Format.fprintf fmt "@[<v>%a@,%a@]"
            (
              pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
                (fun fmt f ->
                   fprintf fmt "%a %a(%a) {@\n@[<v 4>  %a@]@\n}"
                     (fun fmt ot ->
                        match ot with
                        | None -> pp_print_string fmt "void"
                        | Some t -> pp_typ fmt t
                     ) f.fun_return_type
                     Format.pp_print_string f.fun_name
                     (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                        (fun fmt v -> Format.fprintf fmt "%a %a"
                            pp_typ v.vtyp
                            pp_var v
                        )
                     ) f.fun_parameters
                     pp_stmt f.fun_body
                )
            ) u_prog.universal_fundecs
            pp_stmt u_prog.universal_main
        | _ -> default fmt prg
      );
  }


(** {2 Universal expressions} *)
(*  ************************* *)

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
  | E_alloc_addr of addr_kind * mode

  (** Head address. *)
  | E_addr of addr

  (** Length of array or string *)
  | E_len of expr


let () =
  register_expr_with_visitor {
    compare = (fun next e1 e2 ->
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

        | E_alloc_addr(ak1, m1), E_alloc_addr(ak2, m2) ->
          Compare.compose [
            (fun () -> compare_addr_kind ak1 ak2);
            (fun () -> compare_mode m1 m2);
          ]

        | E_addr(a1), E_addr(a2) ->
          compare_addr a1 a2

        | E_len(a1), E_len(a2) -> compare_expr a1 a2

        | _ -> next e1 e2
      );

    print = (fun default fmt exp ->
        match ekind exp with
        | E_array(el) ->
          fprintf fmt "[@[<h>%a@]]"
            (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) el
        | E_subscript(v, e) -> fprintf fmt "%a[%a]" pp_expr v pp_expr e
        | E_function(f) -> fprintf fmt "fun %s" (match f with | User_defined f -> f.fun_name | Builtin f -> f.name)
        | E_call(f, args) ->
          fprintf fmt "%a(%a)"
            pp_expr f
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) args
        | E_alloc_addr(akind, mode) -> fprintf fmt "alloc(%a, %a)" pp_addr_kind akind pp_mode mode
        | E_addr (addr) -> fprintf fmt "%a" pp_addr addr
        | E_len exp -> Format.fprintf fmt "|%a|" pp_expr exp
        | _ -> default fmt exp
      );

    visit = (fun default exp ->
        match ekind exp with
        | E_function _ -> leaf exp

        | E_subscript(v, e) ->
          {exprs = [v; e]; stmts = []},
          (fun parts -> {exp with ekind = (E_subscript(List.hd parts.exprs, List.hd @@ List.tl parts.exprs))})

        | E_alloc_addr _ -> leaf exp

        | E_addr _ -> leaf exp

        | E_array(el) ->
          {exprs = el; stmts = []},
          (fun parts -> {exp with ekind = E_array parts.exprs})

        | E_call(f, args) ->
          {exprs = f :: args; stmts = []},
          (fun parts -> {exp with ekind = E_call(List.hd parts.exprs, List.tl parts.exprs)})

        | _ -> default exp
      );
  }


(** {2 Universal statements} *)
(*  ************************ *)

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

   | S_unit_tests of (string * stmt) list (** list of unit tests and their names *)
   (** Unit tests suite *)

   | S_assert of expr
   (** Unit tests assertions *)

   | S_satisfy of expr
   (** Unit tests satisfiability check *)

   | S_print
   (** Print the abstract flow map at current location *)

   | S_free_addr of addr (** release an address *)


let () =
  register_stmt_with_visitor {
    compare = (fun next s1 s2 ->
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

        | S_unit_tests(tl1), S_unit_tests(tl2) ->
          Compare.list (fun (t1, _) (t2, _) -> Pervasives.compare t1 t2) tl1 tl2

        | S_assert(e1), S_assert(e2) -> compare_expr e1 e2

        | S_satisfy(e1), S_satisfy(e2) -> compare_expr e1 e2

        | S_free_addr a1, S_free_addr a2 ->
          compare_addr a1 a2
        | _ -> next s1 s2
      );

    print = (fun default fmt stmt ->
        match skind stmt with
        | S_expression(e) -> fprintf fmt "%a;" pp_expr e
        | S_if(e, s1, s2) ->
          fprintf fmt "@[<v 4>if (%a) {@,%a@]@,@[<v 4>} else {@,%a@]@,}" pp_expr e pp_stmt s1 pp_stmt s2
        | S_block[] -> fprintf fmt "pass"
        | S_block[s] -> pp_stmt fmt s
        | S_block(l) ->
          fprintf fmt "@[<v>";
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            pp_stmt
            fmt l
          ;
          fprintf fmt "@]"
        | S_return(None) -> pp_print_string fmt "return;"
        | S_return(Some e) -> fprintf fmt "return %a;" pp_expr e
        | S_while(e, s) ->
          fprintf fmt "@[<v 4>while %a {@,%a@]@,}" pp_expr e pp_stmt s
        | S_break -> pp_print_string fmt "break;"
        | S_continue -> pp_print_string fmt "continue;"
        | S_unit_tests (tests) -> pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") (fun fmt (name, test) -> fprintf fmt "test %s:@\n  @[%a@]" name pp_stmt test) fmt tests
        | S_assert e -> fprintf fmt "assert(%a);" pp_expr e
        | S_satisfy e -> fprintf fmt "sat(%a);" pp_expr e
        | S_print -> fprintf fmt "print();"
        | S_free_addr a -> fprintf fmt "free_addr(%a);" pp_addr a
        | _ -> default fmt stmt
      );

    visit = (fun default stmt ->
        match skind stmt with
        | S_break
        | S_continue -> leaf stmt

        | S_expression(e) ->
          {exprs = [e]; stmts = []},
          (fun parts -> {stmt with skind = S_expression(List.hd parts.exprs)})


        | S_if(e, s1, s2) ->
          {exprs = [e]; stmts = [s1; s2]},
          (fun parts -> {stmt with skind = S_if(List.hd parts.exprs, List.hd parts.stmts, List.nth parts.stmts 1)})

        | S_while(e, s)  ->
          {exprs = [e]; stmts = [s]},
          (fun parts -> {stmt with skind = S_while(List.hd parts.exprs, List.hd parts.stmts)})

        | S_block(sl) ->
          {exprs = []; stmts = sl},
          (fun parts -> {stmt with skind = S_block(parts.stmts)})

        | S_return(None) -> leaf stmt

        | S_return(Some e) ->
          {exprs = [e]; stmts = []},
          (function {exprs = [e]} -> {stmt with skind = S_return(Some e)} | _ -> assert false)

        | S_assert(e) ->
          {exprs = [e]; stmts = []},
          (function {exprs = [e]} -> {stmt with skind = S_assert(e)} | _ -> assert false)


        | S_satisfy(e) ->
          {exprs = [e]; stmts = []},
          (function {exprs = [e]} -> {stmt with skind = S_satisfy(e)} | _ -> assert false)

        | S_unit_tests(tests) ->
          let tests_names, tests_bodies = List.split tests in
          {exprs = []; stmts = tests_bodies},
          (function {stmts = tests_bodies} ->
             let tests = List.combine tests_names tests_bodies in
             {stmt with skind = S_unit_tests(tests)}
          )

        | S_free_addr _ -> leaf stmt

        | S_print -> leaf stmt

        | _ -> default stmt
      );
  }


(** {2 Utility functions} *)
(*  ********************* *)

let rec is_universal_type t =
  match t with
  | T_bool | T_int | T_float _
  | T_string | T_addr   | T_unit | T_char ->
    true

  | T_array tt -> is_universal_type tt

  | _ -> false

let mk_not e = mk_unop O_log_not e

let mk_int i ?(typ=T_int) erange =
  mk_constant ~etyp:typ (C_int (Z.of_int i)) erange

let mk_z i ?(typ=T_int) erange =
  mk_constant ~etyp:typ (C_int i) erange

let mk_float ?(prec=F_DOUBLE) f erange =
  mk_constant ~etyp:(T_float prec) (C_float f) erange

let mk_int_interval a b ?(typ=T_int) range =
  mk_constant ~etyp:typ (C_int_interval (Z.of_int a, Z.of_int b)) range

let mk_z_interval a b ?(typ=T_int) range =
  mk_constant ~etyp:typ (C_int_interval (a, b)) range

let mk_float_interval ?(prec=F_DOUBLE) a b range =
  mk_constant ~etyp:(T_float prec) (C_float_interval (a, b)) range

let mk_string s =
  mk_constant ~etyp:T_string (C_string s)

let mk_in ?(strict = false) ?(left_strict = false) ?(right_strict = false) ?(etyp=T_bool) v e1 e2 erange =
  match strict, left_strict, right_strict with
  | true, _, _
  | false, true, true ->
    mk_binop
      (mk_binop e1 O_lt v ~etyp erange)
      O_log_and
      (mk_binop v O_lt e2 ~etyp erange)
      ~etyp
      erange

  | false, true, false ->
    mk_binop
      (mk_binop e1 O_lt v ~etyp erange)
      O_log_and
      (mk_binop v O_le e2 ~etyp erange)
      ~etyp
      erange

  | false, false, true ->
    mk_binop
      (mk_binop e1 O_le v ~etyp erange)
      O_log_and
      (mk_binop v O_lt e2 ~etyp erange)
      ~etyp
      erange

  | false, false, false ->
    mk_binop
      (mk_binop e1 O_le v ~etyp erange)
      O_log_and
      (mk_binop v O_le e2 ~etyp erange)
      ~etyp
      erange

let mk_zero = mk_int 0
let mk_one = mk_int 1

let zero = mk_zero
let one = mk_one

let of_z = mk_z
let of_int = mk_int

let add e1 e2 ?(typ=e1.etyp) range = mk_binop e1 O_plus e2 range ~etyp:typ
let sub e1 e2 ?(typ=e1.etyp) range = mk_binop e1 O_minus e2 range ~etyp:typ
let mul e1 e2 ?(typ=e1.etyp) range = mk_binop e1 O_mult e2 range ~etyp:typ
let div e1 e2 ?(typ=e1.etyp) range = mk_binop e1 O_div e2 range ~etyp:typ
let _mod e1 e2 ?(typ=e1.etyp) range = mk_binop e1 O_mod e2 range ~etyp:typ

let mk_bool b range = mk_constant ~etyp:T_bool (C_bool b) range
let mk_true = mk_bool true
let mk_false = mk_bool false

let mk_addr addr range = mk_expr ~etyp:T_addr (E_addr addr) range

let mk_alloc_addr ?(mode=STRONG) addr_kind range =
  mk_expr (E_alloc_addr (addr_kind, mode)) ~etyp:T_addr range

let is_int_type = function
  | T_int | T_bool -> true
  | _ -> false

let is_float_type = function
  | T_float _ -> true
  | _ -> false

let is_numeric_type = function
  | T_bool | T_int | T_float _ -> true
  | _ -> false

let is_math_type = function
  | T_int | T_float _ | T_bool -> true
  | _ -> false


let mk_assert e range =
  mk_stmt (S_assert e) range

let mk_satisfy e range =
  mk_stmt (S_satisfy e) range

let mk_block block = mk_stmt (S_block block)

let mk_nop range = mk_block [] range

let mk_if cond body orelse range =
  mk_stmt (S_if (cond, body, orelse)) range

let mk_while cond body range =
  mk_stmt (S_while (cond, body)) range

let mk_free_addr a range =
  mk_stmt (S_free_addr a) range

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

module Addr =
struct
  type t = addr
  let compare = compare_addr
  let print = pp_addr
  let from_expr e =
    match ekind e with
    | E_addr addr -> addr
    | _ -> assert false
end

module AddrSet =
struct
  include SetExt.Make(Addr)

  let print fmt s =
    if is_empty s then pp_print_string fmt "∅"
    else
      let l = elements s in
      fprintf fmt "@[<h>{";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
        pp_addr fmt l
      ;
      fprintf fmt "}@]"
end
