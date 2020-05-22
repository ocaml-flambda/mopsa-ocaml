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

(** Translate a CST into a AST by resolving types, variables and
   functions defined in the project. *)

open Location
open Cst


let debug fmt = Debug.debug ~channel:"c_stubs_parser.passes.cst_to_ast" fmt


(** {2 Generic visitors} *)
(** ******************** *)

let rec visit_list visit l prj func =
  match l with
  | [] -> []
  | hd :: tl ->
    let hd' = visit hd prj func in
    let tl' = visit_list visit tl prj func in
    hd' :: tl'

let rec visit_list_ext visit l prj func =
  match l with
  | [] -> [], [], []
  | hd :: tl ->
    let hd', ext1, ext2 = visit hd prj func in
    let tl', ext1', ext2' = visit_list_ext visit tl prj func in
    hd' :: tl', ext1 @ ext1', ext2 @ ext2'

let visit_option visit o prj func =
  match o with
  | None -> None
  | Some x -> Some (visit x prj func)

let visit_pair visitor1 visitor2 (x, y) prj func =
  (visitor1 x prj func, visitor2 y prj func)


(** {2 Types} *)
(** ********* *)

let find_record r prj =
  let open C_AST in
  StringMap.bindings prj.proj_records |>
  List.split |>
  snd |>
  List.find (fun r' -> compare r'.record_org_name r.vname = 0)

let find_typedef t prj =
  let open C_AST in
  StringMap.bindings prj.proj_typedefs |>
  List.split |>
  snd |>
  List.find (fun t' -> compare t'.typedef_org_name t.vname = 0)

let find_enum e prj =
  let open C_AST in
  StringMap.bindings prj.proj_enums |>
  List.split |>
  snd |>
  List.find (fun e' -> compare e'.enum_org_name e.vname = 0)

let find_function f prj =
  try
    let open C_AST in
    bind_range f @@ fun f ->
    StringMap.bindings prj.proj_funcs |>
    List.split |>
    snd |>
    List.find (fun f' -> compare f'.func_org_name f.vname = 0)
  with Not_found ->
    Exceptions.panic_at f.range "function %s not found" f.content.vname

let rec unroll_type t =
  let open C_AST in
  match fst t with
  | T_typedef td -> unroll_type td.typedef_def
  | T_enum e -> T_integer e.enum_integer_type, snd t
  | _ -> t

let visit_qual q =
  C_AST.{
    qual_is_const = q.c_qual_const
  }

let rec visit_qual_typ t prj func : C_AST.type_qual=
  let (t0, q) = t in
  let t0' = visit_typ t0 prj func in
  let q' = visit_qual q in
  t0', q'

and visit_typ t prj func =
  match t with
  | T_void -> C_AST.T_void
  | T_char -> C_AST.(T_integer (Char SIGNED)) (* FIXME: get the signedness defined by the platform *)
  | T_signed_char -> C_AST.(T_integer SIGNED_CHAR)
  | T_unsigned_char -> C_AST.(T_integer UNSIGNED_CHAR)
  | T_signed_short -> C_AST.(T_integer SIGNED_SHORT)
  | T_unsigned_short -> C_AST.(T_integer UNSIGNED_SHORT)
  | T_signed_int -> C_AST.(T_integer SIGNED_INT)
  | T_unsigned_int -> C_AST.(T_integer UNSIGNED_INT)
  | T_signed_long -> C_AST.(T_integer SIGNED_LONG)
  | T_unsigned_long -> C_AST.(T_integer UNSIGNED_LONG)
  | T_signed_long_long -> C_AST.(T_integer SIGNED_LONG_LONG)
  | T_unsigned_long_long -> C_AST.(T_integer UNSIGNED_LONG_LONG)
  | T_signed_int128 -> C_AST.(T_integer SIGNED_INT128)
  | T_unsigned_int128 -> C_AST.(T_integer UNSIGNED_INT128)
  | T_float -> C_AST.(T_float FLOAT)
  | T_double -> C_AST.(T_float DOUBLE)
  | T_long_double -> C_AST.(T_float LONG_DOUBLE)
  | T_array(t, len) -> C_AST.T_array (visit_qual_typ t prj func , visit_array_length len prj func )
  | T_struct(s) -> C_AST.T_record (find_record s prj)
  | T_union(u) -> C_AST.T_record (find_record u prj)
  | T_typedef(t) -> C_AST.T_typedef (find_typedef t prj)
  | T_pointer(t) -> C_AST.T_pointer (visit_qual_typ t prj func )
  | T_enum(e) -> C_AST.T_enum (find_enum e prj)
  | T_unknown -> Exceptions.panic "unknown type not resolved"

and visit_array_length len prj func =
  match len with
  | A_no_length -> C_AST.No_length
  | A_constant_length n -> C_AST.Length_cst n


let int_type = C_AST.(T_integer SIGNED_INT, no_qual)
let bool_type = C_AST.(T_bool, no_qual)
let float_type = C_AST.(T_float FLOAT, no_qual)
let double_type = C_AST.(T_float DOUBLE, no_qual)
let long_type = C_AST.(T_integer SIGNED_LONG, no_qual)
let unsigned_long_type = C_AST.(T_integer UNSIGNED_LONG, no_qual)
let long_long_type = C_AST.(T_integer SIGNED_LONG_LONG, no_qual)
let unsigned_long_long_type = C_AST.(T_integer UNSIGNED_LONG_LONG, no_qual)
let string_type s = C_AST.(T_array((T_integer(SIGNED_CHAR), no_qual), Length_cst (Z.of_int (1 + String.length s))), no_qual)
let char_type = C_AST.(T_integer (Char SIGNED), no_qual)
let pointer_type t = C_AST.(T_pointer t, no_qual)
let void_type = C_AST.(T_void, no_qual)

let is_int_typ t =
  match unroll_type t |> fst with
  | C_AST.T_integer _ -> true
  | _ -> false

let is_pointer_typ t =
  match unroll_type t |> fst with
  | C_AST.T_pointer _ -> true
  | _ -> false

let is_array_typ t =
  match unroll_type t |> fst with
  | C_AST.T_array _ -> true
  | _ -> false

let pointed_type range t =
  match fst (unroll_type t) with
  | C_AST.T_pointer t' -> t'
  | C_AST.T_array(t', _) -> t'
  | _ -> Exceptions.panic_at range "pointed_type(cst_to_ast.ml): unsupported type %s" (C_print.string_of_type_qual t)

let is_record_typ t =
  match fst (unroll_type t) with
  | C_AST.T_record _ -> true
  | _ -> false
  

let subscript_type t = pointed_type t

let attribute_type obj f =
  Exceptions.warn "attribute_typ: supporting only int attributes";
  int_type

let builtin_type f arg =
  match f with
  | LENGTH   -> unsigned_long_type
  | BYTES    -> unsigned_long_type
  | OFFSET -> int_type
  | INDEX -> int_type
  | BASE   -> pointer_type C_AST.(T_void, no_qual)
  | PRIMED -> arg.content.Ast.typ
  | VALID_PTR | VALID_FLOAT | FLOAT_INF | FLOAT_NAN -> bool_type
  | ALIVE    -> bool_type


(** {2 Records} *)
(** *********** *)

let rec find_field range t f =
  match fst (unroll_type t) with
  | C_AST.T_record r ->
    let rec aux = function
      | [] -> raise Not_found

      | field::tl when field.C_AST.field_org_name = f ->
        field

      (* case of anonymous records *)
      | field::tl when field.field_org_name = "" && is_record_typ field.field_type ->
        begin
          try find_field range field.field_type f
          with Not_found -> aux tl
        end

      | _::tl -> aux tl
    in
    aux (Array.to_list r.C_AST.record_fields)

  | _ -> Exceptions.panic_at range "field_type(cst_to_ast.ml): unsupported type %s"
           (C_print.string_of_type_qual t)

let find_field_check t f range =
  try find_field range t f
  with Not_found ->
    Exceptions.panic_at range "type %s has no field named %s" (C_print.string_of_type_qual t) f


(** {2 Expressions} *)
(** *************** *)

let visit_var v range prj func =
  let open C_AST in
  if v.vlocal then
    {
      var_uid = v.vuid; (** FIXME: ensure that v.vuid is unique in the entire project *)
      var_org_name = v.vname;
      var_unique_name = v.vname ^ (string_of_int v.vuid); (** FIXME: give better unique names *)
      var_kind = Variable_local func;
      var_type = visit_qual_typ v.vtyp prj func;
      var_init = None;
      var_range = Clang_AST.{
          range_begin = {
            loc_file = get_range_start v.vrange |> get_pos_file;
            loc_line = get_range_start v.vrange |> get_pos_line;
            loc_column = get_range_start v.vrange |> get_pos_column;
          };
          range_end = {
            loc_file = get_range_end v.vrange |> get_pos_file;
            loc_line = get_range_end v.vrange |> get_pos_line;
            loc_column = get_range_end v.vrange |> get_pos_column;
          }
        };
      var_com = [];
    }
  else
    (* Search for the variable in the parameters of the function or
       the globals of the project *)
    let vars = Array.to_list func.func_parameters @
               (StringMap.bindings prj.proj_vars |> List.split |> snd)
    in
    try List.find (fun v' -> compare v'.var_org_name v.vname = 0) vars
    with Not_found -> Exceptions.panic_at range "undeclared variable %a" pp_var v

let rec promote_expression_type prj (e: Ast.expr with_range) =
  (* Integer promotions (C99 6.3.1.1) *)
  let open C_AST in
  let open C_utils in
  bind_range e @@ fun ee ->
  let t = unroll_type ee.typ in
  match fst t with
  | T_integer (SIGNED_INT | UNSIGNED_INT |
               SIGNED_LONG | UNSIGNED_LONG |
               SIGNED_LONG_LONG | UNSIGNED_LONG_LONG |
               SIGNED_INT128 | UNSIGNED_INT128
              ) ->
    ee

  | T_integer (Char SIGNED | SIGNED_CHAR | SIGNED_SHORT) | T_bool ->
    let tt = (T_integer SIGNED_INT), snd t in
    { kind = E_cast(tt, false, e); typ = tt }

  | T_integer ((Char UNSIGNED | UNSIGNED_CHAR | UNSIGNED_SHORT) as i) ->
    (* signed int wins if it can represent all unsigned values *)
    let ii =
      if sizeof_int prj.proj_target i < sizeof_int prj.proj_target SIGNED_INT
      then SIGNED_INT
      else UNSIGNED_INT
    in
    let tt = (T_integer ii), snd t in
    { kind = E_cast(tt, false, e); typ = tt }

  | T_float _ -> ee

  | T_pointer _ -> ee

  | T_array _ -> ee

  | _ -> Exceptions.panic_at e.range "promote_expression_type: unsupported type %s"
           (C_print.string_of_type_qual t)

let convert_expression_type (e:Ast.expr with_range) t =
  bind_range e @@ fun ee ->
  if compare ee.typ t = 0 then
    ee
  else
  if is_int_typ ee.typ && (is_pointer_typ t || is_array_typ t) then
    (* No cast is added when a pointer is added to an integer *)
    ee
  else
    { kind = E_cast(t, false, e); typ = t }

let int_rank = 
  let open C_AST in
  function
  | Char _ | UNSIGNED_CHAR | SIGNED_CHAR -> 0
  | UNSIGNED_SHORT | SIGNED_SHORT -> 1
  | UNSIGNED_INT | SIGNED_INT -> 2
  | UNSIGNED_LONG | SIGNED_LONG -> 3
  | UNSIGNED_LONG_LONG | SIGNED_LONG_LONG -> 4
  | UNSIGNED_INT128 | SIGNED_INT128 -> 5

let int_sign =
  let open C_AST in
  function
  | SIGNED_CHAR | SIGNED_SHORT | SIGNED_INT | SIGNED_LONG
  | SIGNED_LONG_LONG | SIGNED_INT128 -> true
  | UNSIGNED_CHAR | UNSIGNED_SHORT | UNSIGNED_INT | UNSIGNED_LONG
  | UNSIGNED_LONG_LONG | UNSIGNED_INT128 -> false
  | Char SIGNED -> true
  | Char UNSIGNED -> false

let make_unsigned =
  let open C_AST in
  function
  | Char _ | UNSIGNED_CHAR | SIGNED_CHAR -> UNSIGNED_CHAR
  | UNSIGNED_SHORT | SIGNED_SHORT -> UNSIGNED_SHORT
  | UNSIGNED_INT | SIGNED_INT -> UNSIGNED_INT
  | UNSIGNED_LONG | SIGNED_LONG -> UNSIGNED_LONG
  | UNSIGNED_LONG_LONG | SIGNED_LONG_LONG -> UNSIGNED_LONG_LONG
  | UNSIGNED_INT128 | SIGNED_INT128 -> UNSIGNED_INT128

let rank_float =
  let open C_AST in
  function
  | FLOAT -> 0
  | DOUBLE -> 1
  | LONG_DOUBLE -> 2
  
let binop_type range prj t1 t2 =
  let open C_AST in
  let open C_utils in
  match fst (unroll_type t1), fst (unroll_type t2) with
  | x1, x2 when type_compatible prj.proj_target x1 x2 -> t1

  | T_float a, T_float b ->
     if rank_float a >= rank_float b then t1 else t2

  | T_float _, T_integer _ -> t1
  | T_integer _, T_float _ -> t2
    
  | T_integer a, T_integer b ->
     (* Usual arithmetic conversions (C99 6.3.1.8) *)
     (* same sign: the highest ranked wins *)
     if int_sign a = int_sign b then
       if int_rank a >= int_rank b then t1 else t2
     (* if the unsigned has greater or equal rank, it wins *)
     else if not (int_sign a) && int_rank a >= int_rank b then t1
     else if not (int_sign b) && int_rank b >= int_rank a then t2
     (* if the signed can hold all unsigned values, it wins *)
     else if int_sign a && sizeof_int prj.proj_target a > sizeof_int prj.proj_target b then t1
     else if int_sign b && sizeof_int prj.proj_target b > sizeof_int prj.proj_target a then t2
     (* otherwise, use an unsigned version of the signed type *)
     else if int_sign a then T_integer (make_unsigned a), snd t1
     else T_integer (make_unsigned b), snd t2

  | T_pointer _, T_integer _ -> t1
  | T_integer _, T_pointer _ -> t2

  | T_array _, T_integer _ -> t1
  | T_integer _, T_array _ -> t2

  | T_pointer (T_void,_), T_pointer _ -> t2
  | T_pointer _, T_pointer (T_void,_) -> t1
  | T_pointer (p1,_), T_pointer (p2,_) when type_equal prj.proj_target p1 p2 -> t1

  | T_pointer p, T_array (e,_) when type_qual_compatible prj.proj_target p e -> t1
  | T_array (e,_), T_pointer p when type_qual_compatible prj.proj_target p e -> t2

  | _ -> Exceptions.panic_at range "binop_type: unsupported case: %s and %s"
           (C_print.string_of_type_qual t1)
           (C_print.string_of_type_qual t2)

let rec visit_expr e prj func =
  bind_range e @@ fun ee ->
  let kind, typ = match ee with
    | E_top t ->
      let t = visit_qual_typ t prj func in
      Ast.E_top t, t

    | E_int(n, NO_SUFFIX) -> Ast.E_int(n), int_type

    | E_int(n, LONG) -> Ast.E_int(n), long_type

    | E_int(n, UNSIGNED_LONG) -> Ast.E_int(n), unsigned_long_type

    | E_int(n, LONG_LONG) -> Ast.E_int(n), long_long_type

    | E_int(n, UNSIGNED_LONG_LONG) -> Ast.E_int(n), unsigned_long_long_type

    | E_float(f) -> Ast.E_float(f), float_type

    | E_string(s) -> Ast.E_string(s), string_type s

    | E_char(c) -> Ast.E_char(c), char_type

    | E_invalid -> Ast.E_invalid, pointed_type e.range void_type

    | E_var(v) ->
      let v = visit_var v e.range prj func in
      Ast.E_var v, v.var_type

    | E_unop(op, e')      ->
      let e' = visit_expr e' prj func in
      let e' = promote_expression_type prj e' in
      let ee' =
        with_range
          Ast.{ kind = E_unop(op, e'); typ = e'.content.typ }
          e.range
      in

      E_cast(e'.content.typ, false, ee'), e'.content.typ

    | E_binop(op, e1, e2) ->
      let e1 = visit_expr e1 prj func in
      let e2 = visit_expr e2 prj func in

      let e1 = promote_expression_type prj e1 in
      let e2 = promote_expression_type prj e2 in

      let t = binop_type e.range prj e1.content.typ e2.content.typ in

      let e1 = convert_expression_type e1 t in
      let e2 = convert_expression_type e2 t in

      let ee' = with_range
          Ast.{ kind = E_binop(op, e1, e2); typ = t }
          e.range
      in
      
      begin match op with
        | EQ | NEQ | LT | LE | GT | GE -> ee'.content.kind, ee'.content.typ
        | _ -> Ast.E_cast(t, false, ee'), t
      end

    | E_addr_of(e')       ->
      let e' = visit_expr e' prj func in
      Ast.E_addr_of e', pointer_type e'.content.typ

    | E_deref(e')         ->
      let e' = visit_expr e' prj func in
      Ast.E_deref e', pointed_type e.range e'.content.typ

    | E_cast(t, e') ->
      let t = visit_qual_typ t prj func in
      Ast.E_cast(t, true, visit_expr e' prj func ), t

    | E_subscript(a, i)   ->
      let a  = visit_expr a prj func in
      Ast.E_subscript(a, visit_expr i prj func ), subscript_type e.range a.content.typ

    | E_member(s, f)      ->
      let s = visit_expr s prj func in
      let field = find_field_check s.content.typ f e.range in
      Ast.E_member(s, field.field_index, f), field.field_type

    | E_arrow(p, f) ->
      let p = visit_expr p prj func in
      let field = find_field_check (pointed_type e.range p.content.typ) f e.range in
      Ast.E_arrow(p, field.field_index, f), field.field_type

    | E_sizeof_expr(e) ->
      let e = visit_expr e prj func in
      let typ, _ = e.content.typ in
      let target = Clang_parser.get_target_info (Clang_parser.get_default_target_options ()) in
      let size = C_utils.sizeof_type target typ in
      Ast.E_int(size), int_type

    | E_sizeof_type(t) ->
      let typ, _ = visit_qual_typ t.content prj func in
      let target = Clang_parser.get_target_info (Clang_parser.get_default_target_options ()) in
      let size = C_utils.sizeof_type target typ in
      Ast.E_int(size), int_type

    | E_builtin_call(f,a) ->
      let a = visit_expr a prj func in
      Ast.E_builtin_call(f, a), builtin_type f a

    | E_return -> Ast.E_return, func.func_return
  in
  Ast.{ kind; typ }


(** {2 Formulas} *)
(** ************ *)

let visit_interval i prj func =
  { Ast.itv_lb = visit_expr i.itv_lb prj func;
    itv_open_lb = i.itv_open_lb;
    itv_ub = visit_expr i.itv_ub prj func;
    itv_open_ub = i.itv_open_ub;}


let visit_set s prj func =
  match s with
  | S_interval(itv) -> Ast.S_interval(visit_interval itv prj func)
  | S_resource(r) -> Ast.S_resource(r.vname)

let rec visit_formula f prj func =
  bind_range f @@ fun ff ->
  match ff with
  | F_expr(e) -> Ast.F_expr (visit_expr e prj func )
  | F_bool(b) -> Ast.F_bool b
  | F_binop(op, f1, f2) -> Ast.F_binop(op, visit_formula f1 prj func , visit_formula f2 prj func )
  | F_not f' -> Ast.F_not (visit_formula f' prj func)
  | F_forall(v, t, s, f') ->
    let v' = visit_var v f.range prj func in
    Ast.F_forall(v', visit_set s prj func, visit_formula f' prj func)
  | F_exists(v, t, s, f') ->
    let v' = visit_var v f.range prj func in
    Ast.F_exists(v', visit_set s prj func, visit_formula f' prj func)
  | F_in(e, s) -> Ast.F_in(visit_expr e prj func, visit_set s prj func)
  | F_predicate(p, args) -> Exceptions.panic "cst_to_ast: predicate %a not expanded" pp_var p


(** {2 Stub sections} *)
(** **************** *)

let visit_requires req prj func =
  bind_range req @@ fun req ->
  visit_formula req prj func

let visit_assumes asm prj func =
  bind_range asm @@ fun asm ->
  visit_formula asm prj func

let visit_assigns a prj func =
  bind_range a @@ fun a ->
  Ast.{
    assign_target = visit_expr a.Cst.assign_target prj func;
    assign_offset = (visit_list @@ visit_interval) a.Cst.assign_offset prj func;
  }

let visit_ensures ens prj func =
  bind_range ens @@ fun ens ->
  visit_formula ens prj func

let visit_free free prj func =
  bind_range free @@ fun free ->
  visit_expr free prj func

let visit_local loc prj func =
  bind_range loc @@ fun l ->
  let lvar = visit_var l.lvar loc.range prj func in
  let lval =
    match l.lval with
    | L_new r -> Ast.L_new r.vname
    | L_call (f, args) ->
      let f = find_function f prj in
      Ast.L_call (f, visit_list visit_expr args prj func)
  in
  Ast.{ lvar; lval }

let visit_message msg prj func =
  bind_range msg @@ fun m ->
  { Ast.message_kind = m.message_kind;
    message_body = m.message_body; }

let visit_leaf leaf prj func =
  match leaf with
  | Cst.S_local local ->
    let local = visit_local local prj func in
    Ast.S_local local, [local], []

  | S_assumes assumes ->
    S_assumes (visit_assumes assumes prj func), [], []

  | S_requires requires ->
    S_requires (visit_requires requires prj func), [], []

  | S_assigns assigns ->
    let assigns = visit_assigns assigns prj func in
    S_assigns assigns, [], [assigns]

  | S_ensures ensures ->
    S_ensures (visit_ensures ensures prj func), [], []

  | S_free free ->
    S_free (visit_free free prj func), [], []

  | S_message msg ->
    S_message (visit_message msg prj func), [], []

let visit_case case prj func =
  let body, locals, assigns = visit_list_ext visit_leaf case.content.case_body prj func in
  Ast.{
    case_label = case.content.case_label;
    case_body = body;
    case_locals = locals;
    case_assigns = assigns;
    case_range = case.range;
  }, assigns

let visit_section sect prj func =
  match sect with
  | Cst.S_leaf leaf ->
    let leaf, locals, assigns = visit_leaf leaf prj func in
    Ast.S_leaf leaf, locals, assigns

  | S_case case ->
    let case, assigns = visit_case case prj func in
    S_case (case), [], assigns

  | S_predicate pred -> assert false
  | S_alias alias -> assert false

let visit_alias sects =
  match sects with
  | [ Cst.S_alias alias ] -> alias.content
  | _ -> assert false

(** {2 Entry point} *)
(** *************** *)

let doit
    (prj:C_AST.project)
    (func: C_AST.func)
    (stub:Cst.stub)
  : Ast.stub
  =
  if Cst.is_alias stub then
    Ast.{
      stub_name = func.C_AST.func_org_name;
      stub_params = Array.to_list func.C_AST.func_parameters;
      stub_body = [];
      stub_locals = [];
      stub_assigns = [];
      stub_alias = Some (visit_alias stub.content);
      stub_range = stub.range;
    }
  else
    let body, locals, assigns = visit_list_ext visit_section stub.content prj func in
    Ast.{
      stub_name = func.C_AST.func_org_name;
      stub_params = Array.to_list func.C_AST.func_parameters;
      stub_body = body;
      stub_locals = locals;
      stub_assigns = assigns;
      stub_alias = None;
      stub_range = stub.range;
    }
