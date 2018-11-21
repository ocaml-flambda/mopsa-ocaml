(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Translate a CST into a AST by resolving types, variables and
   functions defined in the project. *)

open Location
open Cst


(** Type maps of resolving types of local variables *)
module Context =
struct

  module Map = MapExt.Make(struct type t = var let compare = compare_var end)
      
  type t = C_AST.type_qual Map.t

  let empty : t = Map.empty

  let add (v:var) (vtyp:C_AST.type_qual) (ctx:t) : t =
    Map.add v vtyp ctx

  let find (v:var) (ctx:t) : C_AST.type_qual =
    Map.find v ctx

end

(** {2 Generic visitors} *)
(** ******************** *)

let rec visit_list visit l prj func ctx =
  match l with
  | [] -> []
  | hd :: tl ->
    let hd' = visit hd prj func ctx in
    let tl' = visit_list visit tl prj func ctx in
    hd' :: tl'

let rec visit_list2 visit l prj func ctx =
  match l with
  | [] -> [], ctx
  | hd :: tl ->
    let hd', ctx' = visit hd prj func ctx in
    let tl', ctx'' = visit_list2 visit tl prj func ctx' in
    hd' :: tl', ctx''

let visit_option visit o prj func ctx =
  match o with
  | None -> None
  | Some x -> Some (visit x prj func ctx)

let visit_pair visitor1 visitor2 (x, y) prj func ctx =
  (visitor1 x prj func ctx, visitor2 y prj func ctx)


(** {2 Visitors for types} *)
(** ********************** *)

let find_record r prj =
  let open C_AST in
  StringMap.bindings prj.proj_records |>
  List.split |>
  snd |>
  List.find (fun r' -> r'.record_org_name == r.vname) 

let find_typedef t prj =
  let open C_AST in
  StringMap.bindings prj.proj_typedefs |>
  List.split |>
  snd |>
  List.find (fun t' -> t'.typedef_org_name == t.vname)

let find_enum e prj =
  let open C_AST in
  StringMap.bindings prj.proj_enums |>
  List.split |>
  snd |>
  List.find (fun e' -> e'.enum_org_name == e.vname) 

let find_function f prj =
  let open C_AST in
  StringMap.bindings prj.proj_funcs |>
  List.split |>
  snd |>
  List.find (fun f' -> f'.func_org_name == f.vname) 

let rec visit_qual_typ t prj func ctx : C_AST.type_qual=
  let (t0, is_const) = t in
  let t0' = visit_typ t0 prj func ctx in
  let qual = C_AST.{ qual_is_const = is_const; } in
  t0', qual

and visit_typ t prj func ctx =
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
  | T_array(t, len) -> C_AST.T_array (visit_qual_typ t prj func ctx, visit_array_length len prj func ctx)
  | T_struct(s) -> C_AST.T_record (find_record s prj)
  | T_union(u) -> C_AST.T_record (find_record u prj)
  | T_typedef(t) -> C_AST.T_typedef (find_typedef t prj)
  | T_pointer(t) -> C_AST.T_pointer (visit_qual_typ t prj func ctx)
  | T_enum(e) -> C_AST.T_enum (find_enum e prj)

and visit_array_length len prj func ctx =
  match len with
  | A_no_length -> C_AST.No_length
  | A_constant_length n -> C_AST.Length_cst n


(** {2 Visitors for expressions} *)
(** **************************** *)

let visit_unop = function
  | PLUS  -> Ast.PLUS
  | MINUS -> Ast.MINUS
  | LNOT  -> Ast.LNOT
  | BNOT  -> Ast.BNOT

let visit_binop = function
  | ADD -> Ast.ADD
  | SUB -> Ast.SUB
  | MUL -> Ast.MUL
  | DIV -> Ast.DIV
  | MOD -> Ast.MOD
  | RSHIFT -> Ast.RSHIFT
  | LSHIFT -> Ast.LSHIFT
  | LOR -> Ast.LOR
  | LAND -> Ast.LAND
  | LT -> Ast.LT
  | LE -> Ast.LE
  | GT -> Ast.GT
  | GE -> Ast.GE
  | EQ -> Ast.EQ
  | NEQ -> Ast.NEQ
  | BOR -> Ast.BOR
  | BAND -> Ast.BAND
  | BXOR -> Ast.BXOR

let visit_var v prj func ctx =
  let open C_AST in
  if v.vlocal then
    let vtyp = Context.find v ctx in
    {
      var_uid = v.vuid; (** FIXME: ensure that v.vuid is unique in the entire project *)
      var_org_name = v.vname;
      var_unique_name = v.vname ^ (string_of_int v.vuid); (** FIXME: give better unique names *)
      var_kind = Variable_local func;
      var_type = vtyp;
      var_init = None;
      var_range = assert false;
      var_com = [];
    }
  else
    (* Search for the variable in the parameters of the function or
       the globals of the project *)
    let vars = Array.to_list func.func_parameters @
               (StringMap.bindings prj.proj_vars |> List.split |> snd)
    in
    try List.find (fun v' -> v'.var_org_name == v.vname) vars
    with Not_found -> Exceptions.panic "cst_to_ast: variable %a not found" pp_var v

let rec visit_expr e prj func ctx =
  bind_range e @@ function
  | E_int(n)            -> Ast.E_int(n)
  | E_float(f)          -> Ast.E_float(f)
  | E_string(s)         -> Ast.E_string(s)
  | E_char(c)           -> Ast.E_char(c)
  | E_var(v)            -> Ast.E_var(visit_var v prj func ctx)
  | E_unop(op, e')      -> Ast.E_unop(visit_unop op, visit_expr e' prj func ctx)
  | E_binop(op, e1, e2) -> Ast.E_binop(visit_binop op, visit_expr e1 prj func ctx, visit_expr e2 prj func ctx)
  | E_addr_of(e')       -> Ast.E_addr_of(visit_expr e' prj func ctx)
  | E_deref(e')         -> Ast.E_deref(visit_expr e' prj func ctx)
  | E_cast(t, e')       -> Ast.E_cast(visit_qual_typ t prj func ctx, visit_expr e' prj func ctx)
  | E_subscript(a, i)   -> Ast.E_subscript(visit_expr a prj func ctx, visit_expr i prj func ctx)
  | E_member(s, f)      -> Ast.E_member(visit_expr s prj func ctx, f)
  | E_arrow(p, f)       -> Ast.E_arrow(visit_expr p prj func ctx, f)
  | E_builtin_call(f,a) -> Ast.E_builtin_call(visit_builtin f, visit_expr a prj func ctx)
  | E_return            -> Ast.E_return

and visit_builtin = function
  | OLD    -> Ast.OLD
  | SIZE   -> Ast.SIZE
  | OFFSET -> Ast.OFFSET
  | BASE   -> Ast.BASE


(** {2 Visitors for formulas} *)
(** ************************* *)

let visit_log_binop = function
  | AND -> Ast.AND
  | OR  -> Ast.OR
  | IMPLIES -> Ast.IMPLIES

let visit_set s prj func ctx =
  match s with
  | S_interval(e1, e2) -> Ast.S_interval(visit_expr e1 prj func ctx, visit_expr e2 prj func ctx)
  | S_resource(r) -> Ast.S_resource(r)

let rec visit_formula f prj func ctx =
  bind_range f @@ fun f ->
  match f with
  | F_expr(e) -> Ast.F_expr (visit_expr e prj func ctx)
  | F_bool(b) -> Ast.F_bool b
  | F_binop(op, f1, f2) -> Ast.F_binop(visit_log_binop op, visit_formula f1 prj func ctx, visit_formula f2 prj func ctx)
  | F_not f' -> Ast.F_not (visit_formula f' prj func ctx)
  | F_forall(v, t, s, f') ->
    let t' = visit_qual_typ t prj func ctx in
    let ctx' = Context.add v t' ctx in
    let v' = visit_var v prj func ctx' in
    Ast.F_forall(v', visit_set s prj func ctx, visit_formula f' prj func ctx')
  | F_exists(v, t, s, f') ->
    let t' = visit_qual_typ t prj func ctx in
    let ctx' = Context.add v t' ctx in
    let v' = visit_var v prj func ctx' in
    Ast.F_exists(v', visit_set s prj func ctx, visit_formula f' prj func ctx')
  | F_in(v, s) -> Ast.F_in(visit_var v prj func ctx, visit_set s prj func ctx)
  | F_free(e) -> Ast.F_free(visit_expr e prj func ctx)
  | F_predicate(p, args) -> Exceptions.panic "cst_to_ast: predicate %a not expanded" pp_var p


(** {2 Visitors for stub sections} *)
(** ****************************** *)

let visit_requires req prj func ctx =
  bind_range req @@ fun req ->
  visit_formula req prj func ctx

let visit_assumes asm prj func ctx =
  bind_range asm @@ fun asm ->
  visit_formula asm prj func ctx

let visit_assigns a prj func ctx =
  bind_range a @@ fun a ->
  Ast.{
    assign_target = visit_expr a.Cst.assign_target prj func ctx;
    assign_offset = visit_option (visit_pair visit_expr visit_expr) a.Cst.assign_offset prj func ctx;
  }

let visit_ensures ens prj func ctx =
  bind_range ens @@ fun ens ->
  visit_formula ens prj func ctx

let visit_local loc prj func ctx =
  bind_pair_range loc @@ fun loc ->
  let t = visit_qual_typ loc.ltyp prj func ctx in
  let ctx' = Context.add loc.lvar t ctx in
  let lvar = visit_var loc.lvar prj func ctx' in
  let lval =
    match loc.lval with
    | L_new r -> Ast.L_new r
    | L_call (f, args) -> Ast.L_call (find_function f prj, visit_list visit_expr args prj func ctx)
  in
  Ast.{ lvar; lval }, ctx'

let visit_case c prj func ctx =
  bind_range c @@ fun c ->
  let requires = visit_list visit_requires c.case_requires prj func ctx in
  let assumes = visit_list visit_assumes c.case_assumes prj func ctx in
  let assigns = visit_list visit_assigns c.case_assigns prj func ctx in
  let local, ctx' = visit_list2 visit_local c.case_local prj func ctx in
  let ensures = visit_list visit_ensures c.case_ensures prj func ctx' in

  Ast.{
    case_label = c.case_label;
    case_assumes = assumes;
    case_requires = requires;
    case_post = {
      post_assigns = assigns;
      post_local   = local;
      post_ensures = ensures;
    }
  }

let doit
    (prj:C_AST.project)
    (func: C_AST.func)
    (stub:Cst.stub with_range)
  : Ast.stub with_range
  =
  bind_range stub @@ fun stub ->
  match stub with
  | S_simple s ->
    let requires = visit_list visit_requires s.simple_stub_requires prj func Context.empty in
    let assigns = visit_list visit_assigns s.simple_stub_assigns prj func Context.empty in
    let local, ctx = visit_list2 visit_local s.simple_stub_local prj func Context.empty in
    let ensures = visit_list visit_ensures s.simple_stub_ensures prj func ctx in

    Ast.{
      stub_requires = requires;
      stub_body = B_simple {
          post_assigns = assigns;
          post_local   = local;
          post_ensures = ensures;
        }
    }

  | S_case c ->
    let requires = visit_list visit_requires c.case_stub_requires prj func Context.empty in
    Ast.{
      stub_requires = requires;
      stub_body = B_case (visit_list visit_case c.case_stub_cases prj func Context.empty);
    }

  
