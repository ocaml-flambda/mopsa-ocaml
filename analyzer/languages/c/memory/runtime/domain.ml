(** Non-relational abstraction of OCaml runtime FFI *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open Ast
open Common
open Common.Points_to
open Common.Runtime
open Common.Base
open Common.Alarms
open Common.Static_points_to
open Common.Runtime
open Common.Type_shapes
open Value
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Stat = Framework.Lattices.Const.Make(Status)
  module Root = Framework.Lattices.Const.Make(Roots)

  module OCamlValue = Shapes.NestedShapes
  module OCamlValueExt = Shapes.OCamlValueExt(OCamlValue)
  module Shape = Shapes.RuntimeShape(OCamlValue)
  module Val  = Framework.Lattices.Pair.Make(Framework.Lattices.Pair.Make(Stat)(Root))(Shape)
  module Map  = Framework.Lattices.Partial_map.Make(Var)(Val)
  module Lock = Framework.Lattices.Const.Make(RuntimeLock)
  module Dom  = Framework.Lattices.Pair.Make(Map)(Lock)

  type t = Dom.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.runtime"
    end)

  let bottom = Dom.bottom
  let top = Dom.top

  let checks = [
    CHK_FFI;
    CHK_FFI_LIVENESS_VALUE;
    CHK_FFI_ROOTS;
    CHK_FFI_RUNTIME_LOCK;
    CHK_FFI_SHAPE
  ]

  (** {2 Lattice operators} *)
  (** ===================== *)

  let is_bottom = Dom.is_bottom

  let subset = Dom.subset

  let join = Dom.join

  let meet = Dom.meet

  let widen ctx = Dom.widen ctx

  let merge pre (a,e) (a',e') =
    let aa,aa' =
      generic_merge (a,e) (a',e')
        ~add:(fun v b (m, l) -> (Map.add v b m, l))
        ~find:(fun v (m, l) -> Map.find v m)
        ~remove:(fun v (m, l) -> (Map.remove v m, l))
        ~custom:(fun stmt -> None)
  in Dom.meet aa aa'


  let update_status var status' map =
    match Map.find_opt var map with
    (* FIXME: look into this function *)
    | None -> Map.add var ((status', Nbt NotRooted), Shape.non_ocaml_value) map
    | Some ((_, roots), shapes) -> Map.add var ((status', roots), shapes)  map

  let update_shapes var shapes' map =
    match Map.find_opt var map with
    | None -> assert false (* this should not happen *)
    | Some ((status, roots), _) -> Map.add var ((status, roots), shapes')  map


  let lookup_status var map =
    match Map.find_opt var map with
    | None -> None
    | Some ((status, _), _) -> Some status

  let lookup_shapes var map =
    match Map.find_opt var map with
    | None -> None
    | Some (_, sh) -> Some sh


  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur (Map.empty, Lock.embed Locked) man flow |>
    Option.some



  (** {2 Expression Status} *)
  (** ==================== *)

  (* S(v) *)
  let status_var man flow var =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match Map.find_opt var m with
    | Some ((stat, _), _) -> Cases.singleton stat flow
    | None ->
      (* FIXME: for type var[size], var is not added to the domain *)
      let () = Debug.debug ~channel:"runtime" "missing variable %a" pp_var var in
      Cases.singleton (Nbt Untracked: Stat.t) flow


  (* S(&v) *)
  let status_addr_of_var man flow var range =
    match vkind var with
    | V_c_stack_var _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | V_cvar _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    (* NOTE: This should only be called right after creating a fresh variable for the address.
      Hence, it is fine to say that the FFI variable is untracked. Alternatively, it could copy
      the status of the contents [status_var m v], which is untracked or possibly alive inside of loops.  *)
    | V_ffi_ptr _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | _ ->
      (* FIXME: unclear whether this is a good idea *)
      Cases.singleton (Nbt Untracked: Stat.t) flow
      (* let flow = raise_or_fail_ffi_unsupported range (Format.asprintf "status(%a) unsupported for this variable kind" pp_var var) man flow in
      Cases.empty flow *)

  (* S(c) *)
  let status_const man flow const =
    Cases.singleton (Nbt Untracked: Stat.t) flow

  let status_binop man flow op (s1: Stat.t) (s2: Stat.t) =
    match s1, s2 with
    | BOT, _ | _, BOT -> Cases.singleton (BOT: Stat.t) flow
    | TOP, _ | _, TOP -> Cases.singleton (TOP: Stat.t) flow
    | Nbt Stale, _ | _, Nbt Stale ->
      Cases.singleton (Nbt Stale: Stat.t) flow
    | Nbt Active, Nbt Active
    | Nbt Active, Nbt Untracked
    | Nbt Untracked, Nbt Active ->
      Cases.singleton (Nbt Active: Stat.t) flow
    | Nbt Untracked, Nbt Untracked ->
      Cases.singleton (Nbt Untracked: Stat.t) flow

  let status_unop man flow op s =
    Cases.singleton s flow

  let status_addr_of man flow e =
    match ekind e with
    | E_var (v, _) -> status_addr_of_var man flow v e.erange
    | E_c_function _ | E_function _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "status(&%a) unsupported for this expression; only variables supported" pp_expr e) e.erange man flow in
      Cases.empty flow


  let status_cast man flow c s =
    Cases.singleton s flow

  let rec status_expr man flow e =
    match ekind e with
    | E_var (var, _)       ->
      status_var man flow var
    | E_constant c         ->
      status_const man flow c
    | E_binop (op, e1, e2) ->
      status_expr man flow e1 >>$ fun s1 flow ->
      status_expr man flow e2 >>$ fun s2 flow ->
      status_binop man flow op s1 s2
    | E_unop(op, e)        ->
      status_expr man flow e >>$ fun s flow ->
      status_unop man flow op s
    | E_addr(a, mode)      ->
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | E_c_address_of e     ->
      status_addr_of man flow e
    | E_c_deref e          ->
      status_deref man flow e e.erange
    | E_c_cast (e,c)       ->
      status_expr man flow e >>$ fun s flow ->
      status_cast man flow c s
    | E_c_function _ | E_function _ | E_c_builtin_function _  ->
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "status(%a) unsupported for this kind of expression" pp_expr e) e.erange man flow in
      Cases.empty flow
and status_deref man flow e range =
    match Static_points_to.eval_opt e flow with
    | None | Some (Fun _) ->
      let flow = raise_ffi_internal_error (Format.asprintf "status(*%a) unsupported for this kind of expression" pp_expr e) e.erange man flow in
      Cases.empty flow
    | Some (Eval(v, _, _)) ->
      (* NOTE: Thist state should be unreachable, because dereference is taken care of
         by an earlier domain. However, in some corner cases we can actually still reach
         this case. Since evaluation of [*e] has not succeeded, we simply return [T],
         because we do not know the precise status. *)
      Cases.singleton (TOP: Stat.t) flow
    | Some Null | Some Invalid | Some Top ->
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | Some (AddrOf ({ base_kind = Var v; }, _, _)) ->
      status_var man flow v
    | Some (AddrOf ({ base_kind = Addr a; }, _, _)) ->
      let flow = raise_ffi_internal_error (Format.asprintf "status(%a) unsupported for addresses" pp_addr a) e.erange man flow in
      Cases.empty flow
    | Some (AddrOf ({ base_kind = String _; }, _, _)) ->
      Cases.singleton (Nbt Untracked: Stat.t) flow
(*
  Will not compute status of the following expressions.
  They should have been taken care of by previous iterators:
        E_alloc_addr, E_call, E_array, E_subscript, E_len,
        E_c_conditional, E_c_array_subscript, E_c_member_access,
        E_c_builtin_call, E_c_arrow_access,
        E_c_assign, E_c_compound_assign, E_c_comma, E_c_increment,
        E_c_statement, E_c_predefined, E_c_var_args, E_c_atomic, E_c_block_object,
        E_c_ffi_call
*)


  (** {2 Expression Shape} *)
  (** ==================== *)

  (* S(v) *)
  let shapes_var man flow var =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match Map.find_opt var m with
    | Some ((_, _), shapes) -> Cases.singleton shapes flow
    | None ->
      (* FIXME: for type var[size], var is not added to the domain *)
      let () = Debug.debug ~channel:"runtime" "missing variable %a" pp_var var
      in Cases.singleton (Shape.non_ocaml_value) flow

  (* S(&v) *)
  let shapes_addr_of_var man flow var range = Cases.singleton (Shape.non_ocaml_value) flow

  (* S(c) *)
  let shapes_const man flow const = Cases.singleton (Shape.non_ocaml_value) flow
  let shapes_binop man flow op =  Cases.singleton (Shape.non_ocaml_value) flow
  let shapes_unop man flow op =  Cases.singleton (Shape.non_ocaml_value) flow

  let shapes_addr_of man flow e =
    match ekind e with
    | E_var (v, _) -> shapes_addr_of_var man flow v e.erange
    | E_c_function _ | E_function _ -> Cases.singleton (Shape.non_ocaml_value) flow
    | _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "shape(&%a) unsupported for this expression; only variables supported" pp_expr e) e.erange man flow in
      Cases.empty flow

  let shapes_cast man flow c s = Cases.singleton s flow

  let rec shapes_expr man flow e =
    match ekind e with
    | E_var (var, _)       ->
      shapes_var man flow var
    | E_constant c         ->
      shapes_const man flow c
    | E_binop (op, e1, e2) ->
      shapes_binop man flow op
    | E_unop(op, e)        ->
      shapes_unop man flow op
    | E_addr(a, mode)      ->
      Cases.singleton (Shape.non_ocaml_value) flow
    | E_c_address_of e     ->
      shapes_addr_of man flow e
    | E_c_deref e          ->
      shapes_deref man flow e e.erange
    | E_c_cast (e,c)       ->
      shapes_expr man flow e >>$ fun s flow ->
      shapes_cast man flow c s
    | E_c_function _ | E_function _ | E_c_builtin_function _  ->
      Cases.singleton Shape.non_ocaml_value flow
    | _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "shapes(%a) unsupported for this kind of expression" pp_expr e) e.erange man flow in
      Cases.empty flow
and shapes_deref man flow e range =
    match Static_points_to.eval_opt e flow with
    | None | Some (Fun _) ->
      let flow = raise_ffi_internal_error (Format.asprintf "shapes(*%a) unsupported for this kind of expression" pp_expr e) e.erange man flow in
      Cases.empty flow
    | Some (Eval(v, _, _)) ->
      (* NOTE: Thist state should be unreachable, because dereference is taken care of
         by an earlier domain. However, in some corner cases we can actually still reach
         this case. Since evaluation of [*e] has not succeeded, we simply return [T],
         because we do not know the precise shapes. *)
      Cases.singleton (Shape.top) flow
    | Some Null | Some Invalid | Some Top ->
      Cases.singleton (Shape.top) flow
    | Some (AddrOf ({ base_kind = Var v; }, _, _)) ->
      shapes_var man flow v
    | Some (AddrOf ({ base_kind = Addr a; }, _, _)) ->
      let flow = raise_ffi_internal_error (Format.asprintf "shapes(%a) unsupported for addresses" pp_addr a)  e.erange man flow in
      Cases.empty flow
    | Some (AddrOf ({ base_kind = String _; }, _, _)) ->
      Cases.singleton (Shape.top) flow
(*
  Will not compute status of the following expressions.
  They should have been taken care of by previous iterators:
        E_alloc_addr, E_call, E_array, E_subscript, E_len,
        E_c_conditional, E_c_array_subscript, E_c_member_access,
        E_c_builtin_call, E_c_arrow_access,
        E_c_assign, E_c_compound_assign, E_c_comma, E_c_increment,
        E_c_statement, E_c_predefined, E_c_var_args, E_c_atomic, E_c_block_object,
        E_c_ffi_call
*)


  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  let mark_var_active var range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let m' = update_status var (Stat.embed Active) m in
    set_env T_cur (m', l) man flow

  let eval_set_shape_var var ss range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match Map.find_opt var m with
    | None ->
      let flow = raise_ffi_shape_missing range pp_var var man flow in
      Cases.empty flow
    | Some ((status, roots), _) ->
      let m' = Map.add var ((status, roots), ss) m in
      set_env T_cur (m', l) man flow

  let unwrap_expr_as_var exp ?(fail = fun man flow -> raise_ffi_not_variable exp man flow)  man flow =
    match ekind (remove_casts exp) with
    | E_var (var, _) ->
      Cases.singleton var flow
    | _ ->
      let flow = fail man flow in
      Cases.empty flow

  let eval_block_as_var base off mode typ range man flow =
    let lval = mk_lval base off typ mode range in
    man.eval lval flow >>$ fun exp flow ->
    match ekind exp with
    | E_var (v, _) -> Cases.singleton (Some v) flow
    | _ -> Cases.singleton None flow

  let eval_deref_to_var exp man flow =
    resolve_pointer exp man flow >>$ fun ptr flow ->
    match ptr with
    | P_block (base, off, mo) ->
      eval_block_as_var base off mo ffi_value_typ exp.erange man flow
    | P_null | P_invalid | P_top | P_fun _ ->
      Cases.singleton None flow

  let eval_ocaml_value_shape_of_var var range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match lookup_shapes var m with
    | None ->
      let flow = raise_ffi_shape_missing range pp_var var man flow in
      Cases.empty flow
    | Some (TOP as sh) | Some (BOT as sh) | Some (Nbt (R _) as sh) ->
      let flow = raise_ffi_shape_non_value range pp_var var Shape.pp_shapes sh man flow in
      Cases.empty flow
    | Some (Nbt (L sh)) ->
      Cases.singleton sh flow


  type integer_result = IntegerEvalFail | Integer of Z.t | IntegerRange of Z.t * Z.t
  type int_result = IntEvalFail | Int of int | IntRange of int * int

  let eval_number_exp_to_integer exp man flow =
    man.eval ~translate:"Universal" exp flow >>$ fun exp' flow ->
    man.ask (Universal.Numeric.Common.mk_int_interval_query exp') flow >>$ fun kind_itv flow ->
    match Itv.bounds_opt kind_itv with
    | Some l, Some r when Z.equal l r -> Cases.singleton (Integer l) flow
    | Some l, Some r -> Cases.singleton (IntegerRange (l, r)) flow
    | _ -> Cases.singleton IntegerEvalFail flow

  let eval_number_exp_to_int exp man flow =
    eval_number_exp_to_integer exp man flow >>$ fun res flow ->
    match res with
    | IntegerEvalFail -> Cases.singleton IntEvalFail flow
    | Integer z ->
      (try Cases.singleton (Int (Z.to_int z)) flow
      with Z.Overflow -> Cases.singleton IntEvalFail flow)
    | IntegerRange (l, r) ->
      (try Cases.singleton (IntRange (Z.to_int l, Z.to_int r)) flow
      with Z.Overflow -> Cases.singleton IntEvalFail flow)




  (** {2 Transformers} *)
  (** ============================================== *)

  let exec_add var man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let m' = Map.add var ((Stat.embed Untracked, Root.embed NotRooted), Shape.non_ocaml_value) m in
    set_env T_cur (m', l) man flow

  let exec_remove var man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let m' = Map.remove var m in
    set_env T_cur (m', l) man flow


  let exec_update var expr man flow =
    status_expr man flow expr >>$ fun status flow ->
    shapes_expr man flow expr >>$ fun shapes flow ->
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let m' = update_status var status m in
    let m'' = update_shapes var shapes m' in
    set_env T_cur (m'', l) man flow


  let exec_check_ext_call_arg arg man flow =
    status_expr man flow arg >>$ fun status flow ->
    begin match status with
    | Nbt Untracked -> Post.return flow
    (* we allow for now to pass active values to external functions *)
    | Nbt Active -> Post.return flow
    | _ ->
      let flow = raise_ffi_inactive_value arg man flow in Post.return flow
    end

  let exec_ext_call f args man flow =
    let check_ext_call_args exprs = List.fold_left (fun acc c -> Post.bind (exec_check_ext_call_arg c man) acc) (Post.return flow) exprs in
    check_ext_call_args args

  let exec_init_with_shape exp sh range man flow =
    man.eval exp flow >>$ fun exp flow ->
    unwrap_expr_as_var exp man flow >>$ fun var flow ->
    mark_var_active var range man flow >>% fun flow ->
    let sh = Shape.ocaml_value (OCamlValueExt.type_shape_to_shapes sh) in
    eval_set_shape_var var sh range man flow


  let exec_assert_ocaml_shape_var var constr_sh range man flow =
    eval_ocaml_value_shape_of_var var range man flow >>$ fun var_sh flow ->
    if OCamlValueExt.compat ~value:var_sh ~constr:constr_sh then
      let flow = safe_ffi_shape_check range man flow in
      Post.return flow
    else
      let flow = raise_ffi_shape_mismatch range (OCamlValue.to_string constr_sh) (OCamlValue.to_string var_sh)  man flow in
      Cases.empty flow


  let exec_assert_shape exp sh range man flow =
    unwrap_expr_as_var exp man flow >>$ fun v flow ->
    let ss = OCamlValueExt.type_shape_to_shapes sh in
    exec_assert_ocaml_shape_var v ss range man flow >>%  fun flow ->
    Post.return flow

  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  (** Entry point of abstract transformers *)
  let exec stmt man flow =
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      exec_add var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return
    | S_remove { ekind = E_var (var, _) } ->
      exec_remove var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow)   |> OptionExt.return
    | S_forget { ekind = E_var (var, _) } ->
      exec_remove var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow)  |> OptionExt.return
    | S_rename ({ ekind = E_var (from, _) }, { ekind = E_var (into, _) }) ->
      (* FIXME: Should we implement this? *)
      None
    | S_c_ext_call (f, args) ->
      exec_ext_call f args man flow |> OptionExt.return
    | S_assign ({ ekind= E_var (var, mode) }, e) ->
      exec_update var e man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return
    | S_ffi_init_with_shape (exp, sh) ->
      exec_init_with_shape exp sh stmt.srange man flow |> OptionExt.return
    | S_ffi_assert_shape (e, sh) ->
      exec_assert_shape e sh stmt.srange man flow |> OptionExt.return
    | _ -> None


  (** {2 FFI function evaluation} *)
  (** ====================== *)

  let eval_mark_active_contents exp man flow =
    eval_deref_to_var exp man flow >>$ fun var flow ->
    begin match var with
    | None ->
      let flow = raise_ffi_internal_error (Format.asprintf "attempting to mark the expression *%a active, which does not evaluate to a variable" pp_expr exp) exp.erange man flow in
      Cases.empty flow
    | Some v ->
      mark_var_active v exp.erange man flow >>% fun flow ->
      Eval.singleton (mk_unit exp.erange) flow
    end

  let eval_mark_active_pointer range exp man flow =
    unwrap_expr_as_var exp man flow >>$ fun var flow ->
    mark_var_active var range man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow


  let eval_garbage_collect range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let upd_set ((s, r), shapes) = if Stat.is_const s Active && not (Root.is_const r Rooted) then ((Stat.embed Stale, r), shapes) else ((s, r), shapes) in
    let m' = Map.map (fun s -> upd_set s) m in
    set_env T_cur (m', l) man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow

  let eval_assert_valid_var var m exp man flow =
    let flow = match lookup_status var m with
    | Some BOT -> flow
    | Some Nbt Active ->
      let flow = safe_ffi_value_liveness_check exp.erange man flow in
      flow
    | Some Nbt Stale | Some Nbt Untracked | None ->
      let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
      flow
    | Some TOP ->
      let flow = raise_ffi_inactive_value ~bottom:false exp man flow in
      flow
    in
      Eval.singleton (mk_unit exp.erange) flow

  let eval_assert_valid exp man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    unwrap_expr_as_var exp man flow >>$ fun var flow ->
    eval_assert_valid_var var m exp man flow


  let exec_register_root_var var range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let vexp = (mk_var var range) in
    match Map.find_opt var m with
    | Some ((_, Nbt Rooted), _) ->
      let flow = raise_ffi_double_root ~bottom:false vexp man flow in
      Post.return flow
    | Some ((Nbt Untracked, _), _) ->
      let flow = raise_ffi_inactive_value ~bottom:true vexp man flow in
      Post.return flow
    | Some ((Nbt Active, Nbt NotRooted), shapes) ->
      let m' = Map.add var ((Nbt Active, Root.embed Rooted), shapes) m in
      set_env T_cur (m', l) man flow >>% fun flow ->
      let flow = safe_ffi_roots_check range man flow in
      Post.return flow
    | _ ->
      let flow = raise_ffi_rooting_failed ~bottom:true vexp man flow in
      Post.return flow

  let exec_unregister_root_var var range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    let vexp = (mk_var var range) in
    match Map.find_opt var m with
    | Some ((status, Nbt Rooted), shapes) ->
      let m' = Map.add var ((status, Root.embed NotRooted), shapes) m in
      set_env T_cur (m', l) man flow >>% fun flow ->
      let flow = safe_ffi_roots_check range man flow in
      Post.return flow
    | Some ((_, _), _) | None ->
      let flow = raise_ffi_not_rooted ~bottom:true vexp man flow in
      Post.return flow

  let eval_register_root range exp man flow =
    eval_deref_to_var exp man flow >>$ fun var flow ->
    match var with
    | None ->
      let flow = raise_ffi_rooting_failed ~bottom:true exp man flow in
      Eval.singleton (mk_unit range) flow
    | Some v ->
      exec_register_root_var v exp.erange man flow >>% fun flow ->
      Eval.singleton (mk_unit exp.erange) flow

  let eval_unregister_root range exp man flow =
    eval_deref_to_var exp man flow >>$ fun var flow ->
    match var with
    | None ->
      let flow = raise_ffi_not_rooted ~bottom:true exp man flow in
      Eval.singleton (mk_unit range) flow
    | Some v ->
      exec_unregister_root_var v exp.erange man flow >>% fun flow ->
      Eval.singleton (mk_unit exp.erange) flow

  let eval_assert_runtime_lock range man flow =
    get_env T_cur man flow >>$ fun ((m, l):  _ * Lock.t) flow ->
    match l with
    | Nbt Locked ->
      let flow = safe_ffi_runtime_lock_check range man flow in
      Eval.singleton (mk_unit range) flow
    | _ ->
      let flow = raise_ffi_runtime_unlocked ~bottom:true range man flow in
      Eval.singleton (mk_unit range) flow

  let eval_update_runtime_lock range new_state man flow =
    get_env T_cur man flow >>$ fun ((m, l):  _ * Lock.t) flow ->
    let l' = (if new_state then Lock.embed Locked else Lock.embed Unlocked) in
    set_env T_cur (m, l') man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow


  let eval_fresh_pointer range man flow =
    man.eval (mk_alloc_addr A_runtime_resource ~mode:STRONG range) flow >>$ fun addr flow ->
    match ekind addr with
    | E_addr (a, _) ->
      let val_var = (mk_ffi_var_expr a range) in
      man.exec (mk_add val_var range) flow >>% fun flow ->
      Cases.singleton (mk_c_address_of val_var range) flow
    | _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "failed to allocate a fresh address") range man flow in
      Cases.empty flow


  let rec eval_ffi_primtive_args args man flow =
    match args with
    | [] -> Cases.singleton [] flow
    | (arg::args) ->
      man.eval arg flow >>$ fun arg flow ->
      eval_ffi_primtive_args args man flow >>$ fun args flow ->
      Cases.singleton (arg :: args) flow



  let eval_is_immediate_var var range man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match Map.find_opt var m with
    | None ->
      let flow = raise_ffi_shape_missing range pp_var var man flow in
      Cases.empty flow
    | Some ((status, roots), ss) ->
      Shape.value_kind_runtime_shape ss flow >>$ fun upd flow ->
      match upd with
      | None ->
        let flow = raise_ffi_shape_non_value range pp_var var Shape.pp_shapes ss man flow in
        Cases.empty flow
      | Some (ss', rt) ->
        let m' = update_shapes var (Shape.ocaml_value ss') m in
        set_env T_cur (m', l) man flow >>% fun flow ->
        Cases.singleton rt flow

  (* Given an expression of type [enum ffi_shape], this function computes
    the corresponding [OCamlValueShape.t] shape. *)
  let eval_ocaml_shape_of_runtime_enum_expr int_expr range man flow =
    eval_number_exp_to_int int_expr man flow >>$ fun num flow ->
    match num with
    | IntEvalFail | IntRange _ ->
      let flow = raise_ffi_shape_number_error range pp_expr int_expr man flow in
      Cases.empty flow
    | Int n ->
      begin match OCamlValueExt.shape_of_runtime_shape n with
      | None ->
        let flow = raise_ffi_shape_number_error range pp_expr int_expr man flow in
        Cases.empty flow
      | Some ss ->
        Cases.singleton ss flow
      end


  let eval_assert_shape v sh range man flow =
    unwrap_expr_as_var v man flow >>$ fun v flow ->
    eval_ocaml_shape_of_runtime_enum_expr sh range man flow >>$ fun ss flow ->
    exec_assert_ocaml_shape_var v ss range man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow

  let eval_is_immediate v range man flow =
    unwrap_expr_as_var v man flow >>$ fun v flow ->
    eval_is_immediate_var v range man flow >>$ fun rt flow ->
    match rt with
    | Pointer -> Eval.singleton (mk_int 0 ~typ:(T_c_integer C_signed_int) range) flow
    | Immediate -> Eval.singleton (mk_int 1 ~typ:(T_c_integer C_signed_int) range) flow


  let eval_set_shape ptr sh range man flow =
    eval_deref_to_var ptr man flow >>$ fun var flow ->
    match var with
    | None ->
      let flow = raise_ffi_shape_missing range pp_expr ptr man flow in
      Cases.empty flow
    | Some v ->
      eval_ocaml_shape_of_runtime_enum_expr sh range man flow >>$ fun ss flow ->
      eval_set_shape_var v (Shape.ocaml_value ss) range man flow >>% fun flow ->
      Eval.singleton (mk_unit range) flow


  let eval_unimplemented range man flow =
    let flow = raise_ffi_unimplemented range man flow in
    Cases.empty flow

  let exec_pass_on_shape_var range pv i cv man flow =
    eval_ocaml_value_shape_of_var pv range man flow >>$ fun sh flow ->
    let sh = OCamlValue.field_shape_at_index sh i in
    eval_set_shape_var cv (Shape.ocaml_value sh) range man flow >>$ fun _ flow ->
    Post.return flow

  let exec_pass_on_shape range parent index child man flow =
    unwrap_expr_as_var parent man flow >>$ fun pv flow ->
    eval_deref_to_var child man flow >>$ fun cv flow ->
    match cv with
    | None ->
      (* FIXME: maybe raise an error *)
      Post.return flow
    | Some cv ->
      eval_number_exp_to_int index man flow >>$ fun res flow ->
      (* FIXME: use the integer version and do something even for ranges *)
        begin match res with
      | IntEvalFail -> Post.return flow
      | IntRange _ -> Post.return flow
      | Int n -> exec_pass_on_shape_var range pv n cv man flow
      end

  let eval_pass_on_shape range parent index child man flow =
    exec_pass_on_shape range parent index child man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow


  let exec_assert_shape_subset_var v1 v2 range man flow =
    eval_ocaml_value_shape_of_var v1 range man flow >>$ fun sh1 flow ->
    eval_ocaml_value_shape_of_var v2 range man flow >>$ fun sh2 flow ->
      if OCamlValue.subset sh1 sh2 then
        Post.return flow
      else
        let flow = raise_ffi_shape_mismatch range (Format.asprintf "a subset of %a" OCamlValue.pp_shapes sh2) (OCamlValue.to_string sh1) man flow in
        Cases.empty flow

  let exec_assert_shape_compat_var v1 v2 range man flow =
    eval_ocaml_value_shape_of_var v1 range man flow >>$ fun sh1 flow ->
    eval_ocaml_value_shape_of_var v2 range man flow >>$ fun sh2 flow ->
      if OCamlValueExt.compat ~value:sh1 ~constr:sh2 then
        Post.return flow
      else
        let flow = raise_ffi_shape_mismatch range (Format.asprintf "a shape compatible with %a" OCamlValue.pp_shapes sh2) (OCamlValue.to_string sh1) man flow in
        Cases.empty flow

  let eval_assert_shape_subset e1 e2 range man flow =
    unwrap_expr_as_var e1 man flow >>$ fun v1 flow ->
    unwrap_expr_as_var e2 man flow >>$ fun v2 flow ->
    exec_assert_shape_subset_var v1 v2 range man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow

  let eval_assert_shape_compat e1 e2 range man flow =
    unwrap_expr_as_var e1 man flow >>$ fun v1 flow ->
    unwrap_expr_as_var e2 man flow >>$ fun v2 flow ->
    exec_assert_shape_compat_var v1 v2 range man flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow

  let eval_ffi_primtive f args range man flow =
    eval_ffi_primtive_args args man flow >>$ fun args flow ->
    match f, args with
    | "_ffi_garbage_collect", [] ->
      eval_garbage_collect range man flow
    | "_ffi_mark_active_contents", [e] ->
      eval_mark_active_contents e man flow
    | "_ffi_mark_active_ptr", [e] ->
      eval_mark_active_pointer range e man flow
    | "_ffi_register_root", [e] ->
      eval_register_root range e man flow
    | "_ffi_unregister_root", [e] ->
      eval_unregister_root range e man flow
    | "_ffi_assert_active", [e] ->
      eval_assert_valid e man flow
    | "_ffi_assert_active_ptr", [e] ->
      eval_assert_valid e man flow
    | "_ffi_assert_shape", [v; sh] ->
      eval_assert_shape v sh range man flow
    | "_ffi_set_shape", [v; sh] ->
      eval_set_shape v sh range man flow
    | "_ffi_assert_locked", [] ->
      eval_assert_runtime_lock range man flow
    | "_ffi_acquire_lock", [] ->
      eval_update_runtime_lock range true man flow
    | "_ffi_release_lock", [] ->
      eval_update_runtime_lock range false man flow
    | "_ffi_fresh_value_ptr", [] ->
      eval_fresh_pointer range man flow
    | "_ffi_is_immediate", [e] ->
      eval_is_immediate e range man flow
    | "_ffi_unimplemented", [] ->
      eval_unimplemented range man flow
    | "_ffi_pass_on_shape", [parent; idx; child] ->
      eval_pass_on_shape range parent idx child man flow
    | "_ffi_assert_shape_subset", [e1; e2] ->
      eval_assert_shape_subset e1 e2 range man flow
    | "_ffi_assert_shape_compat", [e1; e2] ->
        eval_assert_shape_compat e1 e2 range man flow
    | _, _ ->
      let msg = Format.asprintf "unsupported ffi call %s(%a)" f (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args in
      let flow = raise_ffi_internal_error msg range man flow in
      Cases.empty flow


  let eval_var_status var man flow =
    get_env T_cur man flow >>$ fun (m, l) flow ->
    match lookup_status var m with
    | Some s ->
      Cases.singleton s flow
    | None ->
      Cases.singleton Bot_top.TOP flow



  (** Entry point of abstraction evaluations *)
  let eval exp man flow =
    match ekind exp with
    | E_ffi_call (f, args) -> eval_ffi_primtive f args exp.erange man flow |> OptionExt.return
    | _ -> None

  (** {2 Handler of queries} *)
  (** ====================== *)


  let ask : type a r. (a,r) query -> (a,t) man -> a flow ->  (a, r) cases option =
    fun query man flow ->
    match query with
    | Q_ffi_status var ->
        eval_var_status var man flow |> OptionExt.return
    | _ -> None

  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_state printer (a, l) =
    let () = pprint ~path:[Key "runtime lock"] printer (String (Lock.to_string l)) in
    pprint ~path:[Key "runtime"] printer (pbox Map.print a)



  let print_expr man flow printer exp = ()

end

let () =
  register_standard_domain (module Domain)
