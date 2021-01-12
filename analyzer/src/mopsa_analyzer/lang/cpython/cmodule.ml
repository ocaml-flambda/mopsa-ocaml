012(* Truc commun au multilangage: addr du tas/variable/fonction
   Côté C: manipuler des addr plutôt que des bases (il y aura des addr de fonctions/variables/...)
           Var -> Valeur ~> Addr -> Valeur
 *)

(* FIXME: boundaries between C and Python should be applied systematically *)
(* FIXME: Python value should have proper ob_type defined at least.
          Or: we should intercept the calls.
          what if PyLong_Check(python user defined object)?
          -> if it's not in the equiv class, then we need to force the ob_type
*)
(*
  TODO:
   - parameter conversion Python~>C isn't done in all cases
   - support more things in PyParse_Tuple
   - support PyBuild_Value
   - support members in class declarations?
   - prendre les range d'allocation du python plutôt... (ou avoir range+cs pour les kinds d'addr concernés)
*)
(* Intercepts call to PyModule_Create and PyModule_AddObject *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open C.Ast
open C.Common.Points_to
open Python.Ast
open Python.Addr

type assumption_kind +=
   | A_cpython_unsupported_fields of string

let () =
  register_assumption {
      print = (fun next fmt -> function
                | A_cpython_unsupported_fields f ->
                   Format.fprintf fmt "in C/Python analysis, ignoring fields: %a"
                     (Debug.bold Format.pp_print_string) f
                | a -> next fmt a);
      compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_cpython_unsupported_fields s1, A_cpython_unsupported_fields s2 ->
           Stdlib.compare s1 s2
        | _ -> next a1 a2);
    }

module Domain =
  struct

    (* FIXME: propagate changes done by either C or Python during new
       allocations/... which should not happen often *)
    (* OR: sometimes ask the recency for a STRONG addr whatever the cost? *)
    module EquivBaseAddrs = Framework.Lattices.Partial_inversible_map.Make
                              (struct
                                (* FIXME: we'd rather have PointerValue.t *)
                                type t = C.Common.Points_to.points_to
                                let compare p1 p2 =
                                  match p1, p2 with
                                  | P_fun f1, P_fun f2 ->
                                     compare f1.C.Ast.c_func_unique_name f2.C.Ast.c_func_unique_name
                                  | P_block (b1, o1, m1), P_block (b2, o2, m2) ->
                                     Compare.compose [
                                         (fun () -> C.Common.Base.compare_base b1 b2);
                                         (fun () ->
                                           let o1' = match ekind o1 with
                                             | E_binop (O_plus, _, r) -> r
                                             | _ -> o1 in
                                           let o2' = match ekind o2 with
                                             | E_binop (O_plus, _, r) -> r
                                             | _ -> o2 in
                                           (* FIXME: hack on offset comparison.py_cls_a_abases. I think we should evaluate the offset in universal for each block and see what happens *)
                                           compare_expr o1' o2');
                                         (fun () -> Option.compare compare_mode m1 m2);
                                       ]
                                  | _, _ -> Stdlib.compare p1 p2
                                let print = unformat C.Common.Points_to.pp_points_to
                              end)
                              (struct
                                type t = addr
                                let compare = compare_addr
                                let print = unformat pp_addr
                              end)

    include EquivBaseAddrs

    include Framework.Core.Id.GenDomainId(
                struct
                  type nonrec t = t
                  let name = "cpython.cmodule"
                end)

    let checks = []

    let yield_results_before_crash man flow =
      Framework.Output.Text.report man flow ~time:(0.) ~files:[] ~out:None

    let init _ man flow =
      List.iter (fun a -> Hashtbl.add C.Common.Builtins.builtin_functions a ())
        [
          "PyModule_Create2";
          "PyModule_AddObject";
          "PyType_Ready";
          "PyType_GenericAlloc_Helper";
          "PyArg_ParseTuple";
          "PyTuple_Size";
          "PyTuple_GetItem";
          "PyLong_FromLong";
          "PyLong_FromSsize_t";
          "PyLong_AsLong";
        ];

      set_env T_cur EquivBaseAddrs.empty man flow

    exception Null_found

    let get_name_of expr man flow =
      let r = resolve_pointer expr man flow >>$
                (fun points_to flow ->
                  match points_to with
                  | P_block({base_kind = String (s, _, _)}, offset, _) ->
                     Cases.singleton s flow (* FIXME: assert offset = [0, 0] *)
                  | P_null ->
                     raise Null_found
                  | _ -> assert false
                ) in
      assert(Cases.cardinal r <= 1);
      r

    let safe_get_name_of expr man flow =
      let r = resolve_pointer expr man flow >>$
                (fun points_to flow ->
                  match points_to with
                  | P_block({base_kind = String (s, _, _)}, offset, _) ->
                     Cases.singleton (Some s) flow (* FIXME: assert offset = [0, 0] *)
                  | P_null ->
                     Cases.singleton None flow
                  | _ -> panic "points_to %a" pp_points_to points_to
                ) in
      assert(Cases.cardinal r <= 1);
      r

    let set_singleton p a man flow =
      let r = get_env T_cur man flow |>
                set p (Nt (ValueSet.singleton a)) in
      set_env T_cur r man flow

    (* Creates stmt binder_addr · obj_name = obj_addr *)
    let bind_in_python binder_addr obj_name obj_addr range man flow =
      (if compare_addr_kind (akind obj_addr) (akind @@ OptionExt.none_to_exn @@ !Python.Types.Addr_env.addr_integers) = 0 then
        man.eval (mk_var (mk_addr_attr obj_addr "value" T_int) range) flow >>$
          fun int_value flow ->
          Cases.return (Some int_value) flow
      else
        Cases.return None flow) >>$
        fun obj_oe flow ->
        man.exec
          (mk_assign
             (Python.Ast.mk_py_attr
                (Python.Ast.mk_py_object (binder_addr, None) range)
                obj_name ~etyp:(Python.Ast.T_py None) range)
             (Python.Ast.mk_py_object (obj_addr, obj_oe) range)
             range) flow

    let mk_base_expr base range =
      let open C.Common.Base in
      match base.base_kind with
      | Var v  -> mk_var v range
      | Addr a -> mk_addr a range
      | _      -> assert false

    (* Return the expression ( typ* )(( char* )&base + offset) *)
    let mk_base_offset_pointer base offset typ range =
      let base_addr = mk_c_cast (mk_c_address_of (mk_base_expr base range) range) (T_c_pointer s8) range in
      let elem_addr = add base_addr offset ~typ:(T_c_pointer s8) range in
      mk_c_cast elem_addr (T_c_pointer typ) range

    let new_method_from_def binder_addr methd_kind methd man flow =
      let range = erange methd in
      get_name_of (mk_c_member_access_by_name methd "ml_name" range) man flow >>$
        fun methd_name flow ->
        resolve_pointer (mk_c_member_access_by_name methd "ml_meth" range) man flow >>$
          fun methd_function flow ->
          let methd_fundec = match methd_function with
            | P_fun f -> f
            | _ -> assert false in
          man.eval (mk_alloc_addr (Python.Addr.A_py_c_function (methd_fundec.c_func_org_name, methd_fundec.c_func_uid, methd_kind, (binder_addr, None))) range) flow >>$
            fun methd_eaddr flow ->
            let methd_addr = Addr.from_expr methd_eaddr in
            let flow = set_singleton methd_function methd_addr man flow in
            (* bind method to binder *)
            bind_in_python binder_addr methd_name methd_addr range man flow

    let rec fold_until_null func c flow =
      try
        fold_until_null func (c+1) (func c flow)
      with Null_found -> flow


    (* add_pymethoddef "tp_methods" cls_addr "method_descriptor" ecls man flow *)
    let add_pymethoddef field_name binder_addr methd_descr expr man flow =
      let range = erange expr in
      let add_method pos flow =
        man.eval (mk_c_subscript_access
                    (mk_c_arrow_access_by_name expr field_name range)
                    (mk_int pos range)
                    range) flow >>$
          (fun methd flow ->
            new_method_from_def binder_addr methd_descr methd man flow)
        |> post_to_flow man
      in
      fold_until_null add_method 0 flow


    let add_pymemberdef binder_addr expr man flow =
      let range = erange expr in
      let add_member pos flow =
        let range = tag_range range "@%d" pos in
        debug "add member %a" pp_range range;
        man.eval (mk_c_subscript_access
                    (mk_c_arrow_access_by_name expr "tp_members" range)
                    (mk_int pos range)
                    range) flow >>$
          (fun member flow ->
            get_name_of (mk_c_member_access_by_name member "name" range) man flow >>$
              fun member_name flow ->
              debug "name is %s" member_name;
              resolve_pointer (mk_c_address_of member range) man flow >>$
                fun member_points_to flow ->
                debug "points_to %a" pp_points_to member_points_to;
                man.eval (mk_alloc_addr (Python.Addr.A_py_instance (fst @@ Python.Addr.find_builtin "member_descriptor")) range) flow >>$
                  fun member_descr flow ->
                  let member_descr = Addr.from_expr member_descr in
                  let flow = set_singleton member_points_to  member_descr man flow in
                  man.exec (mk_assign (mk_py_attr (mk_py_object (member_descr, None) range) "__name__" range) {(mk_string member_name range) with etyp = T_py None} range) flow >>%
                    bind_in_python binder_addr member_name member_descr range man
          )
        |> post_to_flow man
      in
      fold_until_null add_member 0 flow


    let new_module_from_def expr man flow =
      (*
         1) Allocate A_py_c_module of mod->m_name (@_m)
         2) Forall meth = methods in mod->m_methods:
            n = meth->ml_name
            a) allocate them? A_py_c_function(n) -> @_f
            b) execute @_m·n = @_f
       *)
      let range = erange expr in
      resolve_pointer (mk_c_arrow_access_by_name expr "m_name" range) man flow >>$
        fun points_to flow ->
        let module_name = match points_to with
          | P_block({base_kind = String (s, _, _)}, offset, _) -> s
          | P_null ->
             raise Null_found
          | _ -> assert false in
        man.eval (mk_alloc_addr (Python.Addr.A_py_c_module module_name) range) flow >>$
          fun module_addr flow ->
          let m_addr = Addr.from_expr module_addr in
          let flow = post_to_flow man @@ man.exec (mk_add module_addr range) flow  in
          let flow = set_singleton (mk_c_points_to_bloc (C.Common.Base.mk_addr_base m_addr) (mk_zero range) None) m_addr man flow in
          Eval.singleton module_addr (add_pymethoddef "m_methods" m_addr Builtin_function_or_method expr man flow)

    let resolve_c_pointer_into_addr expr man flow =
      resolve_pointer expr man flow >>$
        (fun points_to flow ->
          debug "searching for %a" pp_points_to points_to;
          if points_to = P_null then raise Null_found else
          let aset = Top.detop @@ find points_to (get_env T_cur man flow) in
          debug "got %a" (ValueSet.fprint SetExt.printer_default pp_addr) aset;
          match ValueSet.cardinal aset with
          | 0 ->
             warn_at expr.erange "resolve_c_pointer_into_addr of %a does not work, falling back" pp_points_to points_to;
             begin match points_to with
             | P_block({base_kind = Addr a}, _, _) ->
                Cases.singleton a flow
             | _ ->
                assert false
             end
          | 1 ->
             Cases.singleton (ValueSet.choose aset) flow
          | _ -> assert false
        )

    (* bind function pointed to by expr, as cls.name in python side *)
    let bind_function_in name expr cls_addr function_kind man flow =
      let range = erange expr in
      resolve_pointer expr man flow >>$
        fun func flow ->
        Post.return
          (match func with
           | P_fun fundec ->
              man.eval (mk_alloc_addr (Python.Addr.A_py_c_function (fundec.c_func_org_name, fundec.c_func_uid, function_kind, (cls_addr, None))) range) flow >>$
                (fun fun_eaddr flow ->
                  let fun_addr = Addr.from_expr fun_eaddr in
                  let flow = set_singleton func fun_addr man flow in
                  bind_in_python cls_addr name fun_addr range man flow)
              |> post_to_flow man

           | _ -> flow)

    let search_c_globals_for flow s =
      let c_prog_globals = (get_c_program flow).c_globals in
      fst @@ List.find
               (fun (x, _) ->
                 match vkind x with
                 | V_cvar v -> v.cvar_orig_name = s
                 | _ -> false) c_prog_globals

    let check_consistent_null function_name man flow range =
      (* this corresponds to _Py_CheckFunctionResult in Objects/call.c *)
      (* check that exc_state is not NULL and raise the corresponding exception+message *)
      (* if exc_state is NULL raise the specific error PyExc_SystemError("... returned NULL without setting an error") *)
      let exc_state = search_c_globals_for flow "exc_state" in
      let exc_msg = search_c_globals_for flow "exc_msg" in
      (* FIXME: call PyErr_Occured() != NULL rather? *)
      try
        resolve_c_pointer_into_addr (mk_var exc_state range) man flow >>$
          fun exc_addr flow ->
          safe_get_name_of (mk_var exc_msg range) man flow >>$
            fun exc_msg flow ->
            let args = match exc_msg with
              | None -> []
              | Some s -> [{(Universal.Ast.mk_string s range) with etyp=(T_py None)}] in
            man.exec (Python.Ast.mk_raise
                        (Python.Ast.mk_py_call
                           (Python.Ast.mk_py_object (exc_addr, None) range)
                           args range)
                        range) flow >>%
              Eval.empty
      with Null_found ->
        man.exec (Python.Ast.mk_raise
                    (Python.Ast.mk_py_call
                       (Python.Ast.mk_py_object (fst @@ find_builtin "SystemError", None) range)
                       [{(Universal.Ast.mk_string (Format.asprintf "%s returned NULL without setting an error" function_name) range) with etyp=(T_py None)}] range)
                    range) flow >>% Eval.empty


    let new_class_from_def ecls man flow =
      let range = erange ecls in
      resolve_pointer ecls man flow >>$
        fun cls flow ->
        let cls_var = match cls with
          | P_block ({base_kind = Var v}, _, _) -> v
          | _ -> assert false in
        man.eval (mk_alloc_addr (Python.Addr.A_py_c_class cls_var) range) flow >>$
          fun cls_eaddr flow ->
          let cls_addr = Addr.from_expr cls_eaddr in
          let flow = set_singleton cls cls_addr man flow in
          (* fill dict with methods, members, getset
             ~> by delegation to the dictionnary/structural type abstraction *)
          bind_function_in "__new__" (mk_c_arrow_access_by_name ecls "tp_new" range) cls_addr Builtin_function_or_method man flow >>%
            fun flow ->
            bind_function_in "__init__" (mk_c_arrow_access_by_name ecls "tp_init" range) cls_addr (Wrapper_descriptor (Some "wrap_init")) man flow >>%
              fun flow ->
              assume
                (mk_binop
                   (mk_c_arrow_access_by_name ecls "tp_as_sequence" range)
                   O_ne
                   (mk_c_null range)
                   ~etyp:T_bool range)
                man flow
                ~fthen:(fun flow ->
                  bind_function_in "__len__" (mk_c_arrow_access_by_name (mk_c_arrow_access_by_name ecls "tp_as_sequence" range) "sq_length" range) cls_addr (Wrapper_descriptor (Some "wrap_lenfunc")) man flow)
                ~felse:(fun flow ->
                  debug "tp_as_sequence is NULL, skipping";
                  Post.return flow)
              >>%
                fun flow ->
                (* FIXME: *)
                debug "add_pymethoddef@.%a" (format @@ Flow.print man.lattice.print) flow;
                let flow = add_pymethoddef "tp_methods" cls_addr Method_descriptor ecls man flow in
                let flow = add_pymemberdef cls_addr ecls man flow in
                let flow = Flow.add_local_assumption (A_cpython_unsupported_fields "tp_numbers, ...") range flow in
                Cases.singleton cls_addr flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      (* FIXME: PyModule_Create is a macro expanded into PyModule_Create2. Maybe we should have a custom .h file *)
      (* FIXME: proper handling *)
      | E_var ({vkind = V_cvar v}, _) when v.cvar_uniq_name = "PyExc_MemoryError" ->
         Eval.singleton (mk_addr (fst @@ Python.Addr.find_builtin "MemoryError") range) flow
         |> OptionExt.return
      | E_var ({vkind = V_cvar v}, _) when v.cvar_uniq_name = "PyExc_TypeError" ->
         Eval.singleton (mk_addr (fst @@ Python.Addr.find_builtin "TypeError") range) flow
         |> OptionExt.return
      | E_var ({vkind = V_cvar v}, _) when v.cvar_uniq_name = "PyExc_AttributeError" ->
         Eval.singleton (mk_addr (fst @@ Python.Addr.find_builtin "AttributeError") range) flow
         |> OptionExt.return
      | E_var ({vkind = V_cvar v}, _) when v.cvar_uniq_name = "PyExc_OverflowError" ->
         Eval.singleton (mk_addr (fst @@ Python.Addr.find_builtin "OverflowError") range) flow
         |> OptionExt.return

      | E_c_builtin_call ("PyModule_Create2", [module_decl; _]) ->
         let addr_type = fst @@ Python.Addr.find_builtin "type" in
         let cur = get_env T_cur man flow in
         let flow =
           if EquivBaseAddrs.KeySet.is_empty @@ Top.detop @@ EquivBaseAddrs.find_inverse addr_type cur then
             (* since we can't do this in init yet (the C program context is initially empty, we don't know what we'll parse and when*)
             let add_class_equiv c_var_name py_bltin_name flow =
               let py_addr = fst @@ Python.Addr.find_builtin py_bltin_name in
               let c_var = search_c_globals_for flow c_var_name in
               debug "cls_equiv c_var %a %a" pp_var c_var pp_typ c_var.vtyp;
               set_singleton
                 (mk_c_points_to_bloc (C.Common.Base.mk_var_base c_var) (mk_zero (Location.mk_program_range [])) None)
                 py_addr
                 man flow
             in
             let add_class_equivs descr flow  =
               List.fold_left (fun flow (c, py) ->
                   add_class_equiv c py flow) flow descr in
             let flow =
               let none_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_none in
               let true_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_true in
               let false_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_false in
               let c_var = search_c_globals_for flow "_Py_NoneStruct" in
               let true_var = search_c_globals_for flow "_Py_TrueStruct" in
               let false_var = search_c_globals_for flow "_Py_FalseStruct" in
               let flow = set_singleton
                 (mk_c_points_to_bloc (C.Common.Base.mk_var_base c_var) (mk_zero (Location.mk_program_range [])) None)
                 none_addr
                 man flow in
               let flow = set_singleton
                 (mk_c_points_to_bloc (C.Common.Base.mk_var_base true_var) (mk_zero (Location.mk_program_range [])) None)
                 true_addr
                 man flow in
               set_singleton
                 (mk_c_points_to_bloc (C.Common.Base.mk_var_base false_var) (mk_zero (Location.mk_program_range [])) None)
                 false_addr
                 man flow
             in
             add_class_equivs
               [
                 ("PyType_Type", "type");
                 ("PyBaseObject_Type", "object");
                 ("PyLong_Type", "int");
                 ("PyUnicode_Type", "str");
                 ("PyList_Type", "list");
                 ("PyExc_MemoryError", "MemoryError");
                 ("PyExc_TypeError", "TypeError");
                 ("PyExc_OverflowError", "OverflowError");
                 ("PyExc_AttributeError", "AttributeError");
                 ("PyExc_SystemError", "SystemError");
                 (* FIXME: add all matches to PyAPI_DATA(PyObject * ) in cpython/Include? *)
               ]
               flow
           else
             flow
         in
         new_module_from_def module_decl man flow
         |> OptionExt.return


      | E_c_builtin_call ("PyModule_AddObject", [module_object; obj_name; obj]) ->
         resolve_c_pointer_into_addr module_object man flow >>$
           (
             fun module_addr flow ->

             get_name_of obj_name man flow >>$
               fun obj_name flow ->

               resolve_c_pointer_into_addr obj man flow >>$
                 fun obj_addr flow ->
                 (* bind class to module *)
                 bind_in_python module_addr obj_name obj_addr range man flow
                 >>= fun flow ->
                 Eval.singleton (mk_zero range)
           )
         |> OptionExt.return


      | E_c_builtin_call ("PyType_Ready", [cls]) ->
         (* add base (FIXME: handle inheritance)
            ~> delegated to the C-level field accesses *)
         (* Py_TYPE(cls) = &PyType_Type *)
         (* FIXME: sometimes I'd like to write some parts of the transfer functions in the analyzed language... *)
         let cheat = C.Ast.find_c_fundec_by_name "PyType_ReadyCheat" flow in
         man.eval (mk_c_call cheat [cls] range) flow >>$
           (fun r flow ->
             debug "after PyType_ReadyCheat:@.%a" (format @@ Flow.print man.lattice.print) flow;
             new_class_from_def cls man flow >>$
               fun cls_addr flow ->
               Eval.singleton (mk_int 0 range) flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyType_GenericAlloc_Helper", args) ->
         let cls = List.hd args in
         resolve_c_pointer_into_addr cls man flow >>$
           (fun cls_addr flow ->
           (* FIXME: should we use the range from where the allocation is performed to help the recency? Or at least use the callstack to disambiguate for those specific instances... *)
           man.eval (mk_alloc_addr (Python.Addr.A_py_instance cls_addr) range) flow >>$
             fun inst_eaddr flow ->
             let inst_addr = Addr.from_expr inst_eaddr in
             let bytes = C.Cstubs.Aux_vars.mk_bytes_var inst_addr in
             debug "%a allocated! Forcing %a %a" pp_expr inst_eaddr pp_expr (mk_var bytes range) pp_expr (List.hd @@ List.tl args);
             (* No need to put this into the equiv, since it's the same thing? *)
             (* let flow = set_singleton (mk_c_points_to_bloc (C.Common.Base.mk_addr_base inst_addr) (mk_zero range) None) inst_addr man flow in *)
             man.exec (mk_add_var bytes range) flow >>%
               man.exec (mk_assign (mk_var bytes range) (List.hd @@ List.tl args) range) >>%
               man.exec (mk_add inst_eaddr range) >>%
               Eval.singleton inst_eaddr
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyLong_FromSsize_t", args)
      | E_c_builtin_call ("PyLong_FromLong", args) ->
         man.eval ~translate:"Universal" (List.hd args) flow >>$
           (fun earg flow ->
             (* FIXME: Python~>C boundary *)
             (* FIXME: forced to attach the value as an addr_attr in universal, and convert it afterwards when going back to python... *)
             (* FIXME: handle addr renaming and propagate it to the value attribute *)
             debug "allocating int at range %a callstack %a" pp_range range Callstack.pp_callstack (Flow.get_callstack flow);
             man.eval (mk_alloc_addr (*~mode:WEAK*) (A_py_instance (fst @@ find_builtin "int")) range) flow >>$
               fun int_addr flow ->
               debug "got int_addr %a" pp_addr (Addr.from_expr int_addr);
               man.exec (mk_assign (mk_var (mk_addr_attr (Addr.from_expr int_addr) "value" T_int) range) earg range) flow >>%
                 (* FIXME: addr vs py_object, we'll need to clean things somehow... *)
                 Eval.singleton (mk_addr (Addr.from_expr int_addr) range)
           )
         |> OptionExt.return


      | E_c_builtin_call ("PyLong_AsLong", args) ->
         (* FIXME: upon translation from Python to C, integer arguments should get a value attribute. Issue if multiple integer arguments... tag it with the precise range otherwise?  Also, need to clean the "value" attribute afterwards *)
         (* FIXME: if PyLong_AsLong is not called on an integer, this should fail *)
         resolve_c_pointer_into_addr (List.hd args) man flow >>$
           (fun addr flow ->
             man.eval (mk_var (mk_addr_attr addr "value" T_int) range) flow >>$
               fun int_value flow ->
               let long_max = mk_z (Z.of_string "9223372036854775807") ~typ:T_int range in
               assume
                 (mk_binop ~etyp:T_bool
                    (mk_binop ~etyp:T_bool int_value O_le long_max range)
                    O_log_and
                    (mk_binop ~etyp:T_bool int_value O_ge (mk_unop ~etyp:T_int O_minus long_max range) range)
                    range)
                 man flow
                 ~fthen:(Eval.singleton int_value)
                 ~felse:(fun flow ->
                   (* Overflow: need to set the error and then return -1 *)
                   let helper = C.Ast.find_c_fundec_by_name "PyLong_AsLong_Helper" flow in
                   man.eval (mk_c_call helper [] range) flow >>$
                     fun _ flow ->
                     Eval.singleton (mk_int ~typ:T_int (-1) range) flow
                 )
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyArg_ParseTuple", args::fmt::refs) ->
         get_name_of fmt man flow >>$
           (fun fmt_str flow ->
             resolve_c_pointer_into_addr args man flow >>$
               (fun addr flow ->
                 man.eval (Python.Ast.mk_py_call
                             (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "len") range)
                             [Python.Ast.mk_py_object (addr, None) range] range) flow >>$
                   (
                     (* Currently does not handle variable size arguments etc *)
                     (* Also, need to check if format is correct *)
                     (*
                        check that size = len(fmt_str), otherwise raise TypeError
                        each conversion may fail: in that case, return 0 FIXME: also, clean
                        if everything succeeds, return 1
                      *)

                     fun earg flow ->
                     let size = match ekind @@ OptionExt.none_to_exn @@ snd @@ object_of_expr earg with
                       | E_constant (C_int z) -> Z.to_int z
                       | _ -> assert false in

                     if String.length fmt_str <> size then
                       let () = debug "wrong number of arguments" in
                       man.eval (mk_c_call (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow)
                                   [mk_var (search_c_globals_for flow "PyExc_TypeError") range;
                                    mk_c_string
                                      (Format.asprintf "function takes exactly %d arguments (%d given)" (String.length fmt_str) size)
                                      range] range) flow >>$
                         fun _ flow ->
                         Eval.singleton (mk_zero  range) flow
                     else

                       let convert_single pos output_ref flow =
                         match fmt_str.[pos] with
                         | 'O' ->
                            man.eval (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (addr, None) range) (mk_int pos ~typ:(Python.Ast.T_py None) range) range) flow >>$
                              (fun obj flow ->
                                match ekind obj, ekind output_ref  with
                                | Python.Ast.E_py_object(addr, _), E_c_address_of c ->
                                   (* FIXME: cleaner Python~>C boundary *)
                                   debug "ParseTuple O ~> %a" pp_addr addr;
                                   let assign_helper = C.Ast.find_c_fundec_by_name "_PyType_Assign_Helper" flow in
                                   let pyobject_typ = under_pointer_type (vtyp @@ List.hd assign_helper.c_func_parameters) in
                                   let type_addr = match akind addr with
                                     | A_py_instance a -> a
                                     | A_py_class _ -> fst @@ find_builtin "type"
                                     | Python.Objects.Py_list.A_py_list -> fst @@ find_builtin "list"
                                     | _ -> assert false in
                                   let flow =
                                     (* FIXME: if addr is weak, the analysis will be stuck to T *)
                                     (* FIXME: if not found, need to add the new class. But that should be one easy BaseAddr/Addr case, with user-defined python class I guess *)
                                     begin match KeySet.choose @@ Top.detop @@ EquivBaseAddrs.find_inverse type_addr (get_env T_cur man flow) with
                                        | P_block ({base_kind = Var v}, _, _) ->
                                           let bytes = C.Cstubs.Aux_vars.mk_bytes_var addr in
                                           let obj = mk_addr ~mode:(Some STRONG) ~etyp:(vtyp @@ List.hd assign_helper.c_func_parameters) addr range in
                                           post_to_flow man
                                             (
                                               man.exec (mk_add (mk_addr ~etyp:(vtyp @@ List.hd assign_helper.c_func_parameters) addr range) range) flow >>%
                                                 man.exec (mk_add_var bytes range) >>%
                                                 man.exec (mk_assign (mk_var ~mode:(Some STRONG) bytes range) (mk_z ~typ:(T_c_integer C_unsigned_long) (sizeof_type pyobject_typ) range) range) >>%
                                                 man.exec (mk_assign
                                                             (mk_c_deref (mk_c_cast
                                                                            (add (mk_c_cast obj (T_c_pointer s8) range) (mk_int 8 range) range)
                                                                            (T_c_pointer (vtyp @@ List.nth assign_helper.c_func_parameters 1)) range) range)

                                                             (mk_c_address_of (mk_var {v with vtyp = under_type @@ vtyp @@ List.nth assign_helper.c_func_parameters 1} range) range)
                                                             range) >>%
                                                 fun flow ->
                                                 debug "result is:@.%a" (format @@ Flow.print man.lattice.print) flow;
                                                 Post.return flow
                                                 (* man.exec (mk_expr_stmt
                                                  *             (mk_c_call assign_helper
                                                  *                [mk_addr ~etyp:(vtyp @@ List.hd assign_helper.c_func_parameters) addr range;
                                                  *                 mk_c_address_of (mk_var {v with vtyp = under_type @@ vtyp @@ List.nth assign_helper.c_func_parameters 1} range) range] range) range) *)
                                             )
                                        | _ -> assert false
                                        end
                                   in
                                   man.exec (mk_assign c (mk_addr addr range) range) flow >>%
                                     fun flow -> Cases.return 1 flow
                                | _ -> assert false
                              )
                         | 'i' ->
                            man.eval (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (addr, None) range) (mk_int pos ~typ:(Python.Ast.T_py None) range) range) flow >>$
                              (* FIXME: this sould be a boundary between python and C.
                                 As such, it should handle integer translation.
                                 Python function call PyLong_AsLong *)
                              (fun obj flow ->
                                match ekind obj, ekind (List.hd refs) with
                                | Python.Ast.E_py_object(addr, oe), E_c_address_of c ->
                                   (* FIXME: check it's an integer *)
                                   if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_integers) = 0 then
                                     man.exec
                                       (mk_assign
                                          (mk_var (mk_addr_attr addr "value" T_int) range)
                                          (OptionExt.none_to_exn oe)
                                          range)
                                       flow >>%
                                       fun flow ->
                                       debug "value should be stored %a@.%a" pp_var (mk_addr_attr addr "value" T_int) (format @@ Flow.print man.lattice.print) flow;
                                       man.exec (mk_assign
                                                   c
                                                   (mk_c_call (C.Ast.find_c_fundec_by_name "PyLong_AsLong" flow) [mk_addr addr range] range)
                                                   range) flow >>%
                                         fun flow -> Cases.return 1 flow
                                   else
                                     let () = debug "wrong type for convert_single integer" in
                                     (* set error *)
                                     man.eval (mk_c_call (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow) [mk_var (search_c_globals_for flow "PyExc_TypeError") range; mk_c_string "an integer is required (got type ???)" range] range) flow >>$
                                       fun _ flow ->
                                       Cases.return 0 flow
                                | _ -> assert false
                              )

                         | _ ->
                            if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
                              let () = warn_at range "PyArg_ParseTuple %s unsupported, but cur is bottom" fmt_str in
                              Cases.return 1 flow
                            else
                              panic_at range "TODO: implement PyArg_ParseTuple %s@.%a" fmt_str (format @@ Flow.print man.lattice.print) flow
                       in

                       let rec process pos refs flow =
                         convert_single pos (List.hd refs) flow >>$
                           fun ret flow ->
                           if ret = 1 then
                             if pos+1 < size then
                               process (pos+1) (List.tl refs) flow
                             else
                               Eval.singleton (mk_one range) flow
                           else
                             (* error *)
                             Eval.singleton (mk_zero range) flow
                       in
                       process 0 refs flow
                   )
               )
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyTuple_Size", [arg]) ->
         resolve_c_pointer_into_addr arg man flow >>$
           (fun addr flow ->
                man.eval (Python.Ast.mk_py_call
                            (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "len") range)
                            [Python.Ast.mk_py_object (addr, None) range] range) flow >>$
                  (
                    fun earg flow ->
                    (* FIXME: handle integer boundary properly.
                       I guess we lose relationality here *)
                    let size = OptionExt.none_to_exn @@ snd @@ object_of_expr earg in
                    Eval.singleton size flow
                    (* panic_at range "tuple size %a %a" pp_expr arg pp_expr earg *)
                  )
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyTuple_GetItem", [tuple; pos]) ->
         debug "PyTuple_GetItem";
         resolve_c_pointer_into_addr tuple man flow >>$
           (fun addr flow ->
             let py_tuple = Python.Ast.mk_py_object (addr, None) range in
             man.eval (mk_c_call (C.Ast.find_c_fundec_by_name "PyLong_FromLong" flow) [pos] range) flow >>$
               fun py_pos flow ->
               debug "PyTuple_GetItem, py_pos = %a" pp_expr py_pos;
               man.eval (Python.Ast.mk_py_call (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "tuple.__getitem__") range)
                           [py_tuple; mk_py_object (Addr.from_expr py_pos, Some (mk_var (mk_addr_attr (Addr.from_expr py_pos) "value" T_int) range)) range] range) flow >>$
                 fun py_elem flow ->
                 (* FIXME: Python~>C boundary *)
                 debug "PyTuple_GetItem: %a" pp_expr py_elem;
                 Eval.singleton (mk_addr (fst @@ object_of_expr py_elem) range) flow
           )
         |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_c_function(name, uid, kind, self)}, _)}, args, kwargs) ->
         (* FIXME: use the equiv map *)
         let cfunc = find_c_fundec_by_uid uid flow in
         let args_types =
           if List.length cfunc.c_func_parameters > 2 then
             warn "ignoring keywords";
           List.map vtyp cfunc.c_func_parameters in
         (* Call it with self, E_py_tuple(args) *)
         let self =
           (* woops, don't forget to convert back to C like values
              FIXME: that also applies to py_args? :/ *)
           match (fst self).addr_kind with
           | A_py_c_class v -> C.Ast.mk_c_address_of (mk_var v range) range
           | _ -> Universal.Ast.mk_addr (fst self) ~etyp:(List.hd args_types) range in
         let self, args =
           match kind with
           | Builtin_function_or_method
             -> self, args
           | Wrapper_descriptor _ | Method_descriptor ->
              Universal.Ast.mk_addr (fst @@ object_of_expr @@ List.hd args) range, List.tl args
         in
         let py_args = mk_expr ~etyp:(T_py None) (E_py_tuple args) (tag_range range "args assignment" ) in
         debug "%s, self is %a, args: %a@.%a" name pp_expr self pp_expr py_args (format @@ Flow.print man.lattice.print) flow;
         let py_kwds =
           (* FIXME: okay, lets cheat here *)
           mk_c_null range
         in
         (* FIXME: if |args| = 1, no need to eval py args *)
         man.eval py_args flow >>$
           (fun py_args flow ->
             let addr_py_args = addr_of_object @@ object_of_expr py_args in
             let cfunc_args =
               match List.length cfunc.c_func_parameters with
               | 1 -> [self]
               | 2 -> self ::
                        (Universal.Ast.mk_addr addr_py_args ~etyp:(List.nth args_types 1) range) :: []
               | 3 -> self ::
                        (Universal.Ast.mk_addr addr_py_args ~etyp:(List.nth args_types 1) range)
                        :: py_kwds :: []
               | _ -> assert false in
             let call = match kind with
               | Wrapper_descriptor (Some wrapper_name) ->
                  let wrapper = C.Ast.find_c_fundec_by_name wrapper_name flow in
                  let wrapper_args =
                    match List.length wrapper.c_func_parameters with
                    | 2 ->
                       let self = List.hd cfunc_args in
                       self ::
                         (mk_c_address_of (mk_expr (E_c_function cfunc) range ~etyp:(mk_c_fun_typ cfunc)) range) :: []
                    | 3 | 4 ->
                       let self, args, kwds_or_nothing = match cfunc_args with
                         | [a] -> a, mk_c_null range, []
                         | [a;b] -> a,b,[]
                         | a::b::c -> a, b, c
                         | _ -> assert false in
                       self :: args ::
                         (mk_c_address_of (mk_expr (E_c_function cfunc) range ~etyp:(mk_c_fun_typ cfunc)) range) ::
                             kwds_or_nothing
                    | _ -> assert false
                  in
                  mk_c_call wrapper wrapper_args range
               | _ ->
                  mk_c_call cfunc cfunc_args range
             in
             (* FIXME: Python~> C boundary on self+args *)
             let open C.Common.Points_to in
             man.eval call flow >>$
               fun call_res flow ->
                  debug "call result %s %a@.%a" name pp_expr call_res (format @@ Flow.print man.lattice.print) flow;
                  try
                    resolve_c_pointer_into_addr call_res man flow >>$
                      fun addr flow ->
                      Eval.singleton (mk_py_object (addr, None) range) flow
                  with Null_found ->
                    check_consistent_null cfunc.c_func_org_name man flow range
           )
         |> OptionExt.return

      (** member descriptors: attr get/set on descriptors defined in C classes *)
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("member_descriptor.__get__", "wrapper_descriptor"))}, _)},
                   member_descr_instance ::
                   inst ::
                   ({ekind = E_py_object ({addr_kind = A_py_c_class c}, _)} as cls_inst) :: [],
                   kwargs) ->
         (* FIXME: reuse the Py/C call machinery+checks above *)
         debug "member_get@.%a" (format @@ Flow.print man.lattice.print) flow;
         let cfunc = find_c_fundec_by_name "member_get" flow in
         let addr_of_object ?(etyp=T_addr) x = mk_addr ~etyp:etyp (addr_of_object @@ object_of_expr x) range in
         let descr_typ, inst_typ, cls_typ = match cfunc.c_func_parameters with
           | [a;b;c] -> vtyp a, vtyp b, vtyp c
           | _ -> assert false in
         let c_cls_inst =
           (* FIXME: replace choose *)
           match KeySet.choose @@ Top.detop @@
                   EquivBaseAddrs.find_inverse
                     (fst @@ object_of_expr cls_inst)
                     (get_env T_cur man flow) with
           | P_block ({base_kind = Var v}, _, _) ->
              mk_c_address_of (mk_var v range) range
           | P_block ({base_kind = Addr a}, _, _) ->
              mk_addr ~etyp:cls_typ a range
           | _ -> assert false
         in
         let addr_inst = addr_of_object ~etyp:inst_typ inst in
         let c_descriptor =
           match KeySet.choose @@ Top.detop @@
                   EquivBaseAddrs.find_inverse
                     (fst @@ object_of_expr member_descr_instance)
                     (get_env T_cur man flow) with
           | P_block ({base_kind = Var v} as base, offset, _) ->
              mk_base_offset_pointer base offset (under_type descr_typ) range
           | P_block ({base_kind = Addr a}, _, _) ->
              mk_addr ~etyp:cls_typ a range
           | _ -> assert false
         in
         let cfunc_args = [c_descriptor; addr_inst; c_cls_inst] in
         let call = mk_c_call cfunc cfunc_args range in
         debug "calling %a" pp_expr call;
         (
           try
             resolve_c_pointer_into_addr call man flow >>$
               (fun addr flow ->
                 match akind addr with
                 | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
                    man.eval (mk_var (mk_addr_attr addr "value" T_int) range) flow >>$
                      fun int_value flow ->
                      Eval.singleton (mk_py_object (addr, Some int_value) range) flow |>
                        Cases.add_cleaners [mk_remove_var  (mk_addr_attr addr "value" T_int) range]
                 | _ -> Eval.singleton (mk_py_object (addr, None) range) flow
               (* panic_at range "md.__get__ on C class results in %a" pp_addr addr *)
               )
           with Null_found ->
             check_consistent_null cfunc.c_func_org_name man flow range
         )
         |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("member_descriptor.__set__", "wrapper_descriptor"))}, _)},
                   member_descr_instance ::
                   ({ekind = E_py_object ({addr_kind = A_py_instance {addr_kind = A_py_c_class c}}, _)} as inst) ::
                   value :: [],
                   kwargs) ->
         let cfunc = find_c_fundec_by_name "PyMember_SetOne" flow in
         let descr_typ, inst_typ, value_typ = match cfunc.c_func_parameters with
           | [a;b;c] -> vtyp a, vtyp b, vtyp c
           | _ -> assert false in
         let addr_of_eobject ?(etyp=T_addr) x = mk_addr ~etyp:etyp (addr_of_object @@ object_of_expr x) range in
         let addr_inst = addr_of_eobject ~etyp:inst_typ inst in
         let addr_value = addr_of_eobject ~etyp:value_typ value in
         let c_descriptor =
           match KeySet.choose @@ Top.detop @@
                   EquivBaseAddrs.find_inverse
                     (fst @@ object_of_expr member_descr_instance)
                     (get_env T_cur man flow) with
           | P_block ({base_kind = Var v} as base, offset, _) ->
              mk_base_offset_pointer base offset (under_type descr_typ) range
           | _ -> assert false
         in
         let cfunc_args = [addr_inst; c_descriptor; addr_value] in
         let call = mk_c_call cfunc cfunc_args range in
         let call flow =
           assume (mk_binop ~etyp:T_c_bool call O_ge (mk_zero range) range) man flow
           ~fthen:(fun flow ->
             man.eval (mk_py_none range) flow
           )
           ~felse:(fun flow ->
             debug "not zero in:@.%a" (format @@ Flow.print man.lattice.print) flow;
             check_consistent_null cfunc.c_func_org_name man flow range
           ) in

         (match ekind addr_value with
          | E_addr ({addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}}, _) ->
             (* Python~>C boundary *)
             (* FIXME: We're moving from Python to C, so we need to attach
                integer values to the addresses (not precise in the python
               p        art but necessary here. *)
             man.exec (mk_assign (mk_var (mk_addr_attr (Addr.from_expr addr_value) "value" T_int) range) (OptionExt.none_to_exn @@ snd @@ object_of_expr value) range) flow >>%
               call
          | _ ->
             call flow)
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()
    let print_state printer a =
      pprint ~path:[Key "C/Python equivalence"] printer (pbox EquivBaseAddrs.print a)

    let merge _ _ _ = assert false
  end


let () = register_standard_domain(module Domain)
