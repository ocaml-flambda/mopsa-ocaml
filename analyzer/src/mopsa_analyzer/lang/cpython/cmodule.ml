(* FIXME: alloc_py_addr of some addresses should be done only if the thing is not already converted. See fix for A_py_c_function in new_method_from_def already written *)
(*
  FIXME:
  - wrap try/with check_consistent_null + handling of addresses in C->Python boundary
    + if a builtin int/str abstract value's can be changed, it needs to be update
  - handle rename_addr, fold_addr for recency allocation (in future, also handle what is used by the AGC?)
  - triggering S_add(addr) should be sent to the correct domains...
  - support PyBuild_Value
  - assert offset = [0, 0] for Points_to

 *)
(* Truc commun au multilangage: addr du tas/variable/fonction
   Côté C: manipuler des addr plutôt que des bases (il y aura des addr de fonctions/variables/...)
           Var -> Valeur ~> Addr -> Valeur
 *)
(*
  TODO:
   - parameter conversion Python~>C isn't done in all cases
   - support more things in PyParse_Tuple
   - prendre les range d'allocation du python plutôt... (ou avoir range+cs pour les kinds d'addr concernés)
*)
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

type check += CHK_CPYTHON_CLASS_READY
type alarm_kind += A_cpython_class_not_ready of expr

let () =
  register_check (fun next fmt -> function
      | CHK_CPYTHON_CLASS_READY -> Format.fprintf fmt "CPython class not readied"
      | a -> next fmt a)

let () =
  register_alarm {
      check = (fun next -> function
                | A_cpython_class_not_ready _ -> CHK_CPYTHON_CLASS_READY
                | a -> next a);
      compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_cpython_class_not_ready e1, A_cpython_class_not_ready e2 ->
           compare_expr e1 e2
        | _ -> next a1 a2
      );
      print = (fun next fmt -> function
                | A_cpython_class_not_ready e ->
                   Format.fprintf fmt "PyTypeObject %a has not been readied using PyType_Ready"
                     pp_expr e
                | m -> next fmt m);
      join = (fun next -> next);
    }

let raise_cpython_class_not_ready ?(bottom=true) e range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_cpython_class_not_ready e) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let builtin_exceptions =
  [
    "PyExc_BaseException";
    "PyExc_Exception";
    "PyExc_StopAsyncIteration";
    "PyExc_StopIteration";
    "PyExc_GeneratorExit";
    "PyExc_ArithmeticError";
    "PyExc_LookupError";
    "PyExc_AssertionError";
    "PyExc_AttributeError";
    "PyExc_BufferError";
    "PyExc_EOFError";
    "PyExc_FloatingPointError";
    "PyExc_OSError";
    "PyExc_ImportError";
    "PyExc_ModuleNotFoundError";
    "PyExc_IndexError";
    "PyExc_KeyError";
    "PyExc_KeyboardInterrupt";
    "PyExc_MemoryError";
    "PyExc_NameError";
    "PyExc_OverflowError";
    "PyExc_RuntimeError";
    "PyExc_RecursionError";
    "PyExc_NotImplementedError";
    "PyExc_SyntaxError";
    "PyExc_IndentationError";
    "PyExc_TabError";
    "PyExc_ReferenceError";
    "PyExc_SystemError";
    "PyExc_SystemExit";
    "PyExc_TypeError";
    "PyExc_UnboundLocalError";
    "PyExc_UnicodeError";
    "PyExc_UnicodeEncodeError";
    "PyExc_UnicodeDecodeError";
    "PyExc_UnicodeTranslateError";
    "PyExc_ValueError";
    "PyExc_ZeroDivisionError";
    "PyExc_BlockingIOError";
    "PyExc_BrokenPipeError";
    "PyExc_ChildProcessError";
    "PyExc_ConnectionError";
    "PyExc_ConnectionAbortedError";
    "PyExc_ConnectionRefusedError";
    "PyExc_ConnectionResetError";
    "PyExc_FileExistsError";
    "PyExc_FileNotFoundError";
    "PyExc_InterruptedError";
    "PyExc_IsADirectoryError";
    "PyExc_NotADirectoryError";
    "PyExc_PermissionError";
    "PyExc_ProcessLookupError";
    "PyExc_TimeoutError";
    "PyExc_EnvironmentError";
    "PyExc_IOError";
    "PyExc_Warning";
    "PyExc_UserWarning";
    "PyExc_DeprecationWarning";
    "PyExc_PendingDeprecationWarning";
    "PyExc_SyntaxWarning";
    "PyExc_RuntimeWarning";
    "PyExc_FutureWarning";
    "PyExc_ImportWarning";
    "PyExc_UnicodeWarning";
    "PyExc_BytesWarning";
    "PyExc_ResourceWarning";
  ]

let () =
  C.Cstubs.Resources.register_is_resource_addr_chain (fun next ak ->
      match ak with
      (* FIXME: other container addresses *)
      | Python.Objects.Py_list.A_py_list
      | Python.Objects.Tuple.A_py_tuple _
      | Python.Objects.Dict.A_py_dict
      | A_py_instance _
      | A_py_class _
      | A_py_c_class _
      | A_py_c_function _
      | A_py_c_module _
      | A_py_function _
      | A_py_method _
      | A_py_module _ -> true
      | _ -> next ak);



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

    let widen ctx = join
    include Framework.Core.Id.GenDomainId(
                struct
                  type nonrec t = t
                  let name = "cpython.cmodule"
                end)

    let checks = []

    let set_singleton p a man flow =
      let r = get_env T_cur man flow |>
                set p (Nt (ValueSet.singleton a)) in
      set_env T_cur r man flow

    let yield_results_before_crash man flow =
      Framework.Output.Text.report man flow ~time:(0.) ~files:[] ~out:None

    let strongify_int_addr_hack addr man range flow =
      (* since integer addresses are always weak in python, this
         creates huge precision issues for the value attribute used to
         pass the abstract value of an integer object *)
      (* to fix this, we may change a weak int addr into a strong one *)
      (* FIXME: this should be done with a DIFFERENT range for each addr *)
      (* FIXME: if this address goes back to Python, we need to fix it in the boundary too *)
      (* ASSUMPTION: the concrete C can't change the value of the object.
         This should be fine on integers *)
      if List.exists (fun a -> compare_addr addr (OptionExt.none_to_exn !a) = 0) [Python.Types.Addr_env.addr_integers; Python.Types.Addr_env.addr_float;
                                                                                  Python.Types.Addr_env.addr_bytes; Python.Types.Addr_env.addr_strings
           ] then
        man.eval (mk_alloc_addr (akind addr) range) flow >>$ fun eaddr flow ->
        let strong = Addr.from_expr eaddr in
        let () = warn "changing %a into strong integer addr %a to improve C precision" pp_addr addr pp_addr strong in
        man.exec (mk_add eaddr range) flow >>% fun flow ->
        Cases.singleton strong flow
      else
        Cases.singleton addr flow

    let init _ man flow =
      List.iter (fun a -> Hashtbl.add C.Common.Builtins.builtin_functions a ())
        [
          "PyModule_Create2";
          "PyModule_AddObject";
          "PyType_Ready";
          "PyType_GenericAlloc_Helper";
          "PyType_IsSubtype";
          "PyArg_ParseTuple";
          "PyArg_ParseTupleAndKeywords";
          "PyArg_UnpackTuple";
          "Py_BuildValue";
          "PyObject_CallFunction";
          "PyObject_CallObject";
          "PyObject_RichCompare";
          "PyObject_RichCompareBool";
          "PyObject_IsTrue";
          "PyLong_FromLong";
          "PyLong_FromUnsignedLong";
          "PyLong_FromSsize_t";
          "PyLong_AsLong";
          "PyLong_AsSsize_t";
          "PyFloat_FromDouble";
          "PyFloat_AsDouble";
          "PyBytes_FromStringAndSize";
          "PyBytes_Size";
          "PyBytes_AsString";
          "PyUnicode_GetLength";
          "PyUnicode_InternFromString";
          "PyUnicode_FromString";
          "PyUnicode_FromKindAndData";
          "PyUnicode_FromWideChar";
          "PyUnicode_AsEncodedString";
          "PyUnicode_AsUnicode";
          "PyUnicode_AsUTF8AndSize";
          "PyTuple_New";
          "PyTuple_SetItem";
          "PyTuple_Size";
          "PyTuple_GetItem";
          "PyTuple_GetSlice";
          "PyList_New";
          "PyList_Size";
          "PyList_GetItem";
          "PyList_SetItem";
          "_PyRange_Size";
          "_PyRange_GetItem";
          "PyWeakref_NewRef";
          "PyWeakref_GetObject";
        ];

      set_env T_cur EquivBaseAddrs.empty man flow

    let mk_avalue_from_pyaddr addr typ range =
      mk_var (mk_addr_attr addr "value" typ) range

    let safe_get_name_of expr man flow =
      let r = resolve_pointer expr man flow >>$
                (fun points_to flow ->
                  match points_to with
                  | P_block({base_kind = String (s, _, _)}, offset, _) ->
                     Cases.singleton (Some (Top.Nt s)) flow
                  | P_top ->
                     Cases.singleton (Some Top.TOP) flow
                  | P_null ->
                     Cases.singleton None flow
                  | _ -> panic "safe_get_name_of points_to %a" pp_points_to points_to
                ) in
      (* assert(Cases.cardinal r <= 1); *)
      r


    (* Creates stmt binder_addr · obj_name = obj_addr *)
    let bind_in_python binder_addr obj_name obj_addr range man flow =
      (* FIXME: bind_in_python should call the boundaries maybe? *)
      (if compare_addr_kind (akind obj_addr) (akind @@ OptionExt.none_to_exn @@ !Python.Types.Addr_env.addr_integers) = 0 then
        man.eval (mk_avalue_from_pyaddr obj_addr T_int range) flow >>$
          fun int_value flow ->
          Cases.return (Some int_value) flow
       else if compare_addr_kind (akind obj_addr) (akind @@ OptionExt.none_to_exn @@ !Python.Types.Addr_env.addr_float) = 0 then
        man.eval (mk_avalue_from_pyaddr obj_addr (T_float F_DOUBLE) range) flow >>$
          fun float_value flow ->
          Cases.return (Some float_value) flow
       else if compare_addr_kind (akind obj_addr) (akind @@ OptionExt.none_to_exn @@ !Python.Types.Addr_env.addr_strings) = 0 then
        man.eval (mk_avalue_from_pyaddr obj_addr T_string range) flow >>$
          fun str_value flow ->
          Cases.return (Some str_value) flow
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

    let alloc_py_addr man addr ?(mode=STRONG) range flow =
      man.eval (mk_alloc_addr ~mode:mode addr range) flow >>$
        fun py_eaddr flow ->
        man.exec ~route:(Semantic "Python") (mk_add py_eaddr range) flow >>%
          Eval.singleton py_eaddr

    let new_method_from_def binder_addr methd_kind methd man flow =
      let range = erange methd in
      safe_get_name_of (mk_c_member_access_by_name methd "ml_name" range) man flow >>$
        fun omethd_name flow ->
        match omethd_name with
        | None -> Cases.singleton false flow
        | Some methd_name ->
           let methd_name = Top.top_to_exn methd_name in
          resolve_pointer (mk_c_member_access_by_name methd "ml_meth" range) man flow >>$
          fun methd_function flow ->
          let methd_fundec = match methd_function with
            | P_fun f -> f
            | _ -> assert false in
          let a_methd_function = Top.detop @@ EquivBaseAddrs.find methd_function (get_env T_cur man flow) in
          (if ValueSet.cardinal a_methd_function > 0 then
            Eval.singleton (mk_addr (ValueSet.choose a_methd_function) range) flow
           else
            man.eval ~translate:"Universal" (mk_c_member_access_by_name methd "ml_flags" range) flow >>$ fun methd_flags flow ->
           let oflags =
             match Bot.bot_to_exn @@ man.ask (Universal.Numeric.Common.mk_int_interval_query methd_flags) flow with
             | Finite l, Finite r when Z.compare l r = 0 -> Some (Z.to_int l)
             | _ -> assert false in
            alloc_py_addr man (Python.Addr.A_py_c_function (methd_fundec.c_func_org_name, methd_fundec.c_func_uid, methd_kind, oflags, (binder_addr, None))) range flow) >>$
              fun methd_eaddr flow ->
              let methd_addr = Addr.from_expr methd_eaddr in
              let flow = set_singleton methd_function methd_addr man flow in
              (* bind method to binder *)
              bind_in_python binder_addr methd_name methd_addr range man flow >>%
                Cases.singleton true

    let rec fold_until_null func c flow =
      func c flow >>$
        (fun continue flow ->
          if continue then fold_until_null func (c+1) flow
          else Post.return flow
        )

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
      in
      assume
        (ne
           (mk_c_arrow_access_by_name expr field_name range)
           (mk_c_null range)
           ~etyp:T_bool range) man flow
        ~fthen:(fun flow ->
          fold_until_null add_method 0 flow)
        ~felse:(fun flow ->
          debug "add_pymethoddef %s %a: NULL found, nothing to do" field_name pp_expr expr;
          Post.return flow)


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
            safe_get_name_of (mk_c_member_access_by_name member "name" range) man flow >>$
              fun omember_name flow ->
              match omember_name with
              | None -> Cases.singleton false flow
              | Some member_name ->
                 let member_name = Top.top_to_exn member_name in
                 debug "name is %s" member_name;
                resolve_pointer (mk_c_address_of member range) man flow >>$
                  fun member_points_to flow ->
                  debug "points_to %a" pp_points_to member_points_to;
                  alloc_py_addr man (Python.Addr.A_py_instance (fst @@ Python.Addr.find_builtin "member_descriptor")) range flow >>$
                    fun member_descr flow ->
                    let member_descr = Addr.from_expr member_descr in
                    let flow = set_singleton member_points_to  member_descr man flow in
                    man.exec (mk_assign (mk_py_attr (mk_py_object (member_descr, None) range) "__name__" range) {(mk_string member_name range) with etyp = T_py None} range) flow >>%
                      bind_in_python binder_addr member_name member_descr range man >>%
                      Cases.singleton true
          )
      in
      assume
        (ne
           (mk_c_arrow_access_by_name expr "tp_members" range)
           (mk_c_null range)
           ~etyp:T_bool range) man flow
        ~fthen:(fun flow ->
          fold_until_null add_member 0 flow)
        ~felse:(fun flow ->
          debug "add_pymemberdef %a tp_members: NULL found, nothing to do" pp_expr expr;
          Post.return flow)


    let new_module_from_def expr man flow =
      (*
         1) Allocate A_py_c_module of mod->m_name (@_m)
         2) Forall meth = methods in mod->m_methods:
            n = meth->ml_name
            a) allocate them? A_py_c_function(n) -> @_f
            b) execute @_m·n = @_f
       *)
      let range = erange expr in
      safe_get_name_of (mk_c_arrow_access_by_name expr "m_name" range) man flow >>$
        (fun omodule_name flow ->
        match omodule_name with
        | None -> assert false
        | Some module_name ->
           let module_name = Top.top_to_exn module_name in
           alloc_py_addr man (Python.Addr.A_py_c_module module_name) range flow >>$
             fun module_addr flow ->
             let m_addr = Addr.from_expr module_addr in
             let flow = set_singleton (mk_c_points_to_bloc (C.Common.Base.mk_addr_base m_addr) (mk_zero range) None) m_addr man flow in
             add_pymethoddef "m_methods" m_addr Builtin_function_or_method expr man flow
             >>% Eval.singleton module_addr
        )

    let resolve_c_pointer_into_addr expr man flow =
      (* FIXME: enable check and make proper casts to PyObject where needed? or allow pytypeobject* ? *)
      (* let () = match etyp expr with
       *   | T_c_pointer t ->
       *      begin match remove_typedef_qual t with
       *      | T_c_record c when c.c_record_kind = C_struct && c.c_record_org_name = "_object" -> ()
       *      | _ -> panic_at expr.erange "resolve_c_pointer_into_addr: got %a of typ %a, expected PyObject*" pp_expr expr pp_typ (etyp expr)
       *      end
       *   | _ -> panic_at expr.erange "resolve_c_pointer_into_addr: got %a of typ %a, expected PyObject*" pp_expr expr pp_typ (etyp expr) in *)
      (* None if Null is found, Some addr otherwise *)
      resolve_pointer expr man flow >>$
        (fun points_to flow ->
          debug "[resolve_c_pointer %a:%a] searching for %a" pp_expr expr pp_typ (etyp expr) pp_points_to points_to;
          if points_to = P_null then Cases.singleton None flow
          else if points_to = P_top then Cases.singleton (Some Top.TOP) flow
          else
          let aset = Top.detop @@ find points_to (get_env T_cur man flow) in
          debug "got %a" (ValueSet.fprint SetExt.printer_default pp_addr) aset;
          if ValueSet.cardinal aset = 1 then
            Cases.singleton (OptionExt.return (Top.Nt (ValueSet.choose aset))) flow
          else if ValueSet.cardinal aset = 0 then
            match points_to with
            | P_block ({base_kind = Var v}, _, _) ->
               begin match remove_typedef_qual @@ vtyp v with
               | T_c_record {c_record_kind = C_struct; c_record_org_name} when c_record_org_name = "_typeobject" ->
                  let range = erange expr in
                  raise_cpython_class_not_ready (mk_var v range) range man flow |>
                  Cases.empty
               | _ -> assert false
               end
            | _ -> panic_at expr.erange "resolve_c_pointer_into_addr %a" pp_points_to points_to;
          else
            assert false
        )

    let c_to_python_boundary ?(safe_check = None) ?(on_null = fun flow -> assert false) ?(on_top = fun flow -> assert false) expr man flow range =
      resolve_c_pointer_into_addr expr man flow >>$
        fun oaddr flow ->
        match oaddr with
        | Some (Nt addr) ->
           begin
             let flow = match safe_check with
               | None -> flow
               | Some c -> Flow.add_safe_check c range flow in
             match akind addr with
             | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
                man.eval (mk_avalue_from_pyaddr addr T_int range) flow >>$
                  fun int_value flow ->
                  Eval.singleton (mk_py_object (addr, Some int_value) range) flow
             | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                man.eval (mk_avalue_from_pyaddr addr (T_float F_DOUBLE) range) flow >>$
                  fun float_value flow ->
                  Eval.singleton (mk_py_object (addr, Some float_value) range) flow
             | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)}
             | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                man.eval (mk_avalue_from_pyaddr addr T_string range) flow >>$
                  fun str_value flow ->
                  Eval.singleton (mk_py_object (addr, Some str_value) range) flow
             | _ -> Eval.singleton (mk_py_object (addr, None) range) flow
           end
        | Some TOP -> on_top flow
        | None -> on_null flow

    (* bind function pointed to by expr, as cls.name in python side *)
    let bind_function_in name expr cls_addr function_kind man flow =
      let range = erange expr in
      resolve_pointer expr man flow >>$
        fun func flow ->
        Post.return
          (match func with
           | P_fun fundec ->
              alloc_py_addr man (Python.Addr.A_py_c_function (fundec.c_func_org_name, fundec.c_func_uid, function_kind, None, (cls_addr, None))) range flow >>$
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
      let exc  = search_c_globals_for flow "exc" in
      man.eval (mk_var exc range) flow >>$
        fun exc flow ->
        resolve_pointer exc man flow >>$
          fun exc_pt flow ->
          resolve_c_pointer_into_addr (mk_c_arrow_access_by_name exc "exc_state" range) man flow >>$
            fun oexc_addr flow ->
            match oexc_addr with
            | Some (Nt exc_addr) ->
               let flow = Flow.add_safe_check Python.Alarms.CHK_PY_SYSTEMERROR range flow in
               safe_get_name_of (mk_c_arrow_access_by_name exc "exc_msg" range) man flow >>$
                 fun exc_msg flow ->
                 debug "%s: exc_addr: %a, exc_msg: %a" function_name pp_addr exc_addr (OptionExt.print (Top.top_fprint Format.pp_print_string)) exc_msg;
                 let args = match exc_msg with
                   | None -> []
                   | Some (Nt s) -> [{(Universal.Ast.mk_string s range) with etyp=(T_py None)}]
                   | Some TOP -> [Python.Ast.mk_py_top T_string range]
                 in
                 (* get callstack used when this exception was raised *)
                 let exc_cs = match exc_pt with
                   | P_block ({base_kind = Addr a}, _, _) ->
                      man.ask (Callstack_tracking.Q_cpython_attached_callstack a) flow
                   | _ -> assert false in
                 (* clean C exception state for next times *)
                 man.exec (mk_c_call_stmt (C.Ast.find_c_fundec_by_name "PyErr_Clear" flow) [] range) flow >>% fun flow ->
                   let old_cs = Flow.get_callstack flow in
                   let flow = Flow.set_callstack exc_cs flow in
                   man.exec (Python.Ast.mk_raise
                               (Python.Ast.mk_py_call
                                  (Python.Ast.mk_py_object (exc_addr, None) range)
                                  args range)
                               range) flow >>% fun flow ->
                   let flow = Flow.set_callstack old_cs flow in
                   Eval.empty flow
            | Some TOP -> assert false
            | None ->
               debug "%s: NULL found" function_name;
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
        alloc_py_addr man (Python.Addr.A_py_c_class cls_var) range flow >>$
          fun cls_eaddr flow ->
          let cls_addr = Addr.from_expr cls_eaddr in
          let flow = set_singleton cls cls_addr man flow in
          (* fill dict with methods, members, getset
             ~> by delegation to the dictionnary/structural type abstraction *)
          bind_function_in "__new__"
            (mk_c_address_of (mk_expr (E_c_function (C.Ast.find_c_fundec_by_name "tp_new_wrapper" flow)) range) range)
            cls_addr Builtin_function_or_method man flow >>%
          bind_function_in "__init__"
            (mk_c_arrow_access_by_name ecls "tp_init" range)
            cls_addr (Wrapper_descriptor (Some "wrap_init")) man >>%
          bind_function_in "__iter__"
            (mk_c_arrow_access_by_name ecls "tp_iter" range)
            cls_addr (Wrapper_descriptor (Some "wrap_unaryfunc")) man >>%
          bind_function_in "__next__"
            (mk_c_arrow_access_by_name ecls "tp_iternext" range)
            cls_addr (Wrapper_descriptor (Some "wrap_next")) man >>%
          bind_function_in "__lt__"
            (* FIXME: multiple fields Python side corresponding to one field on the C side is probably an issue *)
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_lt")) man >>%
          bind_function_in "__le__"
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_le")) man >>%
          bind_function_in "__eq__"
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_eq")) man >>%
          bind_function_in "__ne__"
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_ne")) man >>%
          bind_function_in "__gt__"
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_gt")) man >>%
          bind_function_in "__ge__"
            (mk_c_arrow_access_by_name ecls "tp_richcompare" range)
            cls_addr (Wrapper_descriptor (Some "richcmp_ge")) man >>%
            fun flow ->
              assume
                (ne
                   (mk_c_arrow_access_by_name ecls "tp_as_sequence" range)
                   (mk_c_null range)
                   ~etyp:T_bool range)
                man flow
                ~fthen:(fun flow ->
                  let as_sequence s =  mk_c_arrow_access_by_name (mk_c_arrow_access_by_name ecls "tp_as_sequence" range) s range in
                  Post.return flow >>%
                  bind_function_in "__len__" (as_sequence "sq_length")
                    cls_addr (Wrapper_descriptor (Some "wrap_lenfunc")) man >>%
                  bind_function_in "__contains__" (as_sequence "sq_contains")
                    cls_addr (Wrapper_descriptor (Some "wrap_objobjproc")) man >>%
                  bind_function_in "__add__" (as_sequence "sq_concat")
                    cls_addr (Wrapper_descriptor (Some "wrap_binaryfunc")) man >>%
                  bind_function_in "__mul__" (as_sequence "sq_repeat")
                    cls_addr (Wrapper_descriptor (Some "wrap_indexargfunc")) man >>%
                  bind_function_in "__rmul__" (as_sequence "sq_repeat")
                    cls_addr (Wrapper_descriptor (Some "wrap_indexargfunc")) man >>%
                  bind_function_in "__getitem__" (as_sequence "sq_item")
                    cls_addr (Wrapper_descriptor (Some "wrap_sq_item")) man >>%
                  bind_function_in "__setitem__" (as_sequence "sq_ass_item")
                    cls_addr (Wrapper_descriptor (Some "wrap_sq_setitem")) man >>%
                  bind_function_in "__delitem__" (as_sequence "sq_ass_item")
                    cls_addr (Wrapper_descriptor (Some "wrap_sq_delitem")) man
                )
                ~felse:(fun flow ->
                  debug "tp_as_sequence is NULL, skipping";
                  Post.return flow)
              >>%
                fun flow ->
                (* FIXME: *)
                debug "add_pymethoddef";
                add_pymethoddef "tp_methods" cls_addr Method_descriptor ecls man flow >>%
                  add_pymemberdef cls_addr ecls man >>%
                  fun flow ->
                  let flow = Flow.add_local_assumption (A_cpython_unsupported_fields "tp_numbers, ...") range flow in
                  Cases.singleton cls_addr flow

    let is_py_addr addr =
      match akind addr with
      (* FIXME: other container addresses *)
      | Python.Objects.Py_list.A_py_list
      | Python.Objects.Tuple.A_py_tuple _
      | Python.Objects.Dict.A_py_dict
      | A_py_instance _
      | A_py_class _
      | A_py_c_class _
      | A_py_c_function _
      | A_py_c_module _
      | A_py_function _
      | A_py_method _
      | A_py_module _ -> true
      | _ -> false

    let points_to_to_c_expr pt typ range =
      match pt with
      | P_block ({base_kind = Var v} as base, offset, _) ->
         mk_base_offset_pointer base offset typ range
      | P_block ({base_kind = Addr a}, _, _) ->
         mk_addr ~etyp:typ a range
      | _ -> assert false

    let py_addr_to_c_expr addr typ range man flow =
      points_to_to_c_expr (KeySet.choose @@ Top.detop @@ EquivBaseAddrs.find_inverse addr (get_env T_cur man flow)) typ range

    let rec python_to_c_boundary addr oaddr_ctyp oe ?(size=None) range man (flow: 'a flow) : (expr * 'a flow) =
      (* FIXME: give py_object so that the boundary handles translation of integers with value addr_attr *)
      let pyobject_typ, pytypeobject_typ =
        let assign_helper = C.Ast.find_c_fundec_by_name "_PyType_Assign_Helper" flow in
        under_pointer_type @@ vtyp @@ List.hd assign_helper.c_func_parameters,
        under_pointer_type @@ vtyp @@ List.hd @@ List.tl assign_helper.c_func_parameters in
      let addr_ctyp = match oaddr_ctyp with
        | None -> T_c_pointer pyobject_typ
        | Some s -> s in
      if not @@ is_py_addr addr then mk_addr ~etyp:addr_ctyp addr range, flow else
      if EquivBaseAddrs.is_bottom (get_env T_cur man flow) then let () = debug "python_to_c_boundary: bottom state, skipping" in mk_addr addr ~etyp:addr_ctyp range, flow else
      (* when a python object enters the C scope for the first time, a few things must be done:
         - if it's an instance, set the ob_type pointer correctly so that Py_TYPE works
         - if it's a class,  set the tp_flag correctly (then Py..._Check will be precise)
         in all cases, we add them to the equivalence class. This way, we don't need to perform the operation next time.
         FIXME: addr rename/old does not switch from U to C it seems for Python addresses:
                I think the current domain should be in charge of dispatching correctly... :(
       *)
      let () = debug "python_to_c_boundary %a %a %a" pp_range range pp_addr addr pp_typ addr_ctyp in
      let type_addr = match akind addr with
        | A_py_instance a -> a
        | Python.Objects.Py_list.A_py_list -> fst @@ find_builtin "list"
        | Python.Objects.Dict.A_py_dict -> fst @@ find_builtin "dict"
        | Python.Objects.Tuple.A_py_tuple _ -> fst @@ find_builtin "tuple"
        | A_py_c_class _ | A_py_class _ -> fst @@ find_builtin "type"
        | A_py_module _ | A_py_c_module _ -> fst @@ find_builtin "module"
        | A_py_function _ -> fst @@ find_builtin "function"
        | _ -> panic_at range "parent addr of %a?" pp_addr addr in
      let c_addr, post =
        let inverse = Top.detop @@ EquivBaseAddrs.find_inverse addr (get_env T_cur man flow) in
        match KeySet.choose_opt inverse with
        | Some inverse ->
           let () = debug "%a already converted according to equiv: %a" pp_addr addr pp_points_to inverse in
           points_to_to_c_expr inverse addr_ctyp range, Post.return flow
        | None ->
          (* let type_c_addr, flow = , is_cls =
           *   match akind addr with
           *   (\* | A_py_class _ ->
           *    *    mk_addr type_addr ~etyp:(T_c_pointer pytypeobject_typ) range, flow, true *\)
           *   | _ -> *)
               (* let's check our parent is correct first *)
           let type_c_addr, flow = python_to_c_boundary type_addr (Some (T_c_pointer pytypeobject_typ)) None range man flow in
           let is_cls = match akind addr with
             | A_py_class _ | A_py_c_class _ -> true
             | _ -> false
               (* type_c_addr, flow, false *)
          in
          (* let type_obj = py_addr_to_c_expr type_addr pytypeobject_typ range man flow in *)
          let obj = mk_addr ~mode:(Some STRONG) ~etyp:(T_c_pointer pyobject_typ) addr range in
          let final_obj = mk_addr ~etyp:(T_c_pointer pyobject_typ) addr range in
          let bytes = C.Cstubs.Aux_vars.mk_bytes_var addr in
          let bytes_size = match size with
            | None ->
               let size = sizeof_type (if is_cls then pytypeobject_typ else pyobject_typ) in
               mk_z ~typ:(T_c_integer C_unsigned_long) size range
            | Some s -> s in
          let pt = mk_c_points_to_bloc (C.Common.Base.mk_addr_base addr) (mk_zero range) None in
          let flow = set_singleton pt addr man flow in
          final_obj,
          (
            man.exec ~route:(Semantic "C") (mk_add obj range) flow >>% fun flow ->
              let () = debug "adding bytes var %a" pp_var bytes in
              man.exec ~route:(Semantic "C") (mk_add_var bytes range) flow >>%
              man.exec (mk_assign (mk_var bytes range) bytes_size  range) >>%
              fun flow ->
              let set_default_flags_func = C.Ast.find_c_fundec_by_name "set_default_flags" flow in
              let set_tp_alloc_py_class = C.Ast.find_c_fundec_by_name "set_tp_alloc_py_class" flow in
              let flow =
                if is_cls then
                  (* if cls: should call set_default_flags *)
                  post_to_flow man
                    (
                      (* FIXME:
                         inherit_slots;
                         inheriting builtin types doesn't propagate the types, done by inherit_special in typeobject.c
                       *)
                      let flow =
                        match akind addr with
                        | A_py_class (_, m) ->
                           let base_addr, _ = List.hd @@ List.tl m in
                           let base_obj = py_addr_to_c_expr base_addr pytypeobject_typ range man flow in
                           post_to_flow man
                             (man.exec
                             (mk_expr_stmt
                                (mk_c_call
                                   set_tp_alloc_py_class
                                   [obj; base_obj]
                                   range)
                                range) flow)
                        | _ -> flow in
                      man.exec
                        (mk_expr_stmt
                           (mk_c_call
                              set_default_flags_func
                              [obj]
                              range)
                           range) flow >>%
                        Post.return
                    )
                else flow in
              debug "ob_type assignment: obj = %a, type_obj = %a" pp_expr obj pp_expr type_c_addr;
              (* this is obj->ob_type = type *)
              man.exec (mk_assign
                          (mk_c_deref (mk_c_cast
                                         (add (mk_c_cast obj (T_c_pointer s8) range) (mk_int 8 range) range)
                                         (T_c_pointer (T_c_pointer pytypeobject_typ)) range) range)
                          type_c_addr
                          range) flow
          )
      in
      let post =
        match akind type_addr with
        | A_py_class (C_builtin "int", _) ->
           debug "boundary @ %a, assigning %a.value = %a" pp_range range pp_addr addr (OptionExt.print pp_expr) oe;
           post >>% man.exec (mk_assign
                       (mk_avalue_from_pyaddr addr T_int range)
                       (OptionExt.none_to_exn oe) range)
        | A_py_class (C_builtin "float", _) ->
           debug "boundary @ %a, assigning %a.value = %a" pp_range range pp_addr addr (OptionExt.print pp_expr) oe;
           post >>% man.exec (mk_assign
                       (mk_avalue_from_pyaddr addr (T_float F_DOUBLE) range)
                       (OptionExt.none_to_exn oe) range)
        | A_py_class (C_builtin "str", _)
        | A_py_class (C_builtin "bytes", _) ->
           debug "boundary @ %a, assigning %a.value = %a" pp_range range pp_addr addr (OptionExt.print pp_expr) oe;
           post >>% man.exec (mk_assign
                       (mk_avalue_from_pyaddr addr T_string range)
                       (OptionExt.none_to_exn oe) range)
        | _ -> post in
      c_addr, post_to_flow man post


    let c_set_exception c_exn message range man flow =
      man.exec (mk_c_call_stmt
                  (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow)
                  [
                    mk_var (search_c_globals_for flow c_exn) range;
                    mk_c_string message range
                  ]
                  range) flow

    let normalize_fmt_str fmt_str =
      let mandatory_fmt, optional_fmt = match String.split_on_char '|' fmt_str with
        | [s] -> s, ""
        | [s; s'] -> s, s'
        | _ -> assert false in
      let strip_error_message s = List.hd (String.split_on_char ';' s) in
      let strip_error_funcname s = List.hd (String.split_on_char ':' s) in
      let compute_length s =
        let s = strip_error_message s in
        let s = strip_error_funcname s in
        let rec aux pos acc =
          if pos >= String.length s then acc
          else
            match s.[pos] with
            | '(' | ')' -> aux (pos+1) acc
            | _ -> aux (pos+1) (acc+1)
        in aux 0 0 in
      let min_size = compute_length mandatory_fmt in
      let max_size = min_size + compute_length optional_fmt in
      mandatory_fmt ^ optional_fmt, min_size, max_size

    let len_of man addr flow range =
      man.eval (Python.Ast.mk_py_call
                  (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "len") range)
                  [Python.Ast.mk_py_object (addr, None) range] range) flow >>$ fun earg flow ->
          match ekind @@ OptionExt.none_to_exn @@ snd @@ object_of_expr earg with
          | E_constant (C_int z) -> Cases.singleton (Z.to_int z) flow
          | _ -> assert false

    let pylong_to_c_type arg range man flow (type_min_value, type_max_value, type_name) =
         (* FIXME: upon translation from Python to C, integer arguments should get a value attribute. Issue if multiple integer arguments... tag it with the precise range otherwise?  Also, need to clean the "value" attribute afterwards *)
         resolve_c_pointer_into_addr arg man flow >>$
           (fun oaddr flow ->
             match oaddr with
             | Some (Nt addr) ->
                begin match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
                   man.eval (mk_avalue_from_pyaddr addr T_int range) flow >>$
                     fun int_value flow ->
                     let max_value = mk_z type_max_value ~typ:T_int range in
                     let min_value = mk_z type_min_value ~typ:T_int range in
                     let overflow_check =
                       log_and
                         (le int_value max_value range)
                         (ge int_value min_value range)
                         range in
                     assume
                       overflow_check
                       man flow
                       ~fthen:(Eval.singleton int_value)
                       ~felse:(fun flow ->
                         (* Overflow: need to set the error and then return -1 *)
                         c_set_exception "PyExc_OverflowError" (Format.asprintf "Python int too large to convert to C %s" type_name) range man flow >>%
                           Eval.singleton (mk_int ~typ:T_int (-1) range)
                       )
                | _ ->
                   c_set_exception "PyExc_TypeError" "an integer is required (got type ???)" range man flow >>%
                     fun flow ->
                     Eval.singleton (mk_int ~typ:T_int (-1) range) flow
                end
             | _ -> assert false
           )

    let c_int_to_python arg man flow range =
      man.eval (mk_c_call (C.Ast.find_c_fundec_by_name "PyLong_FromLong" flow) [arg] range) flow >>$
        fun py_pos flow ->
        Eval.singleton (mk_py_object (Addr.from_expr py_pos, Some (mk_avalue_from_pyaddr (Addr.from_expr py_pos) T_int range)) range) flow

    let fold_c_to_python_boundary ?(safe_check = None) man range c_objs flow =
      let rec aux c_objs py_acc flow =
        match c_objs with
        | [] -> Cases.singleton (List.rev py_acc) flow
        | c_hd :: tl ->
           c_to_python_boundary ~safe_check c_hd man flow range >>$
             fun py_object flow ->
             aux tl (py_object :: py_acc) flow
      in aux c_objs [] flow

    let build_value man flow range fmt_str refs =
      let fmt_length = String.length fmt_str in
      let rec process pos ref_pos until acc flow =
        if pos >= until then Cases.singleton (List.rev acc) flow
        else
          let range = tag_range range "process[%d]" pos in
          begin match fmt_str.[pos] with
          | 'c' ->
             let pyunicode_fromwidechar = C.Ast.find_c_fundec_by_name "PyBytes_FromStringAndSize" flow in
             man.eval (mk_c_call pyunicode_fromwidechar
                         [
                           List.nth refs ref_pos;
                           mk_one range
                         ] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 'd' | 'f' ->
             let pyfloat_fromdouble = C.Ast.find_c_fundec_by_name "PyFloat_FromDouble" flow in
             man.eval (mk_c_call pyfloat_fromdouble [mk_c_cast (List.nth refs ref_pos) (T_c_float C_double) range] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 'i' ->
             let pylong_fromlong = C.Ast.find_c_fundec_by_name "PyLong_FromLong" flow in
             (* FIXME: cast to long *)
             (* FIXME: in Cbox_getcounter, if we change self->counter by self->contents, the error is currently really unclear. The translation to Universal fails silently in PyLong_FromLong. How to change that? *)
             man.eval (mk_c_call pylong_fromlong [mk_c_cast (List.nth refs ref_pos) sl range] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 'k' ->
             let pylong_fromunsignedlong = C.Ast.find_c_fundec_by_name "PyLong_FromUnsignedLong" flow in
             (* FIXME: cast *)
             man.eval (mk_c_call pylong_fromunsignedlong [mk_c_cast (List.nth refs ref_pos) ul range] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 'n' ->
             let pylong_fromssize_t = C.Ast.find_c_fundec_by_name "PyLong_FromSsize_t" flow in
             (* FIXME: cast to long *)
             man.eval (mk_c_call pylong_fromssize_t [List.nth refs ref_pos] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow


          | 'O' ->
             (* FIXME: if its NULL, an exception should have been set.
                       Otherwise:                 PyErr_SetString(PyExc_SystemError,
                    "NULL object passed to Py_BuildValue");
              *)
             man.eval (List.nth refs ref_pos) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 's' when pos+1 < fmt_length && fmt_str.[pos+1] = '#'->
             warn_at range "Py_BuildValue: s# processed as s";
             let pyunicode_fromstring = C.Ast.find_c_fundec_by_name "PyUnicode_FromString" flow in
             assume (eq (List.nth refs ref_pos) (mk_c_null range) range) man flow
               ~fthen:(fun flow ->
                 let c_addr, flow = python_to_c_boundary (OptionExt.none_to_exn !Python.Types.Addr_env.addr_none) None None range man flow in
                 process (pos+2) (ref_pos+2) until (c_addr :: acc) flow
               )
               ~felse:(fun flow ->
                 man.eval (mk_c_call pyunicode_fromstring [List.nth refs ref_pos] range) flow >>$
                 fun res_pos flow ->
                 process (pos+2) (ref_pos+2) until (res_pos :: acc) flow
               )

          | 's' ->
             let pyunicode_fromstring = C.Ast.find_c_fundec_by_name "PyUnicode_FromString" flow in
             assume (eq (List.nth refs ref_pos) (mk_c_null range) range) man flow
               ~fthen:(fun flow ->
                 let c_addr, flow = python_to_c_boundary (OptionExt.none_to_exn !Python.Types.Addr_env.addr_none) None None range man flow in
                 process (pos+1) (ref_pos+1) until (c_addr :: acc) flow
               )
               ~felse:(fun flow ->
                 man.eval (mk_c_call pyunicode_fromstring [List.nth refs ref_pos] range) flow >>$
                 fun res_pos flow ->
                 process (pos+1) (ref_pos+1) until (res_pos :: acc) flow
               )

          | 'u' when pos+1 < fmt_length && fmt_str.[pos+1] = '#'->
             let pyunicode_fromwidechar = C.Ast.find_c_fundec_by_name "PyUnicode_FromWideChar" flow in
             man.eval (mk_c_call pyunicode_fromwidechar
                         [
                           List.nth refs ref_pos;
                           List.nth refs (ref_pos+1);
                         ] range) flow >>$
               fun res_pos flow ->
               process (pos+2) (ref_pos+2) until (res_pos :: acc) flow

          | 'u' ->
             let pyunicode_fromwidechar = C.Ast.find_c_fundec_by_name "PyUnicode_FromWideChar" flow in
             let wcslen = C.Ast.find_c_fundec_by_name "wcslen" flow in
             man.eval (mk_c_call pyunicode_fromwidechar [List.nth refs ref_pos; mk_c_call wcslen [List.nth refs ref_pos] range] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow

          | 'y' when pos+1 < fmt_length && fmt_str.[pos+1] = '#'->
             let pybytes_fromsas = C.Ast.find_c_fundec_by_name "PyBytes_FromStringAndSize" flow in
             man.eval (mk_c_call pybytes_fromsas
                         [
                           List.nth refs ref_pos;
                           List.nth refs (ref_pos+1);
                         ] range) flow >>$
               fun res_pos flow ->
               process (pos+2) (ref_pos+2) until (res_pos :: acc) flow

          | 'y' ->
             let pybytes_fromsas = C.Ast.find_c_fundec_by_name "PyBytes_FromStringAndSize" flow in
             let strlen = C.Ast.find_c_fundec_by_name "strlen" flow in
             man.eval (mk_c_call pybytes_fromsas [List.nth refs ref_pos; mk_c_call strlen [List.nth refs ref_pos] range] range) flow >>$
               fun res_pos flow ->
               process (pos+1) (ref_pos+1) until (res_pos :: acc) flow


          | '{' ->
             let closing_bracket_pos =
               let rec search count pos =
                 if fmt_str.[pos] = '}' then
                   if count = 0 then pos
                   else search (count-1) (pos+1)
                 else if fmt_str.[pos] = '{' then
                   search (count+1) (pos+1)
                 else search count (pos+1) in
               search 0 (pos+1) in
             let dict_subfmt = String.sub fmt_str pos (closing_bracket_pos - pos - 1) in
             debug "starting process_dict %s" dict_subfmt;
             process_dict pos ref_pos closing_bracket_pos flow >>$
               fun dict flow ->
               (* FIXME: # in count *)
               process (closing_bracket_pos+1) (ref_pos + 2 * (List.length (String.split_on_char ',' dict_subfmt))) until (dict :: acc) flow

          | '(' ->
             let closing_par_pos =
               let rec search count pos =
                 if fmt_str.[pos] = ')' then
                   if count = 0 then pos
                   else search (count-1) (pos+1)
                 else if fmt_str.[pos] = '(' then
                   search (count+1) (pos+1)
                 else search count (pos+1) in
               search 0 (pos+1) in
             let tuple_subfmt = String.sub fmt_str pos (closing_par_pos - pos - 1) in
             process (pos+1) (ref_pos+1) closing_par_pos [] flow >>$ fun tuple_values flow ->
             fold_c_to_python_boundary man range tuple_values flow >>$ fun tuple_values flow ->
             man.eval (mk_expr (Python.Ast.E_py_tuple tuple_values) range) flow >>$ fun py_tuple flow ->
             let addr_py_tuple, oe_py_tuple = object_of_expr py_tuple in
             let c_addr, flow = python_to_c_boundary addr_py_tuple None oe_py_tuple range man flow in
             (* FIXME: # in count *)
             process (closing_par_pos + 1) (ref_pos + String.length tuple_subfmt) until (c_addr::acc) flow


          | _ -> panic_at range "Py_BuildValue unhandled format %s" fmt_str
          end
      and process_dict beg_pos ref_pos end_pos flow =
        let rec aux cur_pos ref_pos dict_acc flow =
          if cur_pos >= end_pos then Cases.singleton (List.rev dict_acc) flow else
            let () = assert(fmt_str.[cur_pos+1] = ':') in
            let () = assert(fmt_str.[cur_pos+3] = ',' || cur_pos + 3 >= end_pos) in
            process cur_pos ref_pos (cur_pos+1) [] flow >>$ fun key flow ->
            process (cur_pos+2) (ref_pos+1) (cur_pos+3) [] flow >>$ fun value flow ->
            let key = match key with [k] -> k | _ -> assert false in
            let value = match value with [v] -> v | _ -> assert false in
            debug "key = %a, value = %a" pp_expr key pp_expr value;
            aux (cur_pos+4) (ref_pos+2) ((key,value) :: dict_acc) flow
        in
        aux (beg_pos+1) ref_pos [] flow >>$
          fun dict flow ->
          let dict_keys, dict_values = List.split dict in
          fold_c_to_python_boundary man range dict_keys flow >>$ fun dict_keys flow ->
          fold_c_to_python_boundary man range dict_values flow >>$ fun dict_values flow ->
          man.eval (mk_expr (Python.Ast.E_py_dict (dict_keys, dict_values)) range) flow >>$
            fun py_dict flow ->
            let addr_py_dict, oe_py_dict = object_of_expr py_dict in
            let c_addr, flow = python_to_c_boundary addr_py_dict None oe_py_dict range man flow in
            man.eval c_addr flow in
      process 0 0 fmt_length [] flow


    let convert_single man obj fmt output_ref range flow  =
      match ekind obj, ekind output_ref  with
      | Python.Ast.E_py_object(addr, oe), E_c_address_of c ->
         begin match fmt with
         | 'O' ->
            debug "ParseTuple O ~> %a" pp_addr addr;
            strongify_int_addr_hack addr man range flow >>$ fun addr flow ->
            let c_addr, flow = python_to_c_boundary addr None oe range man flow in
            man.exec (mk_assign c c_addr range) flow >>%
              fun flow -> Cases.return 1 flow
         | 'i' ->
            (* FIXME: this sould be a boundary between python and C.
                                 As such, it should handle integer translation.
                                 Python function call PyLong_AsLong *)
            debug "got obj = %a" pp_expr obj;
            if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_integers) = 0 then
              strongify_int_addr_hack addr man range flow >>$ fun addr flow ->
              let c_addr, flow = python_to_c_boundary addr None oe range man flow in
              (* FIXME: maybe replace oe with None, and handle conversion here before giving it to the Helper *)
              debug "value should be stored %a" pp_expr (mk_avalue_from_pyaddr addr T_int range);
              assume (mk_c_call
                        (C.Ast.find_c_fundec_by_name "PyParseTuple_int_helper" flow)
                        [c_addr; mk_c_address_of c range]
                        range)
                man flow
                ~fthen:(Cases.return 1)
                ~felse:(Cases.return 0)
            else
              let () = debug "wrong type for convert_single integer" in
              (* set error *)
              c_set_exception "PyExc_TypeError" "an integer is required (got type ???)" range man flow >>% Cases.return 0

         | 'd'
           | 'f' ->
         (* Call PyFloat_AsDouble. If value is -1 and PyErr_Occurred, return 0. Otherwise return value (or cast for 'f') *)
            if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_float) = 0 then
              strongify_int_addr_hack addr man range flow >>$ fun addr flow ->
              let c_addr, flow = python_to_c_boundary addr None oe range man flow in
              let pyfloat_asdouble = C.Ast.find_c_fundec_by_name "PyFloat_AsDouble" flow in
              let pyerr_occurred = (* not calling the macro as it seems to create issues *)
                mk_c_arrow_access_by_name (mk_var (search_c_globals_for flow "exc") range) "exc_state" range in
              man.eval (mk_c_call pyfloat_asdouble [c_addr] range) flow >>$ fun float_val flow ->
              assume (log_and
                       (eq float_val {(mk_float ~prec:(F_DOUBLE) (-1.) range) with etyp=T_c_float C_double} range)
                       (ne pyerr_occurred (mk_c_null range) range)
                       ~etyp:T_bool range)
                man flow
                ~fthen:(fun flow -> (* conversion error *)
                  debug "conversion error %a" pp_expr float_val;
                  Cases.return 0 flow)
                ~felse:(fun flow ->
                  debug "trying conversion %a %a" pp_typ (etyp c) pp_typ (etyp float_val);
                  man.exec (mk_assign c (if fmt = 'f' then mk_c_cast float_val (T_c_float C_float) range else float_val) range) flow >>% Cases.return 1)
            else
              let () = debug "not a float..." in
              c_set_exception "PyExc_TypeError" "a float is required (got type ???)" range man flow >>% Cases.return 0
         | _ ->
            if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
              let () = warn_at range "PyArg_ParseTuple(AndKeywords)? %c unsupported, but cur is bottom" fmt in
              Cases.return 1 flow
            else
              panic_at range "TODO: implement PyArg_ParseTuple(AndKeywords)? %c" fmt
         end
      | _ -> assert false

    let rec eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_var ({vkind = V_cvar v}, _) when List.mem v.cvar_uniq_name builtin_exceptions ->
         let py_name = String.sub v.cvar_uniq_name 6 (String.length v.cvar_uniq_name - 6) in
         Eval.singleton (mk_addr (fst @@ Python.Addr.find_builtin py_name) range) flow
         |> OptionExt.return

      (* FIXME: PyModule_Create is a macro expanded into PyModule_Create2. Maybe we should have a custom .h file *)
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
             let add_exc_equivs descr flow =
               (* let flow = add_class_equivs descr flow in *)
               List.fold_left (fun flow c ->
                   let py = String.sub c 6 (String.length c - 6) in
                   let py_addr = fst @@ Python.Addr.find_builtin py in
                   set_singleton
                     (mk_c_points_to_bloc (C.Common.Base.mk_addr_base py_addr) (mk_zero (Location.mk_program_range [])) None)
                   py_addr man flow) flow descr
             in
             let flow =
               let none_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_none in
               let true_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_true in
               let false_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_false in
               let ni_addr = OptionExt.none_to_exn !Python.Types.Addr_env.addr_notimplemented in
               let none_var = search_c_globals_for flow "_Py_NoneStruct" in
               let ni_var = search_c_globals_for flow "_Py_NotImplementedStruct" in
               let true_var = search_c_globals_for flow "_Py_TrueStruct" in
               let false_var = search_c_globals_for flow "_Py_FalseStruct" in
               let flow = set_singleton
                            (mk_c_points_to_bloc (C.Common.Base.mk_var_base none_var) (mk_zero (Location.mk_program_range [])) None)
                            none_addr
                            man flow in
               let flow = set_singleton
                            (mk_c_points_to_bloc (C.Common.Base.mk_var_base ni_var) (mk_zero (Location.mk_program_range [])) None)
                            ni_addr
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
             let flow =
               add_class_equivs
                 [
                   ("PyType_Type", "type");
                   ("PyBaseObject_Type", "object");
                   ("PyLong_Type", "int");
                   ("PyFloat_Type", "float");
                   ("PyUnicode_Type", "str");
                   ("PyBytes_Type", "bytes");
                   ("PyList_Type", "list");
                   ("PyRange_Type", "range");
                   ("PyTuple_Type", "tuple");
                   ("PyDict_Type", "dict");
                   ("_PyNone_Type", "NoneType");
                   ("_PyNotImplemented_Type", "NotImplementedType");
                   (* FIXME: add all matches to PyAPI_DATA(PyObject * ) in cpython/Include? *)
                 ]
                 flow in
             let flow =
               add_exc_equivs builtin_exceptions
               flow in
             let init_flags = C.Ast.find_c_fundec_by_name "init_flags" flow in
             (man.exec (mk_expr_stmt (mk_c_call init_flags [] range) range) flow >>% fun flow ->
             let init_exc_state = C.Ast.find_c_fundec_by_name "PyErr_Clear" flow in
             man.exec (mk_expr_stmt (mk_c_call init_exc_state [] range) range) flow)
           else
             Post.return flow
         in
         new_module_from_def module_decl man (post_to_flow man flow)
         |> OptionExt.return


      | E_c_builtin_call ("PyModule_AddObject", [module_object; obj_name; obj]) ->
         c_to_python_boundary module_object man flow range >>$
           (
             fun module_object flow ->
             let module_addr, module_oexpr = object_of_expr module_object in

             safe_get_name_of obj_name man flow >>$
               fun oobj_name flow ->
               let obj_name = Top.top_to_exn (OptionExt.none_to_exn oobj_name) in

               c_to_python_boundary obj man flow range >>$
                 fun obj flow ->
                 let obj_addr, obj_oe = object_of_expr obj in
                 (* bind class to module *)
                 bind_in_python module_addr obj_name obj_addr range man flow
                 >>= fun flow ->
                 Eval.singleton (mk_zero range)
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyType_IsSubtype", [tobj; tobj']) ->
         (* pirouette: transform these PyTypeObjects into Python classes, and assume over the call to Python's issubclass *)
         (* FIXME: the standard issubclass is suppose to call __subclasscheck__, but this function doesn't... *)
         c_to_python_boundary tobj man flow range >>$ (fun py_cls flow ->
         c_to_python_boundary tobj' man flow range >>$ fun py_cls' flow ->
         assume (Python.Addr.mk_py_issubclass py_cls py_cls' range)
           man flow
           ~fthen:(Eval.singleton (mk_one range))
           ~felse:(Eval.singleton (mk_zero range)))
         |> OptionExt.return

      | E_c_builtin_call ("PyType_Ready", [cls]) ->
         (* add base (FIXME: handle inheritance)
            ~> delegated to the C-level field accesses *)
         (* Py_TYPE(cls) = &PyType_Type *)
         (* FIXME: sometimes I'd like to write some parts of the transfer functions in the analyzed language... *)
         let cheat = C.Ast.find_c_fundec_by_name "PyType_ReadyCheat" flow in
         assume (eq (mk_c_call cheat [cls] range) (mk_zero range) range) man flow
           ~fthen:(fun flow ->
             debug "after PyType_ReadyCheat"; (*(format @@ Flow.print man.lattice.print) flow; *)
             new_class_from_def cls man flow >>$
               fun cls_addr flow ->
               Eval.singleton (mk_int 0 range) flow
           )
           ~felse:(fun flow ->
             debug "PyType_Ready already called according to the flag, nothing to do";
             Eval.singleton (mk_int 0 range) flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyType_GenericAlloc_Helper", args) ->
         let cls = List.hd args in
         resolve_c_pointer_into_addr cls man flow >>$
           (fun cls_oaddr flow ->
             let cls_addr = Top.detop @@ OptionExt.none_to_exn cls_oaddr in
             (* FIXME: should we use the range from where the allocation is performed to help the recency? Or at least use the callstack to disambiguate for those specific instances... *)
             alloc_py_addr man (Python.Addr.A_py_instance cls_addr) range flow >>$
               fun inst_eaddr flow ->
               let c_addr, flow = python_to_c_boundary (Addr.from_expr inst_eaddr) None None ~size:(Some (List.hd @@ List.tl args)) range man flow in
               Eval.singleton c_addr flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyLong_FromSsize_t", args)
      | E_c_builtin_call ("PyLong_FromLong", args)
      | E_c_builtin_call ("PyLong_FromUnsignedLong", args) ->
         man.eval ~translate:"Universal" (List.hd args) flow >>$
           (fun earg flow ->
             debug "eval ~translate:Universal %a ~> %a" pp_expr (List.hd args) pp_expr earg;
             (* FIXME: forced to attach the value as an addr_attr in universal, and convert it afterwards when going back to python... *)
             debug "allocating int at range %a callstack %a" pp_range range Callstack.pp_callstack (Flow.get_callstack flow);
             alloc_py_addr man (A_py_instance (fst @@ find_builtin "int")) range flow >>$
               fun int_addr flow ->
               debug "got int_addr %a, putting %a as value" pp_addr (Addr.from_expr int_addr) pp_expr earg;
               let c_addr, flow = python_to_c_boundary (Addr.from_expr int_addr) None (Some earg) range man flow in
               debug "c_addr %a" pp_expr c_addr;
                 (* FIXME: addr vs py_object, we'll need to clean things somehow... *)
               Eval.singleton c_addr flow
           )
         |> OptionExt.return


      | E_c_builtin_call ("PyLong_AsSsize_t", args)
        (* actually, PyLong_AsSsize_t seems to use the same bounds as PyLong_AsLong -_- *)
      | E_c_builtin_call ("PyLong_AsLong", args) ->
         let long_max = Z.of_string  "9223372036854775807" in
         let long_min = Z.of_string "-9223372036854775807" in
         pylong_to_c_type (List.hd args) range man flow (long_min, long_max, "long")
         |> OptionExt.return


      | E_c_builtin_call ("PyFloat_AsDouble", args) ->
         (* FIXME: this function is actually more permissive *)
         resolve_c_pointer_into_addr (List.hd args) man flow >>$ (fun oaddr flow ->
          let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
          match akind addr with
          | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
             (* FIXME: do the cast but for now it just doesn't work *)
             man.eval (mk_unop O_cast (mk_avalue_from_pyaddr addr (T_float F_DOUBLE) range)  ~etyp:(T_c_float C_double) range) flow
          | _ ->
             c_set_exception "PyExc_TypeError" "a float is required (got type ???)" range man flow >>%
               Eval.singleton (mk_int ~typ:T_int (-1) range)

        ) |> OptionExt.return

      | E_c_builtin_call ("PyFloat_FromDouble", args) ->
         man.eval ~translate:"Universal" (List.hd args) flow >>$ (fun earg flow ->
          alloc_py_addr man (A_py_instance (fst @@ find_builtin "float")) range flow >>$ fun float_addr flow ->
          let c_addr, flow = python_to_c_boundary (Addr.from_expr float_addr) None (Some earg) range man flow in
          Debug.debug ~channel:"bug" "result %a" pp_expr c_addr;
          Eval.singleton c_addr flow
        ) |> OptionExt.return

      | E_c_builtin_call ("PyBytes_FromStringAndSize", [v; len]) ->
         man.eval (mk_top (T_py (Some Bytes)) range) flow >>$ (fun py_bytes flow ->
          let py_bytes_addr, py_bytes_oe = object_of_expr py_bytes in
          let c_addr, flow = python_to_c_boundary py_bytes_addr None py_bytes_oe range man flow in
          Eval.singleton c_addr flow
        ) |> OptionExt.return

      | E_c_builtin_call ("PyBytes_Size", args) ->
         resolve_c_pointer_into_addr (List.hd args) man flow >>$
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             (* FIXME: maybe we should do an isinstance check? *)
             if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_bytes) = 0 then
               man.eval (mk_expr ~etyp:T_int (E_len (mk_avalue_from_pyaddr addr T_string range)) range) flow >>$
               fun str_length flow ->
               Eval.singleton str_length flow
             else
               let pyerr_badarg = C.Ast.find_c_fundec_by_name "PyErr_BadArgument" flow in
               man.exec (mk_c_call_stmt pyerr_badarg [] range) flow >>%
                 Eval.singleton (mk_int (-1) ~typ:T_int range)
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyBytes_AsString", args) ->
         let o = List.hd args in
         c_to_python_boundary o man flow range >>$ (fun py_o flow ->
          let addr, oe = object_of_expr py_o in
          if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_bytes) = 0 then
            (* FIXME: conversion is not the best thing to do, maybe we could have a cast? Or a string expression? *)
            let open Universal.Strings.Powerset in
            let strp = man.ask (mk_strings_powerset_query (OptionExt.none_to_exn oe)) flow in
            Eval.join_list ~empty:(fun () -> assert false)
              (StringPower.fold (fun s acc ->
                   debug "PyBytes_AsString, got %s" s;
                   Eval.singleton (mk_c_string s range) flow :: acc
              ) strp [])
          else
            let pyerr_badarg = C.Ast.find_c_fundec_by_name "PyErr_BadArgument" flow in
            man.exec (mk_c_call_stmt pyerr_badarg [] range) flow >>%
              Eval.singleton (mk_int (-1) ~typ:T_int range)
        ) |> OptionExt.return



      | E_c_builtin_call ("PyUnicode_GetLength", args) ->
         (* FIXME: cheating on py_ssize_t *)
         (* let py_ssize_t = C.Ast.ul in *)
         debug "GetLength %a" pp_expr (List.hd args);
         resolve_c_pointer_into_addr (List.hd args) man flow >>$
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             debug "addr is %a %b" pp_addr addr (compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_strings) = 0);
             (* FIXME: maybe we should do an isinstance check? *)
             if compare_addr_kind (akind addr) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_strings) = 0 then
               man.eval (mk_expr ~etyp:T_int (E_len (mk_avalue_from_pyaddr addr T_string range)) range) flow >>$
               fun str_length flow ->
               Eval.singleton str_length flow
             else
               let pyerr_badarg = C.Ast.find_c_fundec_by_name "PyErr_BadArgument" flow in
               man.exec (mk_c_call_stmt pyerr_badarg [] range) flow >>%
                 Eval.singleton (mk_int (-1) ~typ:T_int range)
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyUnicode_InternFromString", args)
      | E_c_builtin_call ("PyUnicode_FromString", args) ->
         resolve_pointer (List.hd args) man flow >>$ (fun pt flow ->
          match pt with
          | P_block ({base_kind = String (s, char_kind, typ)}, _, _) ->
             alloc_py_addr man (A_py_instance (fst @@ find_builtin "str")) range flow >>$
               fun str_addr flow ->
               let c_addr, flow = python_to_c_boundary (Addr.from_expr str_addr) None (Some (mk_string s range)) range man flow in
               Eval.singleton c_addr flow
          | P_top ->
             alloc_py_addr man (A_py_instance (fst @@ find_builtin "str")) range flow >>$
               fun str_addr flow ->
               let c_addr, flow = python_to_c_boundary (Addr.from_expr str_addr) None (Some (mk_top T_string range)) range man flow in
               Eval.singleton c_addr flow
          | _ ->
             panic_at range "PyUnicode_*FromString %a" pp_points_to pt
        ) |> OptionExt.return

      | E_c_builtin_call ("PyUnicode_FromWideChar", args)
      | E_c_builtin_call ("PyUnicode_FromKindAndData", args) ->
         (* FIXME: we don't evaluate the buffer for now *)
         (* FIXME: there is no way to give s=T:string && |s|<=k for now *)
         man.eval (mk_py_top T_string range) flow >>$
           (fun str_addr flow ->
             let c_addr, flow = python_to_c_boundary (fst @@ object_of_expr str_addr) None (Some (mk_top T_string range)) range man flow in
           Eval.singleton c_addr flow)
         |> OptionExt.return

      | E_c_builtin_call ("PyUnicode_AsEncodedString", [unicode; encoding; errors]) ->
         c_to_python_boundary unicode man flow range >>$ (fun py_unicode flow ->
          man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_attr py_unicode "encode" range) [] range) flow >>$ fun py_encoded flow ->
          let addr_py_encoded, oe_py_encoded = object_of_expr py_encoded in
          let c_addr, flow = python_to_c_boundary addr_py_encoded None oe_py_encoded range man flow in
          man.eval c_addr flow
        ) |> OptionExt.return

      | E_c_builtin_call ("PyUnicode_AsUTF8AndSize", [unicode; size]) ->
         c_to_python_boundary unicode man flow range >>$ (fun py_unicode flow ->
          let addr_unicode, oe_unicode = object_of_expr py_unicode in
          if compare_addr_kind (akind addr_unicode) (akind @@ OptionExt.none_to_exn !Python.Types.Addr_env.addr_strings) = 0 then
            resolve_c_pointer_into_addr size man flow >>$ fun size_oaddr flow ->
            (match size_oaddr with
            | None -> Post.return flow
            | Some (Nt size_addr) ->
               man.eval ~route:(Semantic "Python") (Python.Utils.mk_builtin_call "len" [py_unicode] range) flow >>$ fun py_unicode_length flow ->
               let addr_py_u_l, oe_py_u_l = object_of_expr py_unicode_length in
               let c_addr, flow = python_to_c_boundary addr_py_u_l None oe_py_u_l range man flow in
               man.exec (mk_assign (mk_addr size_addr range) (mk_c_call (C.Ast.find_c_fundec_by_name "PyLong_AsSsize_t" flow) [c_addr] range) range) flow
            | Some TOP -> assert false
            ) >>% fun flow ->
            let open Universal.Strings.Powerset in
            let strp = man.ask (mk_strings_powerset_query (OptionExt.none_to_exn oe_unicode)) flow in
            Eval.join_list ~empty:(fun () -> assert false)
              (StringPower.fold (fun s acc ->
                   Eval.singleton (mk_c_string s range) flow :: acc
                 ) strp [])
          else
            let pyerr_badarg = C.Ast.find_c_fundec_by_name "PyErr_BadArgument" flow in
            man.exec (mk_c_call_stmt pyerr_badarg [] range) flow >>%
                 Eval.singleton (mk_c_null range)
        ) |> OptionExt.return


      | E_c_builtin_call ("PyArg_ParseTuple", args::fmt::refs) ->
         safe_get_name_of fmt man flow >>$ (fun ofmt_str flow ->
          let fmt_str = Top.top_to_exn (OptionExt.none_to_exn ofmt_str) in
          resolve_c_pointer_into_addr args man flow >>$ fun tuple_oaddr flow ->
          let tuple_addr = Top.detop @@ OptionExt.none_to_exn tuple_oaddr in
          len_of man tuple_addr flow range >>$ fun tuple_size flow ->
          (* Currently does not handle variable size arguments etc *)
          (* Also, need to check if format is correct *)
          (*
               check that size = len(fmt_str), otherwise raise TypeError
               each conversion may fail: in that case, return 0 FIXME: also, clean
               if everything succeeds, return 1
          *)
          let fmt_str, fmt_str_itv_lo, fmt_str_itv_hi = normalize_fmt_str fmt_str in
          debug "fmt_str_itv = [%d, %d]; size = %d" fmt_str_itv_lo fmt_str_itv_hi tuple_size;
          if tuple_size < fmt_str_itv_lo || tuple_size > fmt_str_itv_hi then
            let msg = Format.asprintf "function takes %s %d argument%s (%d given)"
                        (if fmt_str_itv_hi = fmt_str_itv_lo then "exactly" else if tuple_size < fmt_str_itv_lo then "at least" else "at most")
                        (if tuple_size < fmt_str_itv_lo then fmt_str_itv_lo else fmt_str_itv_hi)
                        (let s =
                           if tuple_size < fmt_str_itv_hi then fmt_str_itv_lo
                           else fmt_str_itv_hi in
                         if s = 1 then "" else "s")
                        tuple_size in
            let () = debug "wrong number of arguments: %s" msg in
            c_set_exception "PyExc_TypeError" msg range man flow >>% Eval.singleton (mk_zero range)
          else
            let rec process pos refs flow =
              let range = tag_range range "convert_single[%d]" pos in
              if pos < tuple_size then
                man.eval (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (tuple_addr, None) range) (mk_int pos ~typ:(Python.Ast.T_py None) range) range) flow >>$ fun obj flow ->
                convert_single man obj fmt_str.[pos] (List.hd refs) range flow >>$ fun ret flow ->
                if ret = 1 then process (pos+1) (List.tl refs) flow
                else Eval.singleton (mk_zero range) flow
              else
                Eval.singleton (mk_one range) flow
            in
            process 0 refs flow
        )
         |> OptionExt.return

      | E_c_builtin_call ("PyArg_ParseTupleAndKeywords", args::kwds::fmt::kwlist::refs) ->
         safe_get_name_of fmt man flow >>$ (fun ofmt_str flow ->
          let fmt_str = Top.top_to_exn (OptionExt.none_to_exn ofmt_str) in
          resolve_c_pointer_into_addr args man flow >>$ fun tuple_oaddr flow ->
          let tuple_addr = Top.detop @@ OptionExt.none_to_exn tuple_oaddr in
          len_of man tuple_addr flow range >>$ fun tuple_size flow ->
          resolve_c_pointer_into_addr kwds man flow >>$ fun dict_oaddr flow ->
          (match dict_oaddr with
          | None -> Cases.return 0 flow
          | Some (Nt dict_addr) -> len_of man dict_addr flow range
          | _ -> assert false ) >>$ fun dict_size flow ->
          let fmt_str, fmt_str_itv_lo, fmt_str_itv_hi = normalize_fmt_str fmt_str in
          let size = tuple_size + dict_size in
          debug "fmt_str_itv = [%d, %d]; size = (%d, %d)" fmt_str_itv_lo fmt_str_itv_hi tuple_size dict_size;
          if size < fmt_str_itv_lo || size > fmt_str_itv_hi then
            let msg = Format.asprintf "function takes %s %d argument%s (%d given)"
                        (if fmt_str_itv_hi = fmt_str_itv_lo then "exactly" else if size < fmt_str_itv_lo then "at least" else "at most")
                        (if size < fmt_str_itv_lo then fmt_str_itv_lo else fmt_str_itv_hi)
                        (let s =
                           if size < fmt_str_itv_hi then fmt_str_itv_lo
                           else fmt_str_itv_hi in
                         if s = 1 then "" else "s")
                        size in
            let () = debug "wrong number of arguments: %s" msg in
            c_set_exception "PyExc_TypeError" msg range man flow >>% Eval.singleton (mk_zero range)
          else
            (* check that |kwlist| = fmt_str_itv_hi ? *)
            (* start with process until size = tuple_size. Then keep index but use kwlist[index] to search for the correct argument in kwds *)
            let rec process pos nb_kwargs refs flow =
              debug "process %d %d" pos nb_kwargs;
              let range = tag_range range "convert_single[%d]" pos in
              if pos < tuple_size then
                let () = debug "in tuple" in
                man.eval (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (tuple_addr, None) range) (mk_int pos ~typ:(Python.Ast.T_py None) range) range) flow >>$ fun obj flow ->
                convert_single man obj fmt_str.[pos] (List.hd refs) range flow >>$ fun ret flow ->
                  if ret = 1 then process (pos+1) nb_kwargs (List.tl refs) flow
                  else
                    let () = debug "ZERO a" in
                    Eval.singleton (mk_zero range) flow
              else if pos < fmt_str_itv_hi (* should be |kwlist| *) && dict_oaddr <> None then
                safe_get_name_of (mk_c_subscript_access kwlist (mk_int pos range) range) man flow >>$ fun okw_name flow ->
                let kw_name = Top.top_to_exn (OptionExt.none_to_exn okw_name) in
                OptionExt.none_to_exn @@ Python.Utils.try_eval_expr man (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (Top.detop @@ OptionExt.none_to_exn dict_oaddr, None) range) (mk_string ~etyp:(T_py None) kw_name range) ~etyp:(T_py None) range) flow
                  ~route:(Semantic "Python")
                  ~on_empty:(fun _ _ _ flow ->
                    debug "haven't found anything for %s" kw_name;
                    process (pos+1) nb_kwargs (List.tl refs) flow |> OptionExt.return )
                  ~on_result:(fun arg_value flow ->
                    debug "found %a for %s" pp_expr arg_value kw_name;
                    convert_single man arg_value fmt_str.[pos] (List.hd refs) range flow >>$ fun ret flow ->
                    if ret = 1 then process (pos+1) (nb_kwargs-1) (List.tl refs) flow
                    else
                      let () = debug "ZERO b" in
                      Eval.singleton (mk_zero range) flow
                  )
              else
                if nb_kwargs > 0 then
                  let () = debug "ZERO c" in
                  c_set_exception "PyExc_TypeError" "invalid number of arguments" range man flow >>% Eval.singleton (mk_zero range)
                else
                  let () = debug "ok" in
                  Eval.singleton (mk_one range) flow
            in
            process 0 dict_size refs flow
        ) |> OptionExt.return

      | E_c_builtin_call ("PyArg_UnpackTuple", args::fname::minargs::maxargs::refs) ->
         (* rewritten into a call to PyArg_ParseTuple *)
         safe_get_name_of fname man flow >>$
           (fun ofname_str flow ->
             let fname_str = Top.top_to_exn (OptionExt.none_to_exn ofname_str) in
             let min_args = Z.to_int @@ OptionExt.none_to_exn @@ c_expr_to_z minargs in
             let additional_args  = (Z.to_int @@ OptionExt.none_to_exn @@ c_expr_to_z maxargs) - min_args in
             let fmt_str = mk_c_string (Format.asprintf "%s%s:%s"
                                        (String.make min_args 'O')
                                        (if additional_args > 0 then "|" ^ (String.make additional_args 'O') else "")
                                        fname_str) range in
             debug "fmt_str = %a@.~> %a" pp_expr fmt_str pp_expr {exp with ekind = E_c_builtin_call ("PyArg_ParseTuple", args::fmt_str::refs)};
             man.eval {exp with ekind = E_c_builtin_call ("PyArg_ParseTuple", args::fmt_str::refs)} flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("Py_BuildValue", fmt::refs) ->
         safe_get_name_of fmt man flow >>$
           (fun ofmt_str flow ->
             let fmt_str = Top.top_to_exn (OptionExt.none_to_exn ofmt_str) in
             build_value man flow range fmt_str refs >>$
               fun tuple flow ->
               if List.length tuple = 1 then
                 Eval.singleton (List.hd tuple) flow
               else
                 fold_c_to_python_boundary man range tuple flow >>$
                   fun py_tuple flow ->
                   man.eval (mk_expr ~etyp:(T_py None) (Python.Ast.E_py_tuple py_tuple) range) flow >>$
                     fun py_tuple flow ->
                     let addr_py_tuple, oe_py_tuple  = object_of_expr py_tuple in
                     let c_addr, flow = python_to_c_boundary addr_py_tuple None oe_py_tuple range man flow in
                     debug "Py_BuildValue: %a %a" pp_expr c_addr pp_typ c_addr.etyp;
                     man.eval c_addr flow
           )
         |> OptionExt.return


      | E_c_builtin_call ("PyObject_CallFunction", callable::fmt::refs) ->
         c_to_python_boundary callable man flow range >>$?
           (fun py_callable flow ->
             safe_get_name_of fmt man flow >>$?
               fun ofmt_str flow ->
               (* FIXME: fmt can be null, meaning no arguments *)
               let fmt_str = Top.top_to_exn (OptionExt.none_to_exn ofmt_str) in
               build_value man flow range fmt_str refs >>$?
                 fun tuple flow ->
                 fold_c_to_python_boundary man range tuple flow >>$?
                   fun py_tuple flow ->
                   (* FIXME: what happens if an exception is raised? *)
                   Python.Utils.try_eval_expr
                     man ~route:(Semantic "Python")
                     (Python.Ast.mk_py_call py_callable py_tuple range) flow
                     ~on_empty:(fun exc_exp exc_str exc_msg flow ->
                       (* exception raised on the python side:
                          let's put in the exc field, and return NULL *)
                       man.eval ~route:(Semantic "Python") (mk_py_type exc_exp range) flow >>$? fun exc_typ flow ->
                       let exc_addr, exc_oe = object_of_expr exc_typ in
                       let exc_msg = Universal.Strings.Powerset.StringPower.elements exc_msg in
                       let setstring msg =
                           man.exec
                             (mk_c_call_stmt
                                (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow)
                                [
                                  mk_addr exc_addr range;
                                  msg
                                ]
                                range) flow
                            >>%
                              man.eval (mk_c_null range) in
                       Eval.join_list ~empty:(fun () -> setstring (mk_c_null range))
                         (List.map (fun exc_msg -> setstring (mk_c_string exc_msg range)) exc_msg)
                       |> OptionExt.return
                     )
                     ~on_result:(fun py_call_res flow ->
                        debug "py_call_res %a" pp_expr py_call_res;
                        let addr_py_res, oe_py_res = object_of_expr py_call_res in
                        let c_addr, flow = python_to_c_boundary addr_py_res None oe_py_res range man flow in
                        debug "PyObject_CallFunction: %a %a" pp_expr c_addr pp_typ c_addr.etyp;
                        man.eval c_addr flow
                     )
           )

      | E_c_builtin_call ("PyObject_CallObject", [callable;arg]) ->
         c_to_python_boundary callable man flow range >>$? (fun py_callable flow ->
         c_to_python_boundary ~on_null:(fun flow -> man.eval ~route:(Semantic "Python") (mk_expr ~etyp:(T_py None) (E_py_tuple []) range) flow) arg man flow range >>$? fun py_arg flow ->
         (* FIXME: we're assuming py_arg is a tuple but we shoudl check *)
         let py_arg_vars = Python.Objects.Tuple.Domain.var_of_eobj py_arg in
         (* FIXME: also handle case where an exception is raised *)
         debug "calling object";
         Python.Utils.try_eval_expr man ~route:(Semantic "Python") (Python.Ast.mk_py_call py_callable (List.map (fun v -> mk_var v range) py_arg_vars) range) flow
           ~on_empty:(fun exc_exp exc_name exc_msg flow ->
                       (* exception raised on the python side:
                          let's put in the exc field, and return NULL *)
                       man.eval ~route:(Semantic "Python") (mk_py_type exc_exp range) flow >>$? fun exc_typ flow ->
                       let exc_addr, exc_oe = object_of_expr exc_typ in
                       let exc_msg = Universal.Strings.Powerset.StringPower.elements exc_msg in
                       let setstring msg =
                           man.exec
                             (mk_c_call_stmt
                                (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow)
                                [
                                  mk_addr exc_addr range;
                                  msg
                                ]
                                range) flow
                            >>%
                              man.eval (mk_c_null range) in
                       Eval.join_list ~empty:(fun () -> setstring (mk_c_null range))
                         (List.map (fun exc_msg -> setstring (mk_c_string exc_msg range)) exc_msg)
                       |> OptionExt.return
           )
           ~on_result:(fun py_call_res flow ->
             debug "calling object done, got %a" pp_expr py_call_res;
             let addr_py_res, oe_py_res = object_of_expr py_call_res in
             let c_addr, flow = python_to_c_boundary addr_py_res None oe_py_res range man flow in
             man.eval c_addr flow
           )
        )

      | E_c_builtin_call ("PyObject_RichCompareBool", [left; right; op]) ->
         c_to_python_boundary left man flow range >>$ (fun py_left flow ->
         c_to_python_boundary right man flow range >>$ fun py_right flow ->
         man.eval ~translate:"Universal" op flow >>$ fun u_op flow ->
         let op = match Bot.bot_to_exn @@ man.ask (Universal.Numeric.Common.mk_int_interval_query u_op) flow with
           | Finite l, Finite r when Z.compare l r = 0 -> Z.to_int l
           | _ -> assert false in
         let py_op = match op with
         | 0 -> O_lt
         | 1 -> O_le
         | 2 -> O_eq
         | 3 -> O_ne
         | 4 -> O_gt
         | 5 -> O_ge
         | _ -> assert false in
         assume ~route:(Semantic "Python") (mk_binop ~etyp:(T_py None) py_left py_op py_right range) man flow
           ~fthen:(Eval.singleton (mk_one range))
           ~felse:(Eval.singleton (mk_zero range))
           ~fboth:(fun f1 f2 -> Eval.singleton (mk_int_interval 0 1 range) (Flow.join man.lattice f1 f2))
        ) |> OptionExt.return

      | E_c_builtin_call ("PyObject_RichCompare", [left; right; op]) ->
         c_to_python_boundary left man flow range >>$ (fun py_left flow ->
         c_to_python_boundary right man flow range >>$ fun py_right flow ->
         man.eval ~translate:"Universal" op flow >>$ fun u_op flow ->
         let op = match Bot.bot_to_exn @@ man.ask (Universal.Numeric.Common.mk_int_interval_query u_op) flow with
           | Finite l, Finite r when Z.compare l r = 0 -> Z.to_int l
           | _ -> assert false in
         let py_op = match op with
         | 0 -> O_lt
         | 1 -> O_le
         | 2 -> O_eq
         | 3 -> O_ne
         | 4 -> O_gt
         | 5 -> O_ge
         | _ -> assert false in
         man.eval ~route:(Semantic "Python") (mk_binop ~etyp:(T_py None) py_left py_op py_right range) flow >>$ fun py_result flow ->
         let py_result_addr, py_result_oe = object_of_expr py_result in
         let c_addr, flow = python_to_c_boundary py_result_addr None py_result_oe range man flow in
         Eval.singleton c_addr flow
        ) |> OptionExt.return

      | E_c_builtin_call ("PyObject_IsTrue", [v]) ->
         c_to_python_boundary v man flow range >>$ (fun py_v flow ->
          assume ~route:(Semantic "Python") (Python.Utils.mk_builtin_call "bool" [py_v] range) man flow
            ~fthen:(Eval.singleton (mk_one range))
            ~felse:(Eval.singleton (mk_zero range))
        ) |> OptionExt.return

      (**
          Humf, tuples are immutable Python side but mutable on the C side.
          So we'll not reuse the python domain for PyTuple_New and PyTuple_SetItem.
          Additionnally, you can make some pretty buggy things if you call PyTuple_New and return the result to Python, and try and access it in Python... woops
          This applies to PyTuple_New and PyTuple_SetItem
       *)
      | E_c_builtin_call ("PyTuple_New", [size]) ->
         (* FIXME: the allocation can also fail *)
         let size = Z.to_int @@ OptionExt.none_to_exn @@ c_expr_to_z size in
         (* FIXME: what happens in the abstract if you py-getitem over pytuple_new? *)
         alloc_py_addr man (Python.Objects.Tuple.A_py_tuple size) range flow >>$ (fun tuple_eaddr flow ->
          let c_addr, flow = python_to_c_boundary (Addr.from_expr tuple_eaddr) None None range man flow in
          let els_var = Python.Objects.Tuple.Domain.var_of_addr (Addr.from_expr tuple_eaddr) in
          let post = List.fold_left (fun post vari ->
                         post >>% man.exec ~route:(Semantic "Python") (mk_add_var vari range)) (Post.return flow) els_var in
          post >>% Eval.singleton c_addr
        )
         |> OptionExt.return

      | E_c_builtin_call ("PyTuple_SetItem", [tuple;pos;item]) ->
         let pos = Z.to_int @@ OptionExt.none_to_exn @@ c_expr_to_z pos in
         c_to_python_boundary tuple man flow range >>$ (fun tuple_obj flow ->
         let tuple_vars = Python.Objects.Tuple.Domain.var_of_eobj tuple_obj in
         c_to_python_boundary item man flow range >>$ fun item_obj flow ->
         if (pos >= 0 && pos < List.length tuple_vars) then
           man.exec ~route:(Semantic "Python") (mk_assign (mk_var ~mode:(Some STRONG) (List.nth tuple_vars pos) range) item_obj range) flow >>%
             Eval.singleton (mk_zero range)
         else
           Eval.singleton (mk_int (-1) range) flow) |> OptionExt.return


      | E_c_builtin_call ("_PyRange_Size", [arg])
      | E_c_builtin_call ("PyList_Size", [arg])
      | E_c_builtin_call ("PyTuple_Size", [arg]) ->
         (* FIXME: check the type *)
         resolve_c_pointer_into_addr arg man flow >>$
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
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
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             let py_tuple = Python.Ast.mk_py_object (addr, None) range in
             (* NB: alternative to c_int_to_python would be to make a query directly? *)
             c_int_to_python pos man flow range >>$ fun py_obj flow ->
               debug "PyTuple_GetItem, py_pos = %a" pp_expr py_obj;
               (* FIXME: if an IndexError is raised? *)
               man.eval (Python.Ast.mk_py_call (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "tuple.__getitem__") range)
                           [py_tuple; py_obj] range) flow >>$
                 fun py_elem flow ->
                 let addr_py_elem, oe_py_elem = object_of_expr py_elem in
                 strongify_int_addr_hack addr_py_elem man range flow >>$ fun addr_py_elem flow ->
                 let c_addr, flow = python_to_c_boundary addr_py_elem None oe_py_elem range man flow in
                 debug "PyTuple_GetItem: %a %a" pp_expr c_addr pp_typ c_addr.etyp;
                 man.eval c_addr flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyTuple_GetSlice", [tuple; start; stop]) ->
         (
           debug "%a" pp_expr exp;
           resolve_c_pointer_into_addr tuple man flow >>$
             fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             let py_tuple = Python.Ast.mk_py_object (addr, None) range in
             c_int_to_python start man flow start.erange >>$
               fun py_start flow ->
               debug "py_start = %a" pp_expr py_start;
               c_int_to_python stop man flow stop.erange >>$
                 fun py_stop flow ->
                 debug "py_stop = %a" pp_expr py_stop;
                 man.eval
                   (mk_expr (Python.Ast.E_py_slice_subscript
                               (py_tuple, py_start, py_stop, Python.Ast.mk_py_none range))
                      range)
                   flow >>$
                   fun py_slice flow ->
                   debug "resulting slice: %a" pp_expr py_slice;
                   let addr_py_slice, oe_py_slice = object_of_expr py_slice in
                   let c_addr, flow = python_to_c_boundary addr_py_slice None oe_py_slice range man flow in
                   Eval.singleton c_addr flow
         )
         |> OptionExt.return

      | E_c_builtin_call ("PyList_New", [size]) ->
         (* FIXME: the allocation can also fail *)
         man.eval ~translate:"Universal" size flow >>$ (fun size flow ->
         (* FIXME: what happens in the abstract if you py-getitem over pytuple_new? *)
         alloc_py_addr man Python.Objects.Py_list.A_py_list range flow >>$ fun list_eaddr flow ->
          let c_addr, flow = python_to_c_boundary (Addr.from_expr list_eaddr) None None range man flow in
          let els_var = Python.Objects.Py_list.Domain.var_of_addr (Addr.from_expr list_eaddr) in
          let list_length = Python.Objects.Py_list.Domain.length_var_of_addr (Addr.from_expr list_eaddr) in
          man.exec ~route:(Semantic "Python") (mk_add_var els_var range) flow >>%
            man.exec ~route:(Semantic "Python") (mk_assign (mk_var list_length range) size range) >>%
            Eval.singleton c_addr
        )
         |> OptionExt.return

      | E_c_builtin_call ("PyList_GetItem", [list; pos]) ->
         resolve_c_pointer_into_addr list man flow >>$
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             let py_list = Python.Ast.mk_py_object (addr, None) (tag_range range "list") in
             (* NB: alternative to c_int_to_python would be to make a query directly? *)
             c_int_to_python pos man flow (tag_range range "pos") >>$ fun py_obj flow ->
               debug "PyList_GetItem, py_pos = %a" pp_expr py_obj;
               (* FIXME: if an IndexError is raised? *)
               man.eval (Python.Ast.mk_py_call (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "list.__getitem__") range)
                           [py_list; py_obj] range) flow >>$
                 fun py_elem flow ->
                 let addr_py_elem, oe_py_elem = object_of_expr py_elem in
                 strongify_int_addr_hack addr_py_elem man range flow >>$ fun addr_py_elemn flow ->
                 let c_addr, flow = python_to_c_boundary addr_py_elem None oe_py_elem range man flow in
                 debug "PyList_GetItem: %a %a" pp_expr c_addr pp_typ c_addr.etyp;
                 man.eval c_addr flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyList_SetItem", [list;pos;item]) ->
         resolve_c_pointer_into_addr list man flow >>$?
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             let py_list = Python.Ast.mk_py_object (addr, None) (tag_range range "list") in
             c_int_to_python pos man flow (tag_range range "pos") >>$? fun py_obj flow ->
             c_to_python_boundary item man flow range >>$? fun py_item flow ->
             Python.Utils.try_eval_expr man ~route:(Semantic "Python")
               (Python.Ast.mk_py_call (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "list.__setitem__") range)
                  [py_list; py_obj; py_item] range)
               flow
               ~on_empty:(fun exc_exp exc_name exc_msg flow ->
                 man.eval ~route:(Semantic "Python") (mk_py_type exc_exp range) flow >>$? fun exc_typ flow ->
                 let exc_addr, exc_oe = object_of_expr exc_typ in
                 let exc_msg = Universal.Strings.Powerset.StringPower.elements exc_msg in
                 let setstring msg =
                   man.exec
                     (mk_c_call_stmt
                        (C.Ast.find_c_fundec_by_name "PyErr_SetString" flow)
                        [
                          mk_addr exc_addr range;
                          msg
                        ]
                        range) flow
                   >>%
                     man.eval (mk_int (-1) range) in
                 Eval.join_list ~empty:(fun () -> setstring (mk_c_null range))
                   (List.map (fun exc_msg -> setstring (mk_c_string exc_msg range)) exc_msg)
                 |> OptionExt.return
               )
               ~on_result:(fun res flow ->
                 Eval.singleton (mk_int 0 range) flow)
           )

      | E_c_builtin_call ("_PyRange_GetItem", [rrange; pos]) ->
         resolve_c_pointer_into_addr rrange man flow >>$
           (fun oaddr flow ->
             let addr = Top.detop @@ OptionExt.none_to_exn oaddr in
             let py_range = Python.Ast.mk_py_object (addr, None) (tag_range range "range") in
             c_int_to_python pos man flow (tag_range range "pos") >>$ fun py_obj flow ->
             (* FIXME: ootb error handling *)
             man.eval (Python.Ast.mk_py_call (Python.Ast.mk_py_object (Python.Addr.find_builtin_function "range.__getitem__") range) [py_range; py_obj] range) flow >>$
               fun py_elem flow ->
               let addr_py_elem, oe_py_elem = object_of_expr py_elem in
               strongify_int_addr_hack addr_py_elem man range flow >>$ fun addr_py_elem flow ->
               let c_addr, flow = python_to_c_boundary addr_py_elem None oe_py_elem range man flow in
               man.eval c_addr flow
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyWeakref_NewRef", [refto; callback]) ->
         assume (eq callback (mk_c_null range) range)
           man flow
           ~fthen:(fun flow ->
             let post =
               match man.ask (Python.Desugar.Import.Q_python_addr_of_module "weakref") flow with
               | Some _ -> Post.return flow
               | None ->
                  let open Filename in
                  let weakrefimport, weakrefimport_channel = Filename.open_temp_file "weakrefimport" ".py" in
                  let weakref_fmt = Format.formatter_of_out_channel weakrefimport_channel in
                  Format.fprintf weakref_fmt "import weakref@.";
                  close_out weakrefimport_channel;
                  let weakref_import = mk_stmt (S_program (Python.Frontend.parse_program [weakrefimport], None)) range in
                  man.exec ~route:(Semantic "Python") weakref_import flow
             in
             post >>% fun flow ->
             let weakref = OptionExt.none_to_exn @@  man.ask (Python.Desugar.Import.Q_python_addr_of_module "weakref") flow in
             c_to_python_boundary refto man flow range >>$ fun py_refto flow ->
             let () = Debug.debug ~channel:"bug" "%a" (format @@ Flow.print man.lattice.print) flow in
             man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_attr (mk_py_object (weakref, None) range) "ref" range) [py_refto] range) flow >>$ fun py_weakref flow ->
             let addr_py_weakref, _ = object_of_expr py_weakref in
             let c_weakref, flow = python_to_c_boundary addr_py_weakref None None range man flow in
             Eval.singleton c_weakref flow
           )
           ~felse:(fun flow -> assert false)
         |> OptionExt.return

      | E_c_builtin_call ("PyWeakref_GetObject", [wkref]) ->
         c_to_python_boundary wkref man flow range >>$ (fun py_wkref flow ->
         man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_attr py_wkref "__call__" range) [] range) flow >>$ fun py_ref flow ->
         let addr_py_ref, oe_py_ref = object_of_expr py_ref in
         let c_ref, flow = python_to_c_boundary addr_py_ref None oe_py_ref range man flow in
         Eval.singleton c_ref flow)
         |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_c_function(name, uid, kind, oflags, self)}, _)}, args, kwargs) ->
         (* cf cfunction_call_varargs + _Py_CheckFunctionResult in call.c *)
         debug "%s: oflags = %a" name (OptionExt.print Format.pp_print_int) oflags;
         let ometh_varargs = Some 1 in
         let ometh_varargs_keywords = Some 3 in
         let ometh_o = Some 8 in
         let ometh_noargs = Some 4 in
         (* FIXME: if oflag = Some METH_O, we can forget the tuple *)
         (* FIXME: use the equiv map *)
         let cfunc = find_c_fundec_by_uid uid flow in
         let args_types = List.map vtyp cfunc.c_func_parameters in
         (* Call it with self, E_py_tuple(args) *)
         let subst_addr_eobj a' e = match ekind e with
           | E_py_object(a, oe) -> {e with ekind = E_py_object(a', oe)}
           | _ -> assert false in
         (match (fst self).addr_kind with
          | A_py_c_class v -> Eval.singleton (C.Ast.mk_c_address_of (mk_var v range) range) flow
          | _ ->
             strongify_int_addr_hack (fst self) man (tag_range range "self") flow >>$ fun addr_self flow ->
             let c_addr, flow = python_to_c_boundary addr_self (Some (List.hd args_types)) (snd self) range man flow in
             Eval.singleton c_addr flow) >>$ (fun self flow ->
         (match kind with
           | Builtin_function_or_method ->
             Cases.return (self, args) flow
           | Wrapper_descriptor _ | Method_descriptor ->
              strongify_int_addr_hack (fst @@ object_of_expr @@ List.hd args) man (tag_range range "_descr") flow >>$ fun fst_arg flow ->
              let c_addr, flow = python_to_c_boundary fst_arg None (snd @@ object_of_expr @@ List.hd args) range man flow in
              Cases.return (subst_addr_eobj (Addr.from_expr c_addr)  (List.hd args), List.tl args) flow) >>$ fun (self, args) flow ->
         (if oflags = None || Stdlib.compare ometh_varargs oflags = 0 || Stdlib.compare ometh_varargs_keywords oflags = 0 then
           Eval.singleton (mk_expr ~etyp:(T_py None) (E_py_tuple args) (tag_range range "args assignment" )) flow
         else if Stdlib.compare ometh_o oflags = 0 then
           let () = debug "METH_O, keeping only first argument" in
           if List.length args > 0 then
             Eval.singleton (List.hd args) flow
           else
             man.exec (Python.Utils.mk_builtin_raise_msg "TypeError" (Format.asprintf "%s takes exactly one argument (%d given)" name (List.length args)) range) flow >>% Eval.empty
         else if Stdlib.compare ometh_noargs oflags = 0 then
            let () = debug "METH_NOARGS, hopefully caught later on?" in
            if List.length args = 0 then
              Eval.singleton (mk_c_null range) flow
            else
             man.exec (Python.Utils.mk_builtin_raise_msg "TypeError" (Format.asprintf "%s takes no argument (%d given)" name (List.length args)) range) flow >>% Eval.empty
         else
           let () = debug "oflags = %a" (OptionExt.print Format.pp_print_int) oflags in
           assert false
         ) >>$ (fun py_args flow ->
         debug "%s, self is %a, args: %a" name pp_expr self pp_expr py_args;
         let py_kwds =
           if Stdlib.compare ometh_varargs_keywords oflags = 0 then
             let keys, values = List.map (fun (so, e) -> mk_string ~etyp:(T_py None) (OptionExt.none_to_exn so) range, e) kwargs |> List.split in
             mk_expr ~etyp:(T_py None) (E_py_dict (keys, values)) (tag_range range "kwargs assignment")
           else
             (* FIXME: okay, lets cheat here *)
             mk_c_null range
         in
         (* FIXME: if |args| = 1, no need to eval py args *)
         man.eval py_args flow >>$ (fun py_args flow ->
           man.eval py_kwds flow >>$ fun py_kwds flow ->
           let cfunc_args =
               match List.length cfunc.c_func_parameters with
               | 1 -> [self]
               | 2 ->
                  self ::
                        {py_args with etyp = (List.nth args_types 1)} :: []
               | 3 -> self ::
                        {py_args with etyp = (List.nth args_types 1)}
                        :: py_kwds :: []
               | _ -> assert false in
             let call = match kind with
               | Wrapper_descriptor (Some wrapper_name) ->
                  (* wrapperdescr_call in descrobject.c *)
                  let wrapper = C.Ast.find_c_fundec_by_name wrapper_name flow in
                  debug "wrapper %s has %d args@.cfunc_args = %a" wrapper_name (List.length wrapper.c_func_parameters) (Format.pp_print_list pp_expr) cfunc_args;
                  let wrapper_args =
                    match List.length wrapper.c_func_parameters with
                    | 2 ->
                       let self = List.hd cfunc_args in
                       self ::
                         (mk_c_address_of (mk_expr (E_c_function cfunc) range ~etyp:(mk_c_fun_typ cfunc)) range) :: []
                    | (3 | 4) as count ->
                       let self, args, kwds_or_nothing = match cfunc_args with
                         | [a] -> a, mk_c_null range, []
                         | [a;b] -> a,b,[]
                         | a::b::c -> a, b, c
                         | _ -> assert false in
                       self :: args ::
                         (mk_c_address_of (mk_expr (E_c_function cfunc) range ~etyp:(mk_c_fun_typ cfunc)) range) ::
                           (if count = 3 then [] else kwds_or_nothing)
                    | _ -> assert false
                  in
                  mk_c_call wrapper wrapper_args range
               | _ ->
                  mk_c_call cfunc cfunc_args range
             in
             let caller, args = match ekind call with
               | E_call (caller, args) -> caller, args
               | _ -> assert false in
             let revargs, flow =
               List.fold_left (fun (args, flow) arg ->
                   match ekind arg with
                   | E_addr (a, _) ->
                      debug "[%s] applying boundary on %a" name pp_addr a;
                      let addr_typ = match etyp arg with | T_addr -> None | t -> Some t in
                      let c_arg, flow = python_to_c_boundary a addr_typ None arg.erange man flow in
                      {c_arg with etyp = etyp arg}::args, flow
                   | E_py_object (a, oe) ->
                      debug "[%s] applying boundary on (%a, %a)" name pp_addr a (OptionExt.print pp_expr) oe;
                      let addr_typ = match etyp arg with | T_py _ -> None | t -> Some t in
                      let c_arg, flow = python_to_c_boundary a addr_typ oe arg.erange man flow in
                      c_arg::args, flow
                   | _ -> arg::args, flow
                 )
                 ([], flow)
               args in
             let call = {call with ekind = E_call(caller, List.rev revargs)} in
             let open C.Common.Points_to in
             debug "call = %a" pp_expr call;
             man.eval ~route:(Semantic "C") call flow >>$
               fun call_res flow ->
               debug "call result %s %a" name pp_expr call_res;
               c_to_python_boundary ~safe_check:(Some Python.Alarms.CHK_PY_SYSTEMERROR) call_res man flow range
                 ~on_null:(fun flow -> check_consistent_null cfunc.c_func_org_name man flow range)
         )
         )
         )
         |> OptionExt.return

      (** member descriptors: attr get/set on descriptors defined in C classes *)
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("member_descriptor.__get__", "wrapper_descriptor"))}, _)},
                   member_descr_instance ::
                     inst ::
                       cls_inst :: [],
                   kwargs) ->
         (* FIXME: reuse the Py/C call machinery+checks above *)
         debug "member_get";
         let cfunc = find_c_fundec_by_name "member_get" flow in
         let addr_of_object ?(etyp=T_addr) x = mk_addr ~etyp:etyp (addr_of_object @@ object_of_expr x) range in
         let descr_typ, inst_typ, cls_typ = match cfunc.c_func_parameters with
           | [a;b;c] -> vtyp a, vtyp b, vtyp c
           | _ -> assert false in
         let c_cls_inst = py_addr_to_c_expr (fst @@ object_of_expr cls_inst) cls_typ range man flow in
         let addr_inst = addr_of_object ~etyp:inst_typ inst in
         let c_descriptor = py_addr_to_c_expr (fst @@ object_of_expr member_descr_instance) (under_type descr_typ) range man flow in
         let cfunc_args = [c_descriptor; addr_inst; c_cls_inst] in
         let call = mk_c_call cfunc cfunc_args range in
         debug "[%a] calling %a" pp_expr exp pp_expr call;
         (
           c_to_python_boundary ~safe_check:(Some Python.Alarms.CHK_PY_SYSTEMERROR) call man flow range
             ~on_null:(fun flow -> check_consistent_null cfunc.c_func_org_name man flow range)
         )
         |> OptionExt.return


      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("member_descriptor.__set__", "wrapper_descriptor"))}, _)},
                   member_descr_instance ::
                     ({ekind = E_py_object ({addr_kind = A_py_instance _}, _)} as inst) ::
                       value :: [],
                   kwargs) ->
         let cfunc = find_c_fundec_by_name "PyMember_SetOne" flow in
         let descr_typ, inst_typ, value_typ = match cfunc.c_func_parameters with
           | [a;b;c] -> vtyp a, vtyp b, vtyp c
           | _ -> assert false in
         let addr_of_eobject x = addr_of_object @@ object_of_expr x in
         let addr_inst = addr_of_eobject inst in
         let addr_value = addr_of_eobject value in
         let c_descriptor = py_addr_to_c_expr (fst @@ object_of_expr member_descr_instance) (under_type descr_typ) range man flow in
         strongify_int_addr_hack addr_inst man (tag_range range "addr_inst") flow >>$ (fun addr_inst flow ->
         strongify_int_addr_hack addr_value man  (tag_range range "addr_value") flow >>$ fun addr_value flow ->
         let c_addr_inst, flow = python_to_c_boundary addr_inst (Some inst_typ) (snd @@ object_of_expr inst) range man flow in
         let c_addr_value, flow = python_to_c_boundary addr_value (Some value_typ) (snd @@ object_of_expr value) range man flow in
         let cfunc_args = [c_addr_inst; c_descriptor; c_addr_value] in
         let call = mk_c_call cfunc cfunc_args range in
         (* FIXME: boundary for c_descriptor? *)
         assume (mk_binop ~etyp:T_c_bool call O_ge (mk_zero range) range) man flow
           ~fthen:(fun flow ->
             (* FIXME: safe check here? *)
             man.eval (mk_py_none range) flow
           )
           ~felse:(fun flow ->
             debug "not zero in:@.%a" (format @@ Flow.print man.lattice.print) flow;
               check_consistent_null cfunc.c_func_org_name man flow range
           )
         )
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      (* FIXME: propagate it to the value attribute *)
      | S_fold ({ekind = E_addr (dst, _)}, [{ekind = E_addr (src, _)}])
      | S_rename ({ekind = E_addr (src, _)}, {ekind = E_addr (dst, _)}) when is_py_addr src && is_py_addr dst ->
         let cur = get_env T_cur man flow in
         (* change cur on both sides *)
         (* NB: we should remove points_to base_addr/addr bindings in favor of a set for those. This would simplify this step too *)
         let ncur, c_rename_needed =
           let points_to_src = find_inverse src cur in
           let cur = remove_inverse src cur in
           match points_to_src with
           | Top.TOP ->
              assert(cur = EquivBaseAddrs.top);
              EquivBaseAddrs.top, true
           | Nt pt ->
              let src_pt = mk_c_points_to_bloc (C.Common.Base.mk_addr_base src) (mk_zero range) None in
              if KeySet.mem src_pt pt then
                let dst_pt = mk_c_points_to_bloc (C.Common.Base.mk_addr_base dst) (mk_zero range) None in
                let pt' = KeySet.add dst_pt (KeySet.remove src_pt pt) in
                add_inverse dst pt' cur, true
              else
                add_inverse dst pt cur, false
         in
         let flow = set_env T_cur ncur man flow in
         (* delegate to C if needed *)
         (if c_rename_needed then
            let post = man.exec ~route:(Semantic "C") stmt flow in
            let exec_value src dst =
              match skind stmt with
              | S_rename _ -> mk_rename src dst
              | S_fold _ -> mk_fold dst [src]
              | _ -> assert false in
            match akind src with
            | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
               post >>% man.exec (exec_value (mk_avalue_from_pyaddr src T_int range) (mk_avalue_from_pyaddr dst T_int range) range)
            | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
               post >>% man.exec (exec_value (mk_avalue_from_pyaddr src (T_float F_DOUBLE) range) (mk_avalue_from_pyaddr dst (T_float F_DOUBLE) range) range)
            | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)}
            | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
               post >>% man.exec (exec_value (mk_avalue_from_pyaddr src T_string range) (mk_avalue_from_pyaddr dst T_string range) range)
            | _ -> post
          else
            Post.return flow) >>%
           (* delegate to python *)
           man.exec ~route:(Semantic "Python") stmt
         |> OptionExt.return

      | _ -> None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()
    let print_state printer a =
      pprint ~path:[Key "C/Python equivalence"] printer (pbox EquivBaseAddrs.print a)

    let merge _ _ _ = assert false
  end


let () = register_standard_domain(module Domain)
