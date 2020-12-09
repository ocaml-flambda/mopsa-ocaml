(* Truc commun au multilangage: addr du tas/variable/fonction
   Côté C: manipuler des addr plutôt que des bases (il y aura des addr de fonctions/variables/...)
           Var -> Valeur ~> Addr -> Valeur
*)
(* Intercepts call to PyModule_Create and PyModule_AddObject *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open C.Ast
open C.Common.Points_to

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
                                type t = C.Common.Points_to.points_to
                                let compare p1 p2 =
                                  match p1, p2 with
                                  | P_fun f1, P_fun f2 ->
                                     compare f1.C.Ast.c_func_unique_name f2.C.Ast.c_func_unique_name
                                  | P_block (b1, o1, m1), P_block (b2, o2, m2) ->
                                     Compare.compose [
                                         (fun () -> C.Common.Base.compare_base b1 b2);
                                         (* FIXME: hack... (fun () -> compare_expr o1 o2); *)
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

    let init _ man flow =
      List.iter (fun a -> Hashtbl.add C.Common.Builtins.builtin_functions a ())
        [
          "PyModule_Create2";
          "PyModule_AddObject";
          "PyType_Ready";
          "PyType_GenericAlloc_Helper";
          "PyArg_ParseTuple"
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

    let set_singleton p a man flow =
      let r = get_env T_cur man flow |>
                set p (Nt (ValueSet.singleton a)) in
      set_env T_cur r man flow

    let addr_of_exp e =
      match ekind e with
      | E_addr a -> a
      | _ -> assert false

    (* Creates stmt binder_addr · obj_name = obj_addr *)
    let mk_bind_in_python binder_addr obj_name obj_addr range =
      mk_assign
        (Python.Ast.mk_py_attr
           (Python.Ast.mk_py_object (binder_addr, None) range)
           obj_name ~etyp:(Python.Ast.T_py None) range)
        (Python.Ast.mk_py_object (obj_addr, None) range)
      range



    let new_method_from_def binder_addr methd man flow =
      let range = erange methd in
      get_name_of (mk_c_member_access_by_name methd "ml_name" range) man flow >>$
        fun methd_name flow ->
        resolve_pointer (mk_c_member_access_by_name methd "ml_meth" range) man flow >>$
          fun methd_function flow ->
          let methd_fundec = match methd_function with
            | P_fun f -> f
            | _ -> assert false in
          man.eval (mk_alloc_addr (Python.Addr.A_py_c_function (methd_fundec.c_func_org_name, methd_fundec.c_func_uid, "builtin_function_or_method", (binder_addr, None))) range) flow >>$
            fun methd_eaddr flow ->
            let methd_addr = addr_of_exp methd_eaddr in
            let flow = set_singleton methd_function methd_addr man flow in
            (* bind method to binder *)
            man.exec (mk_bind_in_python binder_addr methd_name methd_addr range) flow



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
          let m_addr = addr_of_exp module_addr in
          let flow = post_to_flow man @@ man.exec (mk_add module_addr range) flow  in
          let flow = set_singleton (mk_c_points_to_bloc (C.Common.Base.mk_addr_base m_addr) (mk_zero range) None) m_addr man flow in
          let add_method pos flow =
            man.eval (mk_c_subscript_access
                        (mk_c_arrow_access_by_name expr "m_methods" range)
                        (mk_int pos range)
                        range) flow >>$
              (fun methd flow ->
                new_method_from_def m_addr methd man flow)
            |> post_to_flow man
          in
          let rec process_methods c flow  =
            try
              process_methods (c+1) (add_method c flow)
            with Null_found ->
              flow in
          let flow = process_methods 0 flow in
          Eval.singleton module_addr flow

    let resolve_c_pointer_into_addr expr man flow =
      resolve_pointer expr man flow >>$
        (fun points_to flow ->
          debug "searching for %a" pp_points_to points_to;
          let aset = Top.detop @@ find points_to (get_env T_cur man flow) in
          debug "got %a" (ValueSet.fprint SetExt.printer_default pp_addr) aset;
          assert (ValueSet.cardinal aset = 1);
          Cases.singleton (ValueSet.choose aset) flow
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
                 let fun_addr = addr_of_exp fun_eaddr in
                 let flow = set_singleton func fun_addr man flow in
                 man.exec
                   (mk_bind_in_python cls_addr name fun_addr range)
                   flow)
             |> post_to_flow man

          | _ -> flow)



    let new_class_from_def ecls man flow =
      let range = erange ecls in
      resolve_pointer ecls man flow >>$
        fun cls flow ->
        let cls_var = match cls with
          | P_block ({base_kind = Var v}, _, _) -> v
          | _ -> assert false in
        man.eval (mk_alloc_addr (Python.Addr.A_py_c_class cls_var) range) flow >>$
          fun cls_eaddr flow ->
          let cls_addr = addr_of_exp cls_eaddr in
          let flow = set_singleton cls cls_addr man flow in
          (* fill dict with methods, members, getset
             ~> by delegation to the dictionnary/structural type abstraction *)
          bind_function_in "__new__" (mk_c_arrow_access_by_name ecls "tp_new" range) cls_addr "builtin_function_or_method" man flow >>% fun flow ->
          (* FIXME: wrapper around init since it returns an integer *)
          bind_function_in "__init__" (mk_c_arrow_access_by_name ecls "tp_init" range) cls_addr "wrapper_descriptor" man flow >>% fun flow ->
          (* FIXME: *)
          resolve_pointer (mk_c_arrow_access_by_name ecls "tp_methods" range) man flow >>$
            fun tp_methods flow ->
            let flow = Flow.add_local_assumption (A_cpython_unsupported_fields "tp_members, tp_methods, ...") range flow in
            Cases.singleton cls_addr flow



    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      (* FIXME: PyModule_Create is a macro expanded into PyModule_Create2. Maybe we should have a custom .h file *)
      | E_c_builtin_call ("PyModule_Create2", [module_decl; _]) ->
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
                 man.exec
                   (mk_bind_in_python module_addr obj_name obj_addr range)
                   flow >>= fun flow ->
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
           (* FIXME: should we use the range from where the allocation is performed to help the recency? *)
           man.eval (mk_alloc_addr (Python.Addr.A_py_instance cls_addr) range) flow >>$
             fun inst_eaddr flow ->
             let inst_addr = addr_of_exp inst_eaddr in
             let bytes = C.Cstubs.Aux_vars.mk_bytes_var inst_addr in
             man.exec (mk_add_var bytes range) flow >>%
               man.exec (mk_assign (mk_var bytes range) (List.hd @@ List.tl args) range) >>%
               man.exec (mk_add inst_eaddr range) >>%
               Eval.singleton inst_eaddr
               (* No need to put this into the equiv, since it's the same thing? *)
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyArg_ParseTuple", args::fmt::refs) ->
         get_name_of fmt man flow >>$
           (fun fmt_str flow ->
             match fmt_str with
             | "O" ->
                debug "%a" (format @@ Flow.print man.lattice.print) flow;
                (* FIXME: I guess this should be in the equivalence map *)
                resolve_pointer args man flow >>$
                  (fun args_points_to flow ->
                  match args_points_to with
                  | P_block ({base_kind = Addr addr}, _, _) ->
                     man.eval (Python.Ast.mk_py_index_subscript (Python.Ast.mk_py_object (addr, None) range) (mk_zero ~typ:(Python.Ast.T_py None) range) range) flow >>$
                       (fun obj flow ->
                       match ekind obj, ekind (List.hd refs)  with
                       | Python.Ast.E_py_object(addr, _), E_c_address_of c ->
                          man.exec (mk_assign c (mk_addr addr range) range) flow >>%
                            Eval.singleton (mk_one range)
                       | _ -> assert false
                       )
                  | _ -> assert false
                  )
             | _ ->
                panic_at range "TODO: implement PyArg_ParseTuple %s@.%a" fmt_str (format @@ Flow.print man.lattice.print) flow
           )
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
