(* Intercepts call to PyModule_Create and PyModule_AddObject *)


(* PyModule_AddObject(...) *)


open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open C.Ast
open C.Common.Points_to

module Domain =
  struct

    include GenStatelessDomainId(
                struct
                  let name = "cpython.cmodule"
                end)

    let checks = []

    let init _ _ flow =
      List.iter (fun a -> Hashtbl.add C.Common.Builtins.builtin_functions a ())
        [
          "PyModule_Create2";
          "PyModule_AddObject"
        ];
      flow

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

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      (* FIXME: PyModule_Create is a macro expanded into PyModule_Create2. Maybe we should have a custom .h file *)
      | E_c_builtin_call ("PyModule_Create2", [module_decl; _]) ->
         (* PyModule_Create(mod):
            1) Allocate A_py_c_module of mod->m_name (@_m)
            2) Forall meth = methods in mod->m_methods:
               n = meth->ml_name
               a) allocate them? A_py_c_function(n) -> @_f
               b) execute @_m·n = @_f
          *)
         get_name_of (mk_c_arrow_access_by_name module_decl "m_name" range) man flow >>$
           (fun module_name flow ->
             man.eval (mk_alloc_addr (Python.Addr.A_py_c_module module_name) range) flow >>$
                 fun module_addr flow ->
                 let m_addr = match ekind module_addr with
                   | E_addr a -> a
                   | _ -> assert false in
                 let flow = post_to_flow man @@ man.exec (mk_add module_addr range) flow in

                 let add_method pos flow =
                   man.eval (mk_c_subscript_access
                               (mk_c_arrow_access_by_name module_decl "m_methods" range)
                               (mk_int pos range)
                               range) flow >>$
                     (fun methd flow ->
                     get_name_of (mk_c_member_access_by_name methd "ml_name" range) man flow >>$ (* FIXME handle NULL *)
                       fun methd_name flow ->
                       resolve_pointer (mk_c_member_access_by_name methd "ml_meth" range) man flow >>$
                         fun methd_function flow ->
                         let methd_fundec = match methd_function with
                           | P_fun f -> f
                           | _ -> assert false  in
                         man.eval (mk_alloc_addr (Python.Addr.A_py_c_function (methd_fundec.c_func_org_name, methd_fundec.c_func_uid)) range) flow >>$
                         fun methd_addr flow ->
                         let methd_addr = match ekind methd_addr with
                           | E_addr a -> a
                           | _ -> assert false in
                         man.exec
                           (mk_assign
                              (Python.Ast.mk_py_attr
                                 (Python.Ast.mk_py_object (m_addr, None) range)
                                 methd_name ~etyp:(Python.Ast.T_py None) range)
                              (Python.Ast.mk_py_object (methd_addr, None) range)
                              range) flow)
                           |> post_to_flow man
                 in

                 let rec process_methods c flow  =
                   try
                     process_methods (c+1) (add_method c flow)
                   with Null_found ->
                     flow in
                 process_methods 0 flow |>
                   Eval.singleton module_addr
           )
         |> OptionExt.return

      | E_c_builtin_call ("PyModule_AddObject", [module_object; obj_name; obj]) ->
         resolve_pointer module_object man flow >>$
           (
             fun module_object flow ->
             let module_addr = match module_object with
               | P_block ({base_kind = Addr ({addr_kind = Python.Addr.A_py_c_module _} as a)}, _, _) -> a
               | _ -> assert false in
             get_name_of obj_name man flow >>$
               fun obj_name flow ->
               resolve_pointer obj man flow >>$
                 fun obj flow ->
                 let obj_var = match obj with
                   | P_block ({base_kind = Var v}, _, _) -> v
                   | _ -> assert false in

                 man.eval (mk_alloc_addr (Python.Addr.A_py_c_class obj_var) range) flow >>$
                   fun obj_addr flow ->
                   let obj_addr = match ekind obj_addr with
                     | E_addr a -> a
                     | _ -> assert false in

                   man.exec
                     (mk_assign
                        (Python.Ast.mk_py_attr
                           (Python.Ast.mk_py_object (module_addr, None) range)
                           obj_name ~etyp:(Python.Ast.T_py None) range)
                        (Python.Ast.mk_py_object (obj_addr, None) range)
                        range
                     )
                     flow >>= fun flow ->
                   Eval.singleton (mk_zero range)
           )
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()
  end


let () = register_stateless_domain(module Domain)
