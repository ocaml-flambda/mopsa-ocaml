(* Intercepts call to PyModule_Create and PyModule_AddObject *)


(* PyModule_AddObject(...) *)


open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open C.Ast

module Domain =
  struct

    include GenStatelessDomainId(
                struct
                  let name = "cpython.cmodule"
                end)

    let checks = []

    let init _ _ flow = flow

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
         let open C.Common.Points_to in
         man.eval module_decl flow >>$
           (fun module_decl flow ->
             let pymoduledef = match etyp module_decl with
               | T_c_pointer (T_c_record r) -> r
               | _ -> assert false in
             let pymodule_field f = List.find (fun r -> r.c_field_org_name = f) pymoduledef.c_record_fields in
             let name_field = pymodule_field "m_name" in
             let methods_field = pymodule_field "m_methods" in
             resolve_pointer (mk_c_arrow_access module_decl name_field range) man flow >>$
               fun points_to flow ->
               let module_name = match points_to with
                 | P_block({base_kind = String (s, _, _)}, offset,_) ->
                    (* FIXME: assert offset is [0, 0] *)
                    s
                 | _ -> assert false in
               man.eval (mk_alloc_addr (Python.Addr.A_py_c_module module_name) range) flow >>$
                 fun module_addr flow ->
                 let m_addr = match ekind module_addr with
                   | E_addr a -> a
                   | _ -> assert false in
                 let flow = post_to_flow man @@ man.exec (mk_add module_addr range) flow in
                 resolve_pointer
                   (mk_c_subscript_access
                      (mk_c_arrow_access module_decl methods_field range)
                      (mk_one range)
                      range)
                   man flow >>$
                   fun methods flow ->
                   panic_at range "methods[1] : %a@.%a" pp_points_to methods (format @@ Flow.print man.lattice.print) flow
           )
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()
  end


let () = register_stateless_domain(module Domain)
