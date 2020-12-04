open Mopsa
open Sig.Abstraction.Stateless
open Python.Ast
open Python.Addr
open C.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "cpython.pycalls"
      end)

    let checks = []

    let init _ _ flow = flow


    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_c_function(name, uid, self)}, _)}, args, kwargs) ->
         (* Find the c function with uid in the context *)
         let cfunc = find_c_fundec_by_uid uid flow in
         let self_typ, args_typ = match cfunc.c_func_parameters with
           | a::b::[] -> vtyp a, vtyp b
           | _ -> assert false in
         (* Call it with self, E_py_tuple(args) *)
         let self = Universal.Ast.mk_addr (fst self) ~etyp:self_typ range in
         let py_args = mk_expr ~etyp:(T_py None) (E_py_tuple args) range in
         man.eval py_args flow >>$
           (fun py_args flow ->
             let addr_py_args = addr_of_object @@ object_of_expr py_args in
             let args = Universal.Ast.mk_addr addr_py_args ~etyp:args_typ range in
             let call = mk_c_call cfunc [self; args] range in
             let open C.Common.Points_to in
             resolve_pointer call man flow >>$
               fun result flow ->
               (* FIXME: refactor into `py_addr_of_c_pyobj` *)
               (* FIXME: If the result is null, check that an exception has been raised? *)
               let result_var = match result with
                 | P_block ({base_kind = Var v}, _, _) -> v
                 | _ -> panic_at range "result = %a@.%a" pp_points_to result (format @@ Flow.print man.lattice.print) flow in
               man.eval (Universal.Ast.mk_alloc_addr (Python.Addr.A_py_c_class result_var) range) flow >>$
                 (* FIXME: if we get PyType_Type, we'd like to merge this into the "type" builtin of Python. Reduction? *)
                 fun result_addr flow ->
                 let result_addr = match ekind result_addr with
                   | Universal.Ast.E_addr a -> a
                   | _ -> assert false in
                 Eval.singleton (mk_py_object (result_addr, None) range) flow
           )
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
