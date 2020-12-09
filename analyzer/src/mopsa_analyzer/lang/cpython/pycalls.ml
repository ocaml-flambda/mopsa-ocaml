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
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_c_function(name, uid, kind, self)}, _)}, args, kwargs) ->
         (* Find the c function with uid in the context *)
         let cfunc = find_c_fundec_by_uid uid flow in
         let self_typ, args_typ, got_kwds = match cfunc.c_func_parameters with
           | a::b::[] -> vtyp a, vtyp b, false
           | a::b::_::[] ->
              warn "ignoring keywords";
              vtyp a, vtyp b, true
           | _ -> assert false in
         (* Call it with self, E_py_tuple(args) *)
         let self =
           (* woops, don't forget to convert back to C like values
              FIXME
              and that also applies to py_args? :/ *)
           match (fst self).addr_kind with
           | A_py_c_class v -> C.Ast.mk_c_address_of (mk_var v range) range
           | _ -> Universal.Ast.mk_addr (fst self) ~etyp:self_typ range in
         let self, args =
           match kind with
           | "builtin_function_or_method" -> self, args
           | "wrapper_descriptor" -> Universal.Ast.mk_addr (fst @@ object_of_expr @@ List.hd args) range, List.tl args
           | _ -> panic_at range "Call of c function of kind %s unsupported" kind
         in
         let py_args = mk_expr ~etyp:(T_py None) (E_py_tuple args) (tag_range range "args assignment" ) in
         debug "%s, self is %a, args: %a@.%a" name pp_expr self pp_expr py_args (format @@ Flow.print man.lattice.print) flow;
         let py_kwds = (* okay, lets cheat here *)
           (* mk_expr ~etyp:(T_py None) (E_py_tuple []) (tag_range range "kwds") *)
           mk_c_null range
         in
         man.eval py_args flow >>$
           (fun py_args flow ->
             let addr_py_args = addr_of_object @@ object_of_expr py_args in
             let args = Universal.Ast.mk_addr addr_py_args ~etyp:args_typ range in
             let cfunc_args = if got_kwds then [self; args; py_kwds] else [self; args] in
             let call = mk_c_call cfunc cfunc_args range in
             let open C.Common.Points_to in
             man.eval call flow >>$
               fun call_res flow ->
               match cfunc.c_func_return with
               | T_c_integer _ ->
                  debug "got an integer, probably an init";
                  assert (String.sub name (String.length name - 5) 5 = "_init");
                  man.eval (mk_py_none range) flow
               | _ ->
                  debug "call result %a@.%a" pp_expr call_res (format @@ Flow.print man.lattice.print) flow;
                  resolve_pointer call_res man flow >>$
                    fun result flow ->
                    (* FIXME: if return is integer, it's probably an _init function, let's return None *)
                    (* FIXME: refactor into `py_addr_of_c_pyobj` *)
                    (* FIXME: If the result is null, check that an exception has been raised? *)
                    match result with
                    | P_block ({base_kind = Addr a}, _, _) ->
                       Eval.singleton (mk_py_object (a, None) range) flow
                    | P_block ({base_kind = Var result_var}, _, _) ->
                       (* let's assume it's a class declaration...
                        * we should ask cmodule's state for the address matching that...
                  if it's a builtin, maybe we should still create the allocation. Or find the corresponding class *)
                       man.eval (Universal.Ast.mk_alloc_addr (Python.Addr.A_py_c_class result_var) range) flow >>$
                         (* FIXME: if we get PyType_Type, we'd like to merge this into the "type" builtin of Python. Reduction? *)
                         fun result_addr flow ->
                         let result_addr = match ekind result_addr with
                           | Universal.Ast.E_addr a -> a
                           | _ -> assert false in
                         Eval.singleton (mk_py_object (result_addr, None) range) flow


               | _ -> panic_at range "result = %a@.%a" pp_points_to result (format @@ Flow.print man.lattice.print) flow
           )
         |> OptionExt.return

      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
