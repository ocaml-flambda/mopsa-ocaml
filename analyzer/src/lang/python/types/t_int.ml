open Framework.Essentials
open Ast
open Universal.Ast
open MapExt
open Addr
(* gérer les appels sur int + constantes *)

module Domain =
  struct
    type _ domain += D_python_types_t_int : unit domain

    let id = D_python_types_t_int
    let name = "python.types.t_int"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_types_t_int -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, any_zone]; import = []}

    let init _ _ _ = None

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_constant (C_top T_bool)
      | E_constant (C_bool _) ->
         Eval.singleton (mk_expr (Typing.E_get_type_partition (Typingdomain.builtin_inst "bool")) range) flow |> OptionExt.return

      | E_constant (C_int _) ->
         Eval.singleton (mk_expr (Typing.E_get_type_partition (Typingdomain.builtin_inst "int")) range) flow |> OptionExt.return

      (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_compare_op_fun f ->
         Eval.eval_list [e1; e2] man.eval flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume
                 (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [e1; mk_py_object (Addr.find_builtin "int") range] range)
                 ~fthen:(fun true_flow ->
                   Eval.assume
                     (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [e2; mk_py_object (Addr.find_builtin "int") range] range)
                     ~fthen:(fun true_flow ->
                       Eval.singleton (mk_py_top T_bool range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       Eval.singleton expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  OptionExt.return
      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
