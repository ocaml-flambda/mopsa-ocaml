open Framework.Essentials
open Ast
open Universal.Ast
open MapExt
open Addr
(* au moins gérer les strings *)

module Domain =
  struct
    type _ domain += D_python_types_t_set : unit domain

    let id = D_python_types_t_set
    let name = "python.types.t_set"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_types_t_set -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ _ = None

    let eval zs exp (man: ('a, unit) man) (flow:'a flow) : ('a, expr) evl option =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ set.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_compare_op_fun "set" f ->
         Eval.eval_list [e1; e2] man.eval flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume
                 (mk_py_isinstance_builtin e1 "set" range)
                 ~fthen:(fun true_flow ->
                   Eval.assume
                     (mk_py_isinstance_builtin e2 "set" range)
                     ~fthen:(fun true_flow ->
                       man.eval (mk_py_top T_bool range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       man.eval expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__len__")}, _)}, [e], []) ->
         man.eval e flow |>
           Eval.bind (fun el flow ->
               Eval.assume
                 (mk_py_isinstance_builtin e "set" range)
                 ~fthen:(fun true_flow ->
                   man.eval (mk_py_top T_int range) true_flow)
                 ~felse:(fun false_flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                     Eval.empty_singleton)
                 man flow)
         |> OptionExt.return



      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
