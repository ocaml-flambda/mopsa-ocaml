open Mopsa
open Ast
open MapExt
open Addr
open Universal.Ast
(* au moins gérer les strings *)

module Domain =
  struct
    type _ domain += D_python_types_t_list : unit domain

    let id = D_python_types_t_list
    let name = "python.types.t_list"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_types_t_list -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ _ = None

    let eval zs exp (man: ('a, unit) man) (flow:'a flow) : ('a, expr) evl option =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ list.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_compare_op_fun "list" f ->
         Eval.eval_list [e1; e2] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume
                 (mk_py_isinstance_builtin e1 "list" range)
                 ~fthen:(fun true_flow ->
                   Eval.assume
                     (mk_py_isinstance_builtin e2 "list" range)
                     ~fthen:(fun true_flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  OptionExt.return

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__mul__")}, _)}, [e1; e2], []) ->
       *    Eval.eval_list [e1; e2] man.eval flow |>
       *      Eval.bind (fun el flow ->
       *          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
       *          Eval.assume
       *            (mk_py_isinstance_builtin e1 "list" range)
       *            ~fthen:(fun true_flow ->
       *              Eval.assume
       *                (mk_py_isinstance_builtin e2 "int" range)
       *                ~fthen:(fun true_flow ->
       *                (\* FIXME: works only USING TYPES! *\)
       *                  Eval.singleton e1 true_flow
       *                )
       *                ~felse:(fun false_flow ->
       *                  man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
       *                    Eval.empty_singleton)
       *                man flow
       *            )
       *            ~felse:(fun false_flow ->
       *              man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
       *                Eval.empty_singleton)
       *            man flow
       *        )
       *    |> OptionExt.return *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__len__")}, _)}, [e], []) ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
           Eval.bind (fun el flow ->
               Eval.assume
                 (mk_py_isinstance_builtin e "list" range)
                 ~fthen:(fun true_flow ->
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) true_flow)
                 ~felse:(fun false_flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                     Eval.empty_singleton)
                 man flow)
         |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.insert")} as addr, r)} as call, args, []) ->
      (* we're just doing a type analysis, so we can rewrite list.insert into list.append *)
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args = 3 then
           Eval.eval_list args (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
             Eval.bind (fun eargs flow ->
                 let lst, idx, el = match eargs with | [e1; e2; e3] -> e1, e2, e3 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "list" range)
                   ~fthen:(fun flow ->
                     Eval.assume (mk_py_isinstance_builtin idx "int" range)
                       ~fthen:(fun flow ->
                         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_call({call with ekind = E_py_object ({addr with addr_kind = A_py_function (F_builtin "list.append")}, r)}, lst::el::[], [])} flow)
                       ~felse:tyerror
                       man flow)
                   ~felse:tyerror
                   man flow)
           |> OptionExt.return
         else tyerror flow |> OptionExt.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
