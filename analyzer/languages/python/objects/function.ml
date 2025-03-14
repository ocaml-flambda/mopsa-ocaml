(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Definition of python functions and evaluation of their calls. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

let name = "python.objects.function"

type addr_kind +=
  | A_py_staticmethod
  | A_py_classmethod

let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_staticmethod ->  "staticmethod"
             | A_py_classmethod ->  "classmethod"
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak attr ->
             match ak with
             | A_py_staticmethod
               | A_py_classmethod -> List.mem attr ["__new__"; "__init__"; "__get__"]
             | _ -> default ak attr
           )

let () =
  register_is_data_container (fun default ak -> match ak with
      | A_py_classmethod -> false
      | A_py_staticmethod -> false
      | _ -> default ak)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_staticmethod -> fprintf fmt "staticmethod"
          | A_py_classmethod -> fprintf fmt "classmethod"
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | _ -> default a1 a2);})

let () = Universal.Heap.Policies.register_mk_addr (fun default ak ->
             match ak with
             | A_py_staticmethod | A_py_classmethod -> Universal.Heap.Policies.mk_addr_range ak
             | _ -> default ak)


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = name
      end)

    let var_of_addr a = match akind a with
      | A_py_staticmethod | A_py_classmethod -> mk_addr_attr a "__func__" (T_py None)
      | _ -> assert false

    let var_of_eobj e = match ekind e with
      | E_py_object (a, _) -> var_of_addr a
      | _ -> assert false

    let checks = []


    let init _ _ flow = None

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("method.__new__", _))}, _)}, [meth; func; inst], []) ->
         (* FIXME: meth should be a subtype of method *)
         man.eval func flow >>$
           (fun func flow ->
               man.eval inst flow >>$
                 (fun inst flow ->
                     eval_alloc man (A_py_method (object_of_expr func, object_of_expr inst, "method")) range flow |>
                       bind_result (fun addr flow ->
                           let obj = (addr, None) in
                           Eval.singleton (mk_py_object obj range) flow
                         )
                   )
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("function.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        assume (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(man.eval   descr)
          ~felse:(
            man.eval   (mk_py_call (mk_py_object (find_builtin "method") range) [descr; instance] range)
            )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("wrapper_descriptor.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        (* FIXME: No NoneType case otherwise issues on NoneType.__bool__ *) (* to investigate depending on the class of the getter *)
        assume
          (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(fun flow ->
              assume (mk_py_isinstance instance typeofinst range) man flow
                ~fthen:(fun flow ->
                  eval_alloc man (A_py_method (object_of_expr descr, object_of_expr instance, "method-wrapper")) range flow |>
                  bind_result (fun addr flow ->
                      let obj = (addr, None) in
                      Eval.singleton (mk_py_object obj range) flow)
                )
                ~felse:(man.eval   descr)
            )
          ~felse:(fun flow ->
              eval_alloc man (A_py_method (object_of_expr descr, object_of_expr instance, "method-wrapper")) range flow |>
              bind_result (fun addr flow ->
                  let obj = (addr, None) in
                  Eval.singleton (mk_py_object obj range) flow)
            )
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("method_descriptor.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        assume (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(man.eval   descr)
          ~felse:(fun flow ->
              eval_alloc man (A_py_method (object_of_expr descr, object_of_expr instance, "builtin_function_or_method")) range flow |>
              bind_result (fun addr flow ->
                  let obj = (addr, None) in
                  Eval.singleton (mk_py_object obj range) flow)
            )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("classmethod.__new__" as s, _))}, _)}, [cls; func], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("staticmethod.__new__" as s, _))}, _)}, [cls; func], []) ->
        let addr_func = mk_alloc_addr (match List.hd (String.split_on_char '.' s) with
            | "staticmethod" -> A_py_staticmethod
            | "classmethod" -> A_py_classmethod
            | _ -> assert false
          ) range in
        man.eval   addr_func flow >>$
          (fun eaddr_list flow ->
            let addr_func = Addr.from_expr eaddr_list in
            let func_var = var_of_addr addr_func in
            man.exec (mk_assign (mk_var func_var range) func range) flow >>%
            Eval.singleton (mk_py_object (addr_func, None) range)
          )
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("classmethod.__init__", _))}, _)}, [self; func], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("staticmethod.__init__", _))}, _)}, [self; func], []) ->
        man.eval   (mk_py_none range) flow |>
        OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("classmethod.__get__", _))}, _)}, [self; inst; typ], []) ->
        man.eval   self flow >>$
          (fun self flow ->
            let var_func = var_of_eobj self in
            assume (mk_py_isinstance_builtin inst "NoneType" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance inst typ range) man flow
                    ~fthen:(
                      man.eval   (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ; mk_py_type inst range] range)
                    )
                    ~felse:(
                      man.eval   (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ; typ] range)
                    )
                )
              ~felse:(
                man.eval   (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ;  mk_py_type inst range] range)
              )
          )
        |>  OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("staticmethod.__get__", _))}, _)}, [self; _; _], []) ->
        man.eval   self flow >>$
          (fun self flow ->
            let var_func = var_of_eobj self in
            man.eval   (mk_var var_func range) flow
          )
        |> OptionExt.return


    (* 𝔼⟦ f() | isinstance(f, function) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_user pyfundec)}, _)}, args, kwargs) ->
        debug "args: %a@\n" (Format.pp_print_list pp_expr) args;
        debug "kwargs: %a@\n" (Format.pp_print_list (fun fmt (so, e) -> Format.fprintf fmt "%a~>%a" (OptionExt.print Format.pp_print_string) so pp_expr e)) kwargs;
        debug "user-defined function call on %s@\n" pyfundec.py_func_var.vname;
        if not (pyfundec.py_func_vararg = None && pyfundec.py_func_kwarg = None) then
          panic_at range "Calls using vararg or kwargs are not supported";
        let () = debug "defaults=%a kwonly_args=%a" (Format.pp_print_list (OptionExt.print ~none:"None" pp_expr)) pyfundec.py_func_defaults (Format.pp_print_list pp_var) pyfundec.py_func_kwonly_args in
        let param_and_args = List.combine
            (List.map (fun v -> match vkind v with | V_uniq (s, _) -> s | _ -> assert false) pyfundec.py_func_parameters)
            pyfundec.py_func_defaults in
        let param_and_args = param_and_args @ List.combine (List.map (fun v -> match vkind v with | V_uniq (s, _) -> s | _ -> assert false) pyfundec.py_func_kwonly_args) pyfundec.py_func_kwonly_defaults in
        let () = debug "param_and_args = %a" (Format.pp_print_list (fun fmt (s, oe) -> Format.fprintf fmt "%s: %a" s (OptionExt.print pp_expr) oe)) param_and_args in
        (* Replace default_args by kwargs *)
        let py_func_defaults = List.fold_left (fun acc (oname, e) ->
            match oname with
            | None -> assert false
            | Some name ->
              List.map (fun (n, oe) -> if n = name then (n, Some e) else (n, oe)) acc
          ) param_and_args kwargs in
        let () = debug "py_func_defaults = %a" (Format.pp_print_list (fun fmt (s, oe) -> Format.fprintf fmt "%s: %a" s (OptionExt.print ~none:"None" ~some:"Some " pp_expr) oe)) py_func_defaults in
        let py_func_defaults = List.map snd py_func_defaults in
        (* First check the correct number of arguments *)
        let default_args, nondefault_args = List.partition (function None -> false | _ -> true) py_func_defaults in
        let () = debug "args %d default %d non default %d" (List.length args) (List.length default_args) (List.length nondefault_args) in
        OptionExt.return @@
        if List.length args < List.length nondefault_args then
          (
            debug "Too few arguments!@\n";
            let missing = List.length nondefault_args - List.length pyfundec.py_func_parameters in
            let msg = Format.asprintf "%s() missing %d required positional argument%s" pyfundec.py_func_var.vname missing (if missing > 1 then "s" else "") in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
            Eval.empty
          )
        else
        if List.length args > (List.length pyfundec.py_func_parameters) then
          (
            debug "Too many arguments!@\n";
            let msg = Format.asprintf "%s() takes %d positional arguments but %d were given" pyfundec.py_func_var.vname (List.length pyfundec.py_func_parameters) (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
            Eval.empty
          )
        else
          (
            debug "|params| = %d (%a)" (List.length pyfundec.py_func_parameters) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) pyfundec.py_func_parameters;
            debug "|args| = %d (%a)" (List.length args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) args;
            debug "|default| = %d (%a)" (List.length default_args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (OptionExt.print pp_expr)) default_args;
            debug "|non-default| = %d (%a)" (List.length nondefault_args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (OptionExt.print pp_expr)) nondefault_args;
            let args =
              if List.length args = (List.length pyfundec.py_func_parameters + List.length pyfundec.py_func_kwonly_args) then
                args
              else
                (* Remove the first default parameters that are already specified *)
                let default_args =
                  let to_remove = List.length args - List.length nondefault_args in
                  (* + List.length kwargs - List.length default_args in *)
                  let rec remove_first n l =
                    match n with
                    | 0 -> l
                    | _ -> remove_first (n-1) (List.tl l)
                  in
                  let () = debug "%d %d" (List.length default_args) to_remove in
                  if to_remove < 0 || List.length default_args < to_remove then assert false else remove_first to_remove default_args
                in
                (* Fill missing args with default parameters *)
                let default_args = List.map (function Some e -> e | None -> assert false) default_args in
                let rec fill_with_default dfs ndfs args =
                  match args with
                  | [] -> dfs
                  | arg :: args' ->
                    match ndfs with
                    | [] ->
                      (* let dfs' = List.tl dfs in *)
                      arg :: (fill_with_default dfs [] args')
                    | _ :: ndfs' ->
                      arg :: (fill_with_default dfs ndfs' args')
                in

                let args = fill_with_default default_args nondefault_args args in

                debug "|args'| = %d" (List.length args);
                args
            in
            let () = debug "args = %a" (Format.pp_print_list pp_expr) args in
            if List.length args <> (List.length pyfundec.py_func_parameters + List.length pyfundec.py_func_kwonly_args) then
              (
                debug "The number of arguments is not good@\n";
                let msg = Format.asprintf "%s() has too few arguments" pyfundec.py_func_var.vname in
                man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
                Eval.empty
              )
            else
              (* Initialize local variables to undefined value and give the call to {!Universal} *)
              (
                let flow = Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow in
                let flow =
                  if pyfundec.py_func_locals = [] then Post.return flow else
                    man.exec
                      (mk_block (List.mapi (fun i v ->
                           let e =
                             (* Initialize locals with the same name of a builtin with its address *)
                             if is_builtin_var v then (mk_py_object (find_builtin (get_orig_vname v)) range)
                             else mk_expr ~etyp:(T_py None) (E_py_undefined false) range
                           in
                           mk_assign (mk_var v range) e range
                         ) pyfundec.py_func_locals) range)
                      flow
                in
                (* for each cell variables, a cell value is created *)
                (* then, these cell variables are replaced by the @cell.cell_contents *)
                flow >>%
                  bind_list pyfundec.py_func_cellvars (fun cellvar flow ->
                    let range = tag_range exp.erange "cell %a" pp_var cellvar in
                    man.eval (mk_py_call (mk_py_object (find_builtin "cell") range) [] range) flow
                           ) >>$ fun cells flow ->
                let cellmap = List.fold_left2 (fun map var cell -> VarMap.add var cell map) VarMap.empty pyfundec.py_func_cellvars cells in
                let fun_body = Visitor.map_stmt
                                 (fun e -> match ekind e with
                                           | E_var (v, m) when List.mem v pyfundec.py_func_cellvars ->
                                              let cell = VarMap.find v cellmap in
                                              let e' = mk_py_attr cell "cell_contents" range in
                                              Keep e' (* {e with ekind = E_var(v', m)}*)
                                           | _ -> VisitParts e) (fun stmt -> VisitParts stmt) pyfundec.py_func_body in
                (* if the closure variable is part of the param, we propagate the assignment *)
                let pre_body = List.fold_left (fun stmts cellvar ->
                                   if List.mem cellvar (pyfundec.py_func_parameters @ pyfundec.py_func_kwonly_args) then
                                     (mk_assign (mk_py_attr (VarMap.find cellvar cellmap) "cell_contents" range) (mk_var cellvar range) range) :: stmts
                                   else
                                     stmts
                                 ) [] pyfundec.py_func_cellvars in
                let fundec = {
                  fun_orig_name = get_orig_vname pyfundec.py_func_var;
                  fun_uniq_name = pyfundec.py_func_var.vname;
                  fun_parameters = pyfundec.py_func_parameters @ pyfundec.py_func_kwonly_args;
                  fun_locvars = List.filter (fun x -> not @@ List.mem x pyfundec.py_func_cellvars) pyfundec.py_func_locals;
                  fun_body = mk_block [mk_block pre_body range; fun_body] range;
                  fun_return_type = Some (T_py None);
                  fun_return_var = None;
                  fun_range = pyfundec.py_func_range;
                } in
                man.eval (mk_call fundec args exp.erange) flow >>$
                  (fun res flow -> man.eval res flow)
              )
          )
      (* 𝔼⟦ f() | isinstance(f, method) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_method(f, e, t)}, _)}, args, kwargs) ->
        let exp' = mk_py_kall (mk_py_object f range) ((mk_py_object e range) :: args) kwargs range in
        man.eval   exp' flow |> OptionExt.return


      | _ -> None

    let decors_to_call fundec =
      List.filter (fun expr -> match ekind expr with
          | E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, _)
          | E_py_attribute({ekind = E_var( {vkind = V_uniq ("typing",_)}, _)}, _)
          | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, _)}, _, _) -> false
          | _ -> true
        ) fundec.py_func_decors

    let exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      (* 𝕊⟦ def f(arg1, ...): body ⟧ *)
      | S_py_function(func) ->
         (* Allocate an object for the function and assign it to the variable
         representing the name of the function *)
         let kind =
           if is_unsupported_fundec func then F_unsupported (get_orig_vname func.py_func_var)
           else if is_builtin_fundec func then
             let name = builtin_fundec_name func in
             F_builtin (name, builtin_type_name "function" func)
           else F_user func
         in
         debug "creating function object %a" pp_addr_kind (A_py_function kind);
         eval_alloc man (A_py_function kind) stmt.srange flow |>
         bind_result (fun addr flow ->
             let obj = (addr, None) in
             if is_unsupported_fundec func || is_builtin_fundec func then
               add_builtin_function obj ();
             man.exec
               (mk_assign
                  (mk_var func.py_func_var range)
                  (List.fold_left (fun acc decor -> mk_py_call decor [acc] range) (mk_py_object obj range) (decors_to_call func))
                  range
               ) flow
             >>% Post.return
           )
         |> OptionExt.return
      | _ ->
        None


    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
