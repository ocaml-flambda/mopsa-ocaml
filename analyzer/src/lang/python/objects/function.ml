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
             | _ -> default ak)

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
      | A_py_staticmethod | A_py_classmethod -> mk_addr_attr a "__func__" T_any
      | _ -> assert false

    let var_of_eobj e = match ekind e with
      | E_py_object (a, _) -> var_of_addr a
      | _ -> assert false

    let addr_of_expr exp = match ekind exp with
      | E_addr a -> a
      | _ -> assert false

    let alarms = []


    let init _ _ flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("method.__new__", _))}, _)}, [meth; func; inst], []) ->
         (* FIXME: meth should be a subtype of method *)
         man.eval ~route:(Semantic "Python") func flow >>$
           (fun func flow ->
               man.eval ~route:(Semantic "Python") inst flow >>$
                 (fun inst flow ->
                     eval_alloc man (A_py_method (object_of_expr func, inst, "method")) range flow |>
                       bind_some (fun addr flow ->
                           let obj = (addr, None) in
                           Eval.singleton (mk_py_object obj range) flow
                         )
                   )
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("function.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        assume (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(man.eval ~route:(Semantic "Python") descr)
          ~felse:(
            man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_object (find_builtin "method") range) [descr; instance] range)
            )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("wrapper_descriptor.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        (* FIXME: No NoneType case otherwise issues on NoneType.__bool__ *) (* to investigate depending on the class of the getter *)
        assume
          (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(fun flow ->
              assume (mk_py_isinstance instance typeofinst range) man flow
                ~fthen:(fun flow ->
                  eval_alloc man (A_py_method (object_of_expr descr, instance, "method-wrapper")) range flow |>
                  bind_some (fun addr flow ->
                      let obj = (addr, None) in
                      Eval.singleton (mk_py_object obj range) flow)
                )
                ~felse:(man.eval ~route:(Semantic "Python") descr)
            )
          ~felse:(fun flow ->
              eval_alloc man (A_py_method (object_of_expr descr, instance, "method-wrapper")) range flow |>
              bind_some (fun addr flow ->
                  let obj = (addr, None) in
                  Eval.singleton (mk_py_object obj range) flow)
            )
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("method_descriptor.__get__", _))}, _)}, [descr; instance; typeofinst], []) ->
        assume (mk_py_isinstance_builtin instance "NoneType" range) man flow
          ~fthen:(man.eval ~route:(Semantic "Python") descr)
          ~felse:(fun flow ->
              eval_alloc man (A_py_method (object_of_expr descr, instance, "builtin_function_or_method")) range flow |>
              bind_some (fun addr flow ->
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
            let addr_func = addr_of_expr eaddr_list in
            let func_var = var_of_addr addr_func in
            man.exec (mk_assign (mk_var func_var range) func range) flow >>%
            Eval.singleton (mk_py_object (addr_func, None) range)
          )
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("classmethod.__init__", _))}, _)}, [self; func], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("staticmethod.__init__", _))}, _)}, [self; func], []) ->
        man.eval ~route:(Semantic "Python") (mk_py_none range) flow |>
        OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("classmethod.__get__", _))}, _)}, [self; inst; typ], []) ->
        man.eval ~route:(Semantic "Python") self flow >>$
          (fun self flow ->
            let var_func = var_of_eobj self in
            assume (mk_py_isinstance_builtin inst "NoneType" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance inst typ range) man flow
                    ~fthen:(
                      man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ; mk_py_type inst range] range)
                    )
                    ~felse:(
                      man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ; typ] range)
                    )
                )
              ~felse:(
                man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_object (find_builtin "method") range) [mk_var var_func range ;  mk_py_type inst range] range)
              )
          )
        |>  OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("staticmethod.__get__", _))}, _)}, [self; _; _], []) ->
        man.eval ~route:(Semantic "Python") self flow >>$
          (fun self flow ->
            let var_func = var_of_eobj self in
            man.eval ~route:(Semantic "Python") (mk_var var_func range) flow
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("property.__get__", _))}, _)}, [self; instance; _], []) ->
        man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_attr self "fget" range) [instance] range) flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("property.__init__", _))}, _)} as called, [self; getter], []) ->
        man.eval ~route:(Semantic "Python") {exp with ekind = E_py_call(called, [self; getter; mk_py_none range; mk_py_none range; mk_py_none range], [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("property.__init__", _))}, _)} as called, [self; getter; setter], []) ->
        man.eval ~route:(Semantic "Python") {exp with ekind = E_py_call(called, [self; getter; setter; mk_py_none range; mk_py_none range], [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("property.__init__", _))}, _)} as called, [self; getter; setter; deleter], []) ->
        man.eval ~route:(Semantic "Python") {exp with ekind = E_py_call(called, [self; getter; setter; deleter; mk_py_none range], [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("property.__init__", _))}, _)}, [self; getter; setter; deleter; doc], []) ->
        let assignments = [("fget", getter); ("fset", setter); ("fdel", deleter); ("__doc__", doc)] in
        man.exec (mk_block (List.map (fun (field, arg) -> mk_assign (mk_py_attr self field range) arg range)  assignments) range) flow >>%
        man.eval ~route:(Semantic "Python") (mk_py_none range) |>
        OptionExt.return


    (* 𝔼⟦ f() | isinstance(f, function) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_user pyfundec)}, _)}, args, kwargs) ->
        debug "args: %a@\n" (Format.pp_print_list pp_expr) args;
        debug "kwargs: %a@\n" (Format.pp_print_list (fun fmt (so, e) -> Format.fprintf fmt "%a~>%a" (OptionExt.print Format.pp_print_string) so pp_expr e)) kwargs;
        debug "user-defined function call on %s@\n" pyfundec.py_func_var.vname;
        if not (pyfundec.py_func_vararg = None && pyfundec.py_func_kwonly_args = [] && pyfundec.py_func_kwarg = None) then
          panic_at range "Calls using vararg, keyword-only args or kwargs are not supported";
        let param_and_args = List.combine
            (List.map (fun v -> match vkind v with | V_uniq (s, _) -> s | _ -> assert false) pyfundec.py_func_parameters)
            pyfundec.py_func_defaults in
        (* Replace default_args by kwargs *)
        let py_func_defaults = List.fold_left (fun acc (oname, e) ->
            match oname with
            | None -> assert false
            | Some name ->
              List.map (fun (n, oe) -> if n = name then (n, Some e) else (n, oe)) acc
          ) param_and_args kwargs in
        let py_func_defaults = List.map snd py_func_defaults in
        (* First check the correct number of arguments *)
        let default_args, nondefault_args = List.partition (function None -> false | _ -> true) py_func_defaults in
        OptionExt.return @@
        if List.length args < List.length nondefault_args then
          (
            debug "Too few arguments!@\n";
            let missing = List.length nondefault_args - List.length pyfundec.py_func_parameters in
            let msg = Format.asprintf "%s() missing %d required positional argument%s" pyfundec.py_func_var.vname missing (if missing > 1 then "s" else "") in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
            Eval.empty_singleton
          )
        else
        if List.length args > (List.length pyfundec.py_func_parameters) then
          (
            debug "Too many arguments!@\n";
            let msg = Format.asprintf "%s() takes %d positional arguments but %d were given" pyfundec.py_func_var.vname (List.length pyfundec.py_func_parameters) (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
            Eval.empty_singleton
          )
        else
          (
            debug "|params| = %d (%a)" (List.length pyfundec.py_func_parameters) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) pyfundec.py_func_parameters;
            debug "|args| = %d (%a)" (List.length args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) args;
            debug "|default| = %d (%a)" (List.length default_args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (OptionExt.print pp_expr)) default_args;
            debug "|non-default| = %d (%a)" (List.length nondefault_args) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (OptionExt.print pp_expr)) nondefault_args;
            let args =
              if List.length args = (List.length pyfundec.py_func_parameters) then
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
            if List.length args <> (List.length pyfundec.py_func_parameters) then
              (
                debug "The number of arguments is not good@\n";
                let msg = Format.asprintf "%s() has too few arguments" pyfundec.py_func_var.vname in
                man.exec (Utils.mk_builtin_raise_msg "TypeError" msg exp.erange) flow >>%
                Eval.empty_singleton
              )
            else
              (* Initialize local variables to undefined value and give the call to {!Universal} *)
              (
                let flow =
                  if pyfundec.py_func_locals = [] then Post.return flow else
                    man.exec
                      (mk_block (List.mapi (fun i v ->
                           let e =
                             (* Initialize locals with the same name of a builtin with its address *)
                             if is_builtin_var v then (mk_py_object (find_builtin (get_orig_vname v)) range)
                             else mk_expr (E_py_undefined false) range
                           in
                           mk_assign (mk_var v range) e range
                         ) pyfundec.py_func_locals) range)
                      flow
                in

                let fundec = {
                  fun_orig_name = pyfundec.py_func_var.vname;
                  fun_uniq_name = pyfundec.py_func_var.vname;
                  fun_parameters = pyfundec.py_func_parameters;
                  fun_locvars = pyfundec.py_func_locals;
                  fun_body = pyfundec.py_func_body;
                  fun_return_type = Some T_any;
                  fun_return_var = pyfundec.py_func_ret_var;
                  fun_range = pyfundec.py_func_range;
                } in
                flow >>%
                man.eval (mk_call fundec args exp.erange) >>$
                  (fun res flow -> man.eval res flow)
              )
          )
      (* 𝔼⟦ f() | isinstance(f, method) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_method(f, e, t)}, _)}, args, kwargs) ->
        let exp' = mk_py_kall (mk_py_object f range) (e :: args) kwargs range in
        man.eval ~route:(Semantic "Python") exp' flow |> OptionExt.return


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
           if Libs.Py_mopsa.is_unsupported_fundec func then F_unsupported (get_orig_vname func.py_func_var)
           else if Libs.Py_mopsa.is_builtin_fundec func then
             let name = Libs.Py_mopsa.builtin_fundec_name func in
             F_builtin (name, Libs.Py_mopsa.builtin_type_name "function" func)
           else F_user func
         in
         debug "creating function object %a" pp_addr_kind (A_py_function kind);
         eval_alloc man (A_py_function kind) stmt.srange flow |>
         bind_some (fun addr flow ->
             let obj = (addr, None) in
             if Libs.Py_mopsa.is_unsupported_fundec func || Libs.Py_mopsa.is_builtin_fundec func then
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

  end

let () =
  register_stateless_domain (module Domain)
