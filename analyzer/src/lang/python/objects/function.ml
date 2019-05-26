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
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.objects.function"
      end)

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ f() | isinstance(f, function) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_user pyfundec)}, _)}, args, []) ->
         debug "user-defined function call@\n";
         (* First check the correct number of arguments *)
         let default_args, nondefault_args = List.partition (function None -> false | _ -> true) pyfundec.py_func_defaults in
         Option.return @@
           if List.length pyfundec.py_func_parameters < List.length nondefault_args then
             (
               debug "Too few arguments!@\n";
               let flow =
                 man.exec (Utils.mk_builtin_raise "TypeError" exp.erange) flow
               in
               Eval.empty_singleton flow
             )
           else
             if List.length args > (List.length pyfundec.py_func_parameters) then
               (
                 debug "Too many arguments!@\n";
                 let flow =
                   man.exec (Utils.mk_builtin_raise "TypeError" exp.erange) flow
                 in
                 Eval.empty_singleton flow
               )
             else
               (
                 debug "|params| = %d" (List.length pyfundec.py_func_parameters);
                 debug "|args| = %d" (List.length args);
                 debug "|default| = %d" (List.length default_args);
                 debug "|non-default| = %d" (List.length nondefault_args);
                 let args =
                   if List.length args = (List.length pyfundec.py_func_parameters) then
                     args
                   else
                     (* Remove the first default parameters that are already specified *)
                     let default_args =
                       let to_remove = List.length args - List.length nondefault_args in
                       let rec remove_first n l =
                         match n with
                         | 0 -> l
                         | _ -> remove_first (n-1) (List.tl l)
                       in
                       let () = debug "%d %d" (List.length default_args) to_remove in
                       if to_remove < 0 || List.length default_args < to_remove then [] else remove_first to_remove default_args
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
                     let flow =
                       man.exec (Utils.mk_builtin_raise "TypeError" exp.erange) flow
                     in
                     Eval.empty_singleton flow
                   )
                 else
                   (* Initialize local variables to undefined value and give the call to {!Universal} *)
                   (
                     let flow =
                       if pyfundec.py_func_locals = [] then flow else
                       man.exec
                                  (mk_block (List.mapi (fun i v ->
                                                 let e =
                                                   (* Initialize locals with the same name of a builtin with its address *)
                                                   if is_builtin_name v.org_vname then (mk_py_object (find_builtin v.org_vname) range)
                                                   else mk_expr (E_py_undefined false) range
                                                 in
                                                 mk_assign (mk_var v range) e range
                                               ) pyfundec.py_func_locals) range)
                                  flow
                     in

                     let ret_var = mkfresh_ranged (tag_range range "ret_var") () in
                     let fundec = {
                         fun_name = uniq_vname (pyfundec.py_func_var);
                         fun_parameters = pyfundec.py_func_parameters;
                         fun_locvars = pyfundec.py_func_locals;
                         fun_body = pyfundec.py_func_body;
                         fun_return_type = Some T_any;
                         fun_return_var = ret_var;
                         fun_range = pyfundec.py_func_range;
                       } in

                     man.eval (mk_call fundec args exp.erange) flow |>
                     Eval.bind (man.eval)
                   )
               )
      (* 𝔼⟦ f() | isinstance(f, method) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_method(f, e)}, _)}, args, []) ->
        let exp' = mk_py_call (mk_py_object f range) (e :: args) range in
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) exp' flow |> Option.return

      | _ -> None

    let exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      (* 𝕊⟦ def f(arg1, ...): body ⟧ *)
      | S_py_function(func) ->
         debug "creating function object";
         (* Allocate an object for the function and assign it to the variable
         representing the name of the function *)
         let kind =
           if Libs.Py_mopsa.is_unsupported_fundec func then F_unsupported func.py_func_var.org_vname else
             if Libs.Py_mopsa.is_builtin_fundec func then
               let name = Libs.Py_mopsa.builtin_fundec_name func in
               F_builtin name
             else F_user func
         in
         eval_alloc man (A_py_function kind) stmt.srange flow |>
           post_eval man (fun addr flow ->
               let obj = (addr, None) in
               man.exec
                 (mk_assign
                    (mk_var func.py_func_var range)
                    (mk_py_object obj range)
                    range
                 ) flow
               |> Post.return
             )
         |> Option.return
      | _ ->
         None


    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
