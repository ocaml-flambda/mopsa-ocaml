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

(** Data model for attribute access. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.data_model.attribute"
      end)

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = []} (* TODO: add attribute assignment *);
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let alarms = []

    let init _ _ flow = flow

    let eval zs expr man flow =
      let range = erange expr in
      match ekind expr with
      (* Special attributes *)
      | E_py_attribute(obj, ("__dict__" as attr))
        | E_py_attribute(obj, ("__class__" as attr))
        | E_py_attribute(obj, ("__bases__" as attr))
        | E_py_attribute(obj, ("__name__" as attr))
        | E_py_attribute(obj, ("__qualname__" as attr))
        | E_py_attribute(obj, ("__mro__" as attr))
        | E_py_attribute(obj, ("mro" as attr))
        | E_py_attribute(obj, ("__subclass__" as attr)) ->
         panic_at range "Access to special attribute %s not supported" attr

      (* TODO: wtf factor search_mro *)
      (* Attributes of builtins classes are static, so we can be much more efficient *)
      | E_py_attribute ({ekind = E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _)}, attr) ->
        let rec search_mro mro = match mro with
          | [] ->
            Format.fprintf Format.str_formatter "'%s' object has no attribute '%s'" c attr;
            man.exec (Utils.mk_builtin_raise_msg "AttributeError" (Format.flush_str_formatter ()) range) flow |>
            Eval.empty_singleton
          | cls::tl ->
            if is_builtin_attribute cls attr then
              Eval.singleton (mk_py_object (find_builtin_attribute cls attr) range) flow
            else
              search_mro tl in
        search_mro mro |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("getattr", _))}, _)}, e::attr::[], [])  ->
        man.eval attr flow |>
        Eval.bind (fun eattr flow ->
            match ekind eattr with
            | E_py_object (_, Some {ekind = E_constant (C_string attr)}) ->
              man.eval (mk_py_attr e attr range) flow
            | _ ->
              assume (mk_py_isinstance_builtin eattr "str" range) man flow
                ~fthen:(fun flow -> man.eval (mk_py_top T_any range) flow)
                ~felse:(fun flow ->
                    panic_at range "getattr with attr=%a" pp_expr eattr)
          )
        |> Option.return

      (* Other attributes *)
      | E_py_attribute (e, attr) ->
         debug "%a@\n" pp_expr expr;
         let c_attr = mk_constant T_string (C_string attr) range in
         man.eval e ~zone:(Zone.Z_py, Zone.Z_py_obj) flow |>
         Eval.bind (fun exp flow ->
             match ekind exp with
             | E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _) ->
               let rec search_mro mro = match mro with
                 | [] ->
                   Format.fprintf Format.str_formatter "'%s' object has no attribute '%s'" c attr;
                   man.exec (Utils.mk_builtin_raise_msg "AttributeError" (Format.flush_str_formatter ()) range) flow |>
                   Eval.empty_singleton
                 | cls::tl ->
                   if is_builtin_attribute cls attr then
                     Eval.singleton (mk_py_object (find_builtin_attribute cls attr) range) flow
                   else
                     search_mro tl in
               search_mro mro
             | _ ->
               debug "other: %a@\n" pp_expr exp;
               (* In a bottom environment, the only thing that we can
                  do is to search for builtins attributes and resolve
                  them statically.
               *)
               if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom then
                 let oexp = object_of_expr exp in
                 let oname = oobject_name oexp in
                 if oname <> None && is_builtin_name (Option.none_to_exn oname) && is_builtin_attribute oexp attr then
                   let rese = mk_py_object (find_builtin_attribute oexp attr) range in
                   Eval.singleton rese flow
                 else
                   Eval.empty_singleton flow
               else
                 man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type exp range) flow |>
                 Eval.bind (fun class_of_exp flow ->
                     let mro = mro (object_of_expr class_of_exp) in
                     debug "mro of %a: %a" pp_expr class_of_exp (Format.pp_print_list (fun fmt (a, _) -> pp_addr fmt a)) mro;
                     let rec search_mro flow mro = match mro with
                       | [] -> assert false
                       | cls::tl ->
                         assume
                           (mk_expr (E_py_ll_hasattr (mk_py_object cls range, mk_string "__getattribute__" range)) range)
                           ~fthen:(fun flow -> man.eval (mk_expr (E_py_ll_getattr (mk_py_object cls range, mk_string "__getattribute__" range)) range) flow)
                           ~felse:(fun flow -> search_mro flow tl)
                           man flow in
                     search_mro flow mro |>
                     Eval.bind (fun getattribute flow ->
                         bind_list [exp; c_attr] man.eval flow |>
                         bind_some (fun ee flow ->
                             let exp, c_attr = match ee with [e1;e2] -> e1, e2 | _ -> assert false in
                             assume (mk_py_hasattr exp attr range) man flow
                               ~fthen:(fun flow ->
                                   (* FIXME(?): we split the flow to remove the exn tokens. If some appear afterwards (hopefully it's only AttributeErrors), we remove them and put it back to cur *)
                                   let flow_exn_before, flow = Flow.partition (fun tk _ -> match tk with
                                       | Alarms.T_py_exception _ -> true
                                       | _ -> false) flow in
                                   man.eval (mk_py_call getattribute [exp; c_attr] range) flow |>
                                   bind_opt (fun oattr flow ->
                                       Some
                                         (match oattr with
                                          | None ->
                                            (* exn, let's call getattr, and change exn flow into cur *)
                                            let flow = Flow.fold (fun acc tk env ->
                                                match tk with
                                                | Alarms.T_py_exception _ ->
                                                  warn_at range "call to __getattribute__ failed with token %a now trying __getattr__" pp_token tk;
                                                  Flow.add T_cur env man.lattice acc
                                                | _ -> Flow.add tk env man.lattice acc) (Flow.bottom_from flow) flow in
                                            let flow = Flow.join man.lattice flow_exn_before flow in
                                            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr class_of_exp "__getattr__" range) [exp; c_attr] range) flow
                                          | Some attr -> Eval.singleton attr (Flow.join man.lattice flow_exn_before flow)
                                         )
                                     )
                                   |> Option.none_to_exn
                                 )
                               ~felse:(fun flow ->
                                   man.eval (mk_py_call getattribute [exp; c_attr] range) flow
                                 )
                           )
                       )
                   )
           )
         |> Option.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("hasattr", _))}, _)}, [obj; attr], []) ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) obj flow |>
           Eval.bind (fun eobj flow ->
             match ekind eobj with
             | E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _) ->
               let attr = match ekind attr with
                 | E_constant (C_string s) -> s
                 | _ -> assert false in
               let rec search_mro mro = match mro with
                 | [] ->
                   man.eval (mk_py_false range) flow
                 | cls::tl ->
                   if is_builtin_attribute cls attr then
                     man.eval (mk_py_true range) flow
                   else
                     search_mro tl in
               search_mro mro
             | _ ->
               assume (mk_expr (E_py_ll_hasattr (eobj, attr)) range)
                 ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                 ~felse:(fun flow ->
                   (* test with ll_hasattr and search in the MRO otherwise *)
                   let rec search_mro flow mro = match mro with
                     | [] -> man.eval (mk_py_false range) flow
                     | cls::tl ->
                        assume
                          (mk_expr (E_py_ll_hasattr (mk_py_object cls range, attr)) range)
                          ~fthen:(fun flow ->
                            man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> search_mro flow tl)
                          man flow
                   in
                   assume
                     (mk_py_isinstance_builtin eobj "type" range)
                     ~fthen:(fun flow ->
                       let mro = mro (object_of_expr eobj) in
                       search_mro flow mro)
                     ~felse:(fun flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
                         Eval.bind (fun class_of_exp flow ->
                             let mro = mro (object_of_expr class_of_exp) in
                             search_mro flow mro)
                     )
                     man flow
                 )
                 man flow
             )
         |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("setattr", _))}, _)}, [lval; attr; rval], []) ->
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr lval "__setattr__" range) [attr; rval] range) flow
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("delattr", _))}, _)}, [lval; attr], []) ->
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr lval "__delattr__" range) [attr] range) flow
        |> Option.return


      | _ -> None

    let exec zone stmt man flow =
      let range = stmt.srange in
      match skind stmt with
      | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr (mk_py_type lval range) "__setattr__" range) [lval; mk_constant T_any (C_string attr) range; rval] range) flow
        |> Eval.bind (fun e flow -> Post.return flow)
        |> Option.return

      | _ -> None

    let ask _ _ _ = None
  end


let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
