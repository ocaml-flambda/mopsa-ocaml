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

(** Python data model for subscript access. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.data_model.subscript"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses= [Zone.Z_py, Zone.Z_py_obj]}
  }

  let alarms = []

  let init _ _ flow = flow


  let eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_index_subscript(obj, index) ->
      bind_list [obj; index] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun el flow ->
          let eobj, eindex = match el with [obj; index] -> obj, index | _ -> assert false in

          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
          Eval.bind (fun cls flow ->
              assume
                (Utils.mk_hasattr cls "__getitem__" range)
                man flow
                ~fthen:(fun true_flow ->
                    (* we need to keep the unevaluated index here for the type analysis *)
                    let exp' = mk_py_call (mk_py_attr cls "__getitem__" range) [eobj; index] range in
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) exp' true_flow
                  )
                ~felse:(fun false_flow ->
                  let msg = Format.asprintf "'%a' object is not subscriptable" pp_addr_kind (akind @@ fst @@ object_of_expr cls) in
                    let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow in
                    Eval.empty_singleton flow
                  )
            )
        )
      |> OptionExt.return

    | E_py_slice_subscript(obj, start, stop, step) ->
      bind_list [obj; start; stop; step] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun el flow ->
          let eobj, start, stop, step = match el with [obj; start; stop; step] -> obj, start, stop, step | _ -> assert false in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
          Eval.bind (fun cls flow ->
              assume
                (Utils.mk_hasattr cls "__getitem__" range)
                man flow
                ~fthen:(fun true_flow ->
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (Utils.mk_builtin_call "slice" [start; stop; step] range) true_flow |>
                    Eval.bind (fun slice flow ->
                        let exp' = mk_py_call (mk_py_attr cls "__getitem__" range) [eobj; slice] range in
                        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) exp' flow
                      )
                  )
                ~felse:(fun false_flow ->
                  let msg = Format.asprintf "'%a' object is not subscriptable" pp_addr_kind (akind @@ fst @@ object_of_expr cls) in
                    let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow in
                    Eval.empty_singleton flow
                  )
            )
        )
      |> OptionExt.return

    | _ -> None


  let exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_assign({ekind = E_py_index_subscript(obj, index)}, exp) ->
      bind_list [exp; obj; index] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some
        (fun el flow ->
           let exp, eobj, eindex = match el with [exp; obj; index] -> exp, obj, index | _ -> assert false in
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
           bind_some (fun cls flow ->
               assume
                 (Utils.mk_hasattr cls "__setitem__" range)
                 man flow
                 ~fthen:(fun true_flow ->
                     (* we need to keep the unevaluated index here for the type analysis *)
                     let exp' = mk_py_call (mk_py_attr cls "__setitem__" range) [obj; index; exp] range in
                     man.exec {stmt with skind = S_expression(exp')} true_flow |> Post.return
                   )
                 ~felse:(fun false_flow ->
                   let msg = Format.asprintf "'%a' object does not support item assignment" pp_addr_kind (akind @@ fst @@ object_of_expr cls) in
                   man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow |> Post.return
                   )
             )
        )
      |> OptionExt.return

    | S_assign({ekind = E_py_slice_subscript (obj, start, stop, step)}, exp) ->
      bind_list [exp; obj; start; stop; step] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun el flow ->
          let exp, eobj, start, stop, step = match el with [exp; obj; start; stop; step] -> exp, obj, start, stop, step | _ -> assert false in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
          bind_some (fun cls flow ->
              assume
                (Utils.mk_hasattr cls "__setitem__" range)
                man flow
                ~fthen:(fun true_flow ->
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (Utils.mk_builtin_call "slice" [start; stop; step] range) true_flow |>
                    bind_some (fun slice flow ->
                        let exp' = mk_py_call (mk_py_attr cls "__setitem__" range) [eobj; slice; exp] range in
                        man.exec {stmt with skind = S_expression(exp')} flow |> Post.return
                      )
                  )
                ~felse:(fun false_flow ->
                  let msg = Format.asprintf "'%a' object does not support item assignment" pp_addr_kind (akind @@ fst @@ object_of_expr cls) in
                  man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow |> Post.return
                  )
            )
        )
      |> OptionExt.return

    | _ -> None


  let ask _ _ _ = None

end


let () =
  register_stateless_domain (module Domain)
