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

(** Handling of class definition and instantiation. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.class"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let alarms = []

  let init _ _ flow = flow


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    (* 𝔼⟦ C() | isinstance(C, type) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind=A_py_class (C_builtin "type", _)}, _)}, args, []) ->
      None

    | E_py_call({ekind = E_py_object (({addr_kind=A_py_class _}, _) as cls)} as ecls, args, kwargs) ->
      debug "class call  %a@\n@\n" pp_expr exp;
      (* Call __new__ *)
      bind_list args (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun eargs flow ->
          let new_call = mk_py_kall (mk_py_object_attr cls "__new__" range) ((mk_py_object cls range) :: eargs) kwargs range in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) new_call flow |>
          Eval.bind (fun inst flow ->
              assume
                (mk_py_isinstance inst ecls range)
                ~fthen:(fun flow ->
                    debug "init!@\n";
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_kall (mk_py_object_attr cls "__init__" range) (inst :: (*e*)args) kwargs range) flow |>
                    Eval.bind (fun r flow ->
                        assume
                          (mk_py_isinstance_builtin r "NoneType" range)
                          ~fthen:(fun flow -> man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) inst flow)
                          ~felse:(fun flow ->
                              Format.fprintf Format.str_formatter "__init__() should return None, not %a" pp_expr r;
                              let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow in
                              Eval.empty_singleton flow
                            )
                          man flow
                      ))
                ~felse:(fun flow -> Eval.singleton inst flow)
                man flow
            )
        )
      |> OptionExt.return

    | _ -> None

  let rec exec zone stmt (man:('a, unit) man) (flow:'a flow) : 'a post option =
    let range = srange stmt in
    match skind stmt with
    (* 𝕊⟦ class cls: body ⟧ *)
    | S_py_class cls ->
      debug "definition of class %a" pp_var cls.py_cls_var;
      bind_list cls.py_cls_bases (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun bases flow ->
          let bases' =
            match bases with
            | [] -> [find_builtin "object"]
            | _ -> List.map (fun x -> let a, e = object_of_expr x in (a, None))  bases
          in
          if Libs.Py_mopsa.is_builtin_clsdec cls then
            let name = Libs.Py_mopsa.builtin_clsdec_name cls in
            create_builtin_class (C_builtin name) name cls bases' range;
            man.exec cls.py_cls_body flow |>
            Post.return
          else
          if Libs.Py_mopsa.is_unsupported_clsdec cls then
            let name = get_orig_vname cls.py_cls_var in
            create_builtin_class (C_unsupported name) name cls bases' range;
            man.exec cls.py_cls_body flow |>
            Post.return
          else
            try
              debug "bases' = %a@\n" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) (List.map (fun x -> mk_py_object x range) bases');
              let mro = c3_lin ({addr_kind= (A_py_class (C_user cls, bases')); addr_group=G_all; addr_mode = STRONG}, None) in
              debug "MRO of %a: %a@\n" pp_var cls.py_cls_var
                (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                   (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (srange stmt))))
                mro;

              eval_alloc man (A_py_class (C_user cls, mro)) stmt.srange flow |>
              bind_some (fun addr flow ->
                  let obj = (addr, None) in
                  let flow = man.exec (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range) flow in
                  debug "Body of class is %a@\n" pp_stmt cls.py_cls_body;
                  let flow = man.exec cls.py_cls_body flow in
                  let parent = List.hd @@ List.tl mro in
                  man.eval (mk_py_call (mk_py_object_attr parent "__init_subclass__" range) [mk_py_object obj range] range) flow |>
                  Eval.bind (fun _ flow -> Post.return flow)
                )
            with C3_lin_failure ->
              Exceptions.warn "C3 linearization failure during class declaration %a@\n" pp_var cls.py_cls_var;
              man.exec (Utils.mk_builtin_raise_msg "TypeError" "Cannot create a consistent method resolution order (MRO)" range) flow
              |> Post.return
        )
      |> OptionExt.return

    | _ -> None


  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
