(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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
open Mopsa
open Sig.Domain.Intermediate
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute
open Alarms

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.types.nominal_types"
    end)

  let interface = {
    iexec = { provides = []; uses = []; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let alarms = []

  let init _ _ flow = flow

  let class_le (_, b: class_address * py_object list) (d, _: class_address * py_object list) : bool =
    let res = List.exists (fun x -> match akind @@ fst x with
        | A_py_class (x, _) -> x = d
        | _ -> false) b in
    res

  let addr_of_eobject e = match ekind e with
    | E_py_object (a, _) -> a
    | _ -> assert false


  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind
        (fun earg flow ->
           let proceed s = Eval.singleton (mk_py_object (find_builtin s) range) flow in
           match ekind earg with
           | E_py_object ({addr_kind = A_py_instance cls}, _) ->
             Eval.singleton (mk_py_object (cls, None) range) flow
           | E_py_object ({addr_kind = A_py_module m}, _) ->
             proceed "module"
           | E_py_object ({addr_kind = A_py_method (_, _, t)}, _) ->
             proceed t
           | E_py_object ({addr_kind = A_py_function (F_builtin (fname, ftype))}, _) ->
             proceed ftype
           | E_py_object ({addr_kind = A_py_function (F_annot _)}, _) ->
             proceed "function"
           | E_py_object ({addr_kind = A_py_function (F_user f)}, _) ->
             proceed (Libs.Py_mopsa.builtin_type_name "function" f)
           | E_py_object ({addr_kind = A_py_class _}, _) ->
              proceed "type"
           | E_py_object ({addr_kind}, _) ->
              proceed (addr_kind_find_nominal_type addr_kind)
           | _ -> Exceptions.panic_at range "type: todo: %a@\n" pp_expr arg
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("issubclass", _))}, _)}, [cls; cls'], []) ->
      bind_list [cls; cls'] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_cls = match ekind cls with | E_py_object (a, _) -> a | _ -> assert false in
          let addr_cls' = match ekind cls' with | E_py_object (a, _) -> a | _ -> assert false in
          match akind addr_cls, akind addr_cls' with
          | A_py_class (c, mro), A_py_class (c', mro') ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_bool (class_le (c, mro) (c', mro')) range) flow
          | _ -> panic_at range "%a, cls=%a, cls'=%a" pp_expr exp pp_expr cls pp_expr cls')
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("isinstance", _))}, _)}, [obj; attr], []) ->
      (* TODO: if v is a class inheriting from protocol we should check the attributes *)
      bind_list [obj; attr] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          debug "now isinstance(%a, %a) at range %a@\n" pp_expr eobj pp_expr eattr pp_range range (*(Flow.print man.lattice.print) flow*);
          let addr_obj = addr_of_eobject eobj in
          let addr_attr = addr_of_eobject eattr in
          debug "match %a %a" pp_addr addr_obj pp_addr addr_attr;
          match akind addr_obj, akind addr_attr with
          | _, A_py_class (C_builtin "object", _) ->
            man.eval (mk_py_true range) flow

          | A_py_class _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "type") range) flow

          | A_py_function (F_builtin (_, ftype)), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = ftype) range) flow

          | A_py_function (F_user f), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = Libs.Py_mopsa.builtin_type_name "function" f) range) flow

          | A_py_function (F_annot _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "function") range) flow


          | A_py_instance cls, A_py_class (c, mro) ->
            begin match List.find_opt
                          (fun x -> match akind @@ fst x with
                             | A_py_class (C_annot c, _) -> get_orig_vname c.py_cls_a_var = "Protocol"
                             | _ -> false) mro with
            | Some ({addr_kind = A_py_class (C_annot c_proto, _)}, _) ->
              debug "Protocol case!";
              let mk_and conds = List.fold_left (fun acc cond -> mk_binop acc O_py_and cond range) (List.hd conds) (List.tl conds) in
              let conds = List.map (fun x -> mk_py_hasattr eobj (get_orig_vname x) range) (match c with
                  | C_user c -> c.py_cls_static_attributes
                  | C_annot c -> c.py_cls_a_static_attributes
                  | _ -> assert false) in
              if List.length conds = 0 then
                let () = debug "empy attributes for c?!" in
                man.eval (mk_py_true range) flow
              else
                man.eval (mk_and conds) flow
            | Some _ -> assert false
            | None ->
               let ic, imro = match akind cls with
                 | A_py_class (c, m) -> c, m
                 | _ -> assert false in
               man.eval (mk_py_bool (class_le (ic, imro) (c, mro)) range) flow
            end

          | A_py_module _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "module" || c = "object") range) flow

          | A_py_method (_, _, t), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = t || c = "object") range) flow

          | ak, A_py_class (c, b) ->
             let n_ak = addr_kind_find_nominal_type ak in
             begin match c with
             | C_builtin n when n = n_ak -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_true range) flow
             | _ ->
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_issubclass_builtin_l n_ak eattr range) flow
                  (* ~zone:Zone.Z_py
                   * ~fthen:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_true range))
                   * ~felse:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range)) *)
             end

          | _ -> assert false
        )
      |> OptionExt.return

    | _ -> None


  let exec _ _ _ _ = None
  let ask _ _ _ = None
  let refine c m f = Channel.return f

end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain);
