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
           | E_py_object ({addr_kind = Objects.Py_list.A_py_list _}, _) ->
             proceed "list"
           | E_py_object ({addr_kind = Objects.Py_set.A_py_set _}, _) ->
             proceed "set"
           | E_py_object ({addr_kind = Objects.Dict.A_py_dict _}, _) ->
             proceed "dict"
           | E_py_object ({addr_kind = Objects.Tuple.A_py_tuple _}, _) ->
             proceed "tuple"
           | E_py_object ({addr_kind = Objects.Py_list.A_py_iterator (s, _, _)}, _) ->
             Eval.singleton (mk_py_object (find_builtin s) range) flow
           | E_py_object ({addr_kind = Objects.Dict.A_py_dict_view (s, _)}, _) ->
             Eval.singleton (mk_py_object (find_builtin s) range) flow
           | E_py_object ({addr_kind = A_py_module m}, _) ->
             proceed "module"
           | E_py_object ({addr_kind = A_py_method _}, _) ->
             proceed "method"
           | E_py_object ({addr_kind = A_py_function _}, _) ->
             proceed "function"
           | E_py_object ({addr_kind = A_py_class _}, _) ->
             proceed "type"
           | _ -> Exceptions.panic_at range "type: todo: %a@\n" pp_expr arg
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
      bind_list [cls; cls'] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_cls = match ekind cls with | E_py_object (a, _) -> a | _ -> assert false in
          let addr_cls' = match ekind cls' with | E_py_object (a, _) -> a | _ -> assert false in
          match akind addr_cls, akind addr_cls' with
          | A_py_class (c, mro), A_py_class (c', mro') ->
            Eval.singleton (mk_py_bool (class_le (c, mro) (c', mro')) range) flow
          | _ -> assert false)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
      (* TODO: if v is a class inheriting from protocol we should check the attributes *)
      bind_list [obj; attr] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          debug "now isinstance(%a, %a) at range %a@\n" pp_expr eobj pp_expr eattr pp_range range (*(Flow.print man.lattice.print) flow*);
          let addr_obj = addr_of_eobject eobj in
          let addr_attr = addr_of_eobject eattr in
          debug "match %a %a" pp_addr addr_obj pp_addr addr_attr;
          match akind addr_obj, akind addr_attr with
          (* FIXME: isinstance _, object *)
          | A_py_class _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "type") range) flow

          | A_py_function _, A_py_class (C_builtin c, _) ->
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

          | Objects.Py_list.A_py_list _, A_py_class (C_builtin "list", _)
          | Objects.Py_set.A_py_set _, A_py_class (C_builtin "set", _)
          | Objects.Dict.A_py_dict _, A_py_class (C_builtin "dict", _)
          | Objects.Tuple.A_py_tuple _, A_py_class (C_builtin "tuple", _) ->
            man.eval (mk_py_true range) flow

          | Objects.Py_list.A_py_list _, A_py_class (c, b)
          | Objects.Py_set.A_py_set _, A_py_class (c, b)
          | Objects.Dict.A_py_dict _, A_py_class (c, b)
          | Objects.Tuple.A_py_tuple _, A_py_class (c, b) ->
            let str_addr = match akind addr_obj with
              | Objects.Py_list.A_py_list _ -> "list"
              | Objects.Py_set.A_py_set _ -> "set"
              | Objects.Dict.A_py_dict _ -> "dict"
              | Objects.Tuple.A_py_tuple _ -> "tuple"
              | _ -> assert false in
            assume (mk_py_issubclass_builtin_l str_addr eattr range) man flow
              ~fthen:(man.eval (mk_py_false range))
              ~felse:(man.eval (mk_py_false range))

          | Objects.Py_list.A_py_iterator (s, _, _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = s) range) flow

          | Objects.Dict.A_py_dict_view (s, _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = s) range) flow

          | A_py_module _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "module" || c = "object") range) flow

          | A_py_method _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "method" || c = "object") range) flow

          | _ -> assert false
        )
      |> Option.return

    | _ -> None


  let exec _ _ _ _ = None
  let ask _ _ _ = None
  let refine c m f = Channel.return f

end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain);
