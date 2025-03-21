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
open Sig.Abstraction.Stateless
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

  let checks = []

  let init _ _ flow = None

  let class_le (_, b: class_address * py_object list) (d, _: class_address * py_object list) : bool =
    let res = List.exists (fun x -> match akind @@ fst x with
        | A_py_class (x, _) -> x = d
        | _ -> false) b in
    res

  let addr_of_eobject e = match ekind e with
    | E_py_object (a, _) -> a
    | _ -> assert false


  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
      man.eval arg flow >>$
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
             proceed (builtin_type_name "function" f)
           | E_py_object ({addr_kind = A_py_class _}, _) ->
              proceed "type"
           | E_py_object ({addr_kind}, _) ->
              proceed (addr_kind_find_nominal_type addr_kind)
           | _ -> Exceptions.panic_at range "type: todo: %a@\n" pp_expr arg
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("issubclass", _))}, _)} as call, [obj; {ekind = E_py_tuple types}], []) ->
       man.eval obj flow >>$ (fun obj flow ->
        let issubclass s = {exp with ekind = E_py_call(call, [obj; s], [])} in
        if types = [] then man.eval (mk_py_false range) flow else
        let disj = List.fold_left (fun acc typ -> mk_binop ~etyp:(T_py None) acc O_py_or (issubclass typ) range) (issubclass @@ List.hd types) (List.tl types) in
        man.eval disj flow
      ) |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("issubclass", _))}, _)}, [cls; cls'], []) ->
      bind_list [cls; cls'] man.eval flow |>
      bind_result (fun evals flow ->
          let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_cls = match ekind cls with | E_py_object (a, _) -> a | _ -> assert false in
          let addr_cls' = match ekind cls' with | E_py_object (a, _) -> a | _ -> assert false in
          match akind addr_cls, akind addr_cls' with
          | A_py_class (c, mro), A_py_class (c', mro') ->
             man.eval   (mk_py_bool (class_le (c, mro) (c', mro')) range) flow
          | A_py_c_class v, A_py_c_class v' ->
             warn_at range "issubclass unsound for classes %a %a" pp_addr addr_cls pp_addr addr_cls';
             man.eval (mk_py_bool (compare v v' = 0) range) flow
          | A_py_c_class v, A_py_class (C_builtin s, _) ->
             warn_at range "issubclass unsound for classes %a %a" pp_addr addr_cls pp_addr addr_cls';
             man.eval (mk_py_bool (s = "object") range) flow
          | A_py_c_class v, A_py_class (C_user _, _) ->
             warn_at range "issubclass unsound for classes %a %a" pp_addr addr_cls pp_addr addr_cls';
             man.eval (mk_py_false range) flow
          | A_py_class (C_builtin _, _), A_py_c_class v' ->
             warn_at range "issubclass unsound for classes %a %a" pp_addr addr_cls pp_addr addr_cls';
             man.eval (mk_py_false range) flow
          | A_py_class (C_user _, mro), A_py_c_class v' ->
             man.eval (mk_py_bool (List.exists (fun (addr, _) -> compare_addr addr addr_cls' = 0) mro) range) flow
          | _ -> panic_at range "%a" pp_expr exp pp_expr cls pp_expr cls')
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("issubclass", _))}, _)}, args, kwargs) ->
       man.exec (Utils.mk_builtin_raise_msg "TypeError"
                   (Format.asprintf "issubclass expected 2 arguments, got %d" (List.length args + List.length kwargs)) range) flow >>%
         Eval.empty |>
         OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("isinstance", _))}, _)} as call, [obj; {ekind = E_py_tuple types}], []) ->
       man.eval obj flow >>$ (fun obj flow ->
        let isinstance s = {exp with ekind = E_py_call(call, [obj; s], [])} in
        if types = [] then man.eval (mk_py_false range) flow else
        let disj = List.fold_left (fun acc typ -> mk_binop ~etyp:(T_py None) acc O_py_or (isinstance typ) range) (isinstance @@ List.hd types) (List.tl types) in
        man.eval disj flow
      ) |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("isinstance", _))}, _)}, [obj; attr], []) ->
      (* TODO: if v is a class inheriting from protocol we should check the attributes *)
      (* optim: obj may point to different things, but attr usually doesn't, so we evaluate it first *)
      bind_list [attr; obj] (man.eval) flow |>
      bind_result (fun evals flow ->
          let eattr, eobj= match evals with [e1; e2] -> e1, e2 | _ -> assert false in
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
            man.eval (mk_py_bool (c = builtin_type_name "function" f) range) flow

          | A_py_function (F_annot _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "function") range) flow


          | A_py_instance cls, A_py_class (c, mro) ->
            begin match List.find_opt
                          (fun x -> match akind @@ fst x with
                             | A_py_class (C_annot c, _) -> get_orig_vname c.py_cls_a_var = "Protocol"
                             | _ -> false) mro with
            | Some ({addr_kind = A_py_class (C_annot c_proto, _)}, _) ->
              debug "Protocol case!";
              let mk_and conds = List.fold_left (fun acc cond -> mk_binop ~etyp:(T_py None) acc O_py_and cond range) (List.hd conds) (List.tl conds) in
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
               begin match akind cls with
               | A_py_class (ic, imro) ->
                  man.eval (mk_py_bool (class_le (ic, imro) (c, mro)) range) flow
               | A_py_c_class _ ->
                  warn_at range "%a returned false, is that right? (type class not handled correctly)" pp_expr exp;
                  man.eval (mk_py_false range) flow
               | _ -> assert false
               end
            end

          | A_py_instance cls, A_py_c_class c ->
             begin match akind cls with
             | A_py_class (_, imro) ->
                let res = List.exists (fun x -> match akind @@ fst x with
                                                | A_py_c_class x -> compare x c = 0
                                                | _ -> false) imro in
                man.eval (mk_py_bool res range) flow

             | A_py_c_class c' ->
                man.eval (mk_py_bool (compare c c' = 0) range) flow

             | _ -> assert false
             end

          | A_py_module _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "module" || c = "object") range) flow

          | A_py_method (_, _, t), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = t || c = "object") range) flow

          | ak, A_py_class (c, b) ->
             let n_ak = addr_kind_find_nominal_type ak in
             begin match c with
             | C_builtin n when n = n_ak -> man.eval   (mk_py_true range) flow
             | _ ->
                man.eval   (mk_py_issubclass_builtin_l n_ak eattr range) flow
             end

          | _ ->
             man.exec (Utils.mk_builtin_raise_msg "TypeError" "isinstance() arg 2 must be a type or tuple of types" range) flow >>% Eval.empty
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("isinstance", _))}, _)}, args, kwargs) ->
       man.exec (Utils.mk_builtin_raise_msg "TypeError"
                   (Format.asprintf "isinstance expected 2 arguments, got %d" (List.length args + List.length kwargs)) range) flow >>%
         Eval.empty |>
         OptionExt.return

    | _ -> None


  let exec _ _ _ = None
  let ask _ _ _ = None
  let print_expr _ _ _ _ = ()

end

let () = register_stateless_domain (module Domain);
