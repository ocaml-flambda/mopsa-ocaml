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
open Ast
open MapExt
open Addr
open Universal.Ast
(* au moins gérer les strings *)

module Domain =
  struct
    type _ domain += D_python_types_t_string : unit domain

    let id = D_python_types_t_string
    let name = "python.types.t_string"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_types_t_string -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    type stub_signature = {in_args: string list;
                           out_type: Mopsa.typ}
    type stub_db = stub_signature StringMap.t

    let add_signature funname in_args out_type db =
      let out_type = match out_type with
        | "bool" -> T_bool
        | "int" -> T_int
        | "float" -> T_float F_DOUBLE
        | "str" -> T_string
        | "NoneType" -> T_py_none
        | "NotImplementedType" -> T_py_not_implemented
        | _ -> assert false in
      StringMap.add funname {in_args; out_type} db

    let stub_base =
      let str = "str" and int = "int" and bool = "bool" in
      StringMap.empty |>
      add_signature "str.__mul__" [str; int] str |>
      add_signature "str.__rmul__" [str; int] str |>

      add_signature "str.capitalize" [str] str |>
      (* add_signature "str.center" [str; int] str |> *)
      add_signature "str.center" [str; int; str] str |>
      (* add_signature "str.join" [str; List str] str  |>
       * add_signature "str.join" [str; Generator str] [str] [] |> *)

      add_signature "str.lower" [str] str |>
      add_signature "str.upper" [str] str |>
      (* add_signature "str.split" [str] [List str] [] |>
       * add_signature "str.split" [str; str] [List str] [] |>
       * add_signature "str.split" [str; str; int] [List str] [] |> *)
      add_signature "str.replace" [str; str; str] str |>
      (* add_signature "str.replace" [str; str; str; int] [str] [] |> *)
      add_signature "str.rstrip" [str] str |>
      (* add_signature "str.rstrip" [str; str] str |> *)
      add_signature "str.strip" [str] str |>
      (* add_signature "str.strip" [str; str] [str] [] |> *)
      add_signature "str.swapcase" [str] str |>
      add_signature "str.title" [str] str |>
      add_signature "str.__len__" [str] int |>
      (* add_signature "str.__getitem__" [str; int] [str] [] (\* ["IndexError"] *\) |>
       * add_signature "str.__getitem__" [str; slice] [str] [] |> *)
      add_signature "str.__contains__" [str; str] bool |>
      add_signature "str.isalnum" [str] bool |>
      add_signature "str.isalpha" [str] bool |>
      add_signature "str.isdecimal" [str] bool |>
      add_signature "str.isdigit" [str] bool |>
      add_signature "str.islower" [str] bool |>
      add_signature "str.isnumeric" [str] bool  |>
      add_signature "str.isspace" [str] bool |>
      add_signature "str.istitle" [str] bool |>
      add_signature "str.isupper" [str] bool


    let process_simple man flow range exprs instances return =
      Utils.check_instances man flow range exprs instances (fun _ flow -> man.eval (mk_py_top return range) flow)

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ _ = None

    let is_str_binop_fun = function
      | "str.__add__"
        | "str.__mod__"
        (* | "str.__mul__" *)
        | "str.__rmod__"
      (* | "str.__rmul__" *)
            -> true
      | _ -> false

    let eval zs exp (man: ('a, unit) man) (flow:'a flow) : ('a, expr) evl option =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, []) when StringMap.mem f stub_base ->
        debug "function %s in stub_base, processing@\n" f;
        let {in_args; out_type} = StringMap.find f stub_base in
        process_simple man flow range args in_args out_type
        |> OptionExt.return

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__new__")}, _)}), [cls; obj], []) ->
        (* check if obj has str method, run it, check return type (can't be notimplemented) *)
        (* otherwise, call __repr__. Or repr? *)
        (* FIXME!*)
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_string range) flow |> OptionExt.return

      (* 𝔼⟦ str.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
        when is_compare_op_fun "str" f ->
        Eval.eval_list [e1; e2] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
        Eval.bind (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            Eval.assume
              (mk_py_isinstance_builtin e1 "str" range)
              ~fthen:(fun true_flow ->
                  Eval.assume
                    (mk_py_isinstance_builtin e2 "str" range)
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

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
        when is_str_binop_fun f ->
        Eval.eval_list [e1; e2] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
        Eval.bind (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            Eval.assume
              (mk_py_isinstance_builtin e1 "str" range)
              ~fthen:(fun true_flow ->
                  Eval.assume
                    (mk_py_isinstance_builtin e2 "str" range)
                    ~fthen:(fun true_flow ->
                        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_string range) true_flow)
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

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__getitem__")}, _)}, args, []) ->
        Utils.check_instances_disj man flow range args
          [["str"]; ["int"; "slice"]]
          (fun _ flow -> man.eval (mk_py_top T_string range) flow)
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__iter__")}, _)}, args, []) ->
        Utils.check_instances man flow range args
          ["str"]
          (fun args flow ->
             let str = List.hd args in
             let str_addr = match ekind str with
               | E_py_object (a, _) -> a
               | _ -> assert false in
             let it_addr = mk_alloc_addr (Objects.Py_list.A_py_iterator ("str_iterator", str_addr, None)) range in
             man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) it_addr flow |>
             Eval.bind (fun eit_addr flow ->
                 let it_addr = match ekind eit_addr with E_addr a -> a | _ -> assert false in
                 Eval.singleton (mk_py_object (it_addr, None) range) flow
               )
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str_iterator.__next__")}, _)}, args, []) ->
        Utils.check_instances man flow range args
          ["str_iterator"]
          (fun _ flow ->
             let stopiteration_f = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow in
             let flow = Flow.copy_annot stopiteration_f flow in
             let els = man.eval (mk_py_top T_string range) flow in
             let stopiteration = stopiteration_f |> Eval.empty_singleton in
             Eval.join_list (Eval.copy_annot els stopiteration :: els :: [])
          )
        |> OptionExt.return


      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
