(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(* A general smashing abstraction for Python sets, (hopefully)
   irrelevant of the value/type domain *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

type addr_kind +=
  | A_py_set of var (* variable where the smashed elements are stored *)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_set var -> fprintf fmt "set[%a]" pp_var var
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_set v1, A_py_set v2 -> compare_var v1 v2
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.set"
    end)


  module VarInfo = struct type t = var let compare = compare_var let print = pp_var end
  module SetInfo = struct
    type t = Callstack.cs * range
    let compare (cs, r) (cs', r') =
      Compare.compose
        [
          (fun () -> Callstack.compare cs cs');
          (fun () -> compare_range r r')
        ]
    let print fmt (cs, r) =
      Format.fprintf fmt "(%a, %a)"
        Callstack.pp_call_stack cs
        pp_range r
  end

  module Equiv = Equiv.Make(SetInfo)(VarInfo)

  let ctx_key =
    let module K = Context.GenUnitKey(
      struct
        type t = Equiv.t
        let print fmt m =
          Format.fprintf fmt "Set annots: @[%a@]" (Equiv.print ?pp_sep:None) m
      end
      )
    in
    K.key


  let fresh_smashed_var =  mk_fresh_uniq_var "$s*" T_any

  let get_var_equiv (info: SetInfo.t) (e: Equiv.t) =
    try
      Equiv.find_l info e, e
    with Not_found ->
      let var = fresh_smashed_var () in
      let new_eq = Equiv.add (info, var) e in
      var, new_eq

  let get_var_flow (info: SetInfo.t) (f: 'a flow) : var * 'a flow =
    let a = Flow.get_ctx f |>
            Context.find_unit ctx_key
    in
    let var, a = get_var_equiv info a in
    var, Flow.set_ctx (Flow.get_ctx f |> Context.add_unit ctx_key a) f

  let interface = {
    iexec = {provides = []; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit ctx_key Equiv.empty
    ) flow


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_set ls ->
      debug "Skipping set.__new__, set.__init__ for now@\n";
      let els_var, flow = get_var_flow (Flow.get_callstack flow, range) flow in
      let flow = List.fold_left (fun acc el ->
          man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_var range) el range) acc) flow ls in
      let addr_set = mk_alloc_addr (A_py_set els_var) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_set flow |>
      Eval.bind (fun eaddr_set flow ->
          let addr_set = match ekind eaddr_set with
            | E_addr a -> a
            | _ -> assert false in
          Eval.singleton (mk_py_object (addr_set, None) range) flow
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__new__")}, _)}, args, []) ->
      (* todo: check that first arg is set class *)
      man.eval (mk_expr (E_py_set []) range) flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__init__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun eargs flow ->
           man.eval (mk_py_none range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.clear")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["set"]
        (fun args flow ->
           let list = List.hd args in
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_set a}, _) ->a
             | _ -> Exceptions.panic_at range "%a@\n" pp_expr list in
           man.exec (mk_remove_var var_els range) flow |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__contains__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["set"]
        (fun args flow ->
           man.eval (mk_py_top T_bool range) flow)
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, [])
      when is_compare_op_fun "set" f ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["set"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume (mk_py_isinstance_builtin e2 "set" range) man flow
             ~fthen:(man.eval (mk_py_top T_bool range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun args flow ->
           let set = match args with | [l] -> l | _ -> assert false in
           let set_addr = match ekind set with
             | E_py_object ({addr_kind = A_py_set _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("set_iterator", [set_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__next__")}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let set_addr = match ekind iterator with
            | E_py_object ({addr_kind = Py_list.A_py_iterator (s, [a], _)}, _) when s = "set_iterator" -> a
            | _ -> assert false in
          let var_els = match akind set_addr with
            | A_py_set a -> a
            | _ -> assert false in
          let els = man.eval (mk_var var_els ~mode:WEAK range) flow in
          let flow = Flow.set_ctx (Eval.get_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list ~empty:(Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__iter__")}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__len__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun args flow ->
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.add")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["set"]
        (fun args flow ->
           let set, element = match args with | [l; e] -> l, e | _ -> assert false in
           debug "set: %a@\n" pp_expr set;
           let var_els = match ekind set with
             | E_py_object ({addr_kind = A_py_set a}, _) -> a
             | _ -> assert false in
           man.exec (mk_assign (mk_var var_els ~mode:WEAK range) element range) flow |>
           man.eval (mk_py_none range))
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_set_of")}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          let set, set_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume (mk_py_isinstance_builtin set "set" range) man flow
            ~fthen:(fun flow ->
                let var = match ekind set with
                  | E_py_object ({addr_kind = A_py_set a}, _) -> a
                  | _ -> assert false in
                Libs.Py_mopsa.check man
                  (mk_py_isinstance (mk_var ~mode:WEAK var range) set_v range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> Option.return


    | _ -> None


  let exec zone stmt man flow = None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
