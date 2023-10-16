(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2022 The MOPSA Project.                               *)
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
open Sig.Abstraction.Domain
open Universal.Ast
open C.Ast
open C.Common.Points_to
open Top

type ('a, _) query += Q_cpython_attached_callstack : addr -> ('a, Callstack.callstack) query

let () = register_query {
             join = (let f: type a r. query_pool -> (a, r) query -> r -> r -> r =
                       fun next query a b ->
                       match query with
                       | Q_cpython_attached_callstack _ -> assert false
                       | _ -> next.pool_join query a b in
                     f);
             meet = (let f: type a r. query_pool -> (a, r) query -> r -> r -> r =
                       fun next query a b ->
                       match query with
                       | Q_cpython_attached_callstack _ -> assert false
                       | _ -> next.pool_meet query a b in
                     f)
           }

module Domain =
  struct

    module Callstacks = struct
      module CallstackSet = Framework.Lattices.Powerset.Make
                            (struct
                              type t = Callstack.callstack
                              let compare = Callstack.compare_callstack
                              let print = unformat pp_callstack
                            end)

      include CallstackSet

      let max_size = 1
      let bound (x:t) : t =
        match x with
        | Nt s when Set.cardinal s <= max_size -> x
        | _ -> TOP

      let join a1 a2 = CallstackSet.join a1 a2 |> bound

      let add v t =
        add v t |> bound
    end


    module CallstackMap = Framework.Lattices.Partial_map.Make(Addr)(Callstacks)

    include CallstackMap

    include Framework.Core.Id.GenDomainId(
                struct
                  type nonrec t = t
                  let name = "cpython.callstack_tracking"
                end)

    let checks = []

    let init _ man flow =
      Hashtbl.add C.Common.Builtins.builtin_functions "_mopsa_pyerr_bind_cs_to" ();
      set_env T_cur empty man flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_c_builtin_call ("_mopsa_pyerr_bind_cs_to", [exc]) ->
         resolve_pointer exc man flow >>$ (fun pt flow ->
          match pt with
          | P_block({base_kind = Addr a}, _, _) ->
             let cur = get_env T_cur man flow in
             let flow = set_env T_cur
                          (add a (Callstacks.singleton (Flow.get_callstack flow)) cur)
                          man flow in
             debug "%a" (format print) (get_env T_cur man flow);
             Eval.singleton (mk_one range) flow
          | _ -> assert false
        ) |> OptionExt.return

      | _ -> None

    let exec stmt man flow =
      match skind stmt with
      | S_free addr ->
         let cur = get_env T_cur man flow in
         let flow =
           if mem addr cur then
             set_env T_cur (remove addr cur) man flow
           else flow in
         man.exec ~route:(Below name) stmt flow
         |> OptionExt.return
      | _ -> None

    let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> ('a, r) cases option =
      fun query man flow ->
      match query with
      | Q_cpython_attached_callstack a ->
         let cur = get_env T_cur man flow in
         OptionExt.lift (fun cs ->
             assert(Callstacks.cardinal cs = 1);
             Cases.singleton (Callstacks.choose cs) flow)
           (find_opt a cur)

      | _ -> None

    let print_expr _ _ _ _ = ()
    let print_state printer a =
      pprint ~path:[Key "C/Python callstack tracking"] printer (pbox CallstackMap.print a)

    let merge _ _ _ = assert false
  end

let () = register_standard_domain(module Domain)
