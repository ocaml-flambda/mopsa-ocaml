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

module StringSet = Framework.Lattices.Powerset.Make(
  struct
    type t = string
    let compare = Pervasives.compare
    let print = Format.pp_print_string
  end)

type _ query += Q_strings_of_var : var -> string query

let () = register_query {
    join = (let f : type r. query_pool -> r query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_strings_of_var _ -> a ^ b
                | _ -> next.join_query query a b in
            f
           );
    meet = (let f : type r. query_pool -> r query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_strings_of_var _ -> assert false
                | _ -> next.meet_query query a b in f)
  }

module Domain =
struct


  module SMap = Framework.Lattices.Partial_map.Make(
    struct
      type t = var
      let compare = compare_var
      let print = pp_var
    end)(StringSet)

  include SMap

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.values.string"
    end)

  let interface = {
    iexec = { provides = [Zone.Z_py_obj]; uses = []; };
    ieval = { provides = [Zone.Z_py_obj, Zone.Z_py_obj]; uses = []}
  }

  let widen ctx = widen

  let print fmt =
    Format.fprintf fmt "strings: @[%a@]@\n" SMap.print

  let init prog man flow = set_env T_cur empty man flow

  let exec zone stmt man flow =
    debug "exec %a" pp_stmt stmt;
    (* FIXME: is there a way to factor using eval? *)
    match skind stmt with
    | S_assign ({ekind = E_var (v, mode)}, {ekind = E_py_object ({addr_kind = A_py_instance "str"},
                                                                 Some {ekind = E_constant (C_string s)})}) ->
      let cur = get_env T_cur man flow in
      let cur =
        SMap.add v (match mode with
        | WEAK ->
          let old_s = Option.default (StringSet.empty) (SMap.find_opt v cur) in
          StringSet.add s old_s
        | STRONG -> StringSet.singleton s)
          cur in
      set_env T_cur cur man flow
      |> Post.return |> Option.return

    | S_assign ({ekind = E_var (v, mode)}, {ekind = E_py_object ({addr_kind = A_py_instance "str"},
                                                                 Some {ekind = E_var (v', mode')})}) ->
      let cur = get_env T_cur man flow in
      debug "old cur = %a@\n" print cur;
      let set_v' = Option.default StringSet.empty (SMap.find_opt v' cur) in
      let cur = match mode with
        | WEAK ->
          let set_v = Option.default StringSet.empty (SMap.find_opt v cur) in
          SMap.add v (StringSet.join set_v set_v') cur
        | STRONG -> SMap.add v set_v' cur in
      debug "cur = %a@\n" print cur;
      (* FIXME: this is to avoid falling into cur, but that's terrible *)
      (if not @@ is_bottom cur then
        set_env T_cur cur man flow
      else
        flow)
        |> Post.return |> Option.return

    (* FIXME: remove vars / rename / ... not caught in the zone yet ? :/ *)
    (* | S_remove {ekind = E_var (v, _)} ->
     *   let cur = get_env T_cur man flow in
     *   set_env T_cur (SMap.remove v cur) man flow
     *   |> Post.return |> Option.return *)

    | _ ->  None

  let eval zs exp man flow = None
    (* let cur = get_env T_cur man flow in
     * match ekind exp with
     *   | E_var (v, mode) when SMap.mem v cur ->
     *     assert false
     *   | _ -> None *)

  let ask : type r. r query -> ('a, t) man -> 'a flow -> r option = fun query man flow ->
    match query with
    | Q_strings_of_var v ->
      let cur = get_env T_cur man flow in
      (* FIXME *)
      Some (try StringSet.choose @@ SMap.find v cur with Not_found -> "")

    | _ -> None

  let refine channel man flow = Channel.return flow
  let merge _ _ _ = assert false
end

let () = Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain);
