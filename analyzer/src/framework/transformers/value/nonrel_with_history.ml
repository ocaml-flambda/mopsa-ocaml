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

(** [NonrelHistory ∈ 𝒱 → 𝒟] lifts a non-relational value abstraction into
    an abstract domain of partial environments from variables to values and
    keeps history of pre and post conditions of reachable statements.
*)

open Ast.All
open Core.All


(****************************************************************************)
(**                       {2 Reachable states}                              *)
(****************************************************************************)

(** The non-relational domain can store all reachable environments encountered
    during the analysis. They can be exported via query Q_reachable_states,
    which returns a list (loc, pre, post) representing the pre and post
    conditions of the program at location loc. The environments pre and post
    are structured as lists of (variable, value) mapping variables to
    their values in string format.
*)

open Location

(** Export formats of reachable states *)
type value = string
type state = (var * value) list
type history = (range * (state * state)) list

(** Query to extract the collection of reachable states *)
type _ query += Q_reachable_states : history query

let () =
  register_query {
    query_join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_reachable_states ->
            Exceptions.panic ~loc:__LOC__ "join of Q_reachable_states not implemented"

          | _ -> next.join query a b
      in
      f
    );
    query_meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_reachable_states ->
            Exceptions.panic ~loc:__LOC__ "join of Q_reachable_states not implemented"

          | _ -> next.join query a b
      in
      f
      );
  }


(****************************************************************************)
(**                        {2 Domain definition}                            *)
(****************************************************************************)

(** Command line option to activate state collection *)
let opt_collect_states = ref false


module MakeWithHistory(Value: Sig.Value.Lowlevel.VALUE) =
struct

  module D = Nonrel.Make(Value)

  include Sig.Domain.Simplified.MakeIntermediate(D)


  (****************************************************************************)
  (**                    {2 Program state history}                            *)
  (****************************************************************************)

  (* We keep all encountered pre and post states a flow-insensitive  context *)

  module History = MapExt.Make(
    struct
      type t = range
      let compare = compare_range
    end
    )

  (* This trick is necessary to escape the "acyclic type" error later *)
  type s = t

  (** Key of the cache in the flow-insensitive context *)
  let history_key =
    let module C = Context.GenUnitKey(
      struct

        type t = (s * s) History.t

        let print fmt history =
          Format.fprintf fmt "@[<v>%a@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
               (fun fmt (range, (pre,post)) ->
                  Format.fprintf fmt "%a:@  pre: @[%a@]@  post: @[%a@]"
                    pp_range range
                    print pre
                    print post
               )
            ) (History.bindings history)
      end
      )
    in
    C.key

  let init_history flow =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit history_key History.empty
    ) flow


  (** Update the pre-condition of the statement *)
  let update_pre stmt man flow =
    match skind stmt with
    | S_assign _ | S_assume _
      when Location.is_orig stmt.srange ->

      let range = srange stmt in
      let pre = get_domain_env T_cur man flow in

      let ctx = Flow.get_ctx flow in
      let history = Context.find_unit history_key ctx in

      let old_pre, old_post =
        try History.find range history
        with Not_found -> (bottom,bottom)
      in
      let pre' = join old_pre pre in
      let history' = History.add range (pre', old_post) history in

      let ctx' = Context.add_unit history_key history' ctx in
      Flow.set_ctx ctx' flow

    | _ -> flow

  (** Update the post-condition of the statement *)
  let update_post stmt man flow =
    match skind stmt with
    | S_assign _ | S_assume _
      when Location.is_orig stmt.srange ->

      let range = srange stmt in
      let post = get_domain_env T_cur man flow in

      let ctx = Flow.get_ctx flow in
      let history = Context.find_unit history_key ctx in

      let old_pre, old_post =
        try History.find range history
        with Not_found -> (bottom,bottom)
      in
      let post' = join old_post post in
      let history' = History.add range (old_pre, post') history in

      let ctx' = Context.add_unit history_key history' ctx in
      Flow.set_ctx ctx' flow

    | _ -> flow


  (** Get history of reachable states *)
  let get_history flow =
    let ctx = Flow.get_ctx flow in
    Context.find_unit history_key ctx

  let export_state (state:t) : state =
    D.VarMap.fold (fun var value acc ->
        let value_str =
          Value.print Format.str_formatter value;
          Format.flush_str_formatter ()
        in
        (var, value_str) :: acc
      ) state []

  let export_history (history:(t*t) History.t) : history =
    History.fold (fun range (pre,post) acc ->
        (range, (export_state pre, export_state post)) :: acc
      ) history []


  (****************************************************************************)
  (**                       {2 Transfer functions}                            *)
  (****************************************************************************)


  let init prog man flow =
    init prog man flow |>
    init_history

  let exec zone stmt man flow =
    update_pre stmt man flow |>

    exec zone stmt man |>
    Option.lift @@ Post.bind @@ fun flow ->

    update_post stmt man flow |>
    Post.return

  let ask : type r. r query -> ('a,t) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Q_reachable_states ->
        let history = get_history flow in
        Some (export_history history)

      | _ -> ask query man flow



end


(** Create a non-relational abstraction with eventual history caching
    depending on the option opt_collect_states *)
module Make(Value:Sig.Value.Lowlevel.VALUE) () : Sig.Domain.Intermediate.DOMAIN =
  (val
    if !opt_collect_states
    then
      let module M = MakeWithHistory(Value) in
      (module M)
    else
      let module M = Sig.Domain.Simplified.MakeIntermediate(
          Nonrel.Make(Value)
        )
      in
      (module M)
    : Sig.Domain.Intermediate.DOMAIN
  )
