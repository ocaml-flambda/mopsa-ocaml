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

          | _ -> next.meet query a b
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

  include Nonrel.Make(Value)

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

  let init_history ctx =
    Context.uadd history_key History.empty ctx


  (** Update the pre-condition of the statement *)
  let update_pre stmt ctx pre =
    match skind stmt with
    | S_assign _ | S_assume _
      when Location.is_orig stmt.srange ->

      let range = srange stmt in

      let history = Context.ufind history_key ctx in

      let old_pre, old_post =
        try History.find range history
        with Not_found -> (bottom,bottom)
      in
      let pre' = join old_pre pre in
      let history' = History.add range (pre', old_post) history in

      Context.uadd history_key history' ctx


    | _ -> ctx

  (** Update the post-condition of the statement *)
  let update_post stmt ctx post =
    match skind stmt with
    | S_assign _ | S_assume _
      when Location.is_orig stmt.srange ->

      let range = srange stmt in

      let history = Context.ufind history_key ctx in

      let old_pre, old_post =
        try History.find range history
        with Not_found -> (bottom,bottom)
      in
      let post' = join old_post post in
      let history' = History.add range (old_pre, post') history in

      Context.uadd history_key history' ctx

    | _ -> ctx


  (** Get history of reachable states *)
  let get_history ctx =
    Context.ufind history_key ctx

  let export_state (state:t) : state =
    VarMap.fold (fun var value acc ->
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


  let init prog ctx =
    let ctx' = init_history ctx in
    init prog ctx'


  let exec stmt ctx a =
    let ctx = update_pre stmt ctx a in

    exec stmt ctx a |> Option.lift @@ fun (a',ctx) ->

    let ctx = update_post stmt ctx a' in
    a',ctx

  let ask : type r. r query -> Context.uctx -> t -> r option =
    fun query ctx a ->
      match query with
      | Q_reachable_states ->
        let history = get_history ctx in
        Some (export_history history)

      | _ -> ask query ctx a



end


(** Create a non-relational abstraction with eventual history caching
    depending on the option opt_collect_states *)
module Make(Value:Sig.Value.Lowlevel.VALUE) () : Sig.Domain.Simplified.DOMAIN =
  (val
    if !opt_collect_states
    then
      let module M = MakeWithHistory(Value) in
      (module M)
    else
      let module M = Nonrel.Make(Value) in
      (module M)
    : Sig.Domain.Simplified.DOMAIN
  )
