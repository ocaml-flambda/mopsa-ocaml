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

(** [Nonrel ∈ 𝒱 → 𝒟] lifts a non-relational value abstraction into an
    abstract domain of partial environments from variables to values.
*)

open Ast.All
open Core.All


(****************************************************************************)
(**             {2 Domain without history of reachable states}              *)
(****************************************************************************)

module MakeWithoutHistory(Value: Sig.Value.Lowlevel.VALUE) =
struct


  (** {2 Domain header} *)
  (** ***************** *)

  (** Map with variables as keys. *)
  module VarMap =
    Lattices.Partial_map.Make
      (Var)
      (Value)

  include VarMap

  include GenDomainId(struct
      type typ = t
      let name = Value.name
    end)

  let merge pre (post1, log1) (post2, log2) =
    assert false

  let print fmt a =
    Format.fprintf fmt "%s:@ @[   %a@]@\n" Value.display VarMap.print a



  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Expressions annotated with abstract values; useful for assignment and compare. *)
  type aexpr =
    | A_var of var * Value.t
    | A_cst of constant * Value.t
    | A_unop of operator * typ  * aexpr * Value.t
    | A_binop of operator * typ * aexpr * Value.t * aexpr * Value.t
    | A_unsupported

  (** Value manager *)
  let rec vman (a:t) : (Value.t,Value.t) vman = {
    vget = (fun v -> v);
    vset = (fun v _ -> v);
    veval = (fun e ->
        eval e a |>
        Option.default (A_unsupported,Value.top) |>
        snd
      );
    vcast = (fun id v ->
        match Value.cast (vman a) id v with
        | Some rr -> rr
        | None -> Exceptions.panic "cast not handled"
      );
  }


  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  and eval (e:expr) (a:t) : (aexpr * Value.t) option =
    match ekind e with
    | E_var(var, _) ->
      let v = VarMap.find var a in
      (A_var (var, v), v) |>
      Option.return

    | E_constant(c) ->
      let v = Value.of_constant e.etyp c in
      (A_cst (c, v), v) |>
      Option.return

    | E_unop (op,e1) ->
      eval e1 a |> Option.bind @@ fun (ae1, v1) ->
      let v = Value.unop (vman a) e.etyp op v1 in
      (A_unop (op, e.etyp, ae1, v1), v) |>
      Option.return

    | E_binop (op,e1,e2) ->
      eval e1 a |> Option.bind @@ fun (ae1, v1) ->
      eval e2 a |> Option.bind @@ fun (ae2, v2) ->
      let v = Value.binop (vman a) e.etyp op v1 v2 in
      (A_binop (op, e.etyp, ae1, v1, ae2, v2), v) |>
      Option.return

    | _ ->
      (* unsupported -> ⊤ *)
      (* A_unsupported, Value.top *)
      None



  (** Backward refinement of expressions; given an annotated tree, and
      a target value, refine the environment using the variables in the
      expression *)
  let rec refine (ae:aexpr) (v:Value.t) (r:Value.t) (a:t) : t =
    let r' = Value.meet v r in
    match ae with
    | A_var (var, _) ->
      if Value.is_bottom r'
      then bottom
      else VarMap.add var r' a

    | A_cst(_) ->
      if Value.is_bottom r'
      then bottom
      else a

    | A_unop (op, typ, ae1, v1) ->
      let w = Value.bwd_unop (vman a) typ op v1 r' in
      refine ae1 v1 w a

    | A_binop (op, typ, ae1, v1, ae2, v2) ->
      let w1, w2 = Value.bwd_binop (vman a) typ op v1 v2 r' in
      let a1 = refine ae1 v1 w1 a in
      refine ae2 v2 w2 a1

    | A_unsupported ->
      a

  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let rec filter (e:expr) (r:bool) (a:t) : t option =
    match ekind e with

    | E_unop (O_log_not, e) ->
      filter e (not r) a

    | E_binop (O_log_and, e1, e2) ->
      filter e1 r a |> Option.bind @@ fun a1 ->
      filter e2 r a |> Option.bind @@ fun a2 ->
      (if r then meet else join) a1 a2 |>
      Option.return

    | E_binop (O_log_or, e1, e2) ->
      filter e1 r a |> Option.bind @@ fun a1 ->
      filter e2 r a |> Option.bind @@ fun a2 ->
      (if r then join else meet) a1 a2 |>
      Option.return

    | E_constant c ->
      let v = Value.of_constant e.etyp c in
      let w = Value.filter (vman a) v r in
      (if Value.is_bottom w then bottom else a) |>
      Option.return

    | E_var(var, _) ->
      let v = find var a in
      let w = Value.filter (vman a) v r in
      (if Value.is_bottom w then bottom else add var w a) |>
      Option.return

    (* arithmetic comparison part, handled by Value *)
    | E_binop (op, e1, e2) ->
      (* evaluate forward each argument expression *)
      eval e1 a |> Option.bind @@ fun (ae1,v1) ->
      eval e2 a |> Option.bind @@ fun (ae2,v2) ->

      (* apply comparison *)
      let r1, r2 = Value.compare (vman a) e1.etyp op v1 v2 r in

      (* propagate backward on both argument expressions *)
      refine ae2 v2 r2 @@ refine ae1 v1 r1 a |>
      Option.return

    | _ -> assert false



  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog = empty

  let zones = Value.zones

  let rec exec stmt (map:t) : t option =
    match skind stmt with
    | S_remove { ekind = E_var (v, _) }  ->
      VarMap.remove v map |>
      Option.return

    | S_add { ekind = E_var (v, _) } ->
      VarMap.add v Value.top map |>
      Option.return

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars ->
      let vars = List.map (function
            | { ekind = E_var (v, _) } -> v
            | _ -> assert false
          ) vars
      in
      VarMap.fold (fun v _ acc ->
          if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
        ) map map |>
      Option.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let v = VarMap.find var1 map in
      VarMap.remove var1 map |>
      VarMap.add var2 v |>
      Option.return

    | S_forget { ekind = E_var (var, _) } ->
      add var Value.top map |>
      Option.return

    | S_assign ({ ekind= E_var (var, mode) }, e)  ->
      eval e map |> Option.lift @@ fun (_, v) ->
      let map' = VarMap.add var v map in
      begin
        match mode with
        | STRONG -> map'
        | WEAK -> join map map'
      end

    | S_expand ({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let value = find v map in
      List.fold_left (fun acc v' ->
          add v' value acc
        ) map vl |>
      Option.return

    (* FIXME: check weak variables in rhs *)
    | S_assume e ->
      filter e true map

    | _ -> None


  let ask : type r. r Query.query -> t -> r option =
    fun query map ->
      Value.ask (vman map) query


  let refine channel a =
    assert false


end



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
    query_join = (fun next a b ->
        Exceptions.panic ~loc:__LOC__ "join of Q_reachable_states not implemented"
      );
    query_meet = (fun next a b ->
        Exceptions.panic ~loc:__LOC__ "meet of Q_reachable_states not implemented"
      );
  }


(****************************************************************************)
(**              {2 Domain with history of reachable states}                *)
(****************************************************************************)

(** Command line option to activate state collection *)
let opt_collect_states = ref false


module MakeWithHistory(Value: Sig.Value.Lowlevel.VALUE) =
struct

  module D = MakeWithoutHistory(Value)

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
          MakeWithoutHistory(Value)
        )
      in
      (module M)
    : Sig.Domain.Intermediate.DOMAIN
  )
