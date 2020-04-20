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


(** The STACK signature allows implementing domains that are parameterized by
    other domains. However, the stacked and the parameter domain are loosely
    coupled: the stack domain knows only the concrete semantic of its
    parameter, not the abstract one. In addition, in contrast to classic OCaml
    functors, the same instance of the parameter abstract domain can be shared
    with other stack domains.
*)


open Ast
open Program
open Expr
open Stmt
open Context
open Token
open Flow
open Lattice
open Eval
open Log
open Post
open Query
open Zone
open Id
open Interface
open Channel
open Alarm


(*==========================================================================*)
(**                         {2 Stack managers}                              *)
(*==========================================================================*)

(** Managers provide access to full analyzer and the sub-tree
    abstraction of the stack domain.
*)
type ('a, 't, 's) man = ('a,'t,'s) Lowlevel.man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] and ['s] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;
  get_sub : 'a -> 's;
  set_sub : 's -> 'a -> 'a;

  (** Analyzer transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> 'a eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merge logs *)
  get_log : log -> log;
  set_log : log -> log -> log;

  (** Accessors to the sub-tree merge logs *)
  get_sub_log : log -> log;
  set_sub_log : log -> log -> log;

  (** Sub-tree merger *)
  merge_sub : 's -> 's * log -> 's * log -> 's;
}


(*==========================================================================*)
(**                         {2 Stack signature}                             *)
(*==========================================================================*)

(** Unified signature of stacked abstract domains *)
module type STACK =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t id
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

  val alarms : alarm_class list
  (** List of alarms detected by the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: ('a,t,'s) man -> uctx -> t * 's -> t * 's -> bool * 's * 's
  (** [subset (a1, s1) (a2, s2) man] tests whether [a1] is related to
      (or included in) [a2] and unifies the sub-tree elements [s1] and
      [s2]. *)


  val join: ('a,t,'s) man -> uctx -> t * 's -> t * 's -> t * 's * 's
  (** [join (a1, s1) (a2, s2) man] computes an upper bound of [a1]
      and [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val meet: ('a,t,'s) man -> uctx -> t * 's -> t * 's -> t * 's * 's
  (** [meet (a1, s1) (a2, s2) man] computes a lower bound of [a1] and
      [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val widen:
    ('a,t,'s) man -> uctx -> t * 's -> t * 's -> t * 's * 's * bool
  (** [widen ctx (a1, s1) (a2, s2) man] computes an upper bound of
      [a1] and [a2] that ensures stabilization of ascending chains and
      unifies the sub-tree elements [s1] and [s2]. *)


  val merge: t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t,'s) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t,'s) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t,'s) man -> 'a flow -> 'a eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t,'s) man -> 'a flow -> 'r option
  (** Handler of queries *)

  val refine : channel -> ('a,t,'s) man -> 'a flow -> 'a flow with_channel
  (** Refinement using reduction channels *)

end




(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let set_env = Lowlevel.set_env

let get_env = Lowlevel.get_env

let map_env = Lowlevel.map_env

let set_sub_env = Lowlevel.set_sub_env

let get_sub_env = Lowlevel.get_sub_env

let map_sub_env = Lowlevel.map_sub_env

let assume = Lowlevel.assume

let assume_flow = Manager.assume_flow

let switch = Lowlevel.switch

let exec_stmt_on_all_flows = Lowlevel.exec_stmt_on_all_flows

let exec_block_on_all_flows = Lowlevel.exec_block_on_all_flows

let post_to_flow = Lowlevel.post_to_flow

(** [state_exec f ctx man a s] executes transfer function [f] over a
    flow containing domain's state [a] and its sub-tree state [s] *)
let state_exec (f:'a flow -> 'a post) ctx (man:('a,'t,'s) man) (a:'t) (s:'s) : 't * 's =
  (* Create a singleton flow with the given environment *)
  let flow = Flow.singleton
      (Context.empty |> Context.set_unit ctx)
      T_cur
      (man.set a man.lattice.top |> man.set_sub s)
  in
  (* Execute the statement *)
  let flow' = f flow |> post_to_flow man in
  (* Get the resulting environment *)
  get_env T_cur man flow',
  get_sub_env T_cur man flow'
  

(** [state_exec f ctx man s] executes transfer function [f] over a
    flow containing a singleton sub-tree state [s] *)
let sub_state_exec (f:'a flow -> 'a post) ctx (man:('a,'t,'s) man) (s:'s) : 's =
  (* Create a singleton flow with the given environment *)
  let flow = Flow.singleton
      (Context.empty |> Context.set_unit ctx)
      T_cur
      (man.set_sub s man.lattice.top)
  in
  (* Execute the statement *)
  let flow' = f flow |> post_to_flow man in
  (* Get the resulting environment *)
  get_sub_env T_cur man flow'


(*==========================================================================*)
(**                         {2 Low-level cast}                              *)
(*==========================================================================*)

(** Cast a unified signature into a low-level signature *)
module MakeLowlevelStack(S:STACK) : Lowlevel.STACK with type t = S.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = S.t

  let id = S.id

  let name = S.name

  let interface = S.interface

  let alarms = S.alarms

  let bottom = S.bottom

  let top = S.top

  let is_bottom = S.is_bottom

  let print = S.print


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man ctx a a' =
    let b, s, s' = S.subset
        man
        ctx
        (man.get a, man.get_sub a)
        (man.get a', man.get_sub a')
    in
    b, man.set_sub s a, man.set_sub s' a'

  let join man ctx a a' =
    let x, s, s' = S.join
        man
        ctx
        (man.get a, man.get_sub a)
        (man.get a', man.get_sub a')
    in
    x, man.set_sub s a, man.set_sub s' a'

  let meet man ctx a a' =
    let x, s, s' = S.meet
        man
        ctx
        (man.get a, man.get_sub a)
        (man.get a', man.get_sub a')
    in
    x, man.set_sub s a, man.set_sub s' a'

  let widen man ctx a a' =
    let x, s, s', stable = S.widen
        man
        ctx
        (man.get a, man.get_sub a)
        (man.get a', man.get_sub a')
    in
    x, man.set_sub s a, man.set_sub s' a', stable

  let merge = S.merge

  (** {2 Transfer functions} *)
  (** ********************** *)

  let init = S.init

  let exec = S.exec

  let eval = S.eval

  let ask = S.ask

  let refine = S.refine

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


(** Auto-logger lifter used when registering a domain *)
module AutoLogger(S:STACK) : STACK with type t = S.t =
struct
  include S
  let exec zone stmt man flow =
    S.exec zone stmt man flow |>
    OptionExt.lift @@ fun res ->
    Cases.map_log (fun log ->
        man.set_log (
          man.get_log log |> Log.add_stmt_to_log stmt
        ) log
      ) res

end


let stacks : (module STACK) list ref = ref []

let register_stack dom =
  let module S = (val dom : STACK) in
  stacks := (module AutoLogger(S)) :: !stacks

let find_stack name =
  List.find (fun dom ->
      let module S = (val dom : STACK) in
      compare S.name name = 0
    ) !stacks

let mem_stack name =
  List.exists (fun dom ->
      let module S = (val dom : STACK) in
      compare S.name name = 0
    ) !stacks

let names () =
  List.map (fun dom ->
      let module S = (val dom : STACK) in
      S.name
    ) !stacks
