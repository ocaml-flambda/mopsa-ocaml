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
open Manager
open Eval
open Log
open Post
open Zone
open Id
open Interface
open Channel

(** Unified signature of stacked abstract domains *)
module type STACK =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

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

  val subset: 's sman -> t * 's -> t * 's -> bool * 's * 's
  (** [subset (a1, s1) (a2, s2) sman] tests whether [a1] is related to
      (or included in) [a2] and unifies the sub-tree elements [s1] and
      [s2]. *)


  val join: 's sman -> t * 's -> t * 's -> t * 's * 's
  (** [join (a1, s1) (a2, s2) sman] computes an upper bound of [a1]
      and [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val meet: 's sman -> t * 's -> t * 's -> t * 's * 's
  (** [meet (a1, s1) (a2, s2) sman] computes a lower bound of [a1] and
      [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val widen:
    's sman -> uctx -> t * 's -> t * 's -> t * 's * 's * bool
  (** [widen ctx (a1, s1) (a2, s2) sman] computes an upper bound of
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

  val init : program -> ('a, t) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> ('a,'s) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

  val refine : channel -> ('a,t) man -> ('a,'s) man -> 'a flow -> 'a flow with_channel
  (** Refinement using reduction channels *)

end


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

  let bottom = S.bottom

  let top = S.top

  let is_bottom = S.is_bottom

  let print = S.print


  (** {2 Stack manager} *)
  (** ***************** *)

  (** Create a stack manager from a global manager of the sub-tree *)
  let stack_man (man:('a,'s) man) : 's sman = {
    sexec = (fun ?(zone=any_zone) stmt s ->
        (* Create a singleton flow with the given environment *)
        let flow = Flow.singleton
            Context.empty
            T_cur
            (man.lattice.top |> man.set s)
        in
        (* Execute the statement *)
        let flow' = man.exec ~zone stmt flow in
        (* Get the resulting environment *)
        get_domain_env T_cur man flow'
      );

    sask = (fun query s ->
        (* Create a singleton flow with the given environment *)
        let flow = Flow.singleton
            Context.empty
            T_cur
            (man.lattice.top |> man.set s)
        in
        (* Ask the query *)
        man.ask query flow
      );
  }


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man sman a a' =
    let b, s, s' = S.subset (stack_man sman)
        (man.get a, sman.get a)
        (man.get a', sman.get a')
    in
    b, sman.set s a, sman.set s' a'

  let join man sman a a' =
    let x, s, s' = S.join (stack_man sman)
        (man.get a, sman.get a)
        (man.get a', sman.get a')
    in
    x, sman.set s a, sman.set s' a'

  let meet man sman a a' =
    let x, s, s' = S.meet (stack_man sman)
        (man.get a, sman.get a)
        (man.get a', sman.get a')
    in
    x, sman.set s a, sman.set s' a'

  let widen man sman ctx a a' =
    let x, s, s', stable = S.widen (stack_man sman) ctx
        (man.get a, sman.get a)
        (man.get a', sman.get a')
    in
    x, sman.set s a, sman.set s' a', stable

  let merge man pre (post1,log1) (post2,log2) =
    S.merge
      (man.get pre)
      (man.get post1, man.get_log log1)
      (man.get post2, man.get_log log2)


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


let stacks : (module STACK) list ref = ref []

let register_stack dom =
  stacks := dom :: !stacks

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
