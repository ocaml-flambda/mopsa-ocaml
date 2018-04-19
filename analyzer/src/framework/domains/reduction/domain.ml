(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Domains supporting reduction operator.
*)

open Lattice
open Flow
open Manager
open Eval
open Query

(** Reduction channel *)
type channel = Query: 'a query -> channel

(** Pre-reduction flow merger *)
type merger = Ast.stmt

(** Reduction flow *)
type 'a rflow = {
  out: 'a flow;             (** not yet reduced post-condition *)
  publish: channel list;    (** published reduction channels *)
  subscribe: (
    channel (** reduction channel *) ->
    'a flow (** merged post-condition *) ->
    'a rflow option
  ); (** subscription to reduction channels *)
  mergers: merger list; (** pre-reduction mergers *)
}

type 'a reval_case = (Ast.expr * merger list, 'a) eval_case
type 'a revals = (Ast.expr * merger list, 'a) evals

(** Abstract domain signature. *)
module type DOMAIN =
sig

  include Lattice.LATTICE

  val init :
    ('a, t) manager -> Context.context -> Ast.program -> 'a flow ->
    Context.context * 'a flow

  (** Abstract transfer function of statements. *)
  val exec:
    ('a, t) manager -> Context.context -> Ast.stmt -> 'a flow -> 'a rflow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval:
    ('a, t) manager -> Context.context -> Ast.expr -> 'a flow -> 'a revals option

  (** Handler of generic queries. *)
  val ask:
    ('a, t) manager -> Context.context -> 'r Query.query -> 'a flow -> 'r option
end



let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains


module MakeStateful(Domain: DOMAIN) : Stateful.DOMAIN =
struct

  type t = Domain.t

  let bottom = Domain.bottom
  let is_bottom = Domain.is_bottom
  let top = Domain.top
  let is_top = Domain.is_top
  let leq = Domain.leq
  let join = Domain.join
  let meet = Domain.meet
  let widening = Domain.widening
  let print = Domain.print

  let init = Domain.init

  let exec man ctx stmt flow =
    match Domain.exec man ctx stmt flow with
    | None -> None
    | Some out -> Some (out.out)

  let eval man ctx exp flow =
    Domain.eval man ctx exp flow |>
    Eval.oeval_map (fun (evl, flow, cleaner) ->
        match evl with
        | None -> (None, flow, cleaner)
        | Some (exp, _) -> (Some exp, flow, cleaner)
      )

  let ask = Domain.ask

end

let return x = Some x
let fail = None
