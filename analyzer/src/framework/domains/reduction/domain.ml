(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Domains residing within a reduction product.

   When interpreting statements, reduction domains return a post condition and
   a number of reduction channels that can be used by other domains within the
   reduced product to refine their post conditions.

   The post conditions of all domains are combined into a single one by computing
   their meet. However, a merging step is required in order to ensure soundness.
   More particularly, when a domain D1 computes its post condition by modifying some
   information that is maintained by another domain outside the reduced product,
   it needs to inform the other domains about this change so they update their
   post condition in a way to ensure the soundness of the meet with the post condition
   of D1.

   To do so, D1 decorate its post condition with a list of statements to ensure sound
   merge, for e.g. by simply removing the extra information from the post conditions of
   the other domains.
*)

open Lattice
open Flow
open Manager
open Eval
open Query

(** Reduction channels *)
type channel = ..

type channel +=
  | Query: 'a query -> channel
  (** Query reduction channel. Useful for lazy evaluation of reduction information *)

(** Pre-reduction flow merger. Used to unify information generated by a domain D but maintained outside D. *)
type merger = Ast.stmt

(** Reduction flow *)
type 'a rflow = {
  out: 'a flow;             (** post-condition *)
  publish: channel list;    (** published reduction channels *)
  mergers: merger list; (** pre-reduction mergers *)
}

let rec rflow_join (man: 'a flow_manager) rflow1 rflow2 = {
  out = man.join rflow1.out rflow2.out;
  publish = (@) rflow1.publish rflow2.publish;
  mergers = (@) rflow1.mergers rflow2.mergers;
}

let rec rflow_meet (man: 'a flow_manager) rflow1 rflow2 = {
  out = man.meet rflow1.out rflow2.out;
  publish = List.filter (fun channel -> List.mem channel rflow1.publish) rflow2.publish;
  mergers = List.filter (fun merger -> List.mem merger rflow1.mergers) rflow2.mergers;
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

  (** Refine a post condition by exploiting information from a reduction channel *)
  val refine:
    ('a, t) manager -> Context.context -> channel -> 'a flow -> 'a rflow option

  (** Evaluation of expressions. *)
  val eval:
    ('a, t) manager -> Context.context -> Ast.expr -> 'a flow -> 'a revals option

  (** Query handler. *)
  val ask:
    ('a, t) manager -> Context.context -> 'r Query.query -> 'a flow -> 'r option
end



let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains


(** Functor to create a classic standalone domain from a reduction domain. *)
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

let return flow = Some {
    out = flow;
    publish = [];
    mergers = [];
  }

let fail = None

let eval_to_rexec
    (f: 'a -> 'b flow -> 'b rflow)
    (exec: Ast.stmt -> 'b flow -> 'b flow)
    (man: 'b flow_manager)
    ?(empty = (fun flow -> {
          out = flow;
          publish = [];
          mergers = [];
        }))
    (eval: ('a, 'b) evals)
  : 'b rflow =
  eval |> eval_substitute
    (fun (exp', flow, clean) ->
       let flow =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty flow
       in
       let out = clean |> List.fold_left (fun acc stmt ->
           exec stmt acc
         ) flow.out in
       {flow with out}
    )
    (rflow_join man)
    (rflow_meet man)


let eval_to_orexec
    (f: 'a -> 'b flow -> 'b rflow option)
    (exec: Ast.stmt -> 'b flow -> 'b flow)
    (man: 'b flow_manager)
    ?(empty = (fun flow -> return flow))
    (eval: ('a, 'b) evals)
  : 'b rflow option =
  eval |> eval_substitute
    (fun (exp', flow, clean) ->
       let rflow =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty flow
       in
       match rflow with
       | None -> None
       | Some rflow ->
         let out = clean |> List.fold_left (fun acc stmt ->
             exec stmt acc
           ) rflow.out in
         Some {rflow with out}
    )
    (fun flow1 flow2 ->
       match flow1, flow2 with
       | None, x | x, None -> x
       | Some flow1, Some flow2 -> Some (rflow_join man flow1 flow2)
    )
    (fun flow1 flow2 ->
       match flow1, flow2 with
       | None, x | x, None -> x
       | Some flow1, Some flow2 -> Some (rflow_meet man flow1 flow2)
    )
