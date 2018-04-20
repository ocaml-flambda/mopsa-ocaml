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

(** Reduction channels *)
type channel = ..

type channel +=
  | Query: 'a query -> channel
  (** Query reduction channel. Useful for lazy evaluation of post-condition reductions *)

(** Pre-reduction flow merger *)
type merger = Ast.stmt

(** Reduction flow *)
type 'a rflow = {
  out: 'a flow;             (** not yet reduced post-condition *)
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

  val refine:
    ('a, t) manager -> Context.context -> channel -> 'a flow -> 'a rflow option

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
