(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Domains with local computations over flows. *)

open Flow
open Lattice
open Manager


(*==========================================================================*)
                  (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN =
sig

  include Lattice.LATTICE

  val init :
    Ast.program -> t flow_manager -> t flow ->
    t flow

  (** Abstract transfer function of statements. *)
  val exec: Ast.stmt -> t flow_manager -> Context.context -> t flow ->
    t flow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval: Ast.expr -> t flow_manager -> Context.context -> t flow ->
    t flow eval_output option

  (** Handler of generic queries. *)
  val ask: 'r Query.query -> t flow_manager -> Context.context -> t flow ->
    'r option

end

module MakeGlobalDomain(Domain: DOMAIN) : Global.DOMAIN =
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

  let lattice_man : t lattice_manager = {
    bottom;
    top;
    is_bottom;
    is_top;
    leq;
    join;
    meet;
    widening;
    print;
  }

  let flow_man = Flow.lift_lattice_manager lattice_man

  let init prg man fa =
    let fl = man.flow.map (fun eabs tk -> man.ax.get eabs) fa in
    let fl' = Domain.init prg flow_man fl in
    flow_man.map (fun eabs' tk ->
        let eabs = man.flow.get tk fa in
        man.ax.set eabs' eabs
      ) fl'

  let exec stmt (man: ('a, t) manager) ctx (fa: 'a flow) : 'a flow option =
    man.flow.map (fun eabs tk -> man.ax.get eabs) fa |>
    Domain.exec stmt flow_man ctx |>
    Exec.map (fun fa1 ->
        flow_man.map (fun ea1 tk ->
            let ea = man.flow.get tk fa in
            man.ax.set ea1 ea
          ) fa1
      )

  let eval exp man ctx fa =
    man.flow.map (fun env tk -> man.ax.get env) fa |>
    Domain.eval exp flow_man ctx |>
    Eval.map (fun (exp', fa', cls) ->
        let fa'' = flow_man.map (fun ea' tk ->
            let ea = man.flow.get tk fa in
            man.ax.set ea' ea
          ) fa'
        in
        (exp', fa'', cls)
      )

  let ask query man ctx fa =
    man.flow.map (fun env tk -> man.ax.get env) fa |>
    Domain.ask query flow_man ctx

end


let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module GD = MakeGlobalDomain(D) in
  Global.register_domain name (module GD)
