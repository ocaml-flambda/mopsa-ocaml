(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Domains with local computations on [cur] part of the flow abstraction. *)

open Flow
open Lattice
open Manager


(*==========================================================================*)
                  (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN =
sig

  include Lattice.LATTICE

  val init : Ast.program -> t

  val exec: Ast.stmt -> Context.context -> t -> t option
  (** Abstract transfer function of statements. *)

  val eval: Ast.expr -> Context.context -> t -> t eval_output option
  (** Abstract (symbolic) evaluation of expressions. *)

  val ask: 'r Query.query -> Context.context -> t -> 'r option
  (** Handler of generic queries. *)

end

(** Creates a low-level domain from a high-level one. *)
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

  let init prg man fa =
    man.flow.map (fun env -> function
        | TCur -> man.ax.set (Domain.init prg) env
        | _ -> env
      ) fa

  let exec stmt (man: ('a, t) manager) ctx (fa: 'a flow) : 'a flow option =
    let ecur = man.flow.get TCur fa in
    let cur = man.ax.get ecur in
    let cur' = Domain.exec stmt ctx cur in
    match cur' with
    | None -> None
    | Some a ->
      let ecur' = man.ax.set a ecur in
      Some (man.flow.set TCur ecur' fa)

  let eval exp man ctx fa =
    let env = man.flow.get TCur fa in
    let a = man.ax.get env in
    Domain.eval exp ctx a |>
    Eval.map (fun (exp', a', cleaners) ->
        let env' = man.ax.set a' env in
        let fa' = man.flow.set TCur env' fa in
        (exp', fa', cleaners)
      )


  let ask query man ctx fa =
    let env = man.flow.get TCur fa in
    let a = man.ax.get env in
    Domain.ask query ctx a

end


let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module GD = MakeGlobalDomain(D) in
  Global.register_domain name (module GD)
