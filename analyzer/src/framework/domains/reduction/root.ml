(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Root domain of a reduced product. *)

open Flow
open Manager
open Domain

module Make(Domain: Domain.DOMAIN) =
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
    let rflow = Domain.exec man ctx stmt flow in
    match rflow with
    | None -> None
    | Some rflow ->
      (* FIXME: we do here just one reduction iteration.
         Reaching fixpoint is not ensured, but the output is still sound.
      *)
      let flow = List.fold_left (fun flow channel ->
          match rflow.subscribe channel flow with
          | None -> flow
          | Some rflow -> rflow.out
        ) rflow.out rflow.publish
      in
      Some flow

  let eval man ctx exp flow =
    let revl = Domain.eval man ctx exp flow in
    Eval.oeval_map (function
        | None, flow, cleaners -> None, flow, cleaners
        | Some (exp, mergers), flow, cleaners -> Some exp, flow, cleaners
      ) revl

  let ask = Domain.ask

end
