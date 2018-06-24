(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Post-conditions of exec transfer functions *)

open Flow
open Manager

type 'a post = {
  flow : 'a flow;
  mergers : Ast.stmt list;
}

let return ?(mergers = []) flow = Some {flow; mergers}

let map f pc =
  Option.option_lift1 (fun p -> {p with flow = f p.flow}) pc

let add_mergers mergers pc =
  Option.option_lift1 (fun p -> {p with mergers = p.mergers @ mergers}) pc

let join (pc1: 'a post option) (pc2: 'a post option) ~(fjoin: 'a flow -> 'a flow -> 'a flow) : 'a post option =
  Option.option_neutral2 (fun post1 post2 ->
      {
        flow     = fjoin post1.flow post2.flow;
        mergers  = post1.mergers @ post2.mergers;
      }
    ) pc1 pc2

let bind
    ?(zone = Zone.top) (man: ('a, 't) Manager.manager) ctx
    (f: 'e -> 'a Flow.flow -> 'a post option) (evl: ('e, 'a) Eval.eval)
  : 'a post option =
  Eval.fold_ (fun acc case ->
      match case.Eval.result with
      | None -> return case.Eval.flow |>
                join ~fjoin:man.flow.join acc

      | Some e -> f e case.Eval.flow |>
                  map (fun flow ->
                      List.fold_left (fun acc stmt ->
                          man.exec ~zone stmt ctx acc
                        ) flow case.Eval.cleaners
                    ) |>
                  join ~fjoin:man.flow.join acc
    ) None evl
