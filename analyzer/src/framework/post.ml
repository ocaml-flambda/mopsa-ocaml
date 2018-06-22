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

type 'a t = 'a post option

let return ?(mergers = []) flow = Some {flow; mergers}

let map f pc =
  Option.option_lift1 (fun p -> {p with flow = f p.flow}) pc

let add_mergers mergers pc =
  Option.option_lift1 (fun p -> {p with mergers = p.mergers @ mergers}) pc

let join (pc1: 'a t) (pc2: 'a t) ~(fjoin: 'a flow -> 'a flow -> 'a flow) : 'a t =
  Option.option_neutral2 (fun post1 post2 ->
      {
        flow     = fjoin post1.flow post2.flow;
        mergers  = post1.mergers @ post2.mergers;
      }
    ) pc1 pc2

let bind
    (e: Ast.expr)
    ?(zpath = Zone.path_top)
    (man: ('a, 't) Manager.manager) ctx flow
    (f: 'e -> 'a Flow.flow -> 'a t)
  : 'a t =
  let _, zone = zpath in
  man.eval e ~zpath ctx flow |>
  fold_eval (fun acc case ->
      match case.result with
      | None -> return case.flow |> join ~fjoin:man.flow.join acc
      | Some e -> f e case.flow |>
                  map (fun flow ->
                      List.fold_left (fun acc stmt ->
                          man.exec ~zone stmt ctx acc
                        ) flow case.cleaners
                    ) |>
                  join ~fjoin:man.flow.join acc
    ) None
