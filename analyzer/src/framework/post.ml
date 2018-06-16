(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Post-conditions of exec transfer functions *)

open Flow

type 'a t = {
  flow : 'a flow;
  mergers : Ast.stmt list;
}

let of_flow ?(mergers = []) flow = {
  flow;
  mergers;
}

let add_mergers mergers flow = {
  flow with
  mergers = flow.mergers @ mergers;
}

let join (post1: 'a t) (post2: 'a t) ~(flow_join: 'a flow -> 'a flow -> 'a flow) : 'a t =
  {
    flow     = flow_join post1.flow post2.flow;
    mergers  = post1.mergers @ post2.mergers;
  }
