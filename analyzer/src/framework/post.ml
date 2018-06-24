(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Post-conditions of exec transfer functions *)

open Ast
open Manager

type 'a post = {
  flow : 'a Flow.flow;
  mergers : Ast.stmt list;
}

let return ?(mergers = []) flow = Some {flow; mergers}

let map f pc =
  Option.option_lift1 (fun p -> {p with flow = f p.flow}) pc

let add_mergers mergers pc =
  Option.option_lift1 (fun p -> {p with mergers = p.mergers @ mergers}) pc

let join (pc1: 'a post option) (pc2: 'a post option) ~(fjoin: 'a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) : 'a post option =
  Option.option_neutral2 (fun post1 post2 ->
      {
        flow     = fjoin post1.flow post2.flow;
        mergers  = post1.mergers @ post2.mergers;
      }
    ) pc1 pc2

let bind
    ?(zone = Zone.top) (man: ('a, 't) Manager.manager) ctx
    (f: 'e -> 'a Flow.flow -> 'a post option) (evl: ('e, 'a) eval)
  : 'a post option =
  Eval.fold_ (fun acc case ->
      match case.result with
      | None -> return case.flow |>
                join ~fjoin:man.flow.join acc

      | Some e -> f e case.flow |>
                  map (fun flow ->
                      List.fold_left (fun acc stmt ->
                          man.exec ~zone stmt ctx acc
                        ) flow case.cleaners
                    ) |>
                  join ~fjoin:man.flow.join acc
    ) None evl

let assume
    cond ?(zone = Zone.top)
    ~fthen ~felse
    (man: ('a, 't) manager) ctx flow
    ?(fboth = (fun flow1 flow2 -> join ~fjoin:(man.flow.join) (fthen flow1) (felse flow2)))
    ?(fnone = (fun () -> return flow))
    ()
  : 'a post option
  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) ctx flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) ctx flow in
  match Manager.is_cur_bottom man then_flow, Manager.is_cur_bottom man else_flow with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone ()

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> 'a post option)) list)
    ?(zone = Zone.top)
    man ctx flow
  : 'a post option =
  match cases with
  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x x.erange)
            else (mk_assume (mk_not x x.erange) x.erange)
          in
          man.exec s ~zone ctx acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> join (one cond t) acc ~fjoin:man.flow.join) (one cond t) q
  | [] -> None
