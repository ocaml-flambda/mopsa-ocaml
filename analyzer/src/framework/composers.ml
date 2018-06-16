(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transfer function composers *)

open Flow
open Manager

let map
    (f: 'e -> 'a flow -> ('x, 'a) Eval.t option)    
    (evls: ('e, 'a) Eval.t)
  : ('x, 'a) Eval.t option =
  let add_to_option x o =
    match o with
    | None -> Some x
    | Some y -> Some (Eval.join y x)
  in
  Eval.fold (fun acc ev ->
      match ev.result with
      | None -> add_to_option (Eval.singleton None ev.Eval.flow) acc
      | Some result ->
        match f result ev.Eval.flow with
        | None -> acc
        | Some ret ->
          add_to_option (Eval.add_cleaners ev.Eval.cleaners ret) acc
    ) None evls

let post
    (man: ('a, 't) Manager.manager) ctx
    (f: 'e -> 'a Flow.flow -> 'a Post.t option)
    (evl: ('e, 'a) Eval.t)
  : 'a Post.t option =
  let join_option x y =
    match x, y with
    | None, a | a, None -> a
    | Some a, Some b -> Some (Post.join ~flow_join:(man.flow.join) a b)
  in
  Eval.merge
    (fun eval ->
       let post = match eval.Eval.result with
         | None -> Some (Post.of_flow eval.flow)
         | Some exp -> f exp eval.flow
       in
       match post with
       | None -> None
       | Some post ->
         let post' = List.fold_left (fun acc stmt ->
             let flow = acc.Post.flow in
             let flow' = man.exec stmt ctx flow in
             Post.{acc with flow = flow'}
           ) post eval.Eval.cleaners
         in
         Some post'
    ) ~join:join_option evl

let map_eval
  (zpath: Zone.path)
  (e: Ast.expr)
  (man: ('a, 't) Manager.manager) ctx flow
  (f: Ast.expr -> 'a flow -> ('e, 'a) Eval.t option)
  : ('e, 'a) Eval.t option =
  man.eval ~zpath e ctx flow |>
  map f

let post_eval
  (zpath: Zone.path)
  (e: Ast.expr)
  (man: ('a, 't) Manager.manager) ctx flow
  (f: Ast.expr -> 'a Flow.flow -> 'a Post.t option)
  : 'a Post.t option =
  man.eval ~zpath e ctx flow |>
  post man ctx f
