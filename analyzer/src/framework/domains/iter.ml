(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Compose domains in sequence.

    Lattice operations ([bottom], [top], [leq], etc.) are defined pointwise.
    For [exec], [eval] and [ask], we return the result of the first domain
    giving an non-empty answer.
    The order of domains follow the order given in the configuration file.
*)

open Manager
open Domain

module Make(Head: DOMAIN)(Tail: DOMAIN) : DOMAIN =
struct

  type t = Head.t * Tail.t

  let bottom = Head.bottom, Tail.bottom

  let is_bottom (hd, tl) = Head.is_bottom hd || Tail.is_bottom tl

  let top = Head.top, Tail.top

  let is_top (hd, tl) = Head.is_top hd && Tail.is_top tl

  let leq ((hd1, tl1): t) ((hd2, tl2): t) : bool = Head.leq hd1 hd2 && Tail.leq tl1 tl2

  let join ((hd1, tl1): t) ((hd2, tl2): t) : t = (Head.join hd1 hd2), (Tail.join tl1 tl2)

  let meet ((hd1, tl1): t) ((hd2, tl2): t) : t = (Head.meet hd1 hd2), (Tail.meet tl1 tl2)

  let widening (ctx: Context.context) ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.widening ctx hd1 hd2), (Tail.widening ctx tl1 tl2)

  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" Head.print hd Tail.print tl

  let head_man man = {
    man with ax = {
      get = (fun gabs -> fst @@ man.ax.get gabs);
      set = (fun hd gabs -> man.ax.set (hd, snd @@ man.ax.get gabs) gabs);
    }
  }

  let tail_man man = {
    man with ax = {
      get = (fun gabs -> snd @@ man.ax.get gabs);
      set = (fun tl gabs -> man.ax.set (fst @@ man.ax.get gabs, tl) gabs);
    }
  }

  let init prog man ctx flow =
    let ctx, flow = match Head.init prog (head_man man) ctx flow with
      | None -> ctx, flow
      | Some x -> x
    in
    Tail.init prog (tail_man man) ctx flow

  let import_exec = Head.import_exec @ Tail.import_exec
  let export_exec = Head.export_exec @ Tail.export_exec

  let exec zone =
    match List.find_opt (fun z -> Zone.leq z zone) Head.export_exec, List.find_opt (fun z -> Zone.leq z zone) Tail.export_exec with
    | Some z, None ->
      let f = Head.exec z in
      (fun stmt man ctx flow -> f stmt (head_man man) ctx flow)

    | None, Some z ->
      let f = Tail.exec z in
      (fun stmt man ctx flow -> f stmt (tail_man man) ctx flow)

    | Some z1, Some z2 ->
      let f1 = Head.exec z1 in
      let f2 = Tail.exec z2 in
      (fun stmt man ctx flow ->
        match f1 stmt (head_man man) ctx flow with
        | Some post -> Some post
        | None -> f2 stmt (tail_man man) ctx flow
      )

    | None, None -> raise Not_found


  let import_eval = Head.import_eval @ Tail.import_eval
  let export_eval = Head.export_eval @ Tail.export_eval

  let eval zpath =
    match List.find_opt (fun p -> Zone.path_leq p zpath) Head.export_eval, List.find_opt (fun p -> Zone.path_leq p zpath) Tail.export_eval with
    | Some p, None ->
      let f = Head.eval p in
      (fun exp man ctx flow -> f exp (head_man man) ctx flow)

    | None, Some p ->
      let f = Tail.eval p in
      (fun exp man ctx flow -> f exp (tail_man man) ctx flow)

    | Some p1, Some p2 ->
      let f1 = Head.eval p1 in
      let f2 = Tail.eval p2 in
      (fun exp man ctx flow ->
         match f1 exp (head_man man) ctx flow with
         | Some evl -> Some evl
         | None -> f2 exp (tail_man man) ctx flow
      )

    | None, None -> raise Not_found


  let ask query man ctx flow =
    let head_reply = Head.ask query (head_man man) ctx flow in
    let tail_reply = Tail.ask query (tail_man man) ctx flow in
    Query.join query head_reply tail_reply

end
