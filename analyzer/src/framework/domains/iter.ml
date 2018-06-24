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

  let exec_interface = Domain.{
    import = Head.exec_interface.import @ Tail.exec_interface.import;
    export = Head.exec_interface.export @ Tail.exec_interface.export;
  }

  let exec zone =
    match List.find_all (fun z -> Zone.leq z zone) Head.exec_interface.Domain.export, List.find_all (fun z -> Zone.leq z zone) Tail.exec_interface.Domain.export with
    | [], [] -> raise Not_found

    | l, [] ->
      let f = Analyzer.mk_exec_of_zone_list l Head.exec in
      (fun stmt man ctx flow -> f stmt (head_man man) ctx flow)

    | [], l ->
      let f = Analyzer.mk_exec_of_zone_list l Tail.exec in
      (fun stmt man ctx flow -> f stmt (tail_man man) ctx flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_exec_of_zone_list l1 Head.exec in
      let f2 = Analyzer.mk_exec_of_zone_list l2 Tail.exec in
      (fun stmt man ctx flow ->
         match f1 stmt (head_man man) ctx flow with
         | Some post -> Some post
         | None -> f2 stmt (tail_man man) ctx flow
      )


  let eval_interface = Domain.{
    import = Head.eval_interface.import @ Tail.eval_interface.import;
    export = Head.eval_interface.export @ Tail.eval_interface.export;
  }

  let eval zpath =
    match List.find_all (fun p -> Zone.path_leq p zpath) Head.eval_interface.Domain.export, List.find_all (fun p -> Zone.path_leq p zpath) Tail.eval_interface.Domain.export with
    | [], [] -> raise Not_found

    | l, [] ->
      let f = Analyzer.mk_eval_of_zone_path_list l Head.eval in
      (fun exp man ctx flow -> f exp (head_man man) ctx flow)

    | [], l ->
      let f = Analyzer.mk_eval_of_zone_path_list l Tail.eval in
      (fun exp man ctx flow -> f exp (tail_man man) ctx flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_eval_of_zone_path_list l1 Head.eval in
      let f2 = Analyzer.mk_eval_of_zone_path_list l2 Tail.eval in
      (fun exp man ctx flow ->
         match f1 exp (head_man man) ctx flow with
         | Some evl -> Some evl
         | None -> f2 exp (tail_man man) ctx flow
      )



  let ask query man ctx flow =
    let head_reply = Head.ask query (head_man man) ctx flow in
    let tail_reply = Tail.ask query (tail_man man) ctx flow in
    Query.join query head_reply tail_reply

end
