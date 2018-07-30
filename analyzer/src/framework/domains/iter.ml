(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Iterator composer combines domains in sequence. Lattice operations
   ([bottom], [top], [leq], etc.) are defined pointwise. For transfer
   functions ([exec], [eval] and [ask]), the iterator returns the
   result of the first domain giving an non-empty answer.  The order
   of domains follows the order given in the configuration file.  *)

open Manager
open Domain

module Make(Head: DOMAIN)(Tail: DOMAIN) : DOMAIN =
struct

  type t = Head.t * Tail.t

  let bottom =
    Head.bottom, Tail.bottom

  let top =
    Head.top, Tail.top

  let is_bottom (hd, tl) =
    Head.is_bottom hd || Tail.is_bottom tl

  let subset ((hd1, tl1): t) ((hd2, tl2): t) : bool =
    Head.subset hd1 hd2 && Tail.subset tl1 tl2

  let join annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.join annot hd1 hd2), (Tail.join annot tl1 tl2)

  let meet annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.meet annot hd1 hd2), (Tail.meet annot tl1 tl2)

  let widen annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.widen annot hd1 hd2), (Tail.widen annot tl1 tl2)

  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" Head.print hd Tail.print tl

  let head_man man = {
    man with
    get = (fun flow -> fst @@ man.get flow);
    set = (fun hd flow -> man.set (hd, snd @@ man.get flow) flow);
  }

  let tail_man man = {
    man with
    get = (fun flow -> snd @@ man.get flow);
    set = (fun tl flow -> man.set (fst @@ man.get flow, tl) flow);
  }

  let init prog man flow =
    let flow' = match Head.init prog (head_man man) flow with
      | None -> flow
      | Some flow' -> flow'
    in
    Tail.init prog (tail_man man) flow'

  let exec_interface = Domain.{
    import = Head.exec_interface.import @ Tail.exec_interface.import;
    export = Head.exec_interface.export @ Tail.exec_interface.export;
  }

  let exec zone =
    match List.find_all (fun z -> Zone.subset z zone) Head.exec_interface.Domain.export,
          List.find_all (fun z -> Zone.subset z zone) Tail.exec_interface.Domain.export
    with
    | [], [] -> raise Not_found

    | l, [] ->
      let f = Analyzer.mk_exec_of_zone_list l Head.exec in
      (fun stmt man flow -> f stmt (head_man man) flow)

    | [], l ->
      let f = Analyzer.mk_exec_of_zone_list l Tail.exec in
      (fun stmt man flow -> f stmt (tail_man man) flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_exec_of_zone_list l1 Head.exec in
      let f2 = Analyzer.mk_exec_of_zone_list l2 Tail.exec in
      (fun stmt man flow ->
         match f1 stmt (head_man man) flow with
         | Some post -> Some post
         | None -> f2 stmt (tail_man man) flow
      )


  let eval_interface = Domain.{
    import = Head.eval_interface.import @ Tail.eval_interface.import;
    export = Head.eval_interface.export @ Tail.eval_interface.export;
  }

  let eval zpath =
    match List.find_all (fun p -> Zone.subset2 p zpath) Head.eval_interface.Domain.export,
          List.find_all (fun p -> Zone.subset2 p zpath) Tail.eval_interface.Domain.export
    with
    | [], [] -> raise Not_found

    | l, [] ->
      let f = Analyzer.mk_eval_of_zone_list l Head.eval in
      (fun exp man flow -> f exp (head_man man) flow)

    | [], l ->
      let f = Analyzer.mk_eval_of_zone_list l Tail.eval in
      (fun exp man flow -> f exp (tail_man man) flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_eval_of_zone_list l1 Head.eval in
      let f2 = Analyzer.mk_eval_of_zone_list l2 Tail.eval in
      (fun exp man flow ->
         match f1 exp (head_man man)  flow with
         | Some evl -> Some evl
         | None -> f2 exp (tail_man man) flow
      )


  let ask query man flow =
    let head_reply = Head.ask query (head_man man) flow in
    let tail_reply = Tail.ask query (tail_man man) flow in
    Query.join query head_reply tail_reply

end
