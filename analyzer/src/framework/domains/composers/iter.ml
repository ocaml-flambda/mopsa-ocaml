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
open Stateful

module Make(Head: DOMAIN)(Tail: DOMAIN) =
struct

  type t = Head.t * Tail.t

  let bottom = Head.bottom, Tail.bottom

  let is_bottom (hd, tl) = Head.is_bottom hd || Tail.is_bottom tl

  let top = Head.top, Tail.top

  let is_top (hd, tl) = Head.is_top hd && Tail.is_top tl

  let leq (hd1, tl1) (hd2, tl2) = Head.leq hd1 hd2 && Tail.leq tl1 tl2

  let join (hd1, tl1) (hd2, tl2) = (Head.join hd1 hd2), (Tail.join tl1 tl2)

  let meet (hd1, tl1) (hd2, tl2) = (Head.meet hd1 hd2), (Tail.meet tl1 tl2)

  let widening ctx (hd1, tl1) (hd2, tl2) =
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

  let init man ctx prog fa =
    let ctx, fa = Head.init (head_man man) ctx prog fa in
    Tail.init (tail_man man) ctx prog fa

  let exec man ctx stmt gabs =
    let gabs' = Head.exec (head_man man) ctx stmt gabs in
    match gabs' with
    | None -> Tail.exec (tail_man man) ctx stmt gabs
    | _ -> gabs'


  let eval man ctx exp gabs =
    let head_ev = Head.eval (head_man man) ctx exp gabs in
    match head_ev with
    | None -> Tail.eval (tail_man man) ctx exp gabs
    | _ -> head_ev


  let ask man ctx query gabs =
    let head_reply = Head.ask (head_man man) ctx query gabs in
    let tail_reply = Tail.ask (tail_man man) ctx query gabs in
    Query.join query head_reply tail_reply

end
