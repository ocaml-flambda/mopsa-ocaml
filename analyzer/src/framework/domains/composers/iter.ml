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
open Global

module Make(Head: DOMAIN)(Tail: DOMAIN) : DOMAIN =
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

  let init prg man fa =
    Head.init prg (head_man man) fa |>
    Tail.init prg (tail_man man)

  let exec stmt man ctx gabs =
    let gabs' = Head.exec stmt (head_man man) ctx gabs in
    match gabs' with
    | None -> Tail.exec stmt (tail_man man) ctx gabs
    | _ -> gabs'


  let eval exp man ctx gabs =
    let head_ev = Head.eval exp (head_man man) ctx gabs in
    match head_ev with
    | None -> Tail.eval exp (tail_man man) ctx gabs
    | _ -> head_ev


  let ask query man ctx gabs =
    let head_reply = Head.ask query (head_man man) ctx gabs in
    match head_reply with
    | None -> Tail.ask query (tail_man man) ctx gabs
    | _ -> head_reply

end


module MakeStack =
  functor(Head: STACK_DOMAIN) ->
    functor(Tail: STACK_DOMAIN) ->
      functor(Sub: DOMAIN) ->
  struct

    module Head = Head(Sub)
    module Tail = Tail(Sub)

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
    Format.fprintf fmt "%a@,%a" Head.print hd Tail.print tl

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

  let init prg man subax fa =
    Head.init prg (head_man man) subax fa |>
    Tail.init prg (tail_man man) subax


  let exec stmt man subax ctx gabs =
    let gabs' = Head.exec stmt (head_man man) subax ctx gabs in
    match gabs' with
    | None -> Tail.exec stmt (tail_man man) subax ctx gabs
    | _ -> gabs'


  let eval exp man subax ctx gabs =
    let head_ev = Head.eval exp (head_man man) subax ctx gabs in
    match head_ev with
    | None -> Tail.eval exp (tail_man man) subax ctx gabs
    | _ -> head_ev


  let ask query man subax ctx gabs =
    let head_reply = Head.ask query (head_man man) subax ctx gabs in
    match head_reply with
    | None -> Tail.ask query (tail_man man) subax ctx gabs
    | _ -> head_reply

end
