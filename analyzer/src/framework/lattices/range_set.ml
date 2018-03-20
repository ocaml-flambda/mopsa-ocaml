(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Powerset lattice maintaining both an under-approximation and
   an over-approximation.
*)


open Bot

module type VALUE =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make(Value: VALUE) =
struct

  module Set = Set.Make(Value)

  module SSet = Top_set.Make(Value)

  type v = {
    inf: Set.t;
    sup: SSet.t;
  }

  type t = v with_bot

  let bottom = BOT

  let top = Nb {
    inf = Set.empty;
    sup = SSet.top;
  }

  let empty = Nb {
    inf = Set.empty;
    sup = SSet.empty;
  }

  let leq abs1 abs2 =
    bot_included (fun abs1 abs2 ->
        Set.subset abs2.inf abs1.inf && SSet.subset abs1.sup abs2.sup
      ) abs1 abs2

  let unify _ a1 a2 = (a1, a2)

  let join  abs1 abs2 =
    bot_neutral2 (fun abs1 abs2 ->
        {
          inf = Set.inter abs1.inf abs2.inf;
          sup = SSet.union  abs1.sup abs2.sup;
        }
      ) abs1 abs2

  let meet  abs1 abs2 =
    bot_absorb2 (fun abs1 abs2 ->
        let abs = {
          inf = Set.union abs1.inf abs2.inf;
          sup = SSet.meet  abs1.sup abs2.sup;
        } in
        if SSet.subset (Top.Nt abs.inf) abs.sup then
          Nb abs
        else
          BOT
      ) abs1 abs2

  let widening ctx abs1 abs2 =
    bot_neutral2 (fun abs1 abs2 ->
        {
          inf = Set.inter abs2.inf abs1.inf;
          sup = SSet.widening ctx abs1.sup abs2.sup;
        }
      ) abs1 abs2

  let print fmt abs =
    let open Format in

    let print_set fmt s =
      if Set.is_empty s then
        fprintf fmt "∅"
      else
        fprintf fmt "@[<h>{%a}@]"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ", ") Value.print
          ) (Set.elements s)
    in
    bot_fprint (fun fmt abs ->
        fprintf fmt "{| @[%a@] , @[%a@] |}"
          print_set abs.inf
          SSet.print abs.sup
      ) fmt abs

  let is_bottom abs = (abs = BOT)

  let is_top abs =
    bot_dfl1 false (fun abs -> Set.is_empty abs.inf && SSet.is_top abs.sup) abs

  let may_mem x abs =
    bot_dfl1 false (fun abs -> SSet.mem x abs.sup) abs

  let must_mem x abs =
    bot_dfl1 false (fun abs -> Set.mem x abs.inf) abs


  let filter f abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = Set.filter f abs.inf;
          sup = SSet.filter f abs.sup;
        }
      ) abs


  let filter_dfl dfl f abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = Set.filter f abs.inf;
          sup = SSet.filter_dfl dfl f abs.sup;
        }
      ) abs

  let may_exists f abs =
    bot_dfl1 false (fun abs -> SSet.exists f abs.sup) abs

  let singleton x = Nb {
    inf = Set.singleton x;
    sup = SSet.singleton x;
  }

  let add x abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = Set.add x abs.inf;
          sup = SSet.add x abs.sup;
        }
      ) abs

  let may_add x abs =
    bot_absorb1 (fun abs ->
        Nb {
          abs with
          sup = SSet.add x abs.sup;
        }
      ) abs

  let must_add x abs =
    bot_absorb1 (fun abs ->
        Nb {
          abs with
          inf = Set.add x abs.inf;
        }
      ) abs

  let remove x abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = Set.remove x abs.inf;
          sup = SSet.remove x abs.sup;
        }
      ) abs

  let must_remove x abs =
    bot_absorb1 (fun abs ->
        Nb {
          abs with
          inf = Set.remove x abs.inf;
        }
      ) abs


  let may_choose abs =
    match abs with
    | BOT -> raise Not_found
    | Nb abs -> SSet.choose abs.sup

  let may_fold f abs a =
    bot_dfl1 a (fun abs -> SSet.fold f abs.sup a) abs

  let must_not_fold f abs a =
    may_fold (fun x acc ->
        if must_mem x abs then acc else f x acc
      ) abs a

  let must_fold f abs a =
    bot_dfl1 a (fun abs -> Set.fold f abs.inf a)

  let map f abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = Set.map f abs.inf;
          sup = SSet.map f abs.sup;
        }
      ) abs


end
