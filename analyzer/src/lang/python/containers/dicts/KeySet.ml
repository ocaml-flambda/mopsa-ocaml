open Bot

module V = Memory.Value
  
type v = {
  inf: V.t;
  sup: V.t;
}

type t = v with_bot

let bottom = BOT

let top = Nb {
    inf = V.bottom;
    sup = V.top;
}

let empty = Nb {
    inf = V.bottom;
    sup = V.bottom;
  }

let init = empty

let leq abs1 abs2 =
  bot_included (fun abs1 abs2 ->
      V.leq abs2.inf abs1.inf && V.leq abs1.sup abs2.sup
    ) abs1 abs2

let join abs1 abs2 =
  bot_neutral2 (fun abs1 abs2 ->
      {
        inf = V.meet abs1.inf abs2.inf;
        sup = V.join abs1.sup abs2.sup;
      }
    ) abs1 abs2

let meet abs1 abs2 =
  bot_absorb2 (fun abs1 abs2 ->
      let abs = {
        inf = V.join abs1.inf abs2.inf;
        sup = V.meet abs1.sup abs2.sup;
      } in
      if V.leq abs.inf abs.sup then
        Nb abs
      else
        BOT
    ) abs1 abs2

let widening abs1 abs2 =
  bot_neutral2 (fun abs1 abs2 ->
      {
        inf = V.meet abs2.inf abs1.inf;
        sup = if not (V.leq abs2.sup abs1.sup) then V.top else abs1.sup;
      }
    ) abs1 abs2

let print fmt abs =
  bot_fprint (fun fmt abs ->
      Format.fprintf fmt "{| @[%a@] , @[%a@] |}"
        V.print abs.inf
        V.print abs.sup
    ) fmt abs

let is_bottom abs = (abs = BOT)

let is_top abs =
  bot_dfl1 false (fun abs -> V.is_bottom abs.inf && V.is_top abs.sup) abs

let may_add v abs =
  bot_absorb1 (fun abs ->
      Nb {
        inf = V.meet v abs.inf;
        sup = V.join v abs.sup;
      }
    ) abs

let must_add v abs =
  bot_absorb1 (fun abs ->
      Nb {
        inf = V.join v abs.inf;
        sup = V.join v abs.sup;
      }
    ) abs


let remove v abs =
  (** FIXME: removing an abstract value from another requires a complement operator,
      which is missing for numeric abstractions for example.
      So we just return the abstraction not modifed for the moment !
  *)
  abs

  
let may_mem v abs =
  bot_dfl1 false (fun abs -> not @@ V.is_bottom @@ V.meet v abs.sup) abs

let must_mem v abs =
  bot_dfl1 false (fun abs -> not @@ V.is_bottom @@ V.meet v abs.inf) abs

let singleton v =
  Nb {inf = v; sup = v}
