(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python dictionaries. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Domains.Stateful
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr
open Bot

let name = "python.objects.containers.dicts"
let debug fmt = Debug.debug ~channel:name fmt


module V = Memory.Value


(*==========================================================================*)
(**                           {2 Key Abstraction}                           *)
(*==========================================================================*)

(** An abstract key maintains a lower and upper approximation of the values
    of the concrete keys of the dict.
*)
module K =
struct

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

  let widening ctx abs1 abs2 =
    bot_neutral2 (fun abs1 abs2 ->
        {
          inf = V.meet abs2.inf abs1.inf;
          sup = V.widening ctx abs1.sup abs2.sup;
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
end



(*==========================================================================*)
(**                           {2 Abstract Domain}                           *)
(*==========================================================================*)

(* This abstract domain maintains for each reachable address @ of type dict:
   - an abstract key representing an under and upper approximation of concrete
   keys values.
   - an abstract value @.$dv representing an over approximation of the concrete entries
values.
   - an abstract length @.$dl approximating the number of entries.
*)

module Domain =
struct

  include Framework.Lattices.Partial_map.Make
      (struct type t = addr let compare = compare_addr let print = Universal.Pp.pp_addr end)
      (K)

  let is_bottom _ = false
  let print fmt a =
      Format.fprintf fmt "dicts keys:@ @[<h>  %a@]@\n" print a


  let eval man ctx exp flow =
    let range = exp.erange in

    match ekind exp with

    | _ -> None

  let exec man ctx stmt flow =
    match skind stmt with
    | S_rebase_addr(a1, a2, mode) ->
      let abs = get_domain_cur man flow in
      let keyset1 = find a1 abs and keyset2 = find a2 abs in
      let keyset = K.join keyset1 keyset2 in
      let abs' = remove a1 abs |> add a2 keyset in
      let abs'' = match mode with
        | STRONG | EXPAND -> abs'
        | WEAK -> join abs abs'
      in
      let flow = set_domain_cur abs'' man flow in
      return flow

    | _ -> None


  let init man ctx prog flow =
    ctx, set_domain_cur empty man flow

  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
