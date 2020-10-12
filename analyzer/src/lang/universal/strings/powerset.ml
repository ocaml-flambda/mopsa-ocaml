(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Constant strings abstraction *)

open Mopsa
open Ast
open Bot
open Sig.Abstraction.Simplified_value
open Sig.Abstraction.Simplified

module StringPower = Framework.Lattices.Powerset.Make
                       (struct
                         type t = string
                         let compare = Stdlib.compare
                         let print = unformat Format.pp_print_string
                       end)

type _ avalue_kind += V_strings_powerset : StringPower.t avalue_kind


let mk_strings_powerset_query e = Q_avalue(e,V_strings_powerset)

let () =
  register_avalue {
      typ = (fun (type a) next (avk: a avalue_kind) ->
        match avk with
        | V_strings_powerset -> T_string
        | _ -> next.pool_typ avk);
      bottom = (
        let f : type a. avalue_pool -> a avalue_kind -> a =
          fun next avk -> match avk with
                          | V_strings_powerset -> StringPower.bottom
                          | _ -> next.pool_bottom avk in f);
      top = (
        let f : type a. avalue_pool -> a avalue_kind -> a =
          fun next avk -> match avk with
                          | V_strings_powerset -> StringPower.top
                          | _ -> next.pool_top avk in f);
      join = (
        let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
          fun next avk av1 av2 ->
          match avk with
          | V_strings_powerset -> StringPower.join av1 av2
          | _ -> next.pool_join avk av1 av2 in f);
      meet = (
        let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
          fun next avk av1 av2 ->
          match avk with
          | V_strings_powerset -> StringPower.meet av1 av2
          | _ -> next.pool_meet avk av1 av2 in f);
      print = (
        let f : type a. avalue_pool -> a avalue_kind -> Format.formatter -> a -> unit =
          fun next avk fmt av ->
          match avk with
          | V_strings_powerset -> (format StringPower.print) fmt av
          | _ -> next.pool_print avk fmt av in f);
      compare = (
        let f : type a b. avalue_pool -> a avalue_kind -> a -> b avalue_kind -> b -> int =
          fun next avk1 av1 avk2 av2 ->
          match avk1, avk2 with
          | V_strings_powerset, V_strings_powerset -> StringPower.compare av1 av2
          | _ -> next.pool_compare avk1 av1 avk2 av2
          in f
      )
    }

module SimplifiedValue =
struct

  include StringPower

  include GenValueId(struct
      type nonrec t = t
      let name = "universal.strings.powerset"
      let display = "strings"
            end)


  let accept_type = function T_string -> true | _ -> false

  include DefaultValueFunctions

  let constant c t : t =
    match c with
    | C_string s -> singleton s
    | C_avalue(V_strings_powerset, p) -> p
    | _          -> top

  let unop op t v tr =
    match op with
    | _ -> assert false


  let binop op t1 a1 t2 a2 tr =
    match op with
    | O_plus | O_concat ->
      begin match a1, a2 with
        | Top.TOP, _ | _, Top.TOP -> Top.TOP
        | _ ->
          fold (fun s1 acc ->
              fold (fun s2 acc ->
                  add (s1^s2) acc
                ) a2 acc) a1 empty
      end
    | O_mult -> assert false
    | O_eq | O_lt | O_le | O_ge | O_gt | O_ne ->
       Top.TOP
    | _  ->
      panic "todo binop %a" pp_operator op


  let compare op b t1 a1 t2 a2 =
    if is_top a1 || is_top a2 then a1, a2 else
           (*
             { v1 \in a1 | \exists v2 \in a2, v1 op v2 == r },
             { v2 \in a2 | \exists v1 \in a1, v1 op v2 == r }
            *)
      let filter left right =
        StringPower.fold (fun ell acc ->
            if StringPower.exists (fun elr ->
                begin match op with
                  | O_eq -> ell =  elr
                  | O_ne -> ell <> elr
                  | O_lt -> ell <  elr
                  | O_le -> ell <= elr
                  | O_gt -> ell >  elr
                  | O_ge -> ell >= elr
                  | _ -> assert false
                end = b)
                right then StringPower.add ell acc
            else acc
          ) left StringPower.empty in
      filter a1 a2, filter a2 a1

  let avalue : type r. r avalue_kind -> t -> r option =
    fun aval a ->
    match aval with
    | V_strings_powerset -> Some a
    | _ -> None

end

open Sig.Abstraction.Value
module Value =
struct

  module V = MakeValue(SimplifiedValue)
  include SimplifiedValue
  include V

  let rec repeat s nb =
    if nb = 0 then ""
    else if nb = 1 then s
    else let a = repeat (s) (nb/2) in
      if nb mod 2 = 0 then a^a
      else a^a^s

  let eval man e =
    match ekind e with
    | E_binop(O_mult, ({etyp = T_string} as e1), ({etyp = T_int} as e2)) ->
      let strings_e1 = man.eval e1 |> man.get in
      let itv_e2 = man.eval e2 |>
                   man.avalue (Numeric.Common.V_int_interval true) in
      (* FIXME: arbitrary constants... *)
      if ItvUtils.IntItv.is_bounded @@ Bot.bot_to_exn itv_e2 && ItvUtils.IntItv.size @@ Bot.bot_to_exn itv_e2 <= (Z.of_int 5) && not @@ is_top strings_e1 && cardinal strings_e1 <= 3 then
        fold (fun str acc ->
            List.fold_left (fun acc nb ->
                if Z.to_int nb <= 0 then (* FIXME to_int *)
                  add "" acc
                else
                  add (repeat str (Z.to_int nb)) acc
              ) acc (ItvUtils.IntItv.to_list @@ Bot.bot_to_exn itv_e2)
          ) strings_e1 empty
      else
        top

    | _ -> eval man e

  let eval_ext man e =
    match ekind e with
    | E_len ee ->
      let strings_e = man.eval ee |> man.get  in
      if is_top strings_e then
        None
      else
        let itv = elements strings_e |>
                  List.map
                    (fun s -> Numeric.Common.I.cst_int (String.length s)) |>
                    Numeric.Common.I.join_list in
        man.eval (mk_avalue_expr (Numeric.Common.V_int_interval true) itv e.erange) |>
        OptionExt.return

    | _ -> None
end

let () =
  register_value_abstraction (module Value)
