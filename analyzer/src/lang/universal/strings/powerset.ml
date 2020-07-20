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
open Sig.Abstraction.Value
open Sig.Abstraction.Simplified


module StringPower = Framework.Lattices.Powerset.Make
    (struct
      type t = string
      let compare = Stdlib.compare
      let print = Format.pp_print_string
    end)

type _ query +=
  | Q_strings_powerset : expr -> StringPower.t query

let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_strings_powerset _ -> StringPower.join a b
          | _ -> next.join_query query a b in f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_strings_powerset _ -> StringPower.meet a b
          | _ -> next.meet_query query a b in f
    );
  }

module Value =
struct


  include StringPower

  include GenValueId(struct
      type nonrec t = t
      let name = "universal.strings.powerset"
      let display = "strings"
            end)


  let is_string_type = function T_string -> true | _ -> false

  let constant t c =
    if is_string_type t then
      match c with
      | C_string s -> Some (singleton s)
      | _          -> Some top
    else
      None

  let cast man t e =
    if is_string_type t then Some top else None

  let unop op t v =
    match op with
    | _ -> assert false

  let binop op t a1 a2 =
    match op with
    | O_plus ->
      begin match a1, a2 with
        | Top.TOP, _ | _, Top.TOP -> Top.TOP
        | _ ->
          fold (fun s1 acc ->
              fold (fun s2 acc ->
                  add (s1^s2) acc
                ) a2 acc) a1 empty
      end
    | O_mult -> assert false
    | _  -> failwith "todo"

  let filter b t a =
      failwith "todo"

  let bwd_unop _ _ _ _ = failwith "ni"
  let bwd_binop _ _ _ _  = failwith "ni"

  let bwd_cast = default_bwd_cast

  let predicate = default_predicate

  let compare op b t a1 a2 =
    match t with
    | T_string ->
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
    | _ -> a1, a2

  (** {2 Query handlers} *)

  let ask : type r. t value_man -> r query -> r option =
    fun man q ->
    match q with
    (* TODO: query de values.py_string *)
    | _ -> None
end


module Domain =
struct

  module Nonrel = Framework.Abstraction.Combiners.Value.Nonrel.Make(Value)
  module Lifted = Sig.Abstraction.Simplified.MakeDomain(Nonrel)
  include Lifted

  let interface = {
    iexec = { provides = [Zone.Z_u_string]; uses = []; };
    ieval = { provides = [Zone.Z_u, Zone.Z_u_string];           uses = [Zone.Z_u, Zone.Z_u_string; Zone.Z_u, Zone.Z_u_int]; }
  }

  let debug fmt = Debug.debug ~channel:name fmt


  let exec zone stmt (man: ('a, t, 's) man) (flow: 'a flow) : 'a post option =
    match skind stmt with
    | S_assign (x, e) ->
      debug "ok";
      man.eval ~zone:(Zone.Z_u, Zone.Z_u_string) e flow |>
      bind_some_opt (fun ee flow ->
          debug "ee = %a" pp_expr ee;
          Lifted.exec zone {stmt with skind = S_assign(x, ee)} man flow
        )

    | _ ->
      Lifted.exec zone stmt man flow
  (* dans le cas de assign/assume: evaluer e en chaine via eval, puis appeler Lifted.exec *)

  let rec repeat s nb =
    if nb = 0 then ""
    else if nb = 1 then s
    else let a = repeat (s) (nb/2) in
      if nb mod 2 = 0 then a^a
      else a^a^s

  let eval zones expr (man: ('a, t, 's) man) flow =
    let range = erange expr in
    match ekind expr with
    | E_binop (O_mult, e1, e2) when etyp e1 = T_string && etyp e2 = T_int->
      man.eval ~zone:(Zone.Z_u, Zone.Z_u_string) e1 flow |>
      Eval.bind (fun e1 flow ->
          let cur = get_env T_cur man flow in
          let strings_e1 = Nonrel.eval e1 cur |> OptionExt.none_to_exn |> snd in
          let itv_e2 = man.ask (Numeric.Common.Q_int_interval e2) flow in
          (* FIXME: arbitrary constants... *)
          if ItvUtils.IntItv.is_bounded @@ Bot.bot_to_exn itv_e2 && ItvUtils.IntItv.size @@ Bot.bot_to_exn itv_e2 <= (Z.of_int 5) && Value.cardinal strings_e1 <= 3 then
            let results =
              Value.fold (fun str acc ->
                  List.fold_left (fun acc nb ->
                      if Z.to_int nb <= 0 then (* FIXME to_int *)
                        Value.add "" acc
                       else
                         Value.add (repeat str (Z.to_int nb)) acc
                    ) acc (ItvUtils.IntItv.to_list @@ Bot.bot_to_exn itv_e2)
                ) strings_e1 Value.empty in
            Eval.join_list ~empty:(fun () -> failwith "todo")
              (Value.fold (fun s acc -> (Eval.singleton (mk_string s range) flow) :: acc) results [])
          else
            Eval.singleton (mk_top T_string range) flow
        )
      |> OptionExt.return

    | E_len e when etyp e = T_string ->
      man.eval ~zone:(Zone.Z_u, Zone.Z_u_string) e flow |>
      Eval.bind (fun e flow ->
          let cur = get_env T_cur man flow in
          let strings_e = Nonrel.eval e cur |> OptionExt.none_to_exn |> snd in
          Eval.join_list ~empty:(fun () -> failwith "todo")
            (if Value.is_top strings_e then
               man.eval ~zone:(Zone.Z_u, Zone.Z_u_int) (mk_top T_int range) flow :: []
             else Value.fold (fun s acc -> (man.eval ~zone:(Zone.Z_u, Zone.Z_u_int) (mk_int (String.length s) range) flow) :: acc) strings_e [])
        )
      |> OptionExt.return

    | _ -> None


  let ask : type r. r query -> ('a, t, 's) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_strings_powerset e ->
      man.eval ~zone:(Zone.Z_u, Zone.Z_u_string) e flow |>
      Cases.apply
        (fun oe flow ->
           let cur = get_env T_cur man flow in
           Nonrel.eval (OptionExt.none_to_exn oe) cur |> OptionExt.lift snd

        ) (OptionExt.lift2 StringPower.join) (OptionExt.lift2 StringPower.meet)

    | _ -> Lifted.ask query man flow

end

let () =
  register_standard_domain (module Domain)
