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
      let print = unformat Format.pp_print_string
    end)

type ('a, _) query +=
  | Q_strings_powerset : expr -> ('a, StringPower.t) query

let () =
  register_query {
    join = (
      let f : type a r. query_operator -> (a, r) query -> (a->a->a) -> r -> r -> r =
        fun next query join a b ->
          match query with
          | Q_strings_powerset _ -> StringPower.join a b
          | _ -> next.apply query join a b in f
    );
    meet = (
      let f : type a r. query_operator -> (a, r) query -> (a->a->a) -> r -> r -> r =
        fun next query meet a b ->
          match query with
          | Q_strings_powerset _ -> StringPower.meet a b
          | _ -> next.apply query meet a b in f
    );
  }

module Value =
struct


  include StringPower

  include GenValueId(struct
      type nonrec t = t
      let name = "universal.strings.powerset_value"
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
    | O_eq | O_lt | O_le | O_ge | O_gt | O_ne ->
       debug "binop %a %a %a %a" pp_operator op pp_typ t (format StringPower.print) a1 (format StringPower.print) a2;
       Top.TOP
    | _  ->
       panic "todo binop %a" pp_operator op

  let filter b t a =
    debug "filter %b %a %a" b pp_typ t (format StringPower.print) a;
    if t = T_string then a else assert false
      (* failwith "todo filter" *)

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

  let ask : type r. ('a, t) value_man -> ('a, r) query -> r option =
    fun man q ->
    match q with
    (* TODO: query de values.py_string *)
    | _ -> None
end


module Domain =
struct

  module Nonrel = Framework.Combiners.Value.Nonrel.Make(Value)
  type t = Nonrel.t
  let top = Nonrel.top
  let bottom = Nonrel.bottom
  let is_bottom = Nonrel.is_bottom
  let meet = Nonrel.meet
  let join = Nonrel.join
  let subset = Nonrel.subset
  let print_state = Nonrel.print_state
  let widen = Nonrel.widen

  include Framework.Core.Id.GenDomainId(struct
              type nonrec t = t
              let name = "universal.strings.powerset"
            end)

  let merge _ _ _ = assert false

  let init prog man flow =
    set_env T_cur (Nonrel.init prog) man flow

  let routing_table = empty_routing_table

  let checks = []

  let debug fmt = Debug.debug ~channel:name fmt

  let exec stmt (man: ('a, t) Framework.Core.Manager.man) (flow: 'a flow) : 'a post option =
    match skind stmt with
    | S_assign (_, e) | S_remove e | S_add e | S_rename (e, _) | S_forget e | S_expand(e, _) | S_fold(e, _) | S_project (e::_) when etyp e = T_string ->
       let cur = get_env T_cur man flow in
       let ctx = Flow.get_ctx flow in
       let ocur = Nonrel.exec stmt man ctx cur in
       Option.bind ocur (fun cur ->
           let flow = set_env T_cur cur man flow in
           OptionExt.return @@ Post.return flow
         )

    | S_assume e when etyp e = T_bool ->
       let cur = get_env T_cur man flow in
       let ctx = Flow.get_ctx flow in
       let ocur = Nonrel.exec stmt man ctx cur in
       Option.bind ocur (fun cur ->
           let flow = set_env T_cur cur man flow in
           OptionExt.return @@ Post.return flow
         )

    | _ -> None

  let rec repeat s nb =
    if nb = 0 then ""
    else if nb = 1 then s
    else let a = repeat (s) (nb/2) in
      if nb mod 2 = 0 then a^a
      else a^a^s

  let eval expr (man: ('a, t) Framework.Core.Manager.man) (flow: 'a flow) : 'a eval option =
    let range = erange expr in
    match ekind expr with
    | E_binop (O_mult, e1, e2) when etyp e1 = T_string && etyp e2 = T_int->
      man.eval e1 flow >>$
        (fun e1 flow ->
          let cur = get_env T_cur man flow in
          let strings_e1 = Nonrel.eval e1 cur |> OptionExt.none_to_exn |> snd in
          let itv_e2 = man.ask (Numeric.Common.Q_int_interval e2) flow in
          (* FIXME: arbitrary constants... *)
          if ItvUtils.IntItv.is_bounded @@ Bot.bot_to_exn itv_e2 && ItvUtils.IntItv.size @@ Bot.bot_to_exn itv_e2 <= (Z.of_int 5) && not @@ Value.is_top strings_e1 && Value.cardinal strings_e1 <= 3 then
            let results =
              Value.fold (fun str acc ->
                  List.fold_left (fun acc nb ->
                      if Z.to_int nb <= 0 then (* FIXME to_int *)
                        Value.add "" acc
                       else
                         Value.add (repeat str (Z.to_int nb)) acc
                    ) acc (ItvUtils.IntItv.to_list @@ Bot.bot_to_exn itv_e2)
                ) strings_e1 Value.empty in
            Eval.join_list ~empty:(fun () -> failwith "emptycase binop")
              (Value.fold (fun s acc -> (Eval.singleton (mk_string s range) flow) :: acc) results [])
          else
            Eval.singleton (mk_top T_string range) flow
        )
      |> OptionExt.return

    | E_binop (O_plus, e1, e2) when etyp e1 = T_string && etyp e2 = T_string ->
       (man.eval e1 flow >>$?
        fun e1 flow ->
        man.eval e2 flow >>$?
        fun e2 flow ->
        let cur = get_env T_cur man flow in
        Option.bind (Nonrel.eval {expr with ekind = E_binop(O_plus, e1, e2)} cur) (fun (_, value) ->
            Eval.join_list ~empty:(fun () -> assert false)
              (if Value.is_top value then [Eval.singleton (mk_top T_string range) flow]
               else Value.fold (fun s acc -> (Eval.singleton (mk_string s range) flow) :: acc) value [])
            |> OptionExt.return
          )
       )

    | E_len e when etyp e = T_string ->
      man.eval e flow >>$
        (fun e flow ->
          let cur = get_env T_cur man flow in
          let strings_e = Nonrel.eval e cur |> OptionExt.none_to_exn |> snd in
          Eval.join_list ~empty:(fun () -> failwith "empty length")
            (if Value.is_top strings_e then
               man.eval (mk_top T_int range) flow :: []
             else Value.fold (fun s acc -> (man.eval (mk_int (String.length s) range) flow) :: acc) strings_e [])
        )
      |> OptionExt.return

    | _ ->
         None

  let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_strings_powerset e ->
      man.eval e flow |>
      Cases.reduce_result
        (fun e flow ->
           let cur = get_env T_cur man flow in
           Nonrel.eval e cur |> OptionExt.lift snd
        )
        ~join:(OptionExt.lift2 StringPower.join)
        ~meet:(OptionExt.lift2 StringPower.meet)
        ~bottom:(Some StringPower.bottom)

    | _ ->
       let ctx = Flow.get_ctx flow in
       let cur = get_env T_cur man flow in
       Nonrel.ask query man ctx cur


  let print_expr man flow printer exp = ()

end

let () =
  Sig.Abstraction.Domain.register_standard_domain (module Domain)
