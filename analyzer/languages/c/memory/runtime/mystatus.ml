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

(** Interval abstraction of integer values. *)

open Mopsa
open Universal.Ast
open Ast
open Bot
open Sig.Abstraction.Simplified_value
open Common

(** Use the simplified signature for handling homogenous operators *)
module SimplifiedValue =
struct
  open Utils_core.Bot_top


  type status = Untracked | Valid | Invalid

  let compare_status s1 s2 =
    match s1, s2 with 
    | Untracked, Untracked -> 0
    | Valid, Valid -> 0
    | Invalid, Invalid -> 0
    | _, _ -> compare s1 s2

  let pp_status fmt s =
    match s with 
    | Untracked -> Format.pp_print_string fmt "untracked"
    | Valid -> Format.pp_print_string fmt "valid"
    | Invalid -> Format.pp_print_string fmt "invalid"


  type t = status with_bot_top



  include GenValueId(struct
      type nonrec t = t
      let name = "c.runtime.status"
      let display = "runtime status"
    end)
  
  let print (printer: Print.printer) (x: t) : unit =
    match x with 
    | TOP -> unformat Format.pp_print_string printer Utils_core.Top.top_string
    | BOT -> unformat Format.pp_print_string printer Utils_core.Bot.bot_string
    | Nbt s -> unformat pp_status printer s 

  let to_string (x: t) : string =
    match x with 
    | TOP -> Utils_core.Top.top_string
    | BOT -> Utils_core.Bot.bot_string
    | Nbt s -> Format.asprintf "%a" pp_status s

  let pp_value fmt (x: t) =
    match x with 
    | TOP -> Format.pp_print_string fmt Utils_core.Top.top_string
    | BOT -> Format.pp_print_string fmt Utils_core.Bot.bot_string
    | Nbt s -> pp_status fmt s 


  let accept_type (t: typ) = true

  let bottom : t = BOT

  let top : t = TOP

  let top_of_typ t = top

  let is_bottom (abs: t) =
    match abs with 
    | BOT -> true 
    | TOP -> false 
    | Nbt x -> false 

    
  let subset (x:t) (y:t) : bool = 
    let () = Debug.debug ~channel:"runvals" "subset %a and %a" pp_value x pp_value y in 
    match x, y with 
    | BOT, _ -> true 
    | _, TOP -> true 
    | Nbt c1, Nbt c2 -> compare_status c1 c2 = 0
    | _ -> false 



    
  let join (x: t) (y: t) : t =
    let () = Debug.debug ~channel:"runvals" "joining %a and %a" pp_value x pp_value y in 
    match x, y with 
    | TOP, _ -> TOP 
    | _, TOP -> TOP
    | BOT, y -> y
    | x, BOT -> x
    | Nbt c1, Nbt c2 -> if compare_status c1 c2 = 0 then Nbt c1 else TOP
    
  let meet (x: t) (y: t) : t =
    let () = Debug.debug ~channel:"runvals" "meeting %a and %a" pp_value x pp_value y in 
    match x, y with 
    | TOP, y -> y 
    | x, TOP -> x
    | BOT, _ -> BOT
    | _, BOT -> BOT
    | Nbt c1, Nbt c2 -> if compare_status c1 c2 = 0 then Nbt c1 else BOT
      
  let widen ctx x y : t = join x y

  let constant (c: constant) t = 
    let () = Debug.debug ~channel:"runvals" "constant %a: %a" pp_constant c pp_typ t in 
    Nbt Untracked

  let unop op t a tr = a
    (* match op with
    | O_log_not -> a
    | O_minus  -> a
    | O_plus  -> a
    | O_abs -> a
    | O_wrap(l, u) -> a
    | O_bit_invert -> a
    | _ -> top_of_typ tr *)

  let binop op t1 a1 t2 a2 tr = a1
    (* match op with
    | O_plus   -> a1
    | O_minus  -> a1
    | O_mult   -> a1
    | O_div    -> a1
    | O_ediv   -> a1
    | O_pow    -> a1
    | O_eq     -> a1
    | O_ne     -> a1
    | O_lt     -> a1
    | O_le     -> a1
    | O_gt     -> a1
    | O_ge     -> a1
    | O_log_or   -> a1
    | O_log_and  -> a1
    | O_log_xor -> a1
    | O_mod    -> a1
    | O_erem   -> a1
    | O_bit_and -> a1
    | O_bit_or -> a1
    | O_bit_xor -> a1
    | O_bit_rshift -> a1
    | O_bit_lshift -> a1
    | _     -> top_of_typ tr *)


  let filter = default_filter

  let backward_unop = default_backward_unop

  let backward_binop = default_backward_binop

  let compare = default_compare

  let avalue k t = None


  (* let avalue : type r. r avalue_kind -> t -> r option =
    fun aval a ->
    match aval with
    | Common.V_int_interval -> Some a
    | Common.V_int_interval_fast -> Some a
    | Common.V_int_congr_interval -> Some (a, Bot.Nb Common.C.minf_inf)
    | _ -> None *)


end


(** We lift now to the advanced signature to handle casts and queries *)
open Sig.Abstraction.Value
module MyValue =
struct

  include SimplifiedValue

  module V = MakeValue(SimplifiedValue)

  include V

  (** Cast a non-integer value to an integer *)
  let cast man e = top

  (* Evaluation of integer expressions *)
  let eval man e =
    match ekind e with
    (* Casts *)
    (* | E_unop(O_cast,ee) -> cast man ee *)
    | _ -> 
      V.eval man e 
      (* >>$ fun res flow -> 
      man.eval ~route:(Below name) e >>$  *)
      (* Other expressions are handled by the simplified domain *)


  (* Extended backward refinement of casts to integers. *)
  let backward_ext_cast man e ve r = None

  (* Extended backward evaluations *)
  (* let backward_ext man e ve r =
    match ekind e with
    | E_unop(O_cast,ee) ->
      (* We use the extended transfer function because we need to refine
         a non-integer value *)
      backward_ext_cast man ee ve (man.get r)
    | _ -> V.backward_ext man e ve r *)


  (** {2 Utility functions} *)

end


let () =
  register_value_abstraction (module MyValue)
