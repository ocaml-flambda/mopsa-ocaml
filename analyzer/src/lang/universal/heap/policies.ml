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

(** Policies for grouping heap addresses *)

open Mopsa
open Ast

(** {2 Signature of a grouping policy} *)
(** ================================== *)


module type POLICY =
sig
  val name : string
  val mk_addr : addr_kind -> mode -> range -> 'a flow -> addr
end



(** {2 Grouping of addresses by callstack and range *)
(** =============================================== *)

module StackRangePolicy : POLICY = struct

  type addr_group +=
    | G_stack_range of Callstack.cs * range

  let name = "callstack_range"

  let mk_addr kind mode range flow =
    let cs = Callstack.get flow in
    {
      addr_kind = kind;
      addr_mode = mode;
      addr_group = G_stack_range (cs,range)
    }

  let () =
    register_addr_group {
      compare = (fun next g1 g2 ->
          match g1, g2 with
          | G_stack_range(cs,r), G_stack_range(cs',r') ->
            Compare.compose
              [
                (fun () -> Callstack.compare cs cs');
                (fun () -> compare_range r r');
              ]

          | _ -> next g1 g2
        );
      print = (fun next fmt g ->
          match g with
          | G_stack_range(cs,r) ->
            Format.fprintf fmt "(%a, %a)"
              Callstack.pp_call_stack cs
              pp_range r
          | _ -> next fmt g
        );
    }
end



(** {2 Grouping of addresses by callstack} *)
(** ====================================== *)

module StackPlocy : POLICY = struct

  type addr_group +=
    | G_stack of Callstack.cs

  let name = "callstack"

  let mk_addr kind mode range flow =
    let cs = Callstack.get flow in
    {
      addr_kind = kind;
      addr_mode = mode;
      addr_group = G_stack (cs)
    }


  let () =
    register_addr_group {
      compare = (fun next g1 g2 ->
          match g1, g2 with
          | G_stack(cs), G_stack(cs') ->
            Callstack.compare cs cs'

          | _ -> next g1 g2
        );
      print = (fun next fmt g ->
          match g with
          | G_stack(cs) ->
            Callstack.pp_call_stack fmt cs
          | _ -> next fmt g
        );
    }
end



(** {2 Group all addresses} *)
(** ======================= *)

module AllPolicy : POLICY = struct

  let name = "all"

  let mk_addr kind mode range flow =
    {
      addr_kind = kind;
      addr_mode = mode;
      addr_group = G_all
    }

end
