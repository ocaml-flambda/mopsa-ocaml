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

(**
   Pool of allocated heap addresses.

   Maintains the set of allocated addresses by distinguishing between
   recent and old allocations.
*)

open Mopsa
open Framework.Ast
open Ast

let debug fmt = Debug.debug ~channel:"universal.heap.pool" fmt

module type ADDRINFO =
sig
  type t
  val name : string
  val compare : t -> t -> int
  val extract : addr_kind * Callstack.cs * range * int -> t
  val print : Format.formatter -> t -> unit
  val get_flag : t -> int
end

module AddrInfoRecency = struct
  type flag = int
  type t = addr_kind * Callstack.cs * range * flag

  let name = "recency"

  let extract x = x

  let print fmt (a, cs, r, f) =
    Format.fprintf fmt "(%a, %a, %a, %a)"
      pp_addr_kind a
      Callstack.pp_call_stack cs
      pp_range r
      Format.pp_print_int f

  let compare (a, cs, r, f) (a', cs', r', f') =
    Compare.compose
      [
        (fun () -> compare_addr_kind a a');
        (fun () -> Callstack.compare cs cs');
        (fun () -> compare_range r r');
        (fun () -> Pervasives.compare f f')
      ]

  let get_flag (a, cs, r, f) = f
end

module AddrInfoTypes = struct
  type flag = int
  type t = addr_kind * Callstack.cs * flag

  let name = "types"

  let extract (a, cs, r, f) = (a, cs, f)

  let print fmt (a, cs, f) =
    Format.fprintf fmt "(%a, %a, %a)"
      pp_addr_kind a
      Callstack.pp_call_stack cs
      Format.pp_print_int f

  let compare (a, cs, f) (a', cs', f') =
    Compare.compose
      [
        (fun () -> compare_addr_kind a a');
        (fun () -> Callstack.compare cs cs');
        (fun () -> Pervasives.compare f f')
      ]

  let get_flag (a, cs, f) = f
end



module Make
    (AddrInfo: ADDRINFO)
=
struct

  let extract = AddrInfo.extract

  let uid_ref = ref 0
  let get_fresh () =
    let rep = !uid_ref in
    let () = incr uid_ref in
    rep

  let recent_flag = 0
  let old_flag = 1


  module AddrUid =
  struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end

  module Equiv = Equiv.Make(AddrInfo)(AddrUid)

  let get_id_equiv (info: AddrInfo.t) (e: Equiv.t) =
    try
      Equiv.find_l info e, e
    with
    | Not_found ->
      let x = get_fresh () in
      let new_eq = Equiv.add (info, x) e in
      x, new_eq

  type ('a, _) Annotation.key +=
    | KAddr : ('a, Equiv.t) Annotation.key

  let () =
    Annotation.(register_stateless_annot {
        eq = (let f: type a b. (a, b) key -> (Equiv.t, b) eq option =
                function
                | KAddr -> Some Eq
                | _ -> None
              in
              f);
        print = (fun fmt m -> Format.fprintf fmt "Addr uids: @[%a@]" Equiv.print m);
      }) ();
    ()

  let get_id_flow (info: AddrInfo.t) (f: 'a flow) : (int * 'a flow) =
    let e = Flow.get_annot KAddr f in
    let x, e = get_id_equiv info e in
    (x, Flow.set_annot KAddr e f)

  let get_addr_flag addr flow =
    let e = Flow.get_annot KAddr flow in
    try
      AddrInfo.get_flag @@ Equiv.find_r addr.addr_uid e
    with Not_found ->
      Exceptions.panic "get_addr_flag: %a not found" pp_addr addr

  let is_recent addr flow =
    get_addr_flag addr flow = recent_flag

  let is_old addr flow =
    get_addr_flag addr flow = old_flag


  include Framework.Lattices.Powerset.Make(
    struct
      type t = addr
      let compare = compare_addr
      let print = pp_addr
    end
    )
end
