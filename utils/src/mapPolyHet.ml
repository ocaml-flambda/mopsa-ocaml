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

(** Maps with polymorphic heterogeneous contents.

    [module M = Make(Key)] creates a module for encoding mappings from
    [('a, 'b) Key.t] to ['b], where ['b] is a polymorphic in ['a] and
    can change from one mapping to another.

    For a key [('a, 'a t) K], an equality witness should be defined as
    follows:
    {[
      M.register {
        eq = (let check : type b y. (b, y) Key.t -> ('a, b, 'a t, y) eq option =
                function
                | K -> Some Eq
                | _ -> None
              in check);
      }
   ]}
*)

(** Signature of keys *)
module type KEY =
sig
  type ('a, 'b) t
end

module Make = functor(Key: KEY) ->
struct

  (** Witness of type equality *)

  type (_, _) eq = Eq : ('b, 'b) eq

  type ('a, 'b) w = {
    eq : 'c. ('a, 'c) Key.t -> ('b, 'c) eq option;
  }

  type 'a vl =
    | []   : 'a vl
    | (::) : (('a, 'b) w * 'b) * 'a vl -> 'a vl

  type 'a wl =
    | [] : 'a wl
    | (::) : (('a, 'b) w) * 'a wl -> 'a wl

  type 'a t = {
    values: 'a vl;
    witnesses  : 'a wl;
  }

  let register (w: ('a, 'b) w) (m: 'a t) : 'a t =
    {m with witnesses = w :: m.witnesses}

  exception Key_not_found

  let find_witness (k: ('a, 'b) Key.t) (m: 'a t) : ('a, 'b) w =
    let rec aux : type b. ('a, b) Key.t -> 'a wl -> ('a, b) w =
      fun k -> function
        | [] -> raise Key_not_found
        | w :: tl ->
          match w.eq k with
          | Some Eq -> w
          | None -> aux k tl
    in
    aux k m.witnesses

  let empty = {
    values = [];
    witnesses = [];
  }

  let add : type b. ('a, b) Key.t -> b -> 'a t -> 'a t =
    fun k v m ->
      let rec aux : type b. ('a, b) Key.t -> b -> 'a vl -> 'a vl =
        fun k v -> function
          | [] -> [(find_witness k m, v)]
          | hd :: tl ->
            let (w, _) = hd in
            match w.eq k with
            | Some Eq -> (w, v) :: tl
            | None -> hd :: (aux k v tl)
      in
      {m with values = aux k v m.values}

  let find : type b. ('a, b) Key.t -> 'a t -> b =
    fun k m ->
      let rec aux : type c. ('a, c) Key.t -> 'a vl -> c =
        fun k -> function
          | [] -> raise Not_found
          | hd :: tl ->
            let (w, v) = hd in
            match w.eq k with
            | Some Eq -> v
            | None -> aux k tl
      in
      aux k m.values

end


(*

module type D = sig
val init : 'a list -> 'a M.t -> 'a M.t
end;;

module D1 : D = struct
let init (type a) (x: a list) (m: a M.t) : a M.t =
  M.(register {eq = (
      let check : type c. (a, c) key -> (a list, c) eq option =
        function
        | KCache -> Some Eq
        | _ -> None
      in
      check
    )} m)
end;;

*)
