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

(** Utility functions for manipulating lists of value abstractions. *)

open Core.Sig.Value.Lowlevel
open Core.Manager


(****************************************************************************)
(**                              {2 Types}                                  *)
(****************************************************************************)

(** Value abstraction module *)
type 't vmodule = (module VALUE with type t = 't)


(** List of value modules *)
type _ vlist =
  | Nil : unit vlist
  | Cons : 't vmodule * 'b vlist -> ('t * 'b) vlist



(****************************************************************************)
(**                       {2 Value creation}                                *)
(****************************************************************************)

type apply = {
  f: 't. 't vmodule -> 't;
}

(** Create an abstract value *)
let vlist_apply f l =
  let rec aux : type t. t vlist -> t =
    function
    | Nil -> ()
    | Cons(hd,tl) ->
      f.f hd, aux tl
  in
  aux l



type apply2 = {
  f: 't. 't vmodule -> 't -> 't -> 't;
}

(** Create an abstract value from two abstract values *)
let vlist_apply2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> t =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd,tl), (hd1, tl1), (hd2, tl2) ->
        f.f hd hd1 hd2, aux tl tl1 tl2
  in
  aux l v1 v2



(****************************************************************************)
(**                         {2 List mapping}                                *)
(****************************************************************************)


type 'b map = {
  f: 't. 't vmodule -> 'b;
}

(** Call a function pointwise and return a list *)
let vlist_map f l =
  let rec aux : type t. t vlist -> 'b list =
    function
    | Nil -> []
    | Cons(hd,tl) ->
      f.f hd :: aux tl
  in
  aux l



(****************************************************************************)
(**                           {2 Predicates}                                *)
(****************************************************************************)

type pred = {
  f: 't. 't vmodule -> 't -> bool;
}

(** Test an ∃ predicate *)
let vlist_exists f l v =
  let rec aux : type t. t vlist -> t -> bool =
    fun l v ->
      match l, v with
      | Nil, () -> false
      | Cons(hd,tl), (hdv, tlv) ->
        f.f hd hdv || aux tl tlv
  in
  aux l v


(** Test a ∀ predicate *)
let vlist_all f l v =
  let rec aux : type t. t vlist -> t -> bool =
    fun l v ->
      match l, v with
      | Nil, () -> true
      | Cons(hd,tl), (hdv, tlv) ->
        f.f hd hdv && aux tl tlv
  in
  aux l v



type pred2 = {
  f: 't. 't vmodule -> 't -> 't -> bool;
}


(** Test an ∃ predicate on two lists *)
let vlist_exists2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> bool =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> false
      | Cons(hd,tl), (hdv1, tlv1), (hdv2, tlv2) ->
        f.f hd hdv1 hdv2 || aux tl tlv1 tlv2
  in
  aux l v1 v2


(** Test a ∀ predicate on two lists *)
let vlist_all2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> bool =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> true
      | Cons(hd,tl), (hdv1, tlv1), (hdv2, tlv2) ->
        f.f hd hdv1 hdv2 && aux tl tlv1 tlv2
  in
  aux l v1 v2



(****************************************************************************)
(**                 {2 Value creation with managers}                        *)
(****************************************************************************)


(** Create a value manager for the head of the list *)
let hdman man = {
  man with
  vget = (fun v -> man.vget v |> fst);
  vset = (fun hdv v -> man.vset (hdv, man.vget v |> snd) v);
}

(** Create a value manager for the tail of the list *)
let tlman man = {
  man with
  vget = (fun v -> man.vget v |> snd);
  vset = (fun tlv v -> man.vset (man.vget v |> fst, tlv) v);
}


type 'a man_apply = {
  f: 't. 't vmodule -> ('a,'t) vman -> 't;
}

(** Create an abstract value with a manager *)
let vlist_man_apply f l man =
  let rec aux : type t. t vlist -> ('a,t) vman -> t =
    fun l man ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd (hdman man), aux tl (tlman man)
  in
  aux l man


type 'a man_apply_pair = {
  f: 't. 't vmodule -> ('a,'t) vman -> 't * 't;
}


(** Create a pair of abstract values with a manager *)
let vlist_man_apply_pair f l man =
  let rec aux : type t. t vlist -> ('a,t) vman -> t * t =
    fun l man ->
      match l with
      | Nil -> (),()
      | Cons(hd,tl) ->
        let v1, v2 = f.f hd (hdman man) in
        let tl1, tl2 = aux tl (tlman man) in
        (v1,tl1),(v2,tl2)
  in
  aux l man


type ('a,'r) map_man_opt = {
  f : 't. 't vmodule -> ('a,'t) vman -> 'r option
}

let vlist_map_man_opt f l man =
  let rec aux : type t. t vlist -> ('a,t) vman -> 'r list =
    fun l man ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        match f.f hd (hdman man) with
        | Some r -> r :: aux tl (tlman man)
        | None -> aux tl (tlman man)
  in
  aux l man


let vlist_ret_man_opt f l man =
  let rec aux : type t. t vlist -> ('a,t) vman -> 'r option =
    fun l man ->
      match l with
      | Nil -> None
      | Cons(hd,tl) ->
        match f.f hd (hdman man) with
        | Some r -> Some r
        | None -> aux tl (tlman man)
  in
  aux l man


(****************************************************************************)
(**                         {2 Iterators}                                   *)
(****************************************************************************)


type ('a,'b) man_fold = {
  f: 't. 't vmodule -> ('a,'t) vman -> 'b -> 'b;
}


let vlist_man_fold f l man init =
  let rec aux : type t. t vlist -> ('a,t) vman -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd (hdman man) acc in
        aux tl (tlman man) acc'
  in
  aux l man init


(****************************************************************************)
(**                         {2 Pretty printer}                              *)
(****************************************************************************)

type print = {
  f: 't. 't vmodule -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let vlist_print f l fmt sep v =
  let rec aux : type t. t vlist -> Format.formatter -> t -> unit =
    fun l fmt v ->
      match l, v with
      | Nil, () -> ()
      | Cons(m,Nil), (v,()) -> f.f m fmt v
      | Cons(hd,tl), (hdv, tlv) ->
        Format.fprintf fmt "%a%s%a" (f.f hd) hdv sep (aux tl) tlv
  in
  aux l fmt v
