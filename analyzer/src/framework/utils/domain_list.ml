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


(** Utility functions for manipulating lists of abstract domains. *)

open Core.Sig.Domain.Lowlevel
open Core.Manager


(** Abstract domain module *)
type 't dmodule = (module DOMAIN with type t = 't)


(** List of domain modules *)
type _ dlist =
  | Nil : unit dlist
  | Cons : 't dmodule * 'b dlist -> ('t * 'b) dlist



(****************************************************************************)
(**                       {2 Manager creation}                              *)
(****************************************************************************)


(** Create a manager for the head of the list *)
let hdman man = {
  man with
  get = (fun a -> man.get a |> fst);
  set = (fun hd a -> man.set (hd, man.get a |> snd) a);
  get_log = (fun log -> man.get_log log |> Core.Log.first);
  set_log = (fun l log ->
      man.set_log (
        Core.Log.tuple (l, man.get_log log |> Core.Log.second)
      ) log
    );
}

(** Create a value manager for the tail of the list *)
let tlman man = {
  man with
  get = (fun a -> man.get a |> snd);
  set = (fun tl a -> man.set (man.get a |> fst, tl) a);
  get_log = (fun log -> man.get_log log |> Core.Log.second);
  set_log = (fun l log ->
      man.set_log (
        Core.Log.tuple (man.get_log log |> Core.Log.first, l)
      ) log
    );
}



(****************************************************************************)
(**                         {2 Iterators}                                   *)
(****************************************************************************)


type 'b fold = {
  f: 't. 't dmodule -> 'b -> 'b;
}


let dlist_fold f l init =
  let rec aux : type t. t dlist -> 'b -> 'b =
    fun l acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd acc in
        aux tl acc'
  in
  aux l init



type ('a,'b) man_fold = {
  f: 't. 't dmodule -> ('a,'t) man -> 'b -> 'b;
}


let dlist_man_fold f l man init =
  let rec aux : type t. t dlist -> ('a,t) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd (hdman man) acc in
        aux tl (tlman man) acc'
  in
  aux l man init


(****************************************************************************)
(**                       {2 Value creation}                                *)
(****************************************************************************)

type apply = {
  f: 't. 't dmodule -> 't;
}

(** Create an abstract value *)
let dlist_apply f l =
  let rec aux : type t. t dlist -> t =
    function
    | Nil -> ()
    | Cons(hd,tl) ->
      f.f hd, aux tl
  in
  aux l


type 'a man_apply = {
  f: 't. 't dmodule -> ('a,'t) man -> 't;
}

let dlist_man_apply f l man =
  let rec aux : type t. t dlist -> ('a,t) man -> t =
    fun l man ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd (hdman man), aux tl (tlman man)
  in
  aux l man





(****************************************************************************)
(**                         {2 Pretty printer}                              *)
(****************************************************************************)

type 'a man_print = {
  f: 't. 't dmodule -> ('a,'t) man -> Format.formatter -> unit;
}


(** Print an abstract value *)
let dlist_man_print f l man fmt sep =
  let rec aux : type t. t dlist -> ('a,t) man -> Format.formatter -> unit =
    fun l man fmt ->
      match l with
      | Nil -> ()
      | Cons(m,Nil) -> f.f m (hdman man) fmt
      | Cons(hd,tl) ->
        f.f hd (hdman man) fmt;
        Format.fprintf fmt "%s" sep;
        aux tl (tlman man) fmt
  in
  aux l man fmt


(****************************************************************************)
(**                           {2 Predicates}                                *)
(****************************************************************************)

type 'a man_pred = {
  f: 't. 't dmodule -> ('a,'t) man -> bool;
}

(** Test an ∃ predicate *)
let dlist_man_exists f l man =
  let rec aux : type t. t dlist -> ('a,t) man -> bool =
    fun l man ->
      match l with
      | Nil -> false
      | Cons(hd,tl) ->
        f.f hd (hdman man) || aux tl (tlman man)
  in
  aux l man


(** Test a ∀ predicate *)
let dlist_man_forall f l man =
  let rec aux : type t. t dlist -> ('a,t) man -> bool =
    fun l man ->
      match l with
      | Nil -> true
      | Cons(hd,tl) ->
        f.f hd (hdman man) && aux tl (tlman man)
  in
  aux l man
