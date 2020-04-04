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

(** Pool of stack domains.

    The pool of a reduced product [S1 * ... * Sn] is represented as an
    heterogenous list [(t1 * (t2 * ... * (tn * unit))) pool] where
    each type [ti] is the type of the stack [Si].
*)


open Core.All
open Sig.Stacked.Intermediate


(** Abstract stack module *)
type 't stack = (module STACK with type t = 't)


(** List of stack modules *)
type _ stack_list =
  | Nil : unit stack_list
  | Cons : 't stack * 'b stack_list -> ('t * 'b) stack_list



type 'b map = {
  f: 't. 't stack -> 'b;
}


let map f l =
  let rec aux : type t. t stack_list -> 'b list =
    fun l ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd :: aux tl
  in
  aux l


type 'b fold = {
  f: 't. 't stack -> 'b -> 'b;
}


let fold f l init =
  let rec aux : type t. t stack_list -> 'b -> 'b =
    fun l acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd acc in
        aux tl acc'
  in
  aux l init



type create = {
  f: 't. 't stack -> 't;
}

let create f l =
  let rec aux : type t. t stack_list -> t =
    fun l ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd, aux tl
  in
  aux l



type 'a print = {
  f: 't. 't stack -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let print f l sep fmt a =
  let rec aux : type t. t stack_list -> Format.formatter -> t -> unit =
    fun l fmt a ->
      match l, a with
      | Nil, () -> ()
      | Cons(m,Nil), (aa, ()) -> f.f m fmt aa
      | Cons(hd,tl), (hda,tla) ->
        f.f hd fmt hda;
        Format.fprintf fmt "%s" sep;
        aux tl fmt tla
  in
  aux l fmt a


type 'a pred = {
  f: 't. 't stack -> 't -> bool;
}

(** Test an ∃ predicate *)
let exists f l a =
  let rec aux : type t. t stack_list -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> false
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda || aux tl tla
  in
  aux l a


let hdman man = {
  man with
  get = (fun a -> man.get a |> fst);
  set = (fun hd a -> man.set (hd, man.get a |> snd) a);
  get_log = (fun log -> man.get_log log |> Log.first);
  set_log = (fun l log -> man.set_log (Log.tuple (l, man.get_log log |> Log.second)) log);
}

let tlman man = {
  man with
  get = (fun a -> man.get a |> snd);
  set = (fun tl a -> man.set (man.get a |> fst, tl) a);
  get_log = (fun log -> man.get_log log |> Log.second);
  set_log = (fun l log -> man.set_log (Log.tuple (man.get_log log |> Log.first, l)) log);
}


type ('a,'b,'s) man_fold = {
  f: 't. 't stack -> ('a, 't,'s) man -> 'b -> 'b;
}

let man_fold f l man init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd (hdman man) acc in
        aux tl (tlman man) acc'
  in
  aux l man init


type ('a,'b,'s) man_map = {
  f: 't. 't stack -> ('a, 't,'s) man -> 'b;
}

let man_map f l man =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b list =
    fun l man ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd (hdman man) :: aux tl (tlman man)
  in
  aux l man


type ('a,'b,'c,'s) man_map_combined = {
  f: 't. 't stack -> 'b -> ('a,'t,'s) man -> 'c;
}


let man_map_combined f l1 l2 man =
  let rec aux : type t. t stack_list -> 'b list -> ('a,t,'s) man -> 'c list =
    fun l1 l2 man ->
      match l1, l2 with
      | Nil, [] -> []
      | Cons(hd1,tl1), hd2 :: tl2 ->
        f.f hd1 hd2 (hdman man) :: aux tl1 tl2 (tlman man)
      | _ -> assert false
  in
  aux l1 l2 man


type ('a,'b,'c,'s) man_fold_combined = {
  f: 't. 't stack -> 'b -> ('a,'t,'s) man -> 'c -> 'c;
}


let man_fold_combined f l1 l2 man init =
  let rec aux : type t. t stack_list -> 'b list -> ('a,t,'s) man -> 'c -> 'c =
    fun l1 l2 man acc ->
      match l1, l2 with
      | Nil, [] -> acc
      | Cons(hd1,tl1), hd2::tl2 ->
        let acc' = f.f hd1 hd2 (hdman man) acc in
        aux tl1 tl2 (tlman man) acc'
      | _ -> assert false
  in
  aux l1 l2 man init


type ('a,'b,'s) man_fold2 = {
  f: 't. 't stack -> ('a,'t,'s) man -> 't -> 't -> 'b -> 'b;
}


let man_fold2 f l man a1 a2 init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> t -> t -> 'c -> 'c =
    fun l man a1 a2 acc ->
      match l, a1, a2 with
      | Nil, (), () -> acc
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let acc' = f.f hd (hdman man) hda1 hda2 acc in
        aux tl (tlman man) tla1 tla2 acc'
  in
  aux l man a1 a2 init


type ('a,'b,'s) man_fold_apply2 = {
  f: 't. 't stack -> ('a,'t,'s) man -> 't -> 't -> 'b -> 't * 'b;
}


let man_fold_apply2 f l man a1 a2 init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> t -> t -> 'c -> t * 'c =
    fun l man a1 a2 acc ->
      match l, a1, a2 with
      | Nil, (), () -> (), acc
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let hda',acc' = f.f hd (hdman man) hda1 hda2 acc in
        let tla',acc'' = aux tl (tlman man) tla1 tla2 acc' in
        (hda',tla'),acc''
  in
  aux l man a1 a2 init


