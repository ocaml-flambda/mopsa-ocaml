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

(** Heterogeneous lists of modules of stack domains *)


open Core
open Sig.Stacked.Lowlevel


(** {2 Types declaration} *)
(** ********************* *)

(** Stack module *)
type 't stack = (module STACK with type t = 't)


(** List of stack modules *)
type _ stack_list =
  | Nil : unit stack_list
  | Cons : 't stack * 'a stack_list -> ('t * 'a) stack_list


(** List of pairs of stack modules and extra values *)
type (_,'x) pair_stack_list =
  | PNil : (unit,'x) pair_stack_list
  | PCons : 't stack * 'x * ('a,'x) pair_stack_list -> ('t * 'a,'x) pair_stack_list



(** {2 Managers} *)
(** ************ *)


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


(** {2 Constructors} *)
(** **************** *)

type make = { f : 't. 't stack -> 't }
let make {f} l =
  let rec aux : type t. t stack_list -> t =
    fun l ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f hd, aux tl
  in
  aux l



type ('a,'s,'b) make_fold_man = { f: 't. 't stack -> ('a,'t,'s) man -> 'b -> 't * 'b }
let make_fold_man {f} l man init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b -> t * 'b =
    fun l man acc ->
      match l with
      | Nil -> (), acc
      | Cons(hd,tl) ->
        let hda,acc = f hd (hdman man) acc in
        let tla, acc = aux tl (tlman man) acc in
        (hda,tla), acc
  in
  aux l man init



type 'a make_list = { f : 't. 't stack -> 'a }
let make_list {f} l =
  let rec aux : type t. t stack_list -> 'a list =
    fun l ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f hd :: aux tl
  in
  aux l



(** {2 Iterators} *)
(** ************* *)

type iter = { f: 't. 't stack -> 't -> unit; }
let iter {f} l a =
  let rec aux : type t. t stack_list -> t -> unit =
    fun l a ->
      match l, a with
      | Nil, () -> ()
      | Cons(hd,tl), (hda,tla) ->
        f hd hda;
        aux tl tla
  in
  aux l a



type map = { f: 't. 't stack -> 't -> 't }
let map {f} l a =
  let rec aux : type t. t stack_list -> t -> t =
    fun l a ->
      match l, a with
      | Nil, () -> ()
      | Cons(hd,tl), (hda,tla) ->
        f hd hda, aux tl tla
  in
  aux l a


type 'a fold = { f: 't. 't stack -> 't -> 'a -> 'a }
let fold {f} l a init =
  let rec aux : type t. t stack_list -> t -> 'a -> 'a =
    fun l a acc ->
      match l, a with
      | Nil, () -> acc
      | Cons(hd,tl), (hda,tla) ->
        aux tl tla (f hd hda acc)
  in
  aux l a init


type ('a,'s,'b) fold_man = { f: 't. 't stack -> ('a,'t,'s) man -> 'b -> 'b }
let fold_man {f} l man init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        aux tl (tlman man) (f hd (hdman man) acc)
  in
  aux l man init


type 'a fold_module = { f: 't. 't stack -> 'a -> 'a }
let fold_module {f} l init =
  let rec aux : type t. t stack_list -> 'a -> 'a =
    fun l acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        aux tl (f hd acc)
  in
  aux l init


type 'a map_fold = { f: 't. 't stack -> 't -> 'a -> 't * 'a }
let map_fold {f} l a init =
  let rec aux : type t. t stack_list -> t -> 'a -> t * 'a =
    fun l a acc ->
      match l, a with
      | Nil, () -> (), acc
      | Cons(hd,tl), (hda,tla) ->
        let hda',acc = f hd hda acc in
        let tla', acc = aux tl tla acc in
        (hda',tla'), acc
  in
  aux l a init


type ('a,'s,'b) map_fold_man = { f: 't. 't stack -> ('a,'t,'s) man -> 't -> 'b -> 't * 'b }
let map_fold_man {f} l man a init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> t -> 'b -> t * 'b =
    fun l man a acc ->
      match l, a with
      | Nil, () -> (), acc
      | Cons(hd,tl), (hda,tla) ->
        let hda',acc = f hd (hdman man) hda acc in
        let tla', acc = aux tl (tlman man) tla acc in
        (hda',tla'), acc
  in
  aux l man a init



(** {2 Predicates} *)
(** ************** *)

type 'a pred = { f: 't. 't stack -> 't -> bool }

let exists {f} l a =
  let rec aux : type t. t stack_list -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> false
      | Cons(hd,tl), (hda,tla) ->
        f hd hda || aux tl tla
  in
  aux l a

let for_all {f} l a =
  let rec aux : type t. t stack_list -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> true
      | Cons(hd,tl), (hda,tla) ->
        f hd hda && aux tl tla
  in
  aux l a

type ('a,'s,'b) pred_fold_man = { f: 't. 't stack -> ('a,'t,'s) man -> 'b -> bool * 'b }

let exists_fold_man {f} l man init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b -> bool * 'b =
    fun l man acc ->
      match l with
      | Nil -> false, acc
      | Cons(hd,tl) ->
        let b,acc = f hd (hdman man) acc in
        if not b then aux tl (tlman man) acc
        else true,acc
  in
  aux l man init


let for_all_fold_man {f} l man init =
  let rec aux : type t. t stack_list -> ('a,t,'s) man -> 'b -> bool * 'b =
    fun l man acc ->
      match l with
      | Nil -> true, acc
      | Cons(hd,tl) ->
        let b,acc = f hd (hdman man) acc in
        if b then aux tl (tlman man) acc
        else false,acc
  in
  aux l man init


(** {2 Combination with lists} *)
(** ************************** *)

let combine l1 l2 =
  let rec aux : type t. t stack_list -> 'x list -> (t,'x) pair_stack_list =
    fun l1 l2 ->
      match l1,l2 with
      | Nil,[] -> PNil
      | Cons(hd,tl), hdx::tlx -> PCons (hd,hdx,aux tl tlx)
      | _ -> assert false
  in
  aux l1 l2


type ('a,'s,'x,'b) fold_man_pair = { f: 't. 't stack -> ('a,'t,'s) man -> 'x -> 'b -> 'b }
let fold_man_pair {f} l man init =
  let rec aux : type t. (t,'x) pair_stack_list -> ('a,t,'s) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | PNil -> acc
      | PCons(hd,x,tl) ->
        aux tl (tlman man) (f hd (hdman man) x acc)
  in
  aux l man init
