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

(** Low-level signature of a value abstraction. *)

open Ast.All
open Manager
open Context
open Id
open Query


module type VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t value
  (** Identifier of the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val zones : Zone.zone list
  (** Zones in which the value abstraction is defined *)

  val types : typ list
  (** Types abstracted by the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)
  
  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)

  val cast: ('a,t) vman -> 's value -> 'a -> 's option

  (** {2 Lattice operators} *)
  (** ********************* *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  val of_constant : typ -> constant -> t
  (** Create a singleton abstract value from a constant. *)

  val unop : ('a,t) vman -> typ -> operator -> 'a -> t
  (** Forward evaluation of unary operators. *)

  val binop : ('a,t) vman -> typ -> operator -> 'a -> 'a -> t
  (** Forward evaluation of binary operators. *)

  val filter : ('a,t) vman -> 'a -> bool -> t
  (** Keep values that may represent the argument truth value *)


  (** {2 Backward semantics} *)
  (** ********************** *)

  val bwd_unop : ('a,t) vman -> typ -> operator -> 'a -> 'a -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)

  val bwd_binop : ('a,t) vman -> typ -> operator -> 'a -> 'a -> 'a -> (t * t)
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
  *)


  val compare : ('a,t) vman -> typ -> operator -> 'a -> 'a -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false
  *)


  (** {2 Evaluation query} *)
  (** ******************** *)

  val ask : ('a,t) vman -> 'r query -> 'r option

end


(*==========================================================================*)
(**                         {2 Registration}                                *)
(*==========================================================================*)

let values : (module VALUE) list ref = ref []

let register_value v = values := v :: !values

let find_value name =
  List.find (fun v ->
      let module V = (val v : VALUE) in
      compare V.name name = 0
    ) !values


(*==========================================================================*)
(**                  {2 Default backward functions}                         *)
(*==========================================================================*)

let default_bwd_unop ma typ op x r =
  x

let default_bwd_binop man typ op x y r =
  (x, y)

let default_compare man typ op x y b =
  (x, y)


(****************************************************************************)
(**                    {2 List of value abstractions}                       *)
(****************************************************************************)

type 'a vmodule = (module VALUE with type t = 'a)

(** List of value identifiers *)
type _ vlist =
  | Nil : unit vlist
  | Cons : 'a vmodule * 'b vlist -> ('a * 'b) vlist

type create = {
  f: 'a. 'a vmodule -> 'a;
}

let vlist_create f l =
  let rec aux : type t. t vlist -> t =
    function
    | Nil -> ()
    | Cons(hd,tl) ->
      f.f hd, aux tl
  in
  aux l


type 'b export = {
  f: 'a. 'a vmodule -> 'b;
}

let vlist_export f l =
  let rec aux : type t. t vlist -> 'b list =
    function
    | Nil -> []
    | Cons(hd,tl) ->
      f.f hd :: aux tl
  in
  aux l


type pred = {
  f: 'a. 'a vmodule -> 'a -> bool;
}

let vlist_exists f l v =
  let rec aux : type t. t vlist -> t -> bool =
    fun l v ->
      match l, v with
      | Nil, () -> false
      | Cons(hd,tl), (hdv, tlv) ->
        f.f hd hdv || aux tl tlv
  in
  aux l v


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
  f: 'a. 'a vmodule -> 'a -> 'a -> bool;
}

let vlist_exists2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> bool =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> false
      | Cons(hd,tl), (hdv1, tlv1), (hdv2, tlv2) ->
        f.f hd hdv1 hdv2 || aux tl tlv1 tlv2
  in
  aux l v1 v2


let vlist_all2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> bool =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> true
      | Cons(hd,tl), (hdv1, tlv1), (hdv2, tlv2) ->
        f.f hd hdv1 hdv2 && aux tl tlv1 tlv2
  in
  aux l v1 v2


let hdman man = {
  man with
  vget = (fun v -> man.vget v |> fst);
  vset = (fun hdv v -> man.vset (hdv, man.vget v |> snd) v);
}

let tlman man = {
  man with
  vget = (fun v -> man.vget v |> snd);
  vset = (fun tlv v -> man.vset (man.vget v |> fst, tlv) v);
}


type 'a man_apply = {
  f: 't. 't vmodule -> ('a,'t) vman -> 't;
}

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


type apply2 = {
  f: 'a. 'a vmodule -> 'a -> 'a -> 'a;
}

let vlist_apply2 f l v1 v2 =
  let rec aux : type t. t vlist -> t -> t -> t =
    fun l v1 v2 ->
      match l, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd,tl), (hd1, tl1), (hd2, tl2) ->
        f.f hd hd1 hd2, aux tl tl1 tl2
  in
  aux l v1 v2


type 'b fold = {
  f: 'a. 'a vmodule -> 'b -> 'a -> 'b;
}

let vlist_fold f init l v =
  let rec aux : type t. 'b -> t vlist -> t -> 'b =
    fun acc l v ->
      match l, v with
      | Nil, () -> acc
      | Cons(hd,tl), (hdv, tlv) ->
        aux (f.f hd acc hdv) tl tlv
  in
  aux init l v


type iter = {
  f: 'a. 'a vmodule -> 'a -> unit;
}

let vlist_iter f l v =
  let rec aux : type t. t vlist -> t -> unit =
    fun l v ->
      match l, v with
      | Nil, () -> ()
      | Cons(hd,tl), (hdv, tlv) ->
        f.f hd hdv;
        aux tl tlv
  in
  aux l v


type print = {
  f: 'a. 'a vmodule -> Format.formatter -> 'a -> unit;
}


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


type ('a,'r) export_opt = {
  f : 't. 't vmodule -> ('a,'t) vman -> 'r option
}

let vlist_export_opt f l man =
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


let vlist_export_opt2 f l man =
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
