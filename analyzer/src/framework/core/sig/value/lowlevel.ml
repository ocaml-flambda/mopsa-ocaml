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
open Context
open Id
open Query
open Channel



(*==========================================================================*)
(**                         {2 Value manager}                              *)
(*==========================================================================*)


(** Manager for value abstractions *)
type ('a, 't) man = {
  get : 'a -> 't;
  set : 't -> 'a -> 'a;
  eval : expr -> 'a;
  cast : 'r. 'r Id.value -> 'a -> 'r;
}


(*==========================================================================*)
(**                          {2 Value domain}                               *)
(*==========================================================================*)


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

  val get: ('a,t) man -> 's value -> 'a -> 's option
  (** Get a specific value embedded in the abstraction *)

  val set: ('a,t) man -> 's value -> 's -> 'a -> 'a option
  (** Set a specific value embedded in the abstraction *)

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

  val widen: t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  val of_constant : typ -> constant -> t
  (** Create a singleton abstract value from a constant. *)

  val unop : ('a,t) man -> typ -> operator -> 'a -> t
  (** Forward evaluation of unary operators. *)

  val binop : ('a,t) man -> typ -> operator -> 'a -> 'a -> t
  (** Forward evaluation of binary operators. *)

  val filter : ('a,t) man -> 'a -> bool -> t
  (** Keep values that may represent the argument truth value *)


  (** {2 Backward semantics} *)
  (** ********************** *)

  val bwd_unop : ('a,t) man -> typ -> operator -> 'a -> 'a -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)

  val bwd_binop : ('a,t) man -> typ -> operator -> 'a -> 'a -> 'a -> (t * t)
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
  *)


  val compare : ('a,t) man -> typ -> operator -> 'a -> 'a -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false
  *)


  (** {2 Query handler } *)
  (** ****************** *)

  val ask : ('a,t) man -> 'r query -> 'r option


  (** {2 Reduction refinement} *)
  (** ************************ *)

  val refine : ('a,t) man -> channel -> 'a -> 'a with_channel


end


(*==========================================================================*)
(**                  {2 Default backward functions}                         *)
(*==========================================================================*)

let default_bwd_unop ma typ op x r =
  x

let default_bwd_binop man typ op x y r =
  (x, y)

let default_compare man typ op x y b =
  (x, y)


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

let mem_value name =
  List.exists (fun v ->
      let module V = (val v : VALUE) in
      compare V.name name = 0
    ) !values

let names () =
  List.map (fun v ->
      let module V = (val v : VALUE) in
      V.name
    ) !values



(****************************************************************************)
(**                   {2 Lists of value abstractions}                       *)
(****************************************************************************)

(** Value abstraction module *)
type 't vmodule = (module VALUE with type t = 't)


(** List of value modules *)
type _ vlist =
  | Nil : unit vlist
  | Cons : 't vmodule * 'b vlist -> ('t * 'b) vlist



(****************************************************************************)
(**                       {3 Value creation}                                *)
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
(**                         {3 List mapping}                                *)
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
(**                           {3 Predicates}                                *)
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
(**                 {3 Value creation with managers}                        *)
(****************************************************************************)


(** Create a value manager for the head of the list *)
let hdman man = {
  man with
  get = (fun v -> man.get v |> fst);
  set = (fun hdv v -> man.set (hdv, man.get v |> snd) v);
}

(** Create a value manager for the tail of the list *)
let tlman man = {
  man with
  get = (fun v -> man.get v |> snd);
  set = (fun tlv v -> man.set (man.get v |> fst, tlv) v);
}


type 'a man_apply = {
  f: 't. 't vmodule -> ('a,'t) man -> 't;
}

(** Create an abstract value with a manager *)
let vlist_man_apply f l man =
  let rec aux : type t. t vlist -> ('a,t) man -> t =
    fun l man ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd (hdman man), aux tl (tlman man)
  in
  aux l man


type 'a man_apply_pair = {
  f: 't. 't vmodule -> ('a,'t) man -> 't * 't;
}


(** Create a pair of abstract values with a manager *)
let vlist_man_apply_pair f l man =
  let rec aux : type t. t vlist -> ('a,t) man -> t * t =
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
  f : 't. 't vmodule -> ('a,'t) man -> 'r option
}

let vlist_map_man_opt f l man =
  let rec aux : type t. t vlist -> ('a,t) man -> 'r list =
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
  let rec aux : type t. t vlist -> ('a,t) man -> 'r option =
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
(**                         {3 Iterators}                                   *)
(****************************************************************************)


type ('a,'b) man_fold = {
  f: 't. 't vmodule -> ('a,'t) man -> 'b -> 'b;
}


let vlist_man_fold f l man init =
  let rec aux : type t. t vlist -> ('a,t) man -> 'b -> 'b =
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
