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

(** Leaf domains have a simplified interface that gives access to
    their local abstractions. The global manager and the flow abstraction are
    not accessible.
*)

open Ast.All
open Lattice
open Context
open Id
open Zone
open Manager
open Interface
open Token
open Channel

module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Domain name *)

  val zones : zone list
  (** Zones of the provided transfer functions *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Predicates} *)
  (** ************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

  val merge : t -> t * block -> t * block -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> uctx -> t * uctx
  (** Initial abstract element *)

  val exec : stmt -> uctx -> t -> (t * uctx) option
  (** Computation of post-conditions *)

  val ask : 'r Query.query -> uctx -> t -> 'r option
  (** Handler of queries *)


  (** {2 Reduction refinement} *)
  (** ************************ *)

  val refine : channel -> t -> t with_channel


end


(** Create a full domain from a leaf. *)
module MakeIntermediate(D: DOMAIN) : Intermediate.DOMAIN with type t = D.t =
struct

  include D

  let merge pre (post1, log1) (post2, log2) =
    let block1 = Log.get_domain_block log1
    and block2 = Log.get_domain_block log2 in
    D.merge pre (post1, block1) (post2, block2)

  let init prog man flow =
    let ctx = Flow.get_ctx flow in

    let a', uctx = D.init prog (Context.get_unit ctx) in

    set_domain_env T_cur a' man flow |>
    Flow.set_ctx (Context.set_unit uctx ctx)

  let interface = {
    iexec = {
      provides = D.zones;
      uses = [];
    };
    ieval = {
      provides = [];
      uses = [];
    }
  }

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign _ | S_assume _ | S_add _ | S_remove _ | S_rename _
    | S_project _ | S_fold _ | S_expand _ | S_forget _
      ->
      let a = get_domain_env T_cur man flow in
      let ctx = Flow.get_ctx flow in
      D.exec stmt (Context.get_unit ctx) a |>
      Option.lift @@ fun (a',uctx) ->

      set_domain_env T_cur a' man flow |>
      Flow.set_ctx (Context.set_unit uctx ctx) |>
      Post.return

    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow =
    D.ask query (Flow.get_ctx flow |> Context.get_unit) (get_domain_env T_cur man flow)

  let refine channel man flow =
    D.refine channel (get_domain_env T_cur man flow) |>
    Channel.bind @@ fun a ->

    set_domain_env T_cur a man flow |>
    Channel.return

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

let domains : (module DOMAIN) list ref = ref []

let register_domain dom =
  domains := dom :: !domains

let find_domain name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let mem_domain name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains



(****************************************************************************)
(**                      {2 List representation}                            *)
(****************************************************************************)


(** Abstract domain module *)
type 't dmodule = (module DOMAIN with type t = 't)


(** List of domain modules *)
type _ dlist =
  | Nil : unit dlist
  | Cons : 't dmodule * 'b dlist -> ('t * 'b) dlist





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


type 'b fold_apply = {
  f: 't. 't dmodule -> 'b -> 't -> 'b;
}


let dlist_fold_apply f l init a =
  let rec aux : type t. t dlist -> 'b -> t -> 'b =
    fun l acc a ->
      match l,a with
      | Nil,() -> acc
      | Cons(hd,tl), (hda,tla) ->
        let acc' = f.f hd acc hda in
        aux tl acc' tla
  in
  aux l init a


type 'a map_opt = {
  f: 't. 't dmodule -> 't -> 'a option;
}

(** Create an abstract value *)
let dlist_map_opt f l a =
  let rec aux : type t. t dlist -> t -> 'a option =
    fun l a ->
      match l, a with
      | Nil, () -> None
      | Cons(hd,tl), (hda,tla) ->
        match f.f hd hda with
        | None -> aux tl tla
        | Some r -> Some r
  in
  aux l a

(****************************************************************************)
(**                       {2 Value creation}                                *)
(****************************************************************************)

type create = {
  f: 't. 't dmodule -> 't;
}

(** Create an abstract value *)
let dlist_create f l =
  let rec aux : type t. t dlist -> t =
    fun l ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd, aux tl
  in
  aux l


type apply = {
  f: 't. 't dmodule -> 't -> 't;
}

(** Create an abstract value *)
let dlist_apply f l a =
  let rec aux : type t. t dlist -> t -> t =
    fun l a ->
      match l, a with
      | Nil, () -> ()
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda, aux tl tla
  in
  aux l a

type apply_opt = {
  f: 't. 't dmodule -> 't -> 't option;
}

(** Create an abstract value *)
let dlist_apply_opt f l a =
  let rec aux : type t. t dlist -> t -> t option =
    fun l a ->
      match l, a with
      | Nil, () -> None
      | Cons(hd,tl), (hda,tla) ->
        match f.f hd hda, aux tl tla with
        | None, None -> None
        | Some hda', Some tla' -> Some (hda',tla')
        | Some hda', None -> Some (hda',tla)
        | None, Some tla' -> Some (hda,tla')
  in
  aux l a



type apply2 = {
  f: 't. 't dmodule -> 't -> 't -> 't;
}

(** Create an abstract value *)
let dlist_apply2 f l a b =
  let rec aux : type t. t dlist -> t -> t -> t =
    fun l a b ->
      match l, a, b with
      | Nil, (), () -> ()
      | Cons(hd,tl), (hda,tla), (hdb,tlb) ->
        f.f hd hda hdb, aux tl tla tlb
  in
  aux l a b


type apply3 = {
  f: 't. 't dmodule -> 't -> 't -> 't -> 't;
}

(** Create an abstract value *)
let dlist_apply3 f l a b c =
  let rec aux : type t. t dlist -> t -> t -> t -> t =
    fun l a b c ->
      match l, a, b, c with
      | Nil, (), (), () -> ()
      | Cons(hd,tl), (hda,tla), (hdb,tlb), (hdc,tlc) ->
        f.f hd hda hdb hdc, aux tl tla tlb tlc
  in
  aux l a b c



type apply_with_channel = {
  f: 't. 't dmodule -> 't -> 't with_channel;
}

let dlist_apply_with_channel f l a =
  let rec aux : type t. t dlist -> t -> t with_channel=
    fun l a ->
      match l, a with
      | Nil, () -> Channel.return ()
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda |> Channel.bind @@ fun hda ->
        aux tl tla |> Channel.bind @@ fun tla ->
        Channel.return (hda,tla)
  in
  aux l a


(****************************************************************************)
(**                         {2 Pretty printer}                              *)
(****************************************************************************)

type 'a print = {
  f: 't. 't dmodule -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let dlist_print f l sep fmt a =
  let rec aux : type t. t dlist -> Format.formatter -> t -> unit =
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


(****************************************************************************)
(**                           {2 Predicates}                                *)
(****************************************************************************)

type 'a pred = {
  f: 't. 't dmodule -> 't -> bool;
}

(** Test an ∃ predicate *)
let dlist_exists f l a =
  let rec aux : type t. t dlist -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> false
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda || aux tl tla
  in
  aux l a


(** Test a ∀ predicate *)
let dlist_forall f l a =
  let rec aux : type t. t dlist -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> true
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda && aux tl tla
  in
  aux l a


type 'a pred2 = {
  f: 't. 't dmodule -> 't -> 't -> bool;
}

(** Test an ∃ predicate *)
let dlist_exists2 f l a b =
  let rec aux : type t. t dlist -> t -> t -> bool =
    fun l a b ->
      match l, a, b with
      | Nil, (), () -> false
      | Cons(hd,tl), (hda,tla), (hdb,tlb) ->
        f.f hd hda hdb || aux tl tla tlb
  in
  aux l a b


(** Test an ∃ predicate *)
let dlist_forall2 f l a b =
  let rec aux : type t. t dlist -> t -> t -> bool =
    fun l a b ->
      match l, a, b with
      | Nil, (), () -> true
      | Cons(hd,tl), (hda,tla), (hdb,tlb) ->
        f.f hd hda hdb && aux tl tla tlb
  in
  aux l a b
