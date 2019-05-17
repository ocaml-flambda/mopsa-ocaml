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

(** The operator [Product ∈ (𝒟 × ... × 𝒟) × (𝓡 × ... × 𝓡) → 𝒟] creates a
    reduced product between n domains using a set of reduction rules.
*)

open Ast.All
open Core.All
open Context
open Log
open Channel
open Sig.Domain.Simplified
open Sig.Domain.Reduction



(****************************************************************************)
(**                      {2 List representation}                            *)
(****************************************************************************)


(** Abstract domain module *)
type 't dmodule = (module DOMAIN with type t = 't)


(** List of domain modules *)
type _ dlist =
  | Nil : unit dlist
  | Cons : 't dmodule * 'b dlist -> ('t * 'b) dlist


type 'b fold = {
  f: 't. 't dmodule -> 'b -> 'b;
}


let fold f l init =
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


let fold_apply f l init a =
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
let map_opt f l a =
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


type create = {
  f: 't. 't dmodule -> 't;
}

(** Create an abstract value *)
let create f l =
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
let apply f l a =
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
let apply_opt f l a =
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
let apply2 f l a b =
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
let apply3 f l a b c =
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

let apply_with_channel f l a =
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


type 'a print = {
  f: 't. 't dmodule -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let print f l sep fmt a =
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



type 'a pred = {
  f: 't. 't dmodule -> 't -> bool;
}

(** Test an ∃ predicate *)
let exists f l a =
  let rec aux : type t. t dlist -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> false
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda || aux tl tla
  in
  aux l a


(** Test a ∀ predicate *)
let forall f l a =
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
let exists2 f l a b =
  let rec aux : type t. t dlist -> t -> t -> bool =
    fun l a b ->
      match l, a, b with
      | Nil, (), () -> false
      | Cons(hd,tl), (hda,tla), (hdb,tlb) ->
        f.f hd hda hdb || aux tl tla tlb
  in
  aux l a b


(** Test an ∃ predicate *)
let forall2 f l a b =
  let rec aux : type t. t dlist -> t -> t -> bool =
    fun l a b ->
      match l, a, b with
      | Nil, (), () -> true
      | Cons(hd,tl), (hda,tla), (hdb,tlb) ->
        f.f hd hda hdb && aux tl tla tlb
  in
  aux l a b




(****************************************************************************)
(**                       {2 Domain Transformer}                            *)
(****************************************************************************)


(** Specification of a reduced product *)
module type SPEC =
sig
  type t
  val pool : t dlist
  val rules : (module REDUCTION) list
end


(** Factory functor *)
module Make(Spec: SPEC) : DOMAIN with type t = Spec.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = Spec.t

  include Core.Id.GenDomainId(
    struct
      type typ = t
      let name = "transformers.domain.product"
    end
    )

  let zones =
    let f = fun (type a) (m:a dmodule) acc ->
      let module Domain = (val m) in
      Domain.zones @ acc
    in
    fold { f } Spec.pool []



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.bottom
    in
    create { f } Spec.pool


  let top : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.top
    in
    create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a dmodule) fmt aa ->
      let module Domain = (val m) in
      Domain.print fmt aa
    in
    print { f } Spec.pool "" fmt a


  let is_bottom a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.is_bottom aa
    in
    exists { f } Spec.pool a

  let subset a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.subset aa1 aa2
    in
    forall2 { f } Spec.pool a1 a2

  let join a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.join aa1 aa2
    in
    apply2 { f } Spec.pool a1 a2

  let meet a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.meet aa1 aa2
    in
    apply2 { f } Spec.pool a1 a2

  let widen ctx a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.widen ctx aa1 aa2
    in
    apply2 { f } Spec.pool a1 a2

  let merge ctx pre (post1,log1) (post2,log2) =
    let f = fun (type a) (m: a dmodule) pre post1 post2 ->
      let module Domain = (val m) in
      Domain.merge ctx pre (post1,log1) (post2,log2)
    in
    apply3 { f } Spec.pool pre post1 post2


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog ctx =
    let rec aux : type t. t dlist -> uctx -> t * uctx =
      fun l ctx ->
        match l with
        | Nil -> (), ctx
        | Cons(hd,tl) ->
          let module Domain = (val hd) in
          let hda, ctx = Domain.init prog ctx in
          let tla, ctx = aux tl ctx in
          (hda,tla), ctx
    in
    aux Spec.pool ctx

  let ask : type r. r query -> uctx -> t -> r option =
    fun query ctx a ->
      let f = fun (type a) (m: a dmodule) acc aa ->
        let module Domain = (val m) in
        let rep = Domain.ask query ctx aa in
        Option.neutral2 (fun rep acc ->
            meet_query query rep acc
          ) rep acc
      in
      fold_apply { f } Spec.pool None a


  let refine channel a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.refine channel aa
    in
    apply_with_channel { f } Spec.pool a

  let rec refine_lfp channel a =
    let ret, channels = refine channel a |> Channel.destruct in
    List.fold_left (fun acc ch -> refine_lfp ch acc) ret channels


  let reduction_man : Spec.t man = {
    get = (fun id a ->
        Exceptions.panic ~loc:__LOC__ "get not implemented"
      );

    set = (fun id v a ->
        Exceptions.panic ~loc:__LOC__ "set not implemented"
      );

    get_value = (
      let doit : type v. v value -> var -> t -> v =
        fun (type v) (id:v value)  var a ->
        let open Value.Nonrel in

        let f : type a. a dmodule -> a -> v option =
          fun m aa ->
            let module Domain = (val m) in

            match Domain.id with
            | D_nonrel_id vmodule ->
              let module Value = (val vmodule) in
              let v = VarMap.find var aa ~bottomv:Value.bottom ~topv:Value.top in

              let man : (Value.t,Value.t) Sig.Value.Lowlevel.man = {
                get = (fun v -> v);
                set = (fun v _ -> v);
                eval = (fun e -> assert false);
                cast = (fun id v -> assert false);
              }
              in

              Value.get man id v

            | _ -> None
        in

        match map_opt { f } Spec.pool a with
        | Some r -> r
        | None -> Exceptions.panic "value of %a not found" pp_var var

      in
      doit
    );

    set_value = (
        let doit : type v. v value -> var -> v -> t -> t =
          fun (type v) (id:v value) var v a ->
            let open Value.Nonrel in

            let f : type a. a dmodule -> a -> a option =
              fun m aa ->
                let module Domain = (val m) in

                match Domain.id with
                | D_nonrel_id vmodule ->
                  let module Value = (val vmodule) in
                  let x = VarMap.find var aa ~bottomv:Value.bottom ~topv:Value.top in

                  let man : (Value.t,Value.t) Sig.Value.Lowlevel.man = {
                    get = (fun v -> v);
                    set = (fun v _ -> v);
                    eval = (fun e -> assert false);
                    cast = (fun id v -> assert false);
                  }
                  in

                  begin match Value.set man id v x with
                    | Some x' -> Some (VarMap.add var x' aa ~is_bottomv:Value.is_bottom)
                    | None -> None
                  end

                | _ -> None
            in

            match apply_opt { f } Spec.pool a with
            | Some r -> r
            | None -> Exceptions.panic "value of %a not updated" pp_var var

        in
        doit
      );

    ask = (
      let doit : type r. r query -> uctx -> t -> r =
        fun query ctx a ->
          match ask query ctx a with
          | Some r -> r
          | None -> Exceptions.panic "query not handled"
      in
      doit
    );

    refine = refine_lfp;
  }

  let reduce stmt ctx pre post =
    List.fold_left (fun acc rule ->
        let module R = (val rule : REDUCTION) in
        R.reduce stmt reduction_man ctx pre acc
      ) post Spec.rules

  let exec stmt ctx a =
    let rec aux : type t. t dlist -> uctx -> t -> (t*uctx) option =
      fun l ctx a ->
        match l, a with
        | Nil, () -> None
        | Cons(hd,tl), (hda,tla) ->
          let module Domain = (val hd) in
          match Domain.exec stmt ctx hda with
          | None ->
            begin match aux tl ctx tla with
              | None -> None
              | Some (tla',ctx) -> Some ((hda,tla'),ctx)
            end

          | Some (hda',ctx') ->
            match aux tl ctx' tla with
            | None -> Some ((hda',tla),ctx')
            | Some (tla',ctx'') -> Some ((hda',tla'),ctx'')
    in
    aux Spec.pool ctx a |>
    Option.lift @@ fun (aa,ctx) ->
    let a' = reduce stmt ctx a aa in
    (a',ctx)


end




(** Factory function *)

type dpool = D : 'a dlist -> dpool

let type_domain (type a) (d : (module DOMAIN with type t = a)) =
    let module D = (val d) in
    (module D : DOMAIN with type t = a)

let rec type_domain_pool : (module DOMAIN) list -> dpool = function
  | [] -> D Nil
  | hd :: tl ->
    let module D = (val hd) in
    let d = type_domain (module D) in
    let D tl = type_domain_pool tl in
    D (Cons (d, tl))

let make
    (domains: (module DOMAIN) list)
    (rules: (module REDUCTION) list)
  : (module DOMAIN) =

  let D pool = type_domain_pool domains in

  let create_product (type a) (pool: a dlist) =
    let module D = Make(
      struct
        type t = a
        let pool = pool
        let rules = rules
      end)
    in
    (module D : DOMAIN)
  in

  create_product pool
