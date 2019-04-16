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
open Log
open Sig.Domain.Simplified
open Sig.Domain.Reduction



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
    dlist_fold { f } Spec.pool []



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.bottom
    in
    dlist_create { f } Spec.pool


  let top : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.top
    in
    dlist_create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a dmodule) fmt aa ->
      let module Domain = (val m) in
      Domain.print fmt aa
    in
    dlist_print { f } Spec.pool "" fmt a


  let is_bottom a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.is_bottom aa
    in
    dlist_exists { f } Spec.pool a

  let subset a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.subset aa1 aa2
    in
    dlist_forall2 { f } Spec.pool a1 a2

  let join a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.join aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let meet a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.meet aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let widen ctx a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.widen ctx aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let merge pre (post1,log1) (post2,log2) =
    let f = fun (type a) (m: a dmodule) pre post1 post2 ->
      let module Domain = (val m) in
      Domain.merge pre (post1,log1) (post2,log2)
    in
    dlist_apply3 { f } Spec.pool pre post1 post2


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog =
    let f = fun (type a) (m: a dmodule) ->
      let module Domain = (val m) in
      Domain.init prog
    in
    dlist_create { f } Spec.pool


  let ask : type r. r query -> t -> r option =
    fun query a ->
      let f = fun (type a) (m: a dmodule) acc aa ->
        let module Domain = (val m) in
        let rep = Domain.ask query aa in
        Option.neutral2 (fun rep acc ->
            Query.meet query rep acc
          ) rep acc
      in
      dlist_fold_apply { f } Spec.pool None a


  let refine channel a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.refine channel aa
    in
    dlist_apply_with_channel { f } Spec.pool a

  let rec refine_lfp channel a =
    let ret, channels = refine channel a |> Channel.destruct in
    List.fold_left (fun acc ch -> refine_lfp ch acc) ret channels


  let reduction_man : Spec.t man = {
    get = (fun id a ->
        assert false
      );

    set = (fun id v a ->
        assert false
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

        match dlist_map_opt { f } Spec.pool a with
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

            match dlist_apply_opt { f } Spec.pool a with
            | Some r -> r
            | None -> Exceptions.panic "value of %a not updated" pp_var var

        in
        doit
      );

    ask = (
      let doit : type r. r query -> t -> r =
        fun query a ->
          match ask query a with
          | Some r -> r
          | None -> Exceptions.panic "query not handled"
      in
      doit
    );

    refine = refine_lfp;
  }

  let reduce stmt a =
    List.fold_left (fun acc rule ->
        let module R = (val rule : REDUCTION) in
        R.reduce stmt reduction_man acc
      ) a Spec.rules

  let exec stmt a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.exec stmt aa
    in
    dlist_apply_opt { f } Spec.pool a |>
    Option.lift (reduce stmt)




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
