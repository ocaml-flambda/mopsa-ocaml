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

(** Packing functor with static strategy.

    This functor lifts an abstract domain to a set of packs with fewer
    dimensions. Users of the functor need to define a static strategy that
    gives the packs of a given variable.

    The packs are represented as a map from packing keys to abstract
    elements. Lattice operators are defined pointwise. To compute the
    post-state of a statement, the functor inspects the statement to extract
    the affected variables, and propagate the (modified) statement to the
    appropriate pack.
*)

open Mopsa
open Sig.Functor.Simplified
open Sig.Domain.Simplified
open Ast
open Zone
open Format
open Context
open Bot_top


(** Signature of a static packing strategy *)
module type STRATEGY =
sig

  val name : string
  (** Name of the packing strategy *)

  type pack
  (** Packs are finite abstraction of the dimensions of a the underlying domain. *)

  val id : pack id
  (** Unique identifier of the packing strategy *)

  val compare : pack -> pack -> int
  (** Total order of packs *)

  val print : Format.formatter -> pack -> unit
  (** Pretty printer of packs *)

  val init : program -> unit
  (** Initialization of the strategy *)

  val packs_of_var : uctx -> var -> pack list
  (** Return the packs containing a dimension (a variable) *)

end


(** Identifier of packed domains *)
type _ id += D_static_packing : 'k id * 'a id -> ('k,'a) Framework.Lattices.Partial_map.map id


(** Creation of a domain functor from a packing strategy *)
module Make(Strategy:STRATEGY) : FUNCTOR = functor(Domain:DOMAIN) ->
struct


  (** {2 Header of the functor} *)
  (** ************************* *)

  (** Partial map from packs to abstract elements *)
  module Map = Framework.Lattices.Partial_map.Make
      (struct
        type t = Strategy.pack
        let compare = Strategy.compare
        let print = Strategy.print
      end)
      (Domain)

  module PolyMap = MapExtPoly


  (** Set of packs *)
  module Set = SetExt.Make(struct type t = Strategy.pack let compare = Strategy.compare end)


  (** Packs of each variable can be kept in a cache, since the strategy is static *)
  module Cache = Hashtbl.Make(struct
      type t = var
      let equal v1 v2 = v1.vname = v2.vname
      let hash v = Hashtbl.hash v.vname
    end)

  let cache : Strategy.pack list Cache.t = Cache.create 16


  (** Abstract element *)
  type t = Map.t


  (** Id of the packing functor *)
  let id = D_static_packing (Strategy.id, Domain.id)
  let () =
    let open Eq in
    register_id {
      eq = (
        let f : type a. a id -> (a, t) eq option =
          function
          | D_static_packing(strategy,domain) ->
            begin match equal_id strategy Strategy.id, equal_id domain Domain.id with
              | Some Eq, Some Eq -> Some Eq
              | _ -> None
            end
          | _ -> None
        in
        f
      );
    }

  (** The name of the domain is the name of the strategy *)
  let name = Strategy.name

  (** Semantic zone of the functor, inherited from the domain *)
  let zones = Domain.zones


  (** Pretty printer *)
  let print fmt a =
    match a with
    | TOP -> Domain.print fmt Domain.top
    | BOT -> Domain.print fmt Domain.bottom
    | _ ->
      fprintf fmt "@[<v>%a@]"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "")
           (fun fmt (pack,aa) ->
              fprintf fmt "@[{%a}::%a@]"
                Strategy.print pack
                Domain.print aa
           )
        ) (Map.bindings a)



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom = Map.bottom

  let top = Map.top

  let is_bottom = Map.is_bottom

  let subset = Map.subset

  let join = Map.join

  let meet = Map.meet

  let widen = Map.widen

  let merge pre (a,log1) (b,log2) =
    match a, b with
    | TOP, _ | _, TOP -> top
    | BOT, x | x, BOT -> x
    | Nbt m1, Nbt m2 ->
      Nbt (
        PolyMap.map2zo
          (fun _ a1 -> a1)
          (fun _ a2 -> a2)
          (fun pack a1 a2 ->
             let prea = try Map.find pack pre with Not_found -> Domain.top in
             Domain.merge prea (a1,log1) (a2,log2)
          ) m1 m2
      )


  (** {2 Transfer functions} *)
  (** ********************** *)

  (** Initialization *)
  let init prog =
    let () = Strategy.init prog in
    Map.empty


  (** Get the packs of a variable *)
  let packs_of_var ctx v =
    try Cache.find cache v
    with Not_found ->
      let packs = Strategy.packs_of_var ctx v in
      let () = Cache.add cache v packs in
      packs

  (** Get the packs of the variables present in an expression *)
  let rec packs_of_expr ctx e =
    Visitor.fold_expr
      (fun acc ee ->
         match ekind ee with
         | E_var (v,_) ->
           let packs = packs_of_var ctx v |> Set.of_list
           in
           let packs' =
             if Set.is_empty packs then
               match ee.eprev with
               | None -> Set.empty
               | Some eee -> packs_of_expr ctx eee
           else
             packs
           in
           Keep (Set.union packs' acc)
         | _ -> VisitParts acc
      )
      (fun acc s -> VisitParts acc)
      Set.empty e


  (** Get the packs of the variables affected by a statement *)
  let packs_of_stmt ctx stmt =
    Visitor.fold_stmt
      (fun acc e ->
         let packs = packs_of_expr ctx e in
         Visitor.Keep (Set.union packs acc)
      )
      (fun acc stmt ->
         Visitor.VisitParts acc
      )
      Set.empty stmt


  (** Rewrite an expression w.r.t. to a pack by replacing missing variables with their intervals *)
  let resolve_expr_missing_vars ctx pack man e =
    Visitor.map_expr
      (fun ee ->
         match ekind ee with
         | E_var (v,_) ->
           let packs = packs_of_var ctx v in
           if List.exists (fun p -> Strategy.compare p pack = 0) packs then
             Visitor.Keep ee
           else
             let itv = man.ask (Numeric.Common.mk_int_interval_query ee) in
             if Numeric.Values.Intervals.Integer.Value.is_bounded itv then
               let l,u = Numeric.Values.Intervals.Integer.Value.bounds itv in
               Visitor.Keep (mk_z_interval l u ee.erange)
             else
               Visitor.Keep (mk_top ee.etyp ee.erange)
         | _ ->
           Visitor.VisitParts ee
      )
      (fun s -> Visitor.VisitParts s)
      e


  (** Rewrite a statement w.r.t. to a pack by replacing missing variables with their intervals *)
  let resolve_stmt_missing_vars ctx pack man s =
    Visitor.map_stmt
      (fun ee -> Visitor.Keep (resolve_expr_missing_vars ctx pack man ee))
      (fun ss -> Visitor.VisitParts ss)
      s



  (** 𝕊⟦ add v ⟧ *)
  let exec_add_var ctx stmt man a =
    let v = match skind stmt with
      | S_add { ekind = E_var (v,_) } -> v
      | _ -> assert false
    in
    let packs = packs_of_var ctx v in
    let () = Cache.add cache v packs in
    List.fold_left (fun acc pack ->
        let aa = try Map.find pack acc with Not_found -> Domain.top in
        let aa' = Domain.exec ctx stmt man aa |> Option.none_to_exn in
        Map.add pack aa' acc
      ) a packs


  (** 𝕊⟦ v = e; ⟧ *)
  let exec_assign_var ctx stmt man a =
    let v,lval,e = match skind stmt with
      | S_assign ({ ekind = E_var (v,_) } as lval, e ) -> v, lval, e
      | _ -> assert false
    in
    let packs = packs_of_var ctx v in
    List.fold_left (fun acc pack ->
        let aa = try Map.find pack acc with Not_found -> Domain.top in
        let e' = resolve_expr_missing_vars ctx pack man e in
        let stmt' = { stmt with skind = S_assign (lval, e') } in
        let aa' = Domain.exec ctx stmt' man aa |> Option.none_to_exn in
        Map.add pack aa' acc
      ) a packs



  (* 𝕊⟦  ⟧ *)
  let exec ctx stmt man a =
    match skind stmt with
    | S_add {ekind = E_var _} ->
      exec_add_var ctx stmt man a |>
      Option.return

    | S_assign ({ekind = E_var _}, _) ->
      exec_assign_var ctx stmt man a |>
      Option.return

    | _ ->
      let has_vars = Visitor.fold_stmt
          (fun acc e ->
             match ekind e with
             | E_var _ -> Keep true
             | _ -> if acc then Keep true else VisitParts acc
          )
          (fun acc s -> if acc then Keep true else VisitParts acc)
          false stmt
      in
      try
        if not has_vars then
          let a' = Map.map (fun aa ->
              Domain.exec ctx stmt man aa |>
              Option.none_to_exn
            ) a
          in
          Some a'
        else
          (* Statement contains variables, so see which packs are concerned *)
          let packs = packs_of_stmt ctx stmt in
          let a' = Set.fold (fun pack acc ->
              let aa = try Map.find pack acc with Not_found -> Domain.top in
              let stmt' = resolve_stmt_missing_vars ctx pack man stmt in
              let aa' = Domain.exec ctx stmt' man aa |> Option.none_to_exn in
              Map.add pack aa' acc
            ) packs a
          in
          Some a'
      with Option.Found_None -> None


  (** Handler of queries *)
  let ask q a =
    match a with
    | BOT | TOP -> None
    | Nbt m ->
      let rep = PolyMap.map (Domain.ask q) m |>
                PolyMap.bindings |>
                List.map snd
      in
      let rec loop = function
        | [] -> None
        | r :: tl ->
          Option.neutral2 (meet_query q) r (loop tl)
      in
      loop rep

  let refine _ _ = assert false

end

let register_strategy s =
  let module S = (val s : STRATEGY) in
  let module F = Make(S) in
  register_functor S.name (module F)
