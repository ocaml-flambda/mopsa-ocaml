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

(** Static packing functor - lifting an abstract domain to a map of buckets
    with fewer dimensions using a static strategy.
*)

open Mopsa
open Sig.Functor.Simplified
open Ast
open Zone
open Format
open Context


(** Static packing strategy *)
module type STRATEGY =
sig

  val name : string
  (** Name of the packing strategy *)

  type pack
  (** Packs are finite abstraction of the dimensions of a the underlying
      domain.
  *)

  val compare : pack -> pack -> int
  (** Total order of packs *)

  val print : Format.formatter -> pack -> unit
  (** Pretty printer of packs *)

  val init : program -> unit
  (** Initialization of the strategy *)

  val packs_of_var : uctx -> var -> pack list
  (** Return the packs containing a dimension (a variable) *)

end


(** Creation of a domain functor from a packing strategy *)
module Make(Strategy:STRATEGY) : FUNCTOR = functor(Domain:Sig.Domain.Simplified.DOMAIN) ->
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
  include GenDomainId(struct
      type nonrec t = t
      let name = Strategy.name
    end)


  (** Semantic zone of the functor, inherited from the domain *)
  let zones = Domain.zones


  (** Pretty printer *)
  let print fmt a =
    fprintf fmt "packs: %a@\n" Map.print a

  let bottom = Map.bottom

  let top = Map.top

  let is_bottom = Map.is_bottom

  let subset = Map.subset

  let join = Map.join

  let meet = Map.meet

  let widen = Map.widen

  let merge pre (a,log1) (b,log2) =
    match a, b with
    | Map.PolyMap.Top, _ | _, Map.PolyMap.Top -> top
    | Map.PolyMap.Bot, x | x, Map.PolyMap.Bot -> x
    | Map.PolyMap.Finite m1, Map.PolyMap.Finite m2 ->
      Map.PolyMap.Finite (
        Map.PolyMap.Map.map2zo
          (fun _ a1 -> a1)
          (fun _ a2 -> a2)
          (fun pack a1 a2 ->
             let prea = try Map.find pack pre with Not_found -> Domain.top in
             Domain.merge prea (a1,log1) (a2,log2)
          ) m1 m2
      )


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog =
    let () = Strategy.init prog in
    Map.empty

  let packs_of_var ctx v =
    try Cache.find cache v
    with Not_found ->
      let packs = Strategy.packs_of_var ctx v in
      let () = Cache.add cache v packs in
      packs

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


  let resolve_missing_vars ctx pack e =
    Visitor.map_expr
      (fun ee ->
         match ekind ee with
         | E_var (v,_) ->
           let packs = packs_of_var ctx v in
           if List.exists (fun p -> Strategy.compare p pack = 0) packs then
             Visitor.Keep ee
           else
             (* FIXME: use intervals instead of top *)
             Visitor.Keep (mk_top ee.etyp ee.erange)
         | _ ->
           Visitor.VisitParts ee
      )
      (fun s -> Visitor.VisitParts s)
      e



  let exec_add_var ctx stmt a =
    let v = match skind stmt with
      | S_add { ekind = E_var (v,_) } -> v
      | _ -> assert false
    in
    let packs = packs_of_var ctx v in
    let () = Cache.add cache v packs in
    List.fold_left (fun acc pack ->
        let aa = try Map.find pack acc with Not_found -> Domain.top in
        debug "exec %a in %a" pp_stmt stmt Strategy.print pack;
        let aa' = Domain.exec ctx stmt aa |> Option.none_to_exn in
        Map.add pack aa' acc
      ) a packs


  let exec_assign_var ctx stmt a =
    let v,lval,e = match skind stmt with
      | S_assign ({ ekind = E_var (v,_) } as lval, e ) -> v, lval, e
      | _ -> assert false
    in
    let packs = packs_of_var ctx v in
    List.fold_left (fun acc pack ->
        let aa = try Map.find pack acc with Not_found -> Domain.top in
        let e' = resolve_missing_vars ctx pack e in
        let stmt' = { stmt with skind = S_assign (lval, e') } in
        debug "exec %a in %a" pp_stmt stmt Strategy.print pack;
        let aa' = Domain.exec ctx stmt' aa |> Option.none_to_exn in
        Map.add pack aa' acc
      ) a packs



  let exec ctx stmt a =
    match skind stmt with
    | S_add {ekind = E_var _} ->
      exec_add_var ctx stmt a |>
      Option.return

    | S_assign ({ekind = E_var _}, _) ->
      exec_assign_var ctx stmt a |>
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
              Domain.exec ctx stmt aa |>
              Option.none_to_exn
            ) a
          in
          Some a'
        else
          (* Statement contains variables, so see which packs are concerned *)
          let packs = packs_of_stmt ctx stmt in
          let a' = Map.mapi (fun pack aa ->
              if Set.mem pack packs then begin
                debug "exec %a in %a" pp_stmt stmt Strategy.print pack;
                Domain.exec ctx stmt aa |>
                Option.none_to_exn
              end
              else
                aa
            ) a
          in
          Some a'
      with Option.Found_None -> None


  let ask q a =
    match a with
    | Map.PolyMap.Bot | Map.PolyMap.Top -> None
    | Map.PolyMap.Finite m ->
      let rep = Map.PolyMap.Map.map (Domain.ask q) m |>
                Map.PolyMap.Map.bindings |>
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
