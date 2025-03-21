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
open Sig.Abstraction.Simplified_functor
open Sig.Abstraction.Simplified
open Ast
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

  val print : printer -> pack -> unit
  (** Pretty printer of packs *)

  val init : program -> unit
  (** Initialization of the strategy *)

  val packs_of_var : 'a ctx -> var -> pack list
  (** Return the packs containing a dimension (a variable) *)

end


(** Identifier of packed domains *)
type _ id += D_static_packing : 'k id * 'a id -> ('k,'a) Framework.Lattices.Partial_map.map id


let () =
  let open Eq in
  register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a, b) eq option =
        fun next id1 id2 ->
          match id1, id2 with
          | D_static_packing(s1,d1), D_static_packing(s2,d2) ->
            begin match equal_id s1 s2 with
              | Some Eq ->
                begin match equal_id d1 d2 with
                  | Some Eq -> Some Eq
                  | _ -> None
                end
              | _ -> None
            end
          | _ -> next.eq id1 id2
      in
      f
    );
  }


(** Creation of a domain functor from a packing strategy *)
module Make(Strategy:STRATEGY) : SIMPLIFIED_FUNCTOR =
struct

  (** {2 Header of the functor} *)
  (** ************************* *)

  let name = Strategy.name

  module Functor(Domain:SIMPLIFIED) =
  struct

    (** Partial map from packs to abstract elements *)
    module Map = Framework.Lattices.Partial_map.Make
        (struct
          type t = Strategy.pack
          let compare = Strategy.compare
          let print = Strategy.print
        end)
        (struct
          include Domain
          let print = print_state
          let widen x y = assert false
        end)

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


    (** The name of the domain is the name of the strategy *)
    let name = Strategy.name

    (** Identifier of the functor *)
    let id = D_static_packing (Strategy.id, Domain.id)


    (** {2 Lattice operators} *)
    (** ********************* *)

    let bottom = Map.bottom

    let top = Map.top

    let is_bottom = Map.is_bottom

    let subset = Map.subset

    let join = Map.join

    let meet = Map.meet

    let widen ctx a1 a2 =
      Map.map2zo
        (fun _ aa1 -> aa1)
        (fun _ aa2 -> aa2)
        (fun _ aa1 aa2 -> Domain.widen ctx aa1 aa2)
        a1 a2

    let merge pre (a,e1) (b,e2) =
      Map.map2zo
        (fun _ a1 -> a1)
        (fun _ a2 -> a2)
        (fun pack a1 a2 ->
           let prea = try Map.find pack pre with Not_found -> Domain.top in
           Domain.merge prea (a1,e1) (a2,e2)
        ) a b


    (** {2 Transfer functions} *)
    (** ********************** *)

    (** Initialization *)
    let init prog =
      let () = Strategy.init prog in
      Map.empty


    (** Get the packs of a variable *)
    let packs_of_var ctx v =
      let r = 
      try Cache.find cache v
      with Not_found ->
        let packs = Strategy.packs_of_var ctx v in
        let packs' = List.sort_uniq Strategy.compare packs in
        let () = Cache.replace cache v packs' in
        packs' in
      let () = Debug.debug ~channel:name "packs_of_var %a = %a" pp_var v (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (format Strategy.print)) r in r

    (** Get the packs of the variables present in an expression *)
    let rec packs_of_expr ctx e =
      Visitor.fold_expr
        (fun acc ee ->
           match ekind ee with
           | E_var (v,_) ->
             let packs = packs_of_var ctx v |> Set.of_list in
             let packs' =
               if Set.is_empty packs then
                 List.fold_left
                   (fun acc eee ->
                      Set.union acc (packs_of_expr ctx eee)
                   ) Set.empty ee.ehistory
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
    let resolve_expr_missing_vars pack man ctx e =
      Visitor.map_expr
        (fun ee ->
           match ekind ee with
           | E_var (v,_) ->
             let packs = packs_of_var ctx v in
             if List.exists (fun p -> Strategy.compare p pack = 0) packs then
               Visitor.Keep ee
             else
               begin match ee.etyp with
                 | T_int | T_bool ->
                   let itv = man.ask (Numeric.Common.mk_int_interval_query ee) in
                   if Numeric.Values.Intervals.Integer.Value.is_bounded itv then
                     let l,u = Numeric.Values.Intervals.Integer.Value.bounds itv in
                     Visitor.Keep (mk_z_interval l u ee.erange)
                   else
                     Visitor.Keep (mk_top ee.etyp ee.erange)

                 | T_float prec ->
                   let itv = man.ask (Numeric.Common.mk_float_interval_query ee) in
                   if ItvUtils.FloatItvNan.is_finite itv then
                     match itv.itv with
                     | Bot.Nb f ->
                       Visitor.Keep (mk_float_interval ~prec f.lo f.up ee.erange)
                     | _ ->
                       Visitor.Keep (mk_top ee.etyp ee.erange)
                   else
                     Visitor.Keep (mk_top ee.etyp ee.erange)

                 | _ ->
                   Visitor.Keep (mk_top ee.etyp ee.erange)
               end
           | _ ->
             Visitor.VisitParts ee
        )
        (fun s -> Visitor.VisitParts s)
        e


    (** Rewrite a statement w.r.t. to a pack by replacing missing variables with their intervals *)
    let resolve_stmt_missing_vars pack man ctx s =
      Visitor.map_stmt
        (fun ee -> Visitor.Keep (resolve_expr_missing_vars pack man ctx ee))
        (fun ss -> Visitor.VisitParts ss)
        s

    (** Get the manager of a pack *)
    let pack_man pack man : ('a,Domain.t) simplified_man = {
      man with
      exec = (fun stmt -> try man.exec stmt |> Map.find pack with Not_found -> Domain.top);
    }


    (** 𝕊⟦ add v ⟧ *)
    let exec_add_var stmt man ctx a =
      let v = match skind stmt with
        | S_add { ekind = E_var (v,_) } -> v
        | _ -> assert false
      in
      let () = Cache.remove cache v in 
      let packs = packs_of_var ctx v in
      let () = Cache.replace cache v packs in
      List.fold_left (fun acc pack ->
          let aa = try Map.find pack acc with Not_found -> Domain.top in
          let aa' = Domain.exec stmt (pack_man pack man) ctx aa |> OptionExt.none_to_exn in
          Map.add pack aa' acc
        ) a packs


    (** 𝕊⟦ v = e; ⟧ *)
    let exec_assign_var stmt man ctx a =
      let v,lval,e = match skind stmt with
        | S_assign ({ ekind = E_var (v,_) } as lval, e ) -> v, lval, e
        | _ -> assert false
      in
      let packs = packs_of_var ctx v in
      try
        List.fold_left (fun acc pack ->
            let aa = try Map.find pack acc with Not_found -> Domain.top in
            let e' = resolve_expr_missing_vars pack man ctx e in
            let stmt' = { stmt with skind = S_assign (lval, e') } in
            let aa' = Domain.exec stmt' (pack_man pack man) ctx aa |> OptionExt.none_to_exn in
            Map.add pack aa' acc
          ) a packs
        |> OptionExt.return
      with OptionExt.Found_None -> None


    (** 𝕊⟦ expand/fold (v,vl) ⟧ *)
    let exec_expand_fold_var stmt man ctx a =
      let v,vl = match skind stmt with
        | S_expand ({ ekind = E_var (v,_) }, el) -> v, List.map (function { ekind = E_var(v,_) } -> v | _ -> assert false) el
        | S_fold ({ ekind = E_var (v,_) }, el) -> v, List.map (function { ekind = E_var(v,_) } -> v | _ -> assert false) el
        | _ -> assert false
      in
      let packs = packs_of_var ctx v in
      let () = List.iter (Cache.remove cache) (v::vl) in (*Cache.replace cache v packs in*)
      List.fold_left (fun acc pack ->
          let aa = try Map.find pack acc with Not_found -> Domain.top in
          let aa' = Domain.exec stmt (pack_man pack man) ctx aa |> OptionExt.none_to_exn in
          Map.add pack aa' acc
        ) a packs


    (** 𝕊⟦ rename (v1,v2) ⟧ *)
    let exec_rename_var stmt man ctx a =
      let v1,v2 = match skind stmt with
        | S_rename ({ ekind = E_var (v1,_) }, { ekind = E_var (v2,_) }) -> v1,v2
        | _ -> assert false
      in
      let packs = packs_of_var ctx v1 in
      (* let () = Cache.replace cache v2 packs in *)
      let () = Cache.remove cache v2 in
      let () = Cache.remove cache v1 in 
      List.fold_left (fun acc pack ->
          let aa = try Map.find pack acc with Not_found -> Domain.top in
          let aa' = Domain.exec stmt (pack_man pack man) ctx aa |> OptionExt.none_to_exn in
          Map.add pack aa' acc
        ) a packs

    (* let is_var_numeric_type v = is_numeric_type (vtyp v) *)

    (* 𝕊⟦  ⟧ *)
    let exec stmt man ctx a =
      match skind stmt with
      | S_add {ekind = E_var (v, _)} ->
        (
          try
            exec_add_var stmt man ctx a |>
            OptionExt.return
          with OptionExt.Found_None -> None
        )

      | S_assign ({ekind = E_var (v, _)}, _) ->
        (
          try
            exec_assign_var stmt man ctx a
          with OptionExt.Found_None -> None
        )

      | S_expand( {ekind = E_var (v, _)}, _)
      | S_fold( {ekind = E_var (v, _)}, _) ->
        (
          try
            exec_expand_fold_var stmt man ctx a |>
            OptionExt.return
          with OptionExt.Found_None -> None
        )

      | S_rename( {ekind = E_var (v1, _)}, {ekind = E_var (v2, _)}) ->
        (
          try
            exec_rename_var stmt man ctx a |>
            OptionExt.return
          with OptionExt.Found_None -> None
        )

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
            (* let a' = Map.map (fun aa ->
             *     Domain.exec stmt man ctx aa |>
             *     OptionExt.none_to_exn
             *   ) a
             * in
             * Some a' *)
            None
          else
            (* Statement contains variables, so see which packs are concerned *)
            let packs = packs_of_stmt ctx stmt in
            let a' = Set.fold (fun pack acc ->
                let aa = try Map.find pack acc with Not_found -> Domain.top in
                let stmt' = resolve_stmt_missing_vars pack man ctx stmt in
                let aa' = Domain.exec stmt' (pack_man pack man) ctx aa |> OptionExt.none_to_exn in
                Map.add pack aa' acc
              ) packs a
            in
            Some a'
        with OptionExt.Found_None -> None


    (** Handler of queries *)
    let ask q man ctx a =
      match a with
      | BOT | TOP -> None
      | Nbt m ->
        let rep = PolyMap.mapi (fun pack aa -> Domain.ask q (pack_man pack man) ctx aa) m |>
                  PolyMap.bindings |>
                  List.map snd
        in
        let rec loop = function
          | [] -> None
          | r :: tl ->
            OptionExt.neutral2
              (meet_query q)
              r
              (loop tl)
        in
        loop rep

    let lift_pack_printer pack pp printer x =
      let pobj = pbox pp x in
      match pobj with
      | Empty -> ()
      | Map(map,sym) ->
        let bindings = MapExtPoly.fold (fun k v acc ->
            match v with
            | Empty ->
              acc
            | Map (m,_) when MapExtPoly.is_empty m ->
              acc
            | Set (s,_) when SetExtPoly.is_empty s ->
              acc
            | _ ->
              let k' =  fkey "%a(%a)" pp_print_object k (format Strategy.print) pack in
              (k',v) :: acc
          ) map []
        in
        List.iter
          (fun (k,v) ->
             pprint printer ~path:[k] v
          ) bindings
      | _ ->
        let key = fkey "pack(%a)" (format Strategy.print) pack in
        pprint printer ~path:[key]  pobj

    (** State pretty printer *)
    let print_state printer a =
      match a with
      | TOP -> Domain.print_state printer Domain.top
      | BOT -> Domain.print_state printer Domain.bottom
      | _ ->
        Map.bindings a |>
        List.iter
          (fun (pack,aa) ->
             lift_pack_printer pack Domain.print_state printer aa
          )

    (** Pretty printer *)
    let print_expr man ctx a printer exp =
      let packs = packs_of_expr ctx exp in
      Set.iter
        (fun pack ->
           match Map.find_opt pack a with
           | None -> ()
           | Some aa ->
             lift_pack_printer pack (Domain.print_expr (pack_man pack man) ctx aa) printer exp
        ) packs

  end
end

let register_strategy s =
  let module S = (val s : STRATEGY) in
  let module F = Make(S) in
  register_simplified_functor (module F)
