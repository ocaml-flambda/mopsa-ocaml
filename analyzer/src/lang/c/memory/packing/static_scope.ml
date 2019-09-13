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

(** Simple packing strategy based on static scoping of variables.

    The idea is simple: global variables are kept in one pack and
    local variables of each function are kept in separate packs.

    The packs may overlap to preserve some relations between local and
    global variables. Two pivot variables are considered:
      - return variables of functions,
      - formal parameters of functions.
*)

open Mopsa
open Sig.Domain.Simplified
open Universal.Numeric.Packing.Strategy
open Universal.Ast
open Ast
open Common.Base

module Strategy(Domain:DOMAIN) =
struct

  (** Name of the packing strategy *)
  let name = "static_c_scope"

  let debug fmt = Debug.debug ~channel:"packing.static_c_scope" fmt

  (** Packing key *)
  type key =
    | Globals (** Pack of global variables *)
    | Locals of c_fundec (** Pack of local variables of a function *)


  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1.c_func_unique_name f2.c_func_unique_name
    | Globals, Locals _ -> 1
    | Locals _, Globals -> -1


  (** Pretty printer of packing keys *)
  let print fmt = function
    | Globals -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f.c_func_org_name


  (** Packing map binding keys to abstract elements *)
  module Map = MapExt.Make(struct type t = key let compare = compare end)


  (** Set of packing keys *)
  module Set = SetExt.Make(struct type t = key let compare = compare end)


  (** Packs of a base *)
  let packs_of_base b =
    match b with
    | V { vkind = V_cvar {cvar_scope = Variable_global} } -> Set.singleton Globals
    | V { vkind = V_cvar {cvar_scope = Variable_local f} } -> Set.singleton (Locals f)
    | V { vkind = V_cvar {cvar_scope = Variable_parameter f} } -> Set.singleton (Locals f)
    | _ -> Set.empty


  (** Packing function returning packs of a variable *)
  let rec packs_of_expr e =
    match e.ekind with
    | E_var ({ vkind = V_cvar _ } as v, _) -> packs_of_base (V v)
    | E_var ({ vkind = Lowlevel.Cells.Domain.V_c_cell {base}},_) -> packs_of_base base
    | E_var ({ vkind = Lowlevel.String_length.Domain.V_c_string_length (base,_)},_) -> packs_of_base base
    | E_var ({ vkind = Scalars.Pointers.Domain.Domain.V_c_ptr_offset vv},_) -> packs_of_base (V vv)
    | E_var _ ->
      begin match e.eprev with
        | None -> Set.empty
        | Some ee -> packs_of_expr ee
      end
    | E_unop(_, ee) -> packs_of_expr ee
    | E_binop(_, e1, e2) -> Set.union (packs_of_expr e1) (packs_of_expr e2)
    | _ -> Set.empty


  (** Check if an expression belongs to a pack *)
  let mem_pack e pack =
    let packs = packs_of_expr e in
    Set.mem pack packs


  (** Rewrite an expression by replacing variables not belonging to a pack by their over-approximation *)
  let resolve_missing_variables pack e =
    Visitor.fold_map_expr
      (fun found ee ->
         match ekind ee with
         | E_var _ when mem_pack ee pack ->
           Visitor.Keep (true, e)
         | E_var _ ->
           (* FIXME: use intervals instead of top *)
           Visitor.Keep (found, mk_top ee.etyp ee.erange)
         | _ ->
           Visitor.VisitParts (found, ee)
      )
      (fun found s -> Visitor.VisitParts (found, s))
      false e


  (** Inclusion test *)
  let subset m1 m2 =
    Map.for_all2zo
      (fun _ a1 -> false)
      (fun _ a2 -> true)
      (fun _ a1 a2 -> Domain.subset a1 a2)
      m1 m2


  (** Abstract union *)
  let join m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.join a1 a2)
      m1 m2


  (** Abstract intersection *)
  let meet m1 m2 =
    Map.merge (fun _ a1 a2 ->
        match a1, a2 with
        | None, _ | _, None -> None
        | Some aa1, Some aa2 -> Some (Domain.meet aa1 aa2)
      ) m1 m2


  (** Widening operator *)
  let widen ctx m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.widen ctx a1 a2)
      m1 m2


  (** Merging of divergent states *)
  let merge pre (m1,log1) (m2,log2) = join m1 m2


  (** Initial state *)
  let init prog = Map.singleton Globals (Domain.init prog)


  (** Transfer function of assignments *)
  let exec_assign v e range m =
    (* Find packs of the lval *)
    let packs = packs_of_expr v in
    (* Initialize new packs *)
    let m = Set.fold (fun pack acc ->
        if Map.mem pack acc then acc else Map.add pack Domain.top acc
      ) packs m
    in
    (* Execute pointwise *)
    Map.mapi (fun pack a ->
        if Set.mem pack packs then
          let _,e' = resolve_missing_variables pack e in
          let stmt' = mk_assign v e' range in
          Domain.exec stmt' a |> Option.none_to_exn
        else
          a
      ) m

  (** Transfer function of tests *)
  let exec_assume cond range m =
    Map.mapi (fun pack a ->
        let found, cond' = resolve_missing_variables pack cond in
        if found then
          let stmt' = mk_assume cond' range in
          Domain.exec stmt' a |> Option.none_to_exn
        else
          a
      ) m

  (** Transfer functions entry point *)
  let exec stmt m =
    match skind stmt with
    | S_assign({ekind = E_var _} as v, e) ->
      exec_assign v e stmt.srange m |>
      Option.return

    | S_assume(cond) ->
      exec_assume cond stmt.srange m |>
      Option.return

    | _ ->
      try
        let m' = Map.map (fun a ->
            Domain.exec stmt a |> Option.none_to_exn
          ) m
        in
        Some m'
      with Option.Found_None -> None

end

(** Registration *)
let () =
  Universal.Numeric.Packing.Functor.register_strategy "static_c_scope" (module Strategy)
