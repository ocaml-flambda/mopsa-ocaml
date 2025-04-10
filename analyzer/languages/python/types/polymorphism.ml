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

(* Warning: this is a work in progress *)

open Mopsa
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute
open Alarms

module Poly =
struct

  module Partitions =
  struct
    open Bot_top

    module AKS = SetExt.Make(struct type t = addr_kind let compare = compare_addr_kind let pp = pp_addr_kind end)

    type t = (VarSet.t * AKS.t) list with_bot_top

    let is_bottom p = p = BOT

    let bottom = BOT
    let empty =  Nbt []
    let top = TOP

    let find_var v p =
      List.find (fun (vs, _) -> VarSet.mem v vs) p

    let meet =
      bot_top_lift2 (fun p1 p2 ->
          let domain = List.fold_left (fun acc (vars, _) -> VarSet.union acc vars) VarSet.empty p1 in
          let domain2 = List.fold_left (fun acc (vars, _) -> VarSet.union acc vars) VarSet.empty p2 in
          assert (VarSet.equal domain domain2);
          let vmap = VarSet.fold (fun var acc ->
              VarMap.add var (AKS.union (snd @@ find_var var p1) (snd @@ find_var var p2)) acc
            ) domain VarMap.empty in
          let module AKMap = MapExt.Make(AKS) in
          let amap = VarMap.fold (fun var aks acc ->
              match AKMap.find_opt aks acc with
              | None -> AKMap.add aks (VarSet.singleton var) acc
              | Some vs -> AKMap.add aks (VarSet.add var vs) acc
            ) vmap AKMap.empty in
          List.map (fun (x, y) -> (y, x)) (AKMap.bindings amap)
        )

    let widen _ _ _ = failwith "ni widen"
    let subset _ _ = failwith "ni subset"
    let print printer = function
      | Bot_top.BOT -> pp_string printer "⊥"
      | Bot_top.TOP -> pp_string printer "⊤"
      | Bot_top.Nbt p ->
        pp_list
          (fun printer (vs, aks) ->
             pp_obj_list printer
               [ pbox (pp_list (unformat pp_var) ~lopen:"{" ~lsep:"," ~lclose:"}") (VarSet.elements vs);
                 pbox (pp_list (unformat pp_addr_kind) ~lopen:"{" ~lsep:"," ~lclose:"}") (AKS.elements aks) ]
               ~lopen:"(" ~lsep:"~" ~lclose:")"
          ) printer p

    let join l r = panic "ni join"

    let add_var v aks p =
      bot_top_lift1 (fun p ->
          let tochange, others = List.partition (fun (_, aks') -> AKS.equal aks aks') p in
          let vs, _ = List.hd tochange in
          (VarSet.add v vs, aks) :: others) p

  end

  type t = Partitions.t

  let is_bottom = Partitions.is_bottom
  let bottom = Partitions.bottom
  let top = Partitions.top

  let join = Partitions.join
  let meet = Partitions.meet
  let widen = Partitions.widen
  let subset = Partitions.subset

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.polymorphism"
    end)

  let checks = []

  let merge _ _ _ = assert false

  let init prog man flow = set_env T_cur (Partitions.empty) man flow |> Option.some

  let exec stmt man flow =
    match skind stmt with
    | S_assign ({ekind = E_var (v, _)}, {ekind = E_py_undefined _}) -> None
    | S_assign ({ekind = E_var (v, _)}, _) -> OptionExt.return @@ Post.return flow
    | _ -> None

  let rec eval exp man flow = None

  let ask _ _ _ = None

  let print_state printer a =
    pprint ~path:[Key "partitions"] printer (pbox Partitions.print a)

  let print_expr _ _ _ _ = ()

end


let () = register_standard_domain (module Poly);


module Reduction =
struct

  let name = "python.types.reductions.polymorphism"

  let debug fmt = Debug.debug ~channel:name fmt

  let addrenv = Addr_env.Domain.id
  let polymorphism = Poly.id

  let akset_of_aset a =
    Addr_env.Domain.ASet.fold (fun pyaddr acc -> match pyaddr with
        | Def a ->  Poly.Partitions.AKS.add (akind a) acc
        | _ -> acc) a Poly.Partitions.AKS.empty

  let rho_up addrs poly : Poly.t =
    let module AMap = Addr_env.Domain.AMap in
    let module ASet = Addr_env.Domain.ASet in
    let new_eqs =
      AMap.fold (fun var aset acc ->
          if ASet.cardinal aset = 1 then
            Poly.Partitions.add_var var (akset_of_aset aset) acc
          else acc) addrs Poly.Partitions.empty
    in
    Poly.Partitions.meet poly new_eqs

  let rho_down addrs poly : Addr_env.Domain.t =
    let module AMap = Addr_env.Domain.AMap in
    let module ASet = Addr_env.Domain.ASet in
    match poly with
    | Bot_top.BOT | Bot_top.TOP -> panic "bot / top poly rho_down"
    | Nbt poly ->
      List.fold_left (fun acc (vars, aks) ->
          VarSet.fold (fun var acc ->
              AMap.add var (AMap.find var acc |>
                            ASet.filter (fun pya -> match pya with
                                | Def a -> Poly.Partitions.AKS.mem (akind a) aks
                                | _ -> true)) acc
            ) vars acc
        ) addrs poly


  let reduce stmt man rman pre post =
    assert false
    (* let man_addrs = man.get_man addrenv in
     * let man_poly = man.get_man polymorphism in
     *
     * let addrs_post = Sig.Stacked.Lowlevel.get_env T_cur man_addrs flow_post in
     * let poly_post = Sig.Stacked.Lowlevel.get_env T_cur man_poly flow_post in
     *
     * debug "rho_up";
     * let new_poly = rho_up addrs_post poly_post in
     * debug "rho_down";
     * let new_addrs = rho_down addrs_post poly_post in
     *
     * Sig.Stacked.Lowlevel.set_env T_cur new_addrs man_addrs flow_post |>
     * Sig.Stacked.Lowlevel.set_env T_cur new_poly man_poly |> Post.return *)

end

let () = Sig.Reduction.Exec.register_exec_reduction (module Reduction)
