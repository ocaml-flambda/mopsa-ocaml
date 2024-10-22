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

(** Partitioning combiner *)

open Core.All
open Mopsa_utils
open Sig.Combiner.Domain
open Common
open Sig.Abstraction.Partitioning
module M = MapExtPoly

module Make(P:PARTITIONING)(D:DOMAIN_COMBINER) : DOMAIN_COMBINER =
struct

  type t = 
    | Top
    | Map of (P.t, D.t) M.t

  include GenDomainId
      (struct
        type nonrec t = t
        let name = P.name ^ "(" ^ D.name ^ ")"
      end)

  let checks = P.checks @ D.checks |> List.sort_uniq compare
  let domains = DomainSet.add name D.domains
  let semantics = D.semantics
  let routing_table = add_routes (Below P.name) D.domains D.routing_table

  type accessor +=
    | Ax_partitioning_partition of P.t

  let () = register_accessor {
      print = (fun next fmt -> function
          | Ax_partitioning_partition p ->
            P.print fmt p

          | t -> next fmt t
        );
      compare = (fun next t1 t2 ->
          match t1, t2 with
          | Ax_partitioning_partition p1, Ax_partitioning_partition p2 ->
            P.compare p1 p2

          | _ -> compare t1 t2
        );
    }

  type expr_kind += E_partition_predicate of P.t

  let () = register_expr {
      print = (fun next fmt e ->
          match ekind e with
          | E_partition_predicate p ->
            Format.fprintf fmt "partition(%a)" P.print p
          | _ -> next fmt e
        );
      compare = (fun next e1 e2 ->
          match ekind e1, ekind e2 with
          | E_partition_predicate p1, E_partition_predicate p2 ->
            P.compare p1 p2
          | _ -> next e1 e2
        );
    }

  let bottom = Map (M.empty ~compare:P.compare)

  let top = Top

  let singleton p env =
    if D.is_bottom env then bottom
    else Map (M.singleton p env ~compare:P.compare)

  let of_list l =
    Map (M.of_list P.compare l)

  let is_bottom = function
    | Top -> false
    | Map map -> M.for_all (fun _ e -> D.is_bottom e) map

  let domain_man man =
    { man with
      get = (fun tk flow ->
        man.get tk flow >>$ fun m flow ->
          match m with
          | Top -> Cases.singleton D.top flow

          | Map map when M.is_singleton map ->
            let _, e = M.choose map in
            Cases.singleton e flow

          | Map map ->
            let cases =
              M.fold
                (fun p e acc ->
                   let case =
                     set_env tk (singleton p e) man flow >>% fun flow ->
                     Cases.singleton e flow
                   in
                   case :: acc
                ) map []
            in
            Cases.join_list cases ~empty:(fun () -> Cases.singleton D.bottom flow)
        );
      set = (fun tk e flow ->
          man.get tk flow >>$ fun a flow ->
          match a with
          | Top -> Post.return flow

          | Map map when M.is_empty map -> Post.return flow
          
          | Map map ->
            let map' = M.map (fun _ -> e) map in
            man.set tk (Map map') flow
        );

      add_effect = (fun stmt path flow effect_map ->
          match get_singleton_env_from_flow T_cur man flow with
          | Top     -> assert false
          | Map map ->
            M.fold
              (fun p _ acc ->
                 let path' = (Ax_partitioning_partition p) :: path in
                 man.add_effect stmt path' flow acc
              ) map effect_map
        );
    }

  let partition_man man =
    { man with
      get = (fun tk flow ->
          man.get tk flow >>$ fun m flow ->
          match m with
          | Top -> assert false

          | Map map when M.is_singleton map ->
            let p, _ = M.choose map in
            Cases.singleton p flow

          | Map map ->
            let cases =
              M.fold
                (fun p e acc ->
                   let case =
                     set_env tk (singleton p e) man flow >>% fun flow ->
                     Cases.singleton p flow
                   in
                   case :: acc
                ) map []
            in
            Cases.join_list cases ~empty:(fun () -> Cases.empty flow)
        );
      set = (fun tk p flow ->
          man.get tk flow >>$ fun a flow ->
          match a with
          | Top -> Post.return flow

          | Map map when M.is_empty map -> Post.return flow

          | Map map when M.is_singleton map ->
            let _, e = M.choose map in
            set_env tk (singleton p e) man flow

          | Map map ->
            let dman = domain_man man in
            let ctx = Flow.get_ctx flow in
            let e =
              M.fold
                (fun _ e acc -> D.join dman ctx (e, man.lattice.top) (acc, man.lattice.top))
                map D.bottom
            in
            man.set tk (singleton p e) flow
        );
    }

  let subset man ctx (a1, x1) (a2, x2) =
    if a1 == a2 then true
    else
      match a1, a2 with
      | _, Top -> true
      | Top, _ -> false
      | Map map1, Map map2 ->
        let dman = domain_man man in
        let singleton1 = M.is_singleton map1 in
        let singleton2 = M.is_singleton map2 in
        M.for_all2zo
          (fun p1 e1   -> false)
          (fun p2 e2   -> true)
          (fun p e1 e2 ->
             D.subset dman ctx
               (e1, if singleton1 then x1 else set_singleton_env (singleton p e1) ctx man x1)
               (e2, if singleton2 then x2 else set_singleton_env (singleton p e2) ctx man x2)
          )
          map1 map2

  let join man ctx (a1, x1) (a2, x2) =
    if a1 == a2 then a1
    else
      match a1, a2 with
      | Top, _ -> Top
      | _, Top -> Top
      | Map map1, Map map2 ->
        let dman = domain_man man in
        let singleton1 = M.is_singleton map1 in
        let singleton2 = M.is_singleton map2 in
        let map =
          M.map2zo
            (fun p1 e1 -> e1)
            (fun p2 e2 -> e2)
            (fun p e1 e2 ->
               D.join dman ctx
                 (e1, if singleton1 then x1 else set_singleton_env (singleton p e1) ctx man x1)
                 (e2, if singleton2 then x2 else set_singleton_env (singleton p e2) ctx man x2)
            ) map1 map2
        in
        Map map

  let meet man ctx (a1, x1) (a2, x2) =
    if a1 == a2 then a1
    else
      match a1, a2 with
      | Top, _ -> a2
      | _, Top -> a1
      | Map map1, Map map2 ->
        let dman = domain_man man in
        let singleton1 = M.is_singleton map1 in
        let singleton2 = M.is_singleton map2 in
        let map =
          M.fold2zo
            (fun p1 e1 acc -> M.remove p1 acc)
            (fun p2 e2 acc -> acc)
            (fun p e1 e2 acc ->
               let e =
                 D.meet dman ctx
                   (e1, if singleton1 then x1 else set_singleton_env (singleton p e1) ctx man x1)
                   (e2, if singleton2 then x2 else set_singleton_env (singleton p e2) ctx man x2)
               in
               if D.is_bottom e then
                 M.remove p acc
               else
                 M.add p e acc
            ) map1 map2 map1
        in
        Map map

  let widen man ctx (a1, x1) (a2, x2) =
    if a1 == a2 then a1
    else
      match a1, a2 with
      | Top, _ -> Top
      | _, Top -> Top
      | Map map1, Map map2 ->
        let dman = domain_man man in
        let singleton1 = M.is_singleton map1 in
        let singleton2 = M.is_singleton map2 in
        let map =
          M.map2zo
            (fun p1 e1 -> e1)
            (fun p2 e2 -> e2)
            (fun p e1 e2 ->
               D.widen dman ctx
                 (e1, if singleton1 then x1 else set_singleton_env (singleton p e1) ctx man x1)
                 (e2, if singleton2 then x2 else set_singleton_env (singleton p e2) ctx man x2)
            ) map1 map2
        in
        Map map

  let merge path pre (a1, te1) (a2, te2) =
    match a1, a2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Map m1, Map m2 ->
      let m =
        M.map2zo
          (fun p1 e1 -> e1)
          (fun p2 e2 -> e2)
          (fun p e1 e2 ->
             let dpre =
               match pre with
               | Top -> D.top
               | Map m ->
                 try M.find p m
                 with Not_found -> D.top
             in
             let path' = Ax_partitioning_partition p :: path in
             D.merge path' dpre (e1, te1) (e2, te2)
          ) m1 m2
      in Map m

  let init prog man flow =
    set_env T_cur (singleton P.init D.top) man flow >>%? fun flow ->
    match D.init prog (domain_man man) flow with
    | None ->
      Post.return flow |>
      Option.some

    | Some _ as ret -> ret

  exception None_found

  let lift targets pf df cmd man flow =
    let dman = domain_man man in
    let pman = partition_man man in
    man.get T_cur flow >>$? fun a flow ->
    match a with
    | Top ->
      df cmd dman flow

    | Map map when not (sat_targets ~targets ~domains:(DomainSet.singleton name)) ->
      df cmd dman flow

    | Map map ->
      match pf cmd pman flow with
      | None ->
        df cmd dman flow

      | Some _ as r ->
        r

  let exec_add_marker df m stmt man flow =
    get_env T_cur man flow >>$ fun a flow ->
    match a with
    | Top -> Post.return flow
    | Map map ->
      let dman = domain_man man in
      M.fold
        (fun p e acc ->
           let p' = P.add m p in
           let case =
             set_env T_cur (singleton p' e) man flow >>% fun flow' ->
             match df stmt dman flow' with
             | None     -> Post.return flow'
             | Some ret -> ret
           in
           case :: acc
        ) map [] |>
      Cases.join_list ~empty:(fun () -> Cases.empty flow)

  let eval_partition_predicate p range man flow =
    get_env T_cur man flow >>$ fun a flow ->
    match a with
    | Top ->
      Eval.singleton (mk_top T_bool range) flow

    | Map map ->
      match M.find_opt p map with
      | None ->
        let ret = mk_constant (C_bool false) range ~etyp:T_bool in
        Eval.singleton ret flow

      | Some e ->
        let case_true =
          set_env T_cur (singleton p e) man flow >>% fun flow ->
          let ret = mk_constant (C_bool true) range ~etyp:T_bool in
          Eval.singleton ret flow
        in
        let map' = M.remove p map in
        if M.is_empty map' then
          case_true
        else
          let case_false =
            set_env T_cur (Map map') man flow >>% fun flow ->
            let ret = mk_constant (C_bool false) range ~etyp:T_bool in
            Eval.singleton ret flow
          in
          Cases.join case_true case_false

  let ask_partition_predicate range pman flow =
    get_env T_cur pman flow >>$ fun p flow ->
    let e = mk_expr (E_partition_predicate p) ~etyp:T_bool range in
    Cases.singleton e flow

  let exec targets =
    let df = D.exec targets in
    if sat_targets ~targets ~domains:(DomainSet.singleton name) then
      (fun stmt man flow ->
         match skind stmt with
         | S_add_marker m ->
           exec_add_marker df m stmt man flow |>
           Option.some
         | _ ->
           lift targets P.exec df stmt man flow
      )
    else
      lift targets P.exec df

  let eval targets =
    let df = D.eval targets in
    if sat_targets ~targets ~domains:(DomainSet.singleton name) then
      (fun expr man flow ->
         match ekind expr with
         | E_partition_predicate p ->
           eval_partition_predicate p expr.erange man flow |>
           Option.some

         | _ -> lift targets P.eval df expr man flow
      )
    else
      lift targets P.eval df

  let ask targets =
    let df = D.ask targets in
    let pf : type a r. (a, r) query -> (a, P.t) man -> a flow -> (a, r) cases option = fun query pman flow ->
      match query with
      | Q_partition_predicate range ->
        ask_partition_predicate range pman flow |>
        Option.some

      | _ ->
        P.ask query pman flow
    in
    lift targets pf df

  let print_state targets =
    let f = D.print_state targets in
    if sat_targets ~targets ~domains:(DomainSet.singleton name) then
      (fun printer -> function
         | Top ->
           pp_string printer "T"

         | Map map when M.is_empty map ->
           pp_string printer "⊥"

         | Map map ->
           pp_map
             (unformat P.print)
             f
             printer (M.bindings map)
      )
    else
      (fun printer -> function
         | Top ->
           pp_string printer "T"

         | Map map when M.is_empty map ->
           pp_string printer "⊥"

         | Map map ->
           if M.is_singleton map then
             let _, e = M.choose map in
             f printer e
           else
             assert false
      )

  let print_expr targets =
    let df = D.print_expr targets in
    (fun man flow printer exp ->
       let dman = domain_man man in
       man.get T_cur flow |>
       Cases.iter_result
         (fun a flow ->
            match a with
            | Top ->
              df dman flow printer exp

            | Map map when M.is_empty map ->
              pp_string printer "⊥"

            | Map map ->
              pp_mapi
                (unformat P.print)
                (fun printer (p, e) ->
                   let flow = set_env T_cur (singleton p e) man flow |> post_to_flow man in
                   df dman flow printer exp
                )
                printer (M.bindings map)
         )
    )

end
