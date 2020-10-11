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

(** Reduced product of simplified (leaf) domains *)

open Core.All
open Common
open Sig.Reduction.Simplified
open Sig.Combiner.Simplified



(** Create a product of two domains *)
module MakeDomainPair(D1:SIMPLIFIED_COMBINER)(D2:SIMPLIFIED_COMBINER)
  : SIMPLIFIED_COMBINER with type t = D1.t * D2.t =
struct

  type t = D1.t * D2.t

  let id = C_pair(Product,D1.id,D2.id)

  let name = D1.name ^ " ∧ " ^ D2.name

  let domains = DomainSet.union D1.domains D2.domains

  let semantics = SemanticSet.union D1.semantics D2.semantics

  let routing_table = join_routing_table D1.routing_table D2.routing_table

  let bottom : t = D1.bottom, D2.bottom

  let top : t = D1.top, D2.top

  let is_bottom ((a,b):t) : bool =
    D1.is_bottom a || D2.is_bottom b

  let subset ((a1,b1):t) ((a2,b2):t) : bool =
    D1.subset a1 a2 && D2.subset b1 b2

  let apply f1 f2 ((v1,v2) as v) =
    let r1 = f1 v1 in
    let r2 = f2 v2 in
    if r1 == v1 && r2 == v2 then v
    else (r1,r2)

  let apply2 f1 f2 ((v1,v2) as v) ((w1,w2) as w) =
    let r1 = f1 v1 w1 in
    let r2 = f2 v2 w2 in
    if r1 == v1 && r2 == v2 then v else
    if r1 == w1 && r2 == w2 then w
    else (r1,r2)

  let join ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
      apply2 D1.join D2.join v w

  let meet ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
      apply2 D1.meet D2.meet v w

  let widen ctx ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
      apply2 (D1.widen ctx) (D2.widen ctx) v w

  let merge (p1,p2) (a1,e1) (a2,e2) =
    apply2
      (fun v1 w1 -> D1.merge p1 (v1,e1) (w1,e2))
      (fun v2 w2 -> D2.merge p2 (v2,e1) (w2,e2))
      a1 a2

  let hdman (man:('a,t) Sig.Abstraction.Simplified.simplified_man) : (('a,D1.t) Sig.Abstraction.Simplified.simplified_man) = {
    man with
    exec = (fun stmt -> man.exec stmt |> fst);
  }

  let tlman (man:('a,t) Sig.Abstraction.Simplified.simplified_man) : (('a,D2.t) Sig.Abstraction.Simplified.simplified_man) = {
    man with
    exec = (fun stmt -> man.exec stmt |> snd);
  }

  let init prog = D1.init prog, D2.init prog

  let exec targets =
    let recompose_pair ((a1,a2) as a) r1 r2 =
      match r1, r2 with
      | None, None       -> None
      | Some r1, None    -> if a1 == r1 then Some a else Some (r1,a2)
      | None, Some r2    -> if a2 == r2 then Some a else Some (a1,r2)
      | Some r1, Some r2 -> if r1 == a1 && r2 == a2 then Some a else Some (r1,r2)
    in
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D1.domains
    with
    | false, false -> raise Not_found
    | true, false ->
      let f1 = D1.exec targets in
      (fun stmt man ctx ((a1,a2) as a) -> recompose_pair a (f1 stmt (hdman man) ctx a1) None)
    | false, true ->
      let f2 = D2.exec targets in
      (fun stmt man ctx ((a1,a2) as a) -> recompose_pair a None (f2 stmt (tlman man) ctx a2))
    | true, true ->
      let f1 = D1.exec targets in
      let f2 = D2.exec targets in
      (fun stmt man ctx ((a1,a2) as a) ->
         recompose_pair a
           (f1 stmt (hdman man) ctx a1)
           (f2 stmt (tlman man) ctx a2))


  let ask targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D1.domains
    with
    | false, false -> raise Not_found
    | true, false ->
      let f1 = D1.ask targets in
      (fun q man ctx (a1,_) -> f1 q (hdman man) ctx a1)
    | false, true ->
      let f2 = D2.ask targets in
      (fun q man ctx (_,a2) -> f2 q (tlman man) ctx a2)
    | true, true ->
      let f1 = D1.ask targets in
      let f2 = D2.ask targets in
      (fun q man ctx (a1,a2) ->
         OptionExt.neutral2
           (meet_query q
              ~meet:(fun _ _ -> Exceptions.panic "abstract queries called from simplified domains"))
           (f1 q (hdman man) ctx a1)
           (f2 q (tlman man) ctx a2))

  let print_state targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D1.domains
    with
    | false, false -> raise Not_found
    | true, false ->
      let f1 = D1.print_state targets in
      (fun printer (a1,_) ->
         f1 printer a1)
    | false, true ->
      let f2 = D2.print_state targets in
      (fun printer (_,a2) ->
         f2 printer a2)
    | true, true ->
      let f1 = D1.print_state targets in
      let f2 = D2.print_state targets in
      (fun printer (a1,a2) ->
         f1 printer a1;
         f2 printer a2)

  let print_expr targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D1.domains
    with
    | false, false -> raise Not_found
    | true, false ->
      let f1 = D1.print_expr targets in
      (fun man ctx (a1,_) printer e ->
         f1 (hdman man) ctx a1 printer e)
    | false, true ->
      let f2 = D2.print_expr targets in
      (fun man ctx (_,a2) printer e ->
         f2 (tlman man) ctx a2 printer e)
    | true, true ->
      let f1 = D1.print_expr targets in
      let f2 = D2.print_expr targets in
      (fun man ctx (a1,a2) printer e ->
         f1 (hdman man) ctx a1 printer e;
         f2 (tlman man) ctx a2 printer e)

end


(** Create a reduced product from a domain representing a product of
    domains and a list of reduction rules *)
module Make
    (D:SIMPLIFIED_COMBINER)
    (R:sig val rules : (module SIMPLIFIED_REDUCTION) list end)
  : SIMPLIFIED_COMBINER with type t = D.t =
struct

  include D

  let reduction_man man : ('a,D.t) simplified_reduction_man = {
    (* Get the abstract element of a domain *)
    get_env = (fun (type a) (idx:a id) (a:t) ->
        let rec aux : type b. b id -> b -> a = fun idy aa ->
          match idy, aa with
          | C_empty, () -> raise Not_found
          | C_pair(_,hd,tl), (ahd,atl) ->
            begin match equal_id hd idx with
              | Some Eq -> ahd
              | None -> aux tl atl
            end
          | _ ->
            match equal_id idx idy with
            | Some Eq -> aa
            | None -> raise Not_found
        in
        aux id a
    );

    (* Set the abstract element of a domain *)
    set_env = (fun (type a) (idx:a id) (x:a) (a:t) ->
        let rec aux : type b. b id -> b -> b =
          fun idy aa ->
            match idy, aa with
            | C_empty, () -> raise Not_found
            | C_pair(_,hd,tl), (ahd,atl) ->
              begin match equal_id hd idx with
                | Some Eq -> x,atl
                | None    -> ahd, aux tl atl
              end
            | _ ->
              match equal_id idx idy with
              | Some Eq -> x
              | None -> raise Not_found
        in
        aux id a
      );

    (* Get the abstract value of a variable *)
    get_value = (fun (type v) (idx:v id) var a ->
        let open Value.Nonrel in
        let open Bot_top in
        let open Lattices.Partial_map in
        let rec aux : type a. a id -> a -> v =
          fun idy aa ->
            match idy, aa with
            (* The value is extracted from non-relational environments *)
            | D_nonrel vmodule, aa ->
              let module V = (val vmodule) in
              (* Extract the value of the variable. This is not the
                     final result, as the required value may be embedded
                     in a product *)
              let v = match aa with
                | TOP -> V.top
                | BOT -> V.bottom
                | Nbt map -> PMap.find var map
              in

              (* Iterate over combiners to find the required id *)
              let rec iter : type w. w id -> w -> v =
                fun id' v' ->
                  (* Check if id' corresponds to what we are searching for *)
                  match equal_id id' idx with
                  | Some Eq -> v'
                  | None ->
                    (* Otherwise, search in the arguments of the combiner *)
                    match id', v' with
                    | V_pair(id1,id2), (v1,v2) ->
                      begin try iter id1 v1 with Not_found -> iter id2 v2 end
                    | _ -> raise Not_found
              in
              iter V.id v

            | C_pair(_,hd,tl), (ahd,atl) ->
              begin
                try aux hd ahd
                with Not_found -> aux tl atl
              end

            | _ -> raise Not_found
        in
        aux id a
    );

    set_value = (fun (type v) (idx:v id) var (v:v) a ->
        let open Value.Nonrel in
        let open Bot_top in
        let open Lattices.Partial_map in
        let rec aux : type a. a id -> a -> a =
          fun idy aa ->
            match idy, aa with
            (* The value is set in non-relational environments *)
            | D_nonrel vmodule, aa ->
              let module V = (val vmodule) in
              (* Get the old value of the variable. The given
                 value [v] will be put *inside* it (in case of composed values). *)
              let old = match aa with
                | TOP -> V.top
                | BOT -> V.bottom
                | Nbt map -> PMap.find var map
              in

              (* Iterate over the structure of [old] to put [v] in the right place *)
              let rec iter: type w. w id -> w -> w =
                fun id' v'->
                  (* Check if id' corresponds to what we are searching for *)
                  match equal_id id' idx with
                  | Some Eq -> v
                  | None ->
                    (* Otherwise, search in the operands of value combiners *)
                    match id',v' with
                    | V_pair(id1,id2), (v1,v2) ->
                      begin try iter id1 v1,v2 with Not_found -> v1,iter id2 v2 end
                    | _ -> raise Not_found
              in
              let update = iter V.id old in
              if V.is_bottom update then BOT
              else begin match aa with
                | TOP -> TOP
                | BOT -> BOT
                | Nbt map -> Nbt (PMap.add var update map)
              end

            | C_pair(_,hd,tl), (ahd,atl) ->
              begin
                try aux hd ahd, atl
                with Not_found -> ahd, aux tl atl
              end

            | _ -> raise Not_found
        in
        aux id a
      );

    ask = (fun q ctx a -> match D.ask [] q man ctx a with None -> raise Not_found | Some r -> r);
  }

  let reduce stmt man ctx pre post =
    let rman = reduction_man man in
    List.fold_left (fun acc rule ->
        let module Rule = (val rule : SIMPLIFIED_REDUCTION) in
        Rule.reduce stmt rman ctx pre acc
      ) post R.rules

  let exec targets =
    let f = D.exec targets in
    (fun stmt man ctx a ->
       match f stmt man ctx a with
       | None -> None
       | Some r -> Some (reduce stmt man ctx a r))

end


let make (domains:(module SIMPLIFIED_COMBINER) list) ~(rules:(module SIMPLIFIED_REDUCTION) list) : (module SIMPLIFIED_COMBINER) =
  let rec aux = function
    | [] -> assert false
    | [d] -> d
    | hd::tl -> (module MakeDomainPair(val hd:SIMPLIFIED_COMBINER)(val (aux tl):SIMPLIFIED_COMBINER) : SIMPLIFIED_COMBINER)
  in
  (module Make(val (aux domains):SIMPLIFIED_COMBINER)(struct let rules = rules end))
