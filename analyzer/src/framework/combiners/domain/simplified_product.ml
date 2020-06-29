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

(** Reduced product of (leaf) domains with n-ary simplified reduction rules *)

open Ast.All
open Core.All
open Sig.Abstraction.Simplified
open Sig.Reduction.Simplified


type _ id += D_empty_product: unit id
type _ id += D_pair_product: 'a id * 'b id -> ('a*'b) id
    
module EmptyDomain : SIMPLIFIED =
struct
  type t = unit
  let id = D_empty_product
  let () =
    let eq : type a. a id -> (a,unit) Eq.eq option = function
      | D_empty_product -> Some Eq
      | _ -> None
    in register_id { eq }
  let name = ""
  let zones = []
  let bottom = ()
  let top = ()
  let print fmt () = ()
  let is_bottom () = false
  let subset () () = true
  let join () () = ()
  let meet () () = ()
  let widen ctx () () = ()
  let merge () _ _ = ()
  let init p = ()
  let exec ctx stmt man () = None
  let ask q man () = None
end

module MakeDomainPair(D1:SIMPLIFIED)(D2:SIMPLIFIED) : SIMPLIFIED =
struct

  type t = D1.t * D2.t
  
  let id = D_pair_product(D1.id,D2.id)

  let () =
    let eq : type a. a id -> (a,t) Eq.eq option = function
      | D_pair_product(id1,id2) ->
        begin match equal_id id1 D1.id with
          | Some Eq ->
            begin match equal_id id2 D2.id with
              | Some Eq -> Some Eq
              | None -> None
            end
          | None -> None
        end
      | _ -> None
    in register_id { eq }

  let name = ""

  let zones = D1.zones @ D2.zones

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

  let merge (p1,p2) (a1,log1) (a2,log2) =
    apply2
      (fun v1 w1 -> D1.merge p1 (v1,log1) (w1,log2))
      (fun v2 w2 -> D2.merge p2 (v2,log1) (w2,log2))
      a1 a2

  let print fmt (a1,a2) =
    match D2.id with
    | D_empty_product -> D1.print fmt a1
    | _ ->  Format.fprintf fmt "%a@\n%a" D1.print a1 D2.print a2
  
  let hdman (man:t simplified_man) : (D1.t simplified_man) = {
    man with
    exec = (fun stmt -> man.exec stmt |> fst);
  }

  let tlman (man:t simplified_man) : (D2.t simplified_man) = {
    man with
    exec = (fun stmt -> man.exec stmt |> snd);
  }

  let init prog = D1.init prog, D2.init prog

  let exec stmt man ctx ((a1,a2) as a) =
    match D1.exec stmt (hdman man) ctx a1, D2.exec stmt (tlman man) ctx a2 with
    | None, None       -> None
    | Some r1, None    -> if a1 == r1 then Some a else Some (r1,a2)
    | None, Some r2    -> if a2 == r2 then Some a else Some (a1,r2)
    | Some r1, Some r2 -> if r1 == a1 && r2 == a2 then Some a else Some (r1,r2)
    
  let ask q man (a1,a2) =
    OptionExt.neutral2
      (meet_query q)
      (D1.ask q (hdman man) a1)
      (D2.ask q (tlman man) a2)    
end


module Make(D:SIMPLIFIED)(R:sig val rules : (module SIMPLIFIED_REDUCTION) list end) : SIMPLIFIED with type t = D.t =
struct

  include D

  let reduction_man man : D.t simplified_reduction_man = {
    (* Get the abstract element of a domain *)
    get_env = (fun (type a) (idx:a id) (a:t) ->
        let rec aux : type b. b id -> b -> a = fun idy aa ->
          match idy, aa with
          | D_empty_product, () -> raise Not_found
          | D_pair_product(hd,tl), (ahd,atl) ->
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
            | D_empty_product, () -> raise Not_found
            | D_pair_product(hd,tl), (ahd,atl) ->
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
        let open Value.Common in
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

            | D_pair_product(hd,tl), (ahd,atl) ->
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
        let open Value.Common in
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

            | D_pair_product(hd,tl), (ahd,atl) ->
              begin
                try aux hd ahd, atl
                with Not_found -> ahd, aux tl atl
              end

            | _ -> raise Not_found
        in
        aux id a
      );

    ask = (fun q a -> match D.ask q man a with None -> raise Not_found | Some r -> r);  
  }

  let reduce stmt man ctx pre post =
    let rman = reduction_man man in
    List.fold_left (fun acc rule ->
        let module Rule = (val rule : SIMPLIFIED_REDUCTION) in
        Rule.reduce stmt rman ctx pre acc
      ) post R.rules

  let exec stmt man ctx a =
    match exec stmt man ctx a with
    | None -> None
    | Some r -> Some (reduce stmt man ctx a r)
  
end


let make (domains:(module SIMPLIFIED) list) (rules:(module SIMPLIFIED_REDUCTION) list) : (module SIMPLIFIED) =
  let rec aux = function
    | [] -> assert false
    | [d] -> d
    | hd::tl -> (module MakeDomainPair(val hd:SIMPLIFIED)(val (aux tl):SIMPLIFIED) : SIMPLIFIED)
  in
  (module Make(val (aux domains):SIMPLIFIED)(struct let rules = rules end))
