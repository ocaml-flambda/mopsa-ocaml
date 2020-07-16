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

(** Extended domains signatures used by combiners *)

open Ast.All
open Core.All


type combiner =
  | Sequence
  | Compose
  | Product

type _ id +=
  | C_empty : unit id
  | C_pair : combiner * 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | C_empty, C_empty -> Some Eq
        | C_pair(c1,x1,y1), C_pair(c2,x2,y2) ->
          if c1 = c2 then
            match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          else
            None
        | _ -> next.eq id1 id2
      in
      f
    );
  }

type _ id += V_empty : unit id
type _ id += V_pair  : 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | V_empty, V_empty -> Some Eq
        | V_pair(x1,y1), V_pair(x2,y2) ->
          begin match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          end
        | _ -> next.eq id1 id2
      in
      f
    );
  }


let sat_targets ~targets ~nodes =
  targets = [] || List.exists (fun t -> List.mem t nodes) targets


(** Manager of the left argument in a compose topology *)
let fst_pair_man (man:('a, 'b * 'c) man) : ('a, 'b) man = {
  man with
  get = get_pair_fst man;
  set = set_pair_fst man;
  get_log = (fun glog -> man.get_log glog |> Log.get_left_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] log (man.get_log glog |> Log.get_right_log)
    ) glog);
}

(** Manager of the right argument in a compose topology *)
let snd_pair_man (man:('a, 'b * 'c) man) : ('a, 'c) man = {
  man with
  get = get_pair_snd man;
  set = set_pair_snd man;
  get_log = (fun glog -> man.get_log glog |> Log.get_right_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] (man.get_log glog |> Log.get_left_log) log
    ) glog);
}


let rec find_domain_man : type b c. target:b id -> tree:c id -> ('a,c) man -> ('a,b) man = fun ~target ~tree man ->
  match tree with
  | C_empty -> raise Not_found
  | C_pair(_,left,right) ->
    begin
      try find_domain_man target left (fst_pair_man man)
      with Not_found -> find_domain_man target right (snd_pair_man man)
    end
  | _ ->
    match equal_id target tree with
    | Some Eq -> man
    | None -> raise Not_found

let rec mem_domain : type b c. target:b id -> tree:c id -> bool = fun ~target ~tree ->
  match tree with
  | C_empty -> false
  | C_pair(_,left,right) -> mem_domain target left || mem_domain target right
  | _ ->
    match equal_id target tree with
    | Some Eq -> true
    | None -> false



module EmptyValue : Sig.Domain.Value.VALUE =
struct
  type t = unit
  let id = V_empty
  let name = ""
  let display = ""
  let bottom = ()
  let top = ()
  let print fmt () = ()
  let is_bottom () = false
  let subset () () = true
  let join () () = ()
  let meet () () = ()
  let widen () () = ()
  let constant t c = None
  let cast man t e = None
  let unop op t () = ()
  let binop op t () () = ()
  let filter b t () = ()
  let bwd_unop op t () () = ()
  let bwd_binop op t () () () = ((),())
  let bwd_cast man t e () = ()
  let predicate op b t () = ()
  let compare op b t () () = ((),())
  let ask man q = None
end
