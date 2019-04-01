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

(** Journal logs used to merge two post-conditions that diverged due to a
    fork-join trajectory in the abstraction DAG. 
*)

open Ast.Stmt

(** Logs *)
type log =
  (** Empty log *)
  | L_empty

  (** Logs of a singleton domain *)
  | L_singleton of block (** Block of statements received by the domain *) *
                   log   (** Inner logs of the domain *)
  
  (** Logs of a compound domain *)
  | L_tuple of log (** Logs of the first domain *) *
               log (** Logs of the second domain *)

(** Concatenate two logs *)
let rec concat log1 log2 =
  match log1, log2 with
  | L_empty, x | x, L_empty -> x
  | L_singleton (b1, l1), L_singleton (b2, l2) -> L_singleton (b1 @ b2, concat l1 l2)
  | L_tuple (fst1,snd1), L_tuple (fst2,snd2) -> L_tuple (concat fst1 fst2, concat snd1 snd2)
  | _ -> assert false

(** Empty log *)
let empty = L_empty

(** Test if a log is empty *)
let is_empty log =
  match log with
  | L_empty -> true
  | _ -> false

let tuple (fst, snd) =
  L_tuple (fst, snd)

let first log =
  match log with
  | L_empty -> L_empty
  | L_tuple(fst,_) -> fst
  | _ -> assert false

let second log =
  match log with
  | L_empty -> L_empty
  | L_tuple(_,snd) -> snd
  | _ -> assert false


(** Return the block of statement logged by a domain *)
let get_domain_block log =
  match log with
  | L_singleton(block, _) -> block
  | _ -> assert false

(** Return the inner logs of the domain *)
let get_domain_inner_log log =
  match log with
  | L_singleton(_, l) -> l
  | _ -> assert false

(** Append a statement to the logs of a domain *)
let append stmt log =
  match log with
  | L_empty -> L_singleton ([stmt], L_empty)
  | L_singleton (block, inner) -> L_singleton (block @ [stmt], inner)
  | L_tuple (fst,snd) -> assert false

(** Append a statement to the logs of the first domain in a tuple configuration *)
let append_fst stmt log =
  match log with
  | L_empty -> L_tuple (append stmt empty, empty)
  | L_singleton (block, inner) -> assert false
  | L_tuple (fst,snd) -> L_tuple (append stmt fst, snd)

(** Append a statement to the logs of the second domain in a tuple configuration *)
let append_snd stmt log =
  match log with
  | L_empty -> L_tuple (empty, append stmt empty)
  | L_singleton (block, inner) -> assert false
  | L_tuple (fst,snd) -> L_tuple (fst, append stmt snd)
