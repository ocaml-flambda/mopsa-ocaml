(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Queries to retrieve variable values during an interactive session *)

open Mopsa_utils
open Core.All
open Format
open Location
open Callstack


(** {2 Debug queries} *)
(** ***************** *)

(* In order to retrieve structured information from the abstract environment,
   each language should implement a handler for the query [Q_debug_variable_value].
   The query [Q_debug_variable_value] retrieves the value of a given
   variable as a [var_value] record, containing a textual representation of
   the value, and a structural encoding of the eventual sub-values.
*)

(** Value of a variable *)
type var_value = {
  var_value: string option;            (** Direct value of the variable *)
  var_value_type : typ;                (** Type of the value *)
  var_sub_value: var_sub_value option; (** Sub-values of the variable *)
}


(** Sub-value of a variable *)
and var_sub_value =
  | Named_sub_value   of (string (** key *) * var_value (** value *)) list
  (** Named sub-values are maps from field names to values *)

  | Indexed_sub_value of var_value list
  (** Indexed sub-values are arrays of values *)


(** Query to retrieve the value of a given variable *)
type ('a,_) query += Q_debug_variable_value : var -> ('a,var_value) query

type ('a,_) query += Q_debug_addr_value : addr -> ('a,var_value) query

let () =
  register_query {
      join = (let doit : type a r. query_pool -> (a,r) query -> r -> r -> r =
          fun next query a b ->
          match query with
          | Q_debug_addr_value addr ->
             assert (a.var_value = None && b.var_value = None);
             let var_sub_value = begin match a.var_sub_value, b.var_sub_value with
                                 | None, Some sb -> Some sb
                                 | Some sa, None -> Some sa
                                 | Some Indexed_sub_value la, Some Indexed_sub_value lb -> Some (Indexed_sub_value (la @ lb))
                                 | Some Named_sub_value ma, Some Named_sub_value mb -> Some (Named_sub_value (ma @ mb))
                                 | _, _ -> assert false
                                 end in
             {var_value=None; var_value_type = T_any; var_sub_value}

          | _ -> next.pool_join query a b
        in doit
      );
      meet = (fun next q a b -> next.pool_meet q a b);
    }


(** Compare two var values *)
let rec compare_var_value v1 v2 =
  Compare.compose [
    (fun () -> Compare.option compare v1.var_value v2.var_value);
    (fun () -> Compare.option compare_var_sub_value v1.var_sub_value v2.var_sub_value);
  ]


(** Compare two var sub-values *)
and compare_var_sub_value sv1 sv2 =
  match sv1, sv2 with
  | Named_sub_value m1, Named_sub_value m2 ->
    Compare.list (fun x1 x2 -> Compare.pair compare compare_var_value x1 x2) m1 m2

  | Indexed_sub_value l1, Indexed_sub_value l2 ->
    Compare.list compare_var_value l1 l2

  | _ -> compare sv1 sv2



(** Print a key with its type *)
let pp_key_with_type fmt (k,t) =
  match t with
  | T_any -> pp_print_string fmt k
  | _      ->Format.fprintf fmt "%s : %a" k pp_typ t


(** Print a variable with its type *)
let pp_var_with_type fmt (v,t) =
  match t with
  | T_any -> pp_var fmt v
  | _      ->Format.fprintf fmt "%a : %a" pp_var v pp_typ t


(** Print the value of a variable *)
let rec pp_var_value fmt v =
  pp_print_option (Debug.color_str Debug.blue) fmt v.var_value;
  match v.var_sub_value with
  | None -> ()
  | Some sv -> fprintf fmt "@,@[<v2>  %a@]" pp_var_sub_value sv


(** Print the values of sub-variables *)
and pp_var_sub_value fmt = function
  | Named_sub_value l ->
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (k,v) -> fprintf fmt "%a = %a" pp_key_with_type (k,v.var_value_type) pp_var_value v)
      ) l

  | Indexed_sub_value l ->
    (* Group consecutive elements with the same value *)
    let rec group_by_value = function
      | []        -> []
      | (i,v)::tl ->
        match group_by_value tl with
        | [] -> [(i,i,v)]
        | (a,b,w)::tl ->
          if compare_var_value v w = 0
          then (i,b,v)::tl
          else (i,i,v)::(a,b,w)::tl
    in
    List.mapi (fun i v -> (i,v)) l |>
    group_by_value |>
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (i,j,v) ->
            if i = j
            then fprintf fmt "[%d] = %a" i pp_var_value v
            else fprintf fmt "[%d-%d] = %a" i j pp_var_value v
         )
      )
