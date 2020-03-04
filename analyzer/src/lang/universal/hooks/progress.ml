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

(** Hook for displaying progress of the analysis *)

open Location
open Mopsa
open Format
open Ast
open Zone


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "progress"
  let exec_zones = [Z_any]
  let eval_zones = [Z_u,Z_any]


  (** {2 Entries of the progress table} *)
  (** ********************************* *)

  (** Set of location ranges *)
  module RangeSet = SetExt.Make(struct type t = range let compare = compare_range end)

  (** Entry of the progression table *)
  type entry = {
    name: string;                   (** Name of the function *)
    mutable range: range option;    (** Location of the currently analyzed statement *)
    all_stmt_range_set: RangeSet.t; (** Location of all statements in the function body *)
    mutable analyzed_stmt_range_set: RangeSet.t; (** Locations of analyzed statements *)
    nb_all_stmt: int;               (** Total number of statements *)
    mutable nb_analyzed_stmt: int;  (** Number of analyzed statements *)
  }

  (** Print an entry of the progress table *)
  let pp_entry fmt e =
    match e.range with
    | None -> fprintf fmt "%s %d%%" e.name (100*e.nb_analyzed_stmt/e.nb_all_stmt)
    | Some r ->
      let pos = get_range_start r in
      fprintf fmt "%s:%d %d%%" e.name pos.pos_line (100*e.nb_analyzed_stmt/e.nb_all_stmt)


  (** {2 Utilities for moving the terminam cursor} *)
  (** ******************************************** *)

  (** Clear the current line and move the cursor at its beginning *)
  let clear_line () =
    printf "\027[2K\r@?"

  (** Move the cursor one line above *)
  let up_line () =
    printf "\027[1A@?"


  (** {2 Progression table} *)
  (** ********************* *)

  (** Progression table as a stack of entries *)
  let table : entry Stack.t = Stack.create ()


  (** Return the set of statements in the body of a function *)
  let get_function_statements f =
    Visitor.fold_stmt
      (fun acc e -> VisitParts acc)
      (fun acc s ->
         match skind s with
         | S_block _ -> VisitParts acc
         | _ -> VisitParts (RangeSet.add s.srange acc)
      ) RangeSet.empty f.fun_body


  (** Insert a new entry in the progress table *)
  let before_call f =
    let all_stmt_range_set = get_function_statements f in
    let entry = {
      name = f.fun_orig_name;
      range = None;
      all_stmt_range_set;
      analyzed_stmt_range_set = RangeSet.empty;
      nb_all_stmt = RangeSet.cardinal all_stmt_range_set;
      nb_analyzed_stmt = 0;
    }
    in
    Stack.push entry table;
    printf "@.%a@?" pp_entry entry


  (** Remove the head function from the progress table *)
  let after_call () =
    let _ = Stack.pop table in
    (* Clear the current line (displaying the progress of the removed entry) 
       and move the cursor to the line before.
    *)
    clear_line ();
    if Stack.is_empty table then ()
    else (
      up_line ();
      clear_line ();
      printf "%a@?" pp_entry (Stack.top table)
    )


  (** Update the progress table before a statement is analyzed *)
  let before_stmt range =
    if Stack.is_empty table
    then ()
    else (
      let entry = Stack.top table in
      if not @@ RangeSet.mem range entry.all_stmt_range_set then ()
      else (
        entry.range <- Some range;
        clear_line ();
        printf "%a@?" pp_entry entry
      )
    )

  (** Update the progress table after a statement is analyzed *)
  let after_stmt range =
    if Stack.is_empty table
    then ()
    else (
      let entry = Stack.top table in
      if not @@ RangeSet.mem range entry.all_stmt_range_set then ()
      else if RangeSet.mem range entry.analyzed_stmt_range_set then ()
      else (
        entry.analyzed_stmt_range_set <- RangeSet.add range entry.analyzed_stmt_range_set;
        entry.nb_analyzed_stmt <- entry.nb_analyzed_stmt + 1;
        clear_line ();
        printf "%a@?" pp_entry entry
      )
    )  

  

  (** {2 Initialization} *)
  (** ****************** *)


  let init ctx = ()



  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec zone stmt man flow =
    before_stmt stmt.srange


  let on_after_exec zone stmt man post =
    after_stmt stmt.srange


  let on_before_eval zone exp man flow =
    match ekind exp with
    | E_call ({ ekind = E_function (User_defined f) }, args) -> before_call f
    | _ -> ()


  let on_after_eval zone exp man evl =
    match ekind exp with
    | E_call ({ ekind = E_function (User_defined f) }, args) -> after_call ()
    | _ -> ()

  let on_finish man flow = ()

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
