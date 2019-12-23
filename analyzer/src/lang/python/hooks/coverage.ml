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

(* A prototype hook for displaying analysis coverage. Currently a POC *)

open Location
open Mopsa
open Format
open Ast
open Zone


module Hook =
struct

  let name = "coverage"
  let exec_zones = [Z_any]
  let eval_zones = [Z_py,Z_any]


  module RangeSet = SetExt.Make(struct type t = range let compare = compare_range end)

  (* on voudrait juste les lignes en fait ? *)
  (* mais il faut éviter lignes 100-200 sur un appel de fonction, vu qu'on veut plus de précision ensuite. il faut faire des cas particuliers? *)
  type entry = {
    all_ranges: RangeSet.t;
    mutable analyzed_ranges: RangeSet.t
  }

  let table : (string, entry) Hashtbl.t = Hashtbl.create 1

  let add_file filename body =
    let program_ranges =
      Visitor.fold_stmt
        (fun acc e -> VisitParts acc)
        (fun acc s -> match skind s with
           | Universal.Ast.S_block _ -> VisitParts acc
           | S_py_function f -> VisitParts acc (* special range for class/function declaration? *)
           | S_py_class _ -> VisitParts acc
           | S_py_if (e, _, _) -> VisitParts (RangeSet.add e.erange acc)
           | _ -> VisitParts (RangeSet.add s.srange acc)
        )
        RangeSet.empty body in
    Hashtbl.add table filename
      {all_ranges = program_ranges; analyzed_ranges = RangeSet.empty}

  let init ctx = ()

  let on_before_exec zone stmt man flow =
    ()

  let on_after_exec zone stmt man post =
    match skind stmt with
    | S_py_function _ | S_py_class _ | Universal.Ast.S_block _ -> ()
    | S_assign (_, {ekind = E_py_object _}) -> () (* FIXME to avoid __init__ = <<function init>> *)
    | _ ->
      let range = srange stmt in
      let file = get_range_file range in
      let entry = Hashtbl.find table file in
      if RangeSet.mem range entry.all_ranges && get_pos_line (get_range_end range) = get_pos_line (get_range_start range) then
        (* FIXME: the fixme above wasn't working, so we keep only one-line statements (alternative tag the whole range used for inner evaluations) *)
        let () = Debug.debug ~channel:"coverage" "stmt %a, range %a" pp_stmt stmt pp_range range in
        entry.analyzed_ranges <- RangeSet.add range entry.analyzed_ranges


  let on_before_eval zone exp man flow =
    ()

  let on_after_eval zone exp man evl =
    ()


  let on_finish man flow =
    let open Unix in
    let open Filename in
    let time = Unix.localtime (Unix.gettimeofday ()) in
    let dirname = Format.asprintf "/tmp/coverage_%d_%d_%d_%d%d%d"
        time.tm_mday
        (time.tm_mon+1)
        (time.tm_year+1900)
        time.tm_hour
        time.tm_min
        time.tm_sec in
    let () = Unix.mkdir dirname 0o755 in
    Hashtbl.iter (fun filename entry ->
        let fname = remove_extension @@ basename filename in
        let oc = open_out (dirname ^ "/" ^ fname ^ ".cov") in
        let ocf = Format.formatter_of_out_channel oc in
        let file = open_in filename in
        let rec process_file lineno =
          try
            let l = input_line file in
            if RangeSet.exists (fun r ->
                let start = get_range_start r in
                let stop = get_range_end r in
                get_pos_line start <= lineno && lineno <= get_pos_line stop) entry.analyzed_ranges then (* okay, statement reached *)
              Format.fprintf ocf "\027[38;5;%dm %s\027[0m@." (List.assoc "green" Debug.colors) l
            else
              (* statement not analyzed *)
              Format.fprintf ocf "\027[38;5;%dm %s\027[0m@." (List.assoc "red" Debug.colors) l;
            process_file (lineno+1)
          with End_of_file -> close_in file in
        process_file 1;
        flush oc;
        close_out oc
      ) table

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
