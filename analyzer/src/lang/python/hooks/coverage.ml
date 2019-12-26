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
(* TODO: endroits à bottom. Pourquoi ne pas plutôt collecter les statements feuille au début et ensuite reconnaître ceux là? *)

open Location
open Mopsa
open Format
open Ast
open Zone
open Sig.Domain.Manager


module Hook =
struct

  let name = "coverage"
  let exec_zones = [Z_any]
  let eval_zones = [Z_py,Z_any]

  let compare_stmt_andrange s1 s2 =
    Compare.compose
      [ (fun () -> compare_range s1.srange s2.srange);
        (fun () -> compare_stmt s1 s2) ]

  let compare_expr_andrange e1 e2 =
    Compare.compose
      [ (fun () -> compare_range e1.erange e2.erange);
        (fun () -> compare_expr e1 e2) ]

  module ExprSet = SetExt.Make(struct type t = expr let compare = compare_expr_andrange end)
  module StmtSet = SetExt.Make(struct type t = stmt let compare = compare_stmt_andrange end)

  (* on voudrait juste les lignes en fait ? *)
  (* mais il faut éviter lignes 100-200 sur un appel de fonction, vu qu'on veut plus de précision ensuite. il faut faire des cas particuliers? *)
  type entry = {
    mutable never_analyzed_exprs : ExprSet.t;
    mutable never_analyzed_stmts : StmtSet.t;
    mutable always_bottom_exprs : ExprSet.t;
    mutable always_bottom_stmts : StmtSet.t;
    mutable reachable_exprs : ExprSet.t;
    mutable reachable_stmts : StmtSet.t;
  }

  let table : (string, entry) Hashtbl.t = Hashtbl.create 1

  let add_file filename body =
    let init_exprs, init_stmts =
      Visitor.fold_stmt
        (fun acc e -> VisitParts acc)
        (fun (acce, accs) s ->
           match skind s with
           | S_assign _ | Universal.Ast.S_return _ -> Keep (acce, StmtSet.add s accs)
           | S_py_function _
           | S_py_class _ -> VisitParts (acce, accs)
           | _ ->
           let parts, _ = split_stmt s in
           let exprs = ExprSet.of_list parts.exprs in
           if List.length parts.stmts > 0 then
             VisitParts (ExprSet.union exprs acce, accs)
           else
             Keep (ExprSet.union exprs acce, StmtSet.add s accs)
           (*   VisitParts (RangeSet.add s.srange acc)
            * match skind s with
            * |
            * | Universal.Ast.S_block _ -> VisitParts acc
            * | S_py_function f -> VisitParts acc (\* special range for class/function declaration? *\)
            * | S_py_class _ -> VisitParts acc
            * | S_py_if (e, _, _) -> VisitParts (RangeSet.add e.erange acc)
            * | S_py_for (target, iterator, _, _) ->
            *   VisitParts (RangeSet.add target.erange (RangeSet.add iterator.erange acc))
            * | Universal.Ast.S_return _ -> Keep (RangeSet.add s.srange acc)
            * | S_py_aug_assign _ -> Keep (RangeSet.add s.srange acc)
            * | _ -> VisitParts (RangeSet.add s.srange acc) *)
        )
        (ExprSet.empty, StmtSet.empty) body in
    Debug.debug ~channel:"coverage" "init_exprs = @[%a@]@.init_stmts = @[%a@]@."
      (ExprSet.fprint SetExt.printer_default pp_expr_with_range) init_exprs
      (StmtSet.fprint SetExt.printer_default pp_stmt_with_range) init_stmts;
    Hashtbl.add table filename
      {never_analyzed_exprs = init_exprs;
       never_analyzed_stmts = init_stmts;
       always_bottom_exprs = ExprSet.empty;
       always_bottom_stmts = StmtSet.empty;
       reachable_exprs = ExprSet.empty;
       reachable_stmts = StmtSet.empty}

  let init ctx = ()

  let is_cur_bottom man flow =
    man.lattice.is_bottom (Flow.get T_cur man.lattice flow)

  let on_before_exec zone stmt man flow =
    match skind stmt with
    | S_py_function _ | S_py_class _ | Universal.Ast.S_block _ -> ()
    | S_assign (_, {ekind = E_py_object _}) -> ()
    | Universal.Ast.S_expression {ekind = E_py_call({ekind = E_py_object ({addr_kind = Addr.A_py_function (F_user _)}, _)}, _, _)} -> ()
    | _ ->
      let range = srange stmt in
      let file = get_range_file range in
      let entry = Hashtbl.find table file in
      if StmtSet.mem stmt entry.never_analyzed_stmts then
        let () = entry.never_analyzed_stmts <- StmtSet.remove stmt entry.never_analyzed_stmts in
        if is_cur_bottom man flow then
          entry.always_bottom_stmts <- StmtSet.add stmt entry.always_bottom_stmts
        else
          entry.reachable_stmts <- StmtSet.add stmt entry.reachable_stmts
      else if StmtSet.mem stmt entry.always_bottom_stmts && not @@ is_cur_bottom man flow then
          let () = entry.always_bottom_stmts <- StmtSet.remove stmt entry.always_bottom_stmts in
          entry.reachable_stmts <- StmtSet.add stmt entry.reachable_stmts

  let on_after_exec zone stmt man post = ()

  let on_before_eval zone exp man flow =
    let range = erange exp in
    let file = get_range_file range in
    let entry = Hashtbl.find table file in
    (* Debug.debug ~channel:"coverage" "considering %a@.ExprSet = %a@." pp_expr_with_range exp (ExprSet.fprint SetExt.printer_default pp_expr_with_range) entry.reachable_exprs; *)
    if ExprSet.mem exp entry.never_analyzed_exprs then
      (* let () = Debug.debug ~channel:"coverage" "never analyzed@." in *)
      let () = entry.never_analyzed_exprs <- ExprSet.remove exp entry.never_analyzed_exprs in
      if is_cur_bottom man flow then
        entry.always_bottom_exprs <- ExprSet.add exp entry.always_bottom_exprs
      else
        (* let () = Debug.debug ~channel:"coverage" "added to reachables@." in *)
        entry.reachable_exprs <- ExprSet.add exp entry.reachable_exprs
    else if ExprSet.mem exp entry.always_bottom_exprs && not @@ is_cur_bottom man flow then
      (* let () = Debug.debug ~channel:"coverage" "always bottom@." in *)
      let () = entry.always_bottom_exprs <- ExprSet.remove exp entry.always_bottom_exprs in
      entry.reachable_exprs <- ExprSet.add exp entry.reachable_exprs
    (* else
     *   Debug.debug ~channel:"coverage" "wtf@." *)

  let on_after_eval zone exp man evl = ()

  let is_comment l =
    let lt = String.trim l in
    String.length lt > 0 && lt.[0] = '#'

  let on_finish man flow =
    let open Unix in
    let open Filename in
    let time = Unix.localtime (Unix.gettimeofday ()) in
    let dirname = Format.asprintf "/tmp/coverage_%02d_%02d_%04d_%02d%02d%02d"
        time.tm_mday
        (time.tm_mon+1)
        (time.tm_year+1900)
        time.tm_hour
        time.tm_min
        time.tm_sec in
    let () = Unix.mkdir dirname 0o755 in
    (* Format.printf "Coverage:@.";
     * Hashtbl.iter (fun filename entry ->
     *     let whole_size = ListExt.fold_left (+) 0 (List.map StmtSet.cardinal [entry.never_analyzed_stmts; entry.always_bottom_stmts; entry.reachable_stmts]) in
     *     let size = ListExt.fold_left (+) 0 (List.map StmtSet.cardinal [entry.always_bottom_stmts; entry.reachable_stmts]) in
     *     Format.printf "\t%s: %.2f%% @." filename (100. *. (float_of_int size) /. (float_of_int whole_size))
     *   ) table; *)
    Hashtbl.iter (fun filename entry ->
        let fname = remove_extension @@ basename filename in
        if fname = "mopsa" || fname = "stdlib" then () else
        let oc = open_out (dirname ^ "/" ^ fname ^ ".cov") in
        (* let ocf = Format.std_formatter in *)
        let ocf = Format.formatter_of_out_channel oc in
        let file = open_in filename in
        let covered_lines = ref 0 in
        let rec process_file lineno =
          let search_stmt = (fun s ->
              let r = s.srange in
              let start = get_range_start r in
              let stop = get_range_end r in
              get_pos_line start <= lineno && lineno <= get_pos_line stop
            ) in
          let search_expr = (fun e ->
                let r = e.erange in
                let start = get_range_start r in
                let stop = get_range_end r in
                (* Debug.debug ~channel:"coverage" "search_expr %d %a (%a--%a)" lineno pp_expr e pp_position start pp_position stop; *)
                get_pos_line start = lineno && lineno = get_pos_line stop
            ) in
          try
            let l = input_line file in
            if is_comment l || l = "" then
              let () = incr covered_lines in
              Format.fprintf ocf "%s@." l
            else if StmtSet.exists search_stmt entry.reachable_stmts then (* okay, statement reached *)
              let () = incr covered_lines in
              Format.fprintf ocf "\027[38;5;%dm%s\027[0m@." (List.assoc "green" Debug.colors) l
            else if StmtSet.exists search_stmt entry.always_bottom_stmts then
              let () = incr covered_lines in
              Format.fprintf ocf "\027[38;5;%dm%s\027[0m@." (List.assoc "yellow" Debug.colors) l
            else
              begin
                match ExprSet.choose_opt @@ ExprSet.filter search_expr entry.reachable_exprs with
              | Some e ->
                let () = incr covered_lines in
                let range = e.erange in
                let n = String.length l in
                let cols, cole = 0, n
                                   (*get_pos_column @@ get_range_start range,
                                     get_pos_column @@ get_range_end range *) in
                Format.fprintf ocf "%s\027[38;5;%dm%s\027[0m%s@."
                  (String.sub l 0 cols)
                  (List.assoc "green" Debug.colors)
                  (String.sub l cols (cole-cols))
                  (String.sub l cole (n-cole))
              | None ->
                begin match ExprSet.choose_opt @@ ExprSet.filter search_expr entry.always_bottom_exprs with
                  | Some e ->
                    let () = incr covered_lines in
                    let range = e.erange in
                    let n = String.length l in
                    let cols, cole = 0, n (*get_pos_column @@ get_range_start range,
                                            get_pos_column @@ get_range_end range *) in
                    Format.fprintf ocf "%s\027[38;5;%dm%s\027[0m%s@."
                      (String.sub l 0 cols)
                      (List.assoc "yellow" Debug.colors)
                      (String.sub l cols (cole-cols))
                      (String.sub l cole (n-cole))
                  | None ->
                    (* statement not analyzed *)
                    Format.fprintf ocf "\027[38;5;%dm%s\027[0m@." (List.assoc "red" Debug.colors) l
                end
              end;
            process_file (lineno+1)
          with End_of_file ->
            let () = Format.printf "(Yet inaccurate) Coverage of %s: %.2f%%@." filename (100. *. float_of_int  !covered_lines /. (float_of_int lineno)) in
            close_in file in
          process_file 1;
        flush oc;
        close_out oc
      ) table

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
