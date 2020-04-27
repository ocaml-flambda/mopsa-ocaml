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
    (* when adding a file, we collect all leaf statements (ie statements not containing stmts themselves), and toplevel expressions defined in those statements. We will then work only on those collected statements (except for a few hacks in on_after_eval *)
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
        )
        (ExprSet.empty, StmtSet.empty) body in
    (* Debug.debug ~channel:"coverage" "init_exprs = @[%a@]@.init_stmts = @[%a@]@."
     *   (ExprSet.fprint SetExt.printer_default pp_expr_with_range) init_exprs
     *   (StmtSet.fprint SetExt.printer_default pp_stmt_with_range) init_stmts; *)
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
    | S_py_function _ | S_py_class _ -> ()
    | Universal.Ast.S_block (l, _) when l <> [] -> ()
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

  let on_after_exec zone stmt man flow post = ()

  let on_before_eval zone exp man flow =
    let range = erange exp in
    let file = get_range_file range in
    let entry = Hashtbl.find table file in
    if ExprSet.mem exp entry.never_analyzed_exprs then
      let () = entry.never_analyzed_exprs <- ExprSet.remove exp entry.never_analyzed_exprs in
      if is_cur_bottom man flow then
        entry.always_bottom_exprs <- ExprSet.add exp entry.always_bottom_exprs
      else
        entry.reachable_exprs <- ExprSet.add exp entry.reachable_exprs
    else if ExprSet.mem exp entry.always_bottom_exprs && not @@ is_cur_bottom man flow then
      let () = entry.always_bottom_exprs <- ExprSet.remove exp entry.always_bottom_exprs in
      entry.reachable_exprs <- ExprSet.add exp entry.reachable_exprs

  let on_after_eval zone exp man flow evl =
    match ekind exp with
    (* to highlight the def bla(params): if bla is ever called *)
    | E_py_call ({ekind = E_py_object ({addr_kind = Addr.A_py_function (F_user f)}, _)} as caller, _, _) ->
      let start = get_range_start f.py_func_range in
      let stop =
        let r = get_range_start f.py_func_body.srange in
        mk_pos (get_pos_file r) (max (get_pos_line r - 1) (get_pos_line start)) 80 in
      let range = mk_orig_range start stop in
      let entry = Hashtbl.find table (get_range_file range) in
      entry.reachable_exprs <- ExprSet.add {caller with erange=range} entry.reachable_exprs

    (* to highlight the class Bla: if an object is ever instantiated *)
    | E_py_call ({ekind = E_py_object ({addr_kind = Addr.A_py_class (C_user c, _)}, _)} as caller, _, _) ->
      let start = get_range_start c.py_cls_range in
      let stop =
        let r = get_range_start c.py_cls_body.srange in
        mk_pos (get_pos_file r) (max (get_pos_line r - 1) (get_pos_line start)) 80 in
      let range = mk_orig_range start stop in
      let entry = Hashtbl.find table (get_range_file range) in
      entry.reachable_exprs <- ExprSet.add {caller with erange=range} entry.reachable_exprs

    | _ -> ()

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
    let total_lineno = ref 0 in
    Format.printf "Detailed coverage files are being written in %s@." dirname;
    Hashtbl.iter (fun filename entry ->
        let fname = remove_extension @@ basename filename in
        if fname = "mopsa" || fname = "stdlib" || extension (basename filename) <> ".py" then () else
        let oc = open_out (dirname ^ "/" ^ fname ^ ".cov") in
        let ocf = Format.formatter_of_out_channel oc in
        let file = open_in filename in
        let covered_lines = ref 0 in
        let rec process_file lineno =
          let search_stmt = (fun s ->
              let r = s.srange in
              let start = get_range_start r in
              let stop = get_range_end r in
              (match r with
               | R_tagged (s, _) ->
                 String.sub s 0 8 = "implicit"
              | _ -> true ) &&
              get_pos_line start <= lineno && lineno <= get_pos_line stop
            ) in
          let search_expr = (fun e ->
                let r = e.erange in
                let start = get_range_start r in
                let stop = get_range_end r in
                get_pos_line start = lineno && lineno = get_pos_line stop
            ) in
          try
            let l = input_line file in
            if is_comment l || l = "" || List.mem (String.trim l) ["else:"; "try:"]  then
              (*  FIXME: better treatment of else/try... *)
              let () = incr covered_lines in
              Format.fprintf ocf "%s@." l
            else if StmtSet.exists search_stmt entry.reachable_stmts then (* okay, statement reached *)
              let () = incr covered_lines in
              Format.fprintf ocf "\027[38;5;%dm%s\027[0m@." (List.assoc "green" Debug.colors) l
            else if StmtSet.exists search_stmt entry.always_bottom_stmts then
              (* let () = Format.printf "always bottom: %a" pp_stmt_with_range (StmtSet.choose @@ StmtSet.filter search_stmt entry.always_bottom_stmts) in *)
              let () = incr covered_lines in
              Format.fprintf ocf "\027[38;5;%dm%s\027[0m@." (List.assoc "yellow" Debug.colors) l
            else
              begin
                match ExprSet.choose_opt @@ ExprSet.filter search_expr entry.reachable_exprs with
              | Some e ->
                let () = incr covered_lines in
                let n = String.length l in
                let cols, cole = 0, n
                (* to highlight only the expression and not the whole line *)
                                   (*min 0 (get_pos_column @@ get_range_start e.erange),
                                     max (get_pos_column @@ get_range_end e.erange) n*) in
                Format.fprintf ocf "%s\027[38;5;%dm%s\027[0m%s@."
                  (String.sub l 0 cols)
                  (List.assoc "green" Debug.colors)
                  (String.sub l cols (cole-cols))
                  (String.sub l cole (n-cole))
              | None ->
                begin match ExprSet.choose_opt @@ ExprSet.filter search_expr entry.always_bottom_exprs with
                  | Some e ->
                    (* let () = Format.printf "always bottom: %a" pp_expr_with_range e in *)
                    let () = incr covered_lines in
                    let n = String.length l in
                    let cols, cole = 0, n
                      (*min 0 (get_pos_column @@ get_range_start e.erange),
                        max (get_pos_column @@ get_range_end e.erange) n*) in
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
            let () = Format.printf "Coverage of %s: %.2f%% (%d lines)@." filename (100. *. float_of_int  !covered_lines /. (float_of_int (lineno - 1))) !covered_lines in
            total_lineno := !total_lineno + !covered_lines;
            close_in file in
          process_file 1;
          flush oc;
        close_out oc
      ) table;
    Format.printf "In total, %d lines were analyzed@." !total_lineno
end

let () =
  Core.Hook.register_stateless_hook (module Hook)
