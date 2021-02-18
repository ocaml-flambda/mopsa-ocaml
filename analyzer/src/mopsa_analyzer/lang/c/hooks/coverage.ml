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

(** Coverage - compute coverage statistics *)

open Mopsa
open Format
open Ast
open Stubs.Ast
open Universal.Ast


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "c.coverage"


  (** {2 Command-line options} *)
  (** ************************ *)

  (** Option to show a per-line coverage report *)
  let opt_per_line_coverage = ref false
  let () = register_builtin_option {
      key = "-c-show-line-coverage";
      category = "Coverage";
      doc = " turn on per-line coverage reporting";
      spec = ArgExt.Set opt_per_line_coverage;
      default = "";
    }


  (** {2 Function coverage} *)
  (** ********************* *)

  module RangeSet = SetExt.Make
      (struct
        type t = range
        let compare = compare_range
      end)

  type fun_cov = {
    fundec: c_fundec;
    total: RangeSet.t;
    reachable: RangeSet.t;
  }

  (** Initialize function coverage *)
  let init_fun_cov f : fun_cov option =
    match f.c_func_body with
    | None -> None
    | Some body ->
      (* Search atomic statements in the body of f *)
      let total = Visitor.fold_stmt
          (fun acc e -> VisitParts acc)
          (fun acc s ->
             match skind s with
             | S_block ([],_) -> Keep acc
             | S_if(cond,_,_)
             | S_while(cond,_)
             | S_c_do_while(_,cond)
             | S_c_for(_, Some cond, _, _)
             | S_c_switch(cond,_) ->
               VisitParts (RangeSet.add cond.erange acc)

             | S_assign _
             | S_expression _
             | S_c_break _
             | S_c_continue _
             | S_c_declaration _
             | S_c_return _
             | S_c_goto _ ->
               Keep (RangeSet.add s.srange acc)

             | _ ->
               VisitParts acc
          ) RangeSet.empty body
      in
      Some { fundec = f; total; reachable = RangeSet.empty }

  let update_fun_cov range fcov : fun_cov =
    if not (RangeSet.mem range fcov.total)
    then fcov
    else { fcov with reachable = RangeSet.add range fcov.reachable }


  (** {2 Program coverage} *)
  (** ******************** *)

  type prog_cov = (string,fun_cov) Hashtbl.t

  let cov : prog_cov = Hashtbl.create 16

  (** Initialize coverage table *)
  let init_cov functions : unit =
    Hashtbl.clear cov;
    List.iter
      (fun f ->
         match init_fun_cov f with
         | None -> ()
         | Some fcov -> Hashtbl.add cov f.c_func_unique_name fcov
      ) functions

  (** Update coverage table *)
  let update_cov (cs:callstack) range : unit =
    let c = callstack_top cs in
    match Hashtbl.find_opt cov c.call_fun_uniq_name with
    | None -> ()
    | Some fcov -> Hashtbl.replace cov c.call_fun_uniq_name (update_fun_cov range fcov)

  
  (** Print coverage statistics *)
  let print_cov_stats () =
    (* Get analyzed functions *)
    let analyzed_funs,total_funs,fcovs = Hashtbl.fold
        (fun f fcov (analyzed,total,fcovs) ->
           let analyzed',fcovs' =
             if not (RangeSet.is_empty fcov.reachable) then
               analyzed+1,fcov::fcovs
             else
               analyzed,fcovs
           in
           analyzed', total+1, fcovs'
        ) cov (0,0,[]) in
    (* Group coverage by file *)
    let map,total,reachable = List.fold_left
        (fun (map,total,reachable) fcov ->
           let file = get_range_file fcov.fundec.c_func_name_range |>
                      relative_path in
           let total' = total + RangeSet.cardinal fcov.total in
           let reachable' = reachable + RangeSet.cardinal fcov.reachable in
           let old = match MapExt.StringMap.find_opt file map with
             | None -> []
             | Some l -> l in
           let map' = MapExt.StringMap.add file (fcov::old) map in
           map',total',reachable'
        ) (MapExt.StringMap.empty,0,0) fcovs in
    (* Function to print a list of functions coverage in a file *)
    let pp_fcovs fmt l =
      printf "@[<v 2>  %a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt fcov ->
              let total = RangeSet.cardinal fcov.total in
              let reachable = RangeSet.cardinal fcov.reachable in
              fprintf fmt "'%a' %d%% of %d statement%a analyzed" (Debug.bold pp_print_string) fcov.fundec.c_func_org_name (if total=0 then 0 else 100*reachable/total) total Debug.plurial_int total;
              if !opt_per_line_coverage then
                fprintf fmt "@,@[<v 2>  %a@]"
                  (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                     (fun fmt range ->
                        fprintf fmt "%a %a"
                          (fun fmt -> function
                             | true -> Debug.color_str "green" fmt "✓"
                             | false -> Debug.color_str "red" fmt "✗"
                          ) (RangeSet.mem range fcov.reachable)
                          pp_relative_range range
                     )
                  ) (RangeSet.elements fcov.total)
              else
                ()
           )
        ) l in
    printf "%d function%a analyzed out of %d@\n" analyzed_funs Debug.plurial_int analyzed_funs total_funs;
    printf "%d%% of %d statement%a analyzed@\n@\n" (if total=0 then 0 else 100*reachable/total) total  Debug.plurial_int total;
    (* Group by file if more than one file analyzed *)
    let () = match MapExt.StringMap.bindings map with
      | [] -> assert false
      | [_,fcovs] -> printf "%a" pp_fcovs fcovs
      | l ->
        printf "@[<v>%a@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
             (fun fmt (file,fcovs) ->
                fprintf fmt "@[<v 2>%a:@,%a@]" (Debug.bold pp_print_string) file pp_fcovs fcovs
             )
          ) l
    in
    printf "@\n"

    


  (** {2 Initialization} *)
  (** ****************** *)

  let init ctx =
    (* Initialize coverage table *)
    let prog = find_ctx c_program_ctx ctx in
    init_cov prog.c_functions


  (** {2 Events handlers} *)
  (** ******************* *)

  let process_range range man flow =
    let cs = Flow.get_callstack flow in
    if is_empty_callstack cs then () else
    if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then ()
    else update_cov cs range

  let on_before_exec route stmt man flow =
    process_range stmt.srange man flow

  let on_after_exec route stmt man flow post = ()

  let on_before_eval route semantic exp man flow =
    process_range exp.erange man flow

  let on_after_eval route semantic exp man flow evl = ()

  let on_finish man flow =
    print_cov_stats ()

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
