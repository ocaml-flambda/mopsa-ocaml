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

(** Hook to profile time spent in analyzing function calls *)

open Mopsa
open Hook
open Ast
open Zone


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "profiler"

  let exec_zones = [Z_any]
  let eval_zones = [Z_any, Z_any]

  let debug fmt = Debug.debug ~channel:"profiler" fmt


  (** {2 Command-line options} *)
  (** ************************ *)

  (** Export results as flame graph samples *)
  let opt_export_flame_graph = ref false

  let () = register_builtin_option {
      key = "-flamegraph";
      category = "Profiling";
      doc = " export profiling results as flame graph samples";
      spec = ArgExt.Set opt_export_flame_graph;
      default = "false";
    }

  (** Path to the output flame graph file *)
  let opt_flame_graph_output = ref "mopsa.log"

  let () = register_builtin_option {
      key = "-flamegraph-output";
      category = "Profiling";
      doc = " path to output flame graph data file";
      spec = ArgExt.Set_string opt_flame_graph_output;
      default = "mopsa.flamegraph.data";
    }

      

  (** {2 Timing records} *)
  (** ****************** *)

  (** Timing record of a function call *)
  type timing = {
    callstack : string list; (** Call stack *)
    time      : float;       (** Time spent in the function *)
  }

  let pp_timing fmt t =
    Format.fprintf fmt "%a %d"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";") Format.pp_print_string) (List.rev t.callstack)
      (1000000.0 *. t.time |> int_of_float)


  (** Collection of past timing records *)
  let records : timing Queue.t = Queue.create ()

  (** Current timing record *)
  let cur : timing ref = ref { callstack = []; time = 0. }


  (** {2 Call detection} *)
  (** ****************** *)

  (** A call is detected when the depth of the call stack increases *)
  let detect_call cs =
    let depth = Callstack.length cs in
    let cur_depth = List.length !cur.callstack - 1 in
    if depth = cur_depth then () else
    if depth = cur_depth + 1 then
      (* Call detected. First, stop the timer of previous call, save
         it in the queue and create a new timing record *)
      let t = Sys.time () in
      let timing = { !cur with time = t -. !cur.time } in
      Queue.push timing records;
      let call = Callstack.top cs in
      debug "call to %s, cur = %a, depth = %d, cur_depth = %d" call.call_fun pp_timing !cur depth cur_depth;
      let timing = { callstack = call.call_fun :: !cur.callstack ; time = t; } in
      cur := timing
    else panic "call detection: unsupported call stack configuration: current = %d, previous = %d" cur_depth depth


  (** A return is detected when the depth of the call stack is decreased *)
  let detect_return cs =
    let depth = Callstack.length cs in
    let cur_depth = List.length !cur.callstack - 1 in
    if depth = cur_depth then () else
    if depth = cur_depth - 1 then
      (* Return detected. Stop the timer of the returning call, save
         it in the queue and restore the timing record of the previous
         call *)
      let t = Sys.time () in
      debug "return from %s, cur = %a, depth = %d, cur_depth = %d" (List.hd !cur.callstack) pp_timing !cur depth cur_depth;
      let timing = { !cur with time = t -. !cur.time } in
      Queue.push timing records;
      let timing = { callstack = List.tl !cur.callstack ; time = t; } in
      cur := timing
    else panic "return detection: unsupported call stack configuration: current = %d, previous = %d" cur_depth depth


  (** {2 Statistics} *)
  (** ************** *)

  (** Export timing records as flame graph samples *)
  let export_flame_graph () =
    let o = open_out !opt_flame_graph_output in
    let fmt = Format.formatter_of_out_channel o in
    Queue.iter (fun t ->
        Format.fprintf fmt "%a@.%!" pp_timing t
      ) records

  (** Print the statistics table *)
  let print_stats () =
    let module FunStat = MapExt.StringMap in
    (* For each function compute:
       - The total time
       - The self time: time spent in statements of the function
       - The number of times the function was called 
    *)
    let total,self,times,_,longest_fname_length = Queue.fold (fun (total,self,times,last,longest_fname_length) timing ->
        let fname = List.hd timing.callstack in
        let longest_fname_length = if String.length fname > longest_fname_length then String.length fname else longest_fname_length in
        let self = FunStat.add fname (timing.time +. try FunStat.find fname self with Not_found -> 0.) self in
        let total = List.fold_left (fun total f ->
            FunStat.add f (timing.time +. try FunStat.find f total with Not_found -> 0.) total
          ) total timing.callstack 
        in
        let times =
          if List.length last < List.length timing.callstack then
            FunStat.add fname (1 + try FunStat.find fname times with Not_found -> 0) times
          else
            times
        in
        total,self,times,timing.callstack,longest_fname_length
      ) (FunStat.empty,FunStat.empty,FunStat.empty,[],0) records
    in
    (* Sort functions by the total time *)
    let sorted = FunStat.bindings total |>
                 List.sort (fun (_, total) (_, total') ->
                     compare total' total
                   )
    in
    let open Format in
    printf "Profiling:@.";
    List.iter (fun (fname, total) ->
        printf "%s%s   %.4fs(total)   %.4fs(self)   x%d@."
          fname
          (String.make (longest_fname_length - String.length fname) ' ')
          total
          (FunStat.find fname self)
          (FunStat.find fname times)
        ;
      ) sorted

  

  (** {2 Events handlers} *)
  (** ******************* *)

  let init ctx =
    let t = Sys.time () in
    let timing = { callstack = ["%program"]; time = t } in
    cur := timing


  let on_before_exec zone stmt man flow =
    detect_call (Flow.get_callstack flow)

  let on_after_exec zone stmt man post = ()
    
  let on_before_eval zone exp man flow = ()
    
  let on_after_eval zone exp man eval =
    detect_return (Eval.get_ctx eval |> Context.find_unit Callstack.ctx_key)

  let on_finish man flow =
    let t = Sys.time () in
    let timing = { !cur with time = t -. !cur.time } in
    Queue.push timing records;
    if !opt_export_flame_graph then export_flame_graph ();
    print_stats ()


end

let () =
  register_stateless_hook (module Hook)
