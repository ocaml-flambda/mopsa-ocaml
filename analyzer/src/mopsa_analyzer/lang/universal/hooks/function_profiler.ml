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


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "function_profiler"

  let debug fmt = Debug.debug ~channel:"function_profiler" fmt


  (** {2 Command-line options} *)
  (** ************************ *)

  (** Path of the output flame graph samples file *)
  let opt_flame_graph_path = ref ""

  let () = register_builtin_option {
      key = "-flamegraph";
      category = "Profiling";
      doc = " path where flame graphs samples are saved";
      spec = ArgExt.Set_string opt_flame_graph_path;
      default = "";
    }


  (** Resolution of the flame graph samples *)
  let opt_flame_graph_resolution = ref "ms"

  let () = register_builtin_option {
      key = "-flamegraph-resolution";
      category = "Profiling";
      doc = " resolution of the flame graph samples";
      spec = ArgExt.Symbol (["s"; "ms"; "us"; "ns"], (fun r -> opt_flame_graph_resolution := r));
      default = "ms";
    }



  (** {2 Timing records} *)
  (** ****************** *)

  (** Timing record of a function call *)
  type timing = {
    callstack : string list; (** Call stack *)
    time      : float;       (** Time spent in the function *)
  }


  (** Collection of past timing records *)
  let records : timing Queue.t = Queue.create ()

  (** Current timing record *)
  let cur : timing ref = ref { callstack = []; time = 0. }


  (** {2 Call stack observer} *)
  (** *********************** *)

  (** Update the state when a call is detected *)
  let call_detected (call:callsite) =
    (* First, stop the timer of previous call, save it in the queue
       and create a new timing record *)
    let t = Sys.time () in
    let timing = { !cur with time = t -. !cur.time } in
    Queue.push timing records;
    let timing = { callstack = call.call_fun_orig_name :: !cur.callstack ; time = t; } in
    cur := timing


  (** Update the state when a return is detected *)
  let return_detected () =
    (* Stop the timer of the returning call, save it in the queue and
       restore the timing record of the previous call *)
    let t = Sys.time () in
    let timing = { !cur with time = t -. !cur.time } in
    Queue.push timing records;
    let timing = { callstack = List.tl !cur.callstack ; time = t; } in
    cur := timing


  (** Observe the call stack and update the timing records *)
  let observe_callstack (cs:callstack) range =
    let depth = callstack_length cs in
    (* Decrease by 1 the current depth to take into account the first
       hidden %program call, added by init but not present in the call
       stack *)
    let cur_depth = List.length !cur.callstack - 1 in
    if depth = cur_depth then () else
    if depth = cur_depth + 1 then call_detected (callstack_top cs) else
    if depth = cur_depth - 1 then return_detected ()
    else panic_at range "unsupported call stack configuration: current = %d, previous = %d" cur_depth depth


  (** {2 Statistics} *)
  (** ************** *)

  (** Print a timing record as a flame graph sample *)
  let pp_timing_sample fmt t =
    let resolution = match !opt_flame_graph_resolution with
      | "s"  -> 1.0
      | "ms" -> 1000.0
      | "us" -> 1000000.0
      | "ns" -> 1000000000.0
      | _    -> assert false
    in
    Format.fprintf fmt "%a %d"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";") Format.pp_print_string) (List.rev t.callstack)
      (resolution *. t.time |> int_of_float)


  (** Export timing records as flame graph samples *)
  let export_flame_graph () =
    let o = open_out !opt_flame_graph_path in
    let fmt = Format.formatter_of_out_channel o in
    Queue.iter (fun t ->
        Format.fprintf fmt "%a@.%!" pp_timing_sample t
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
    printf "Functions profiling:@.";
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


  let on_before_exec route stmt man flow =
    observe_callstack (Flow.get_callstack flow) stmt.srange

  let on_after_exec route stmt man flow post = ()

  let on_before_eval route semantic exp man flow = ()

  let on_after_eval route semantic exp man flow eval =
    observe_callstack (Cases.get_ctx eval |> find_ctx Context.callstack_ctx_key) exp.erange

  let on_finish man flow =
    let t = Sys.time () in
    let timing = { !cur with time = t -. !cur.time } in
    Queue.push timing records;
    if !opt_flame_graph_path <> "" then export_flame_graph ();
    print_stats ()


end

let () =
  register_stateless_hook (module Hook)
