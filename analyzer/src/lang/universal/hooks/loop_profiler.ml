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

(** Hook to profile loops iterations *)

open Mopsa
open Hook
open Ast


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "loop_profiler"

  let debug fmt = Debug.debug ~channel:"loop_profiler" fmt


  (** {2 Profiling records} *)
  (** ********************* *)

  (** A loop is identified by its range and its body. The information of body
      is necessary to compute the number of iterations *)
  type loop = {
    range: range;
    body: stmt;
  }

  module LoopMap = MapExt.Make
      (struct
        type t = loop
        let compare l1 l2 = compare_range l1.range l2.range
      end)

  (** For each loop, and for each time we hit the loop, we compute the number
      of iterations until we exit the loop *)
  type iterations = int list

  (** Statistics table mapping loops to iterations stats *)
  let stats : iterations LoopMap.t ref = ref LoopMap.empty

  (** Print statistics table *)
  let print fmt () =
    Format.fprintf fmt "Loops profiling:@.";
    LoopMap.iter
      (fun loop iterations ->
         let sum = List.fold_left (+) (0) iterations in
         let nb = List.length iterations in
         let avg = (float_of_int sum) /. (float_of_int nb) in
         Format.fprintf fmt "  %a: %d time%a, %.2f avg. iterations (%a)@."
           pp_relative_range loop.range
           nb Debug.plurial_int nb
           avg
           Format.(pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_print_int) iterations
      ) !stats

  (** A frame in the stack of still-executing loops, for tracking the number
      iterations *)
  type frame = {
    loop: loop;
    iterations: int;
    inside: bool; (* Flag indicating whether we are inside the loop body *)
  }

  (** Stack of still-executing loops *)
  let stack : frame list ref = ref []

  (** Check whether a statement is the body of the top loop *)
  let is_loop_body stmt =
    match !stack with
    | [] -> false
    | hd::_ -> hd.loop.body == stmt

  (** Event raised when we enter a loop *)
  let on_enter_loop range body =
    if is_loop_body body then ()
    else
      let frame = { loop = {range;body};
                    iterations = 0;
                    inside = false; } in
      stack := frame :: !stack

  (** Event raised when we start a new iteration *)
  let on_enter_body () =
    match !stack with
    | hd::tl when not hd.inside ->
      stack := { hd with iterations = hd.iterations + 1; inside = true} :: tl
    | _ -> ()

  (** Event raised when we terminate an iteration *)
  let on_exit_body () =
    match !stack with
    | hd::tl when hd.inside ->
      stack := { hd with inside = false} :: tl
    | _ -> ()

  (** Event raised when we exit a loop *)
  let on_exit_loop range body =
    match !stack with
    | hd::tl when is_loop_body body ->
      (* Save the number of iterations in the stats table *)
      let old = try LoopMap.find hd.loop !stats with Not_found -> [] in
      stats := LoopMap.add hd.loop (hd.iterations::old) !stats;
      (* Pop the frame from the stack *)
      stack := tl

    | _ -> ()


  (** {2 Events handlers} *)
  (** ******************* *)

  let init ctx = ()

  let on_before_exec route stmt man flow =
    match skind stmt with
    | S_while(_,body) ->
      on_enter_loop stmt.srange body

    | _ when is_loop_body stmt ->
      on_enter_body ()

    | _ -> ()

  let on_after_exec route stmt man flow post =
    match skind stmt with
    | S_while (_,body) ->
      on_exit_loop stmt.srange body

    | _ when is_loop_body stmt ->
      on_exit_body ()

    | _ -> ()

  let on_before_eval route exp man flow = ()

  let on_after_eval route exp man flow eval = ()

  let on_finish man flow =
    print Format.std_formatter ()

end

let () =
  register_stateless_hook (module Hook)
