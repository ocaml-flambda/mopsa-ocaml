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

(** Profiler hook for C intra-procedural flows *)

open Mopsa
open Hook
open Ast
open Zone
open Universal.Ast
open Stubs.Ast

module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "c.hooks.profiler"

  let exec_zones = []
  let eval_zones = [(Z_c, Z_c_low_level); (Universal.Zone.Z_u, Z_any)]

  let init ctx = ()


  (** Command-line options for the sort key *)
  let opt_sort_key = ref "total"
  let () =
    register_builtin_option {
      key = "-profiler-sort-key";
      category = "Debugging";
      doc = " sort key of the profiler table";
      spec = ArgExt.Symbol (
          ["total"; "self"; "count"],
          (fun s -> opt_sort_key := s)
        );
      default = "total";
    }


  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec zone stmt man flow = ()
  let on_after_exec zone stmt man flow = ()


  (** Timing information of a function call *)
  type t = {
    total_timer : float; (** Timer for the amount of time spent in a function and its children  *)
    total : float; (** Amount of time spent in a function and its children  *)
    self_timer : float; (** Timer for the amount of time spent in a function, without considering its children *)
    self : float; (** Amount of time spent in a function, without considering its children *)
  }


  (** Stack of timing information *)
  let stack = Stack.create ()

  (** Suspend the self timer of the stack top element *)
  let suspend_self t =
    if Stack.is_empty stack
    then ()
    else
      let timing = Stack.pop stack in
      let timing = { timing with self = timing.self +. (t -. timing.self_timer) } in
      Stack.push timing stack

  (** Resume the self timer of the stack pop element *)
  let resume_self t =
    if Stack.is_empty stack
    then ()
    else
      let timing = Stack.pop stack in
      let timing = { timing with self_timer = t } in
      Stack.push timing stack


  (** Push a timing object of a new call to the stack *)
  let push_timing t =
    let timing = { total = 0.; total_timer = t; self = 0.; self_timer = t } in
    Stack.push timing stack


  (** Pop timing information from the stack *)
  let pop_timing t =
    let timing = Stack.pop stack in
    { timing with total = t -. timing.total_timer; self = t -. timing.self_timer }


  (** Statistics table *)
  let stats = Hashtbl.create 16


  (** Save a timing record in the statistics table *)
  let save_stat fname timing =
    Hashtbl.add stats fname timing


  (** Print the statistics table *)
  let print_stats () =
    let module M = MapExt.StringMap in
    let m = Hashtbl.fold (fun fname timing acc ->
        let total,self,nb = try M.find fname acc with Not_found -> 0.,0.,0 in
        M.add fname (total +. timing.total, self +. timing.self, nb + 1) acc
      ) stats M.empty
    in
    let sorted = M.bindings m |>
                List.sort (fun (_, (total, self, count)) (_, (total', self', count')) ->
                    match !opt_sort_key with
                    | "total" -> Float.compare total' total
                    | "self" -> Float.compare self' self
                    | "count" -> count' - count
                    | _ -> assert false
                  )
    in
    let open Format in
    printf "Function calls profiling:@.";
    List.iter (fun (fname, (total,self,nb)) ->
        printf "%s:@." fname;
        printf "  Total: %.4fs@." total;
        printf "  Self:  %.4fs@." self;
        printf "  Count: %7d@." nb;
      ) sorted


  (** Extract the function name from call expressions *)
  let get_function_name_opt exp =
    match ekind exp with
    | E_call({ ekind = E_function (User_defined {fun_name = fname}) }, _)
    | E_stub_call({ stub_func_name = fname}, _)
    | E_c_builtin_call(fname, _) ->
      Some fname

    | _ -> None


  (** Handle a call to a function *)
  let handle_call fname =
    let t = Sys.time () in
    suspend_self t;
    push_timing t


  (** Handle a return from a function *)
  let handle_return fname =
    let t = Sys.time () in
    let timing = pop_timing t in
    resume_self t;
    save_stat fname timing


  let on_before_eval zone exp man flow =
    get_function_name_opt exp |>
    Option.apply (fun fname ->
        handle_call fname
      ) ()


  let on_after_eval zone exp man eval =
    get_function_name_opt exp |>
    Option.apply (fun fname ->
        handle_return fname
      ) ()

  let on_finish man flow =
    print_stats ()


end

let () =
  register_stateless_hook (module Hook)
