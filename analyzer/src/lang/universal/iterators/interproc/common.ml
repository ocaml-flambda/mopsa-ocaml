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

(** Common transfer functions for handling function calls *)


open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Callstack


(** {2 Return flow token} *)
(** ===================== *)

type token +=
  | T_return of range * expr option
  (** [T_return(l, ret)] represents flows reaching a return statement at
      location [l]. The option expression [ret] keeps the returned expression
      if present. *)

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_return(r1, _), T_return(r2, _) -> compare_range r1 r2
        | _ -> next tk1 tk2
      );
    print = (fun next fmt -> function
        | T_return(r, Some e) -> Format.fprintf fmt "return %a" pp_expr e
        | T_return(r, None) -> Format.fprintf fmt "return"
        | tk -> next fmt tk
      );
  }


(** {2 Function inlining} *)
(** ===================== *)

(** Check that no recursion is happening *)
let check_recursion f flow =
  let cs = Flow.get_callstack flow in
  List.exists (fun cs -> cs.call_fun = f.fun_name) cs

  

(** Initialize function parameters *)
let init_fun_params f args range man flow =
  (* Clear all return flows *)
  let flow0 = Flow.filter (fun tk env ->
      match tk with
      | T_return _ -> false
      | _ -> true
    ) flow
  in

  (* Add parameters and local variables to the environment *)
  let new_vars = f.fun_parameters @ f.fun_locvars in

  (* Assign arguments to parameters *)
  let parameters_assign = List.mapi (fun i (param, arg) ->
      mk_assign (mk_var param range) arg range
    ) (List.combine f.fun_parameters args) in

  let init_block = mk_block parameters_assign range in

  (* Execute body *)
  new_vars, man.exec init_block flow0



(** Execute function body and save the return value *)
let exec_fun_body f ret range man flow =
  (* Update call stack *)
  let flow1 = Flow.push_callstack f.fun_name range flow in

  (* Execute the body of the function *)
  let flow2 = man.exec f.fun_body flow1 in

  (* Iterate over return flows and assign the returned value to ret *)
  let flow3 =
    Flow.fold (fun acc tk env ->
        match tk with
        | T_return(_, None) ->
          Flow.add T_cur env man.lattice acc

        | T_return(_, Some e) ->
          Flow.set T_cur env man.lattice acc |>
          man.exec (mk_add_var ret range) |>
          man.exec (mk_assign (mk_var ret e.erange) e e.erange) |>
          Flow.join man.lattice acc

        | _ ->
          Flow.add tk env man.lattice acc
      )
      (Flow.copy_ctx flow2 flow1 |> Flow.copy_alarms flow2 |> Flow.remove T_cur)
      flow2
  in

  (* Restore call stack *)
  let _, flow4 = Flow.pop_callstack flow3 in
  flow4


(** Inline a function call *)
let inline f params ret range man flow =
  let flow =
    match check_recursion f flow with
    | true ->
      Soundness.warn_at range
        "recursive call on function %s ignored" f.fun_name
      ;
      man.exec (mk_add_var ret range) flow |>
      man.exec (mk_assign (mk_var ret range) (mk_top ret.vtyp range) range)

    | false ->
      exec_fun_body f ret range man flow
  in
  (* Remove parameters and local variables from the environment *)
  let ignore_stmt_list =
    List.map (fun v ->
        mk_remove_var v range
      ) params
  in

  Eval.singleton (mk_var ret range) flow ~cleaners:(ignore_stmt_list @ [mk_remove_var ret range])



