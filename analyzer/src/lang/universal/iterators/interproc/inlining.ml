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

(** Inter-procedural iterator by inlining.  *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone
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


(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "universal.iterators.interproc.inlining"
    end)


  (** Zoning definition *)
  let interface = {
    iexec = { provides = [Z_u]; uses = [] };
    ieval = { provides = [Z_u, Z_any]; uses = [] };
  }

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit Callstack.ctx_key Callstack.empty
    ) flow

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_return e ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (stmt.srange, e)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)

  let inline_function_assign_args man f args range flow =
    let cs = Callstack.get flow in
    if List.exists (fun cs -> cs.call_fun = f.fun_name) cs then
      Exceptions.panic_at range "Recursive call on function %s detected...@\nCallstack = %a@\n" f.fun_name Callstack.print cs;

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

    (* Update call stack *)
    let flow1 = Callstack.push f.fun_name range flow0 in

    (* Execute body *)
    new_vars, man.exec init_block flow1


  let inline_function_exec_body man f args range new_vars flow ret =
    (* Check that no recursion is happening *)

    debug "cs flow = %a@\n" Callstack.print (Callstack.get flow);
    let flow2 = man.exec f.fun_body flow in

    debug "cs flow2 = %a@\n" Callstack.print (Callstack.get flow2);
    (* Iterate over return flows and assign the returned value to ret *)
    let flow3 =
      Flow.fold (fun acc tk env ->
          match tk with
          | T_return(_, None) -> Flow.add T_cur env man.lattice acc

          | T_return(_, Some e) ->
            Flow.set T_cur env man.lattice acc |>
            man.exec (mk_add_var ret range) |>
            man.exec (mk_assign (mk_var ret e.erange) e e.erange) |>
            Flow.join man.lattice acc

          | _ -> Flow.add tk env man.lattice acc
        )
        (Flow.remove T_cur (Flow.copy_ctx flow2 flow))
        flow2
    in

    debug "cs flow3 = %a@\n" Callstack.print (Callstack.get flow3);
    (* Restore call stack *)
    let _, flow3 = Callstack.pop flow3 in

    (* Remove parameters and local variables from the environment *)
    let ignore_stmt_list =
      List.mapi (fun i v ->
          mk_remove_var v range
        ) (new_vars)
    in

    Eval.singleton (mk_var ret range) flow3 ~cleaners:(ignore_stmt_list @ [mk_remove_var ret range])


  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function (User_defined f)}, args) ->
      let ret_typ = match f.fun_return_type with None -> T_any | Some t -> t in
      let ret = mk_range_attr_var range "ret_var" ret_typ in
      let new_vars, flow = inline_function_assign_args man f args range flow in
      inline_function_exec_body man f args range new_vars flow ret
      |> Option.return

    | _ -> None

  let ask _ _ _ = None

end


let () =
  register_domain (module Domain)
