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

let name = "universal.iterators.interproc.common"
let debug fmt = Debug.debug ~channel:name fmt

let opt_continue_on_recursive_call : bool ref = ref true


let () =
  register_domain_option name {
    key = "-stop-rec";
    category = "Interprocedural Analysis";
    doc = "";
    spec = ArgExt.Clear opt_continue_on_recursive_call;
    default = " continue with top during recursive calls"
  }


(** {2 Return flow token} *)
(** ===================== *)

type token +=
  | T_return of range * bool
  (** [T_return(l, b)] represents flows reaching a return statement at
      location [l]. The boolean is true iff a return expression is present *)

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_return(r1, b1), T_return(r2, b2) ->
          (* we may return different things at one same location (for example due to disjunctions *)
          Compare.compose
            [ (fun () -> compare_range r1 r2);
              (fun () -> Stdlib.compare b1 b2);
            ]
        | _ -> next tk1 tk2
      );
    print = (fun next fmt -> function
        | T_return(r, b) -> Format.fprintf fmt "return[r = %a, b = %b@" pp_range r b
        | tk -> next fmt tk
      );
  }


(** {2 Return variable} *)
(** =================== *)


(** Return variable of a function call *)
type var_kind += V_return of expr

(** Registration of the kind of return variables *)
let () =
  register_var {
    print = (fun next fmt v ->
        match v.vkind with
        | V_return e -> Format.fprintf fmt "ret(%a)" pp_expr e
        | _ -> next fmt v
      );
    compare = (fun next v1 v2 ->
        match v1.vkind, v2.vkind with
        | V_return e1, V_return e2 ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_range e1.erange e2.erange);
          ]
        | _ -> next v1 v2
      );
  }

(** Constructor of return variables *)
let mk_return_var call =
  let uniq_name =
    let () = Format.fprintf Format.str_formatter "ret(%a)@@%a" pp_expr call pp_range call.erange in
    Format.flush_str_formatter ()
  in
  mkv uniq_name (V_return call) call.etyp



(** {2 Contexts to keep return variable} *)
(** =================================== *)

let return_key =
  let module K = Context.GenUnitKey(
    struct
      type t = var
      let print fmt v =
        Format.fprintf fmt "Return var: %a" pp_var v
    end
    )
  in
  K.key


let get_last_call_site flow =
  let cs = Flow.get_callstack flow in
  let hd, _ = Callstack.pop cs in
  hd.call_site

(** {2 Recursion checks} *)
(** ==================== *)


(** Check that no recursion is happening *)
let check_recursion f range flow =
  let cs = Flow.get_callstack flow in
  if cs = [] then false
  else
    List.exists (fun cs -> Callstack.compare_call cs {call_fun=f.fun_name; call_site=range} = 0) (List.tl cs)

let check_nested_calls f range flow =
  let cs = Flow.get_callstack flow in
  if cs = [] then false
  else List.exists (fun call -> call.call_fun = f) (List.tl cs)



(** {2 Function inlining} *)
(** ===================== *)


(** Initialize function parameters *)
let init_fun_params f args range man flow =
  (* Clear all return flows *)
  let flow0 = Flow.filter (fun tk env ->
      match tk with
      | T_return _ -> false
      | _ -> true
    ) flow
  in

  (* Update call stack *)
  let flow1 = Flow.push_callstack f.fun_name range flow0 in

  if check_nested_calls f.fun_name range flow1 then
    begin
      debug "nested calls detected on %s, performing parameters and locvar renaming" f.fun_name;
      (* Add parameters and local variables to the environment *)
      let add_range = (fun p -> mk_attr_var p (Format.fprintf Format.str_formatter "%a" pp_range range;
                                               Format.flush_str_formatter ()) p.vtyp) in

      let function_vars = f.fun_parameters @ f.fun_locvars in
      let fun_parameters = List.map add_range f.fun_parameters in
      let fun_locvars = List.map add_range f.fun_locvars in

      (* TODO: do this transformation only if we detect f in the callstack? That could work? *)
      let new_body = Visitor.map_stmt (fun e -> match ekind e with
          | E_var (v, m) when List.exists (fun v' -> compare_var v v' = 0) function_vars ->
            Keep {e with ekind = E_var(add_range v, m)}
          | _ -> VisitParts e) (fun s -> VisitParts s) f.fun_body in
      debug "moved body from:%a@\nto %a@\n" pp_stmt f.fun_body pp_stmt new_body;

      (* Assign arguments to parameters *)
      let parameters_assign = List.rev @@ List.fold_left (fun acc (param, arg) ->
          mk_assign (mk_var param range) arg range ::
          mk_add_var param range :: acc
        ) [] (List.combine fun_parameters args) in

      let init_block = mk_block parameters_assign range in


      (* Execute body *)
      fun_parameters, fun_locvars, new_body, man.exec init_block flow1

    end
  else
    begin
      (* Assign arguments to parameters *)
      let parameters_assign = List.rev @@ List.fold_left (fun acc (param, arg) ->
          mk_assign (mk_var param range) arg range ::
          mk_add_var param range :: acc
        ) [] (List.combine f.fun_parameters args) in

      let init_block = mk_block parameters_assign range in

      (* Execute body *)
      f.fun_parameters, f.fun_locvars, f.fun_body, man.exec init_block flow1
    end


(** Execute function body and save the return value *)
let exec_fun_body f body ret range man flow =
  (* Save the return variable in the context and backup the old one *)
  let oldreturn, flow1 =
    match ret with
    | None -> None, flow
    | Some ret ->
      (try Some (Context.find_unit return_key (Flow.get_ctx flow)) with Not_found -> None),
      Flow.set_ctx (Context.add_unit return_key ret (Flow.get_ctx flow)) flow in

  (* Execute the body of the function *)
  let flow2 = man.exec body flow1 in

  (* Copy the new context and alarms from flow2 to original flow flow1 *)
  let flow3 = Flow.copy_ctx flow2 flow1 |> Flow.copy_alarms flow2 in

  (* Cut the T_cur flow *)
  let flow3 = Flow.remove T_cur flow3 in

  (* Restore return and callstack contexts *)
  let flow4 = match oldreturn with
    | None -> flow2
    | Some ret -> Flow.set_ctx
                    (Context.add_unit return_key ret (Flow.get_ctx flow3)) flow3 in

  (* Restore call stack *)
  let _, flow5 = Flow.pop_callstack flow4 in

  (* Retrieve non-return flows *)
  let flow6 =
    Flow.fold
      (fun acc tk env ->
         match tk with
         | T_return _ -> acc
         | _ -> Flow.add tk env man.lattice acc
      )
      flow5 flow2
  in
  
  (* Separate different return flows into separate post-states *)
  let postl =
    Flow.fold (fun acc tk env ->
        match tk with
        | T_return _ ->
          let flow = Flow.set T_cur env man.lattice flow6 in
          Post.return flow :: acc

        | _ -> acc
      )
      [] flow2
  in

  Post.join_list postl ~empty:(fun () -> Post.return flow6)


(** Inline a function call *)
let inline f params locals body ret range man flow =
  let post =
    match check_recursion f range flow with
    | true ->
      begin
        Soundness.warn_at range
          "recursive call on function %s ignored" f.fun_name
        ;
        match ret with
        | None -> Post.return flow
        | Some v ->
          if !opt_continue_on_recursive_call then
            man.exec (mk_add_var v range) flow |>
            man.exec (mk_assign (mk_var v range) (mk_top v.vtyp range) range) |>
            Post.return
          else
            panic_at range "recursive call on function %s" f.fun_name
      end

    | false ->
      exec_fun_body f body ret range man flow >>$ fun () flow ->
      (* Remove local variables from the environment. Remove of parameters is
         postponed after finishing the statement, to keep relations between
         the passed arguments and the return value. *)
      man.exec (mk_block (List.map (fun v ->
          mk_remove_var v range
        ) locals) range) flow |>
      Post.return
  in
  post >>$ fun () flow ->
  match ret with
  | None ->
    Eval.singleton (mk_unit range) flow

  | Some v ->
    Eval.singleton (mk_var v range) flow ~cleaners:(
      mk_remove_var v range ::
      List.map (fun v ->
          mk_remove_var v range
        ) params
    )
