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
open Ast


let name = "universal.iterators.interproc.common"
let debug fmt = Debug.debug ~channel:name fmt

(** Option to limit recursion depth *)
let opt_recursion_limit = ref 2

let () = register_domain_option name {
    key = "-recursion-limit";
    doc = "Limit of recursive calls";
    category = "Interprocedural Analysis";
    spec = ArgExt.Set_int opt_recursion_limit;
    default = string_of_int !opt_recursion_limit;
  }

let opt_rename_local_variables_on_recursive_call : bool ref = ref true

let () =
  register_shared_option (name ^ ".renaming") {
    key = "-disable-var-renaming-recursive-call";
    category = "Interprocedural Analysis";
    doc = " disable renaming of local variables when detecting recursive calls";
    spec = ArgExt.Clear opt_rename_local_variables_on_recursive_call;
    default = ""
  }


(******************)
(** Trace markers *)
(******************)

type marker += M_return of range

let () = register_marker {
    marker_name = (fun next -> function
        | M_return _ -> "return"
        | m          -> next m
      );
    marker_print = (fun next fmt -> function
        | M_return(range) ->
          Format.fprintf fmt "return@@%a" pp_relative_range range
        | m ->
          next fmt m
      );
    marker_compare = (fun next m1 m2 ->
        match m1, m2 with
        | M_return(r1), M_return(r2) ->
          compare_range r1 r2
        | _ ->
          next m1 m2
      );
  }

(** {2 Return flow token} *)
(** ===================== *)

type token +=
  | T_return of range
  (** [T_return(l)] represents flows reaching a return statement at
      location [l] *)

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_return(r1), T_return(r2) -> compare_range r1 r2
        | _ -> next tk1 tk2
      );
    print = (fun next fmt -> function
        | T_return(r) -> Format.fprintf fmt "return[%a]@" pp_range r
        | tk -> next fmt tk
      );
  }


(** {2 Return variable} *)
(** =================== *)


(** Return variable of a function call *)
type var_kind += V_return of expr (* call expression *)

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
          Compare.pair compare_expr compare_range
            (e1, e1.erange)
            (e2, e2.erange)
        | _ -> next v1 v2
      );
  }

(** Constructor of return variables *)
let mk_return call =
  let uniq_name = Format.asprintf "ret(%a)@@%a" pp_expr call pp_range call.erange in
  mkv uniq_name (V_return call) call.etyp



(** {2 Contexts to keep return variable} *)
(** =================================== *)

module ReturnKey = GenContextKey(
  struct
    type 'a t = var
    let print pp fmt v =
      Format.fprintf fmt "Return variable: %a" pp_var v
  end
  )

let return_key = ReturnKey.key

let get_last_call_site flow =
  let cs = Flow.get_callstack flow in
  let hd, _ = pop_callstack cs in
  hd.call_range


(** {2 Ignore recursion assumption} *)
(** =============================== *)

type assumption_kind += A_ignore_recursion_side_effect of string

let () = register_assumption {
    print = (fun next fmt -> function
        | A_ignore_recursion_side_effect f ->
          Format.fprintf fmt "ignoring side effects of recursive call to '%a'"
            (Debug.bold Format.pp_print_string) f
        | a -> next fmt a
      );
    compare = (fun next a1 a2 ->
        match a1,a2 with
        | A_ignore_recursion_side_effect f1, A_ignore_recursion_side_effect f2 ->
          compare f1 f2
        | _ -> next a1 a2
      );
  }

(** {2 Recursion checks} *)
(** ==================== *)

(** Check that no recursion is happening *)
let check_recursion f_orig f_uniq range cs =
  let site = {call_fun_orig_name=f_orig; call_fun_uniq_name=f_uniq; call_range=range} in
  let rec iter i = function
    | [] -> false
    | site'::tl ->
      if compare_callsite site site' = 0 then
        if i < !opt_recursion_limit then
          iter (i + 1) tl
        else
          true
      else
        iter i tl
  in
  iter 0 cs

let check_nested_calls f cs =
  if cs = [] then false
  else List.exists (fun call -> call.call_fun_uniq_name = f) (List.tl cs)


(** {2 Function inlining} *)
(** ===================== *)


(** Initialize function parameters *)
let init_fun_params f args range man flow =
  (* Update the call stack *)
  let flow = Flow.push_callstack f.fun_orig_name ~uniq:f.fun_uniq_name range flow in
  let init_range = tag_range f.fun_range "init" in

  if f.fun_parameters = [] then
    [], f.fun_locvars, f.fun_body, Post.return flow
  else
  if !opt_rename_local_variables_on_recursive_call &&
     check_nested_calls f.fun_uniq_name (Flow.get_callstack flow)
  then
    begin
      debug "nested calls detected on %s, performing parameters and locvar renaming" f.fun_orig_name;
      (* Add parameters and local variables to the environment *)
      let add_range = (fun p -> mk_attr_var p (Format.asprintf "%a" pp_range range) p.vtyp) in

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
      (* FIXME: the sub-expressions of arg have a range in the caller
         body. Since we have updated the callstack, we should be now
         in the callee body. We need a way to rewrite the ranges in
         arg! *)
      let parameters_assign = List.rev @@ List.fold_left (fun acc (param, arg) ->
          mk_assign (mk_var param init_range) arg init_range ::
          mk_add_var param init_range :: acc
        ) [] (List.combine fun_parameters args) in

      let init_block = mk_block parameters_assign init_range in


      (* Execute body *)
      fun_parameters, fun_locvars, new_body, man.exec init_block flow

    end
  else
    begin
      (* Assign arguments to parameters *)
      (* FIXME: the sub-expressions of arg have a range in the caller
         body. Since we have updated the callstack, we should be now
         in the callee body. We need a way to rewrite the ranges in
         arg! *)
      let parameters_assign = List.rev @@ List.fold_left (fun acc (param, arg) ->
          mk_assign (mk_var param init_range) arg init_range ::
          mk_add_var param init_range :: acc
        ) [] (List.combine f.fun_parameters args) in

      let init_block = mk_block parameters_assign init_range in

      (* Execute body *)
      f.fun_parameters, f.fun_locvars, f.fun_body, man.exec init_block flow
    end


(** Execute function body and save the return value *)
let exec_fun_body f body ret range man flow =
  (* Save the return variable in the context and backup the old one *)
  let oldreturn, flow1 =
    match ret with
    | None -> None, flow
    | Some ret ->
      (try Some (find_ctx return_key (Flow.get_ctx flow)) with Not_found -> None),
      Flow.set_ctx (add_ctx return_key ret (Flow.get_ctx flow)) flow in

  (* Clear all return flows *)
  let flow2 = Flow.filter (fun tk env ->
      match tk with
      | T_return _ -> false
      | _ -> true
    ) flow1
  in

  (* Execute the body of the function *)
  let post2 = man.exec body flow2 in

  (* Restore return and callstack contexts *)
  let post3 = match oldreturn with
    | None -> post2
    | Some ret ->
      Cases.set_ctx
        (add_ctx return_key ret (Cases.get_ctx post2)) post2 in

  (* Restore call stack *)
  let _,cs = Cases.get_callstack post3 |>
             Callstack.pop_callstack in
  let post4 = Cases.set_callstack cs post3 in

  post4 >>% fun flow3 ->

  (* Copy the new context and report from flow3 to original flow flow1 *)
  let flow4 = Flow.copy_ctx flow3 flow1 |> Flow.copy_report flow3 in

  (* Cut the T_cur flow *)
  let flow4 = Flow.remove T_cur flow4 in

  (* Retrieve non-cur/return flows in flow3 and put them in flow4 *)
  let flow5 =
    Flow.fold
      (fun acc tk env ->
         match tk with
         | T_cur | T_return _ -> acc
         | _                  -> Flow.add tk env man.lattice acc
      )
      flow4 flow3
  in

(* Create a separate post-state for each return flow in flow3 *)
  let postl =
    Flow.fold (fun acc tk env ->
        match tk with
        | T_cur | T_return _ ->
          let flow = Flow.set T_cur env man.lattice flow5 in
          Post.return flow :: acc

        | _ -> acc
      )
      [] flow3
  in

  Cases.join_list postl ~empty:(fun () -> Post.return flow5)

(** Inline a function call *)
let inline f params locals body ret range man flow =
  let post =
    if check_recursion f.fun_orig_name f.fun_uniq_name range (Flow.get_callstack flow)
    then
      let flow =
        Flow.add_local_assumption
          (A_ignore_recursion_side_effect f.fun_orig_name)
          range flow
      in
      match ret with
      | None -> Post.return flow
      | Some v ->
        man.exec (mk_add_var v range) flow >>%
        man.exec (mk_assign (mk_var v range) (mk_top v.vtyp range) range)
    else
      exec_fun_body f body ret range man flow >>%
      (* Remove local variables from the environment. Remove of parameters is
         postponed after finishing the statement, to keep relations between
         the passed arguments and the return value. *)
      man.exec (mk_block (List.map (fun v ->
          mk_remove_var v range
        ) locals) range)
  in
  post >>% fun flow ->
  match ret with
  | None ->
    Eval.singleton (mk_unit range) flow ~cleaners:(
      List.map (fun v ->
          mk_remove_var v range
        ) params
    )

  | Some v ->
    man.eval (mk_var v range) flow
    |> Cases.add_cleaners (
      mk_remove_var v range ::
      List.map (fun v ->
          mk_remove_var v range
        ) params
    )
