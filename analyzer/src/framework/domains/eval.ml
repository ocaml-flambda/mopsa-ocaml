(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Utility functions for [eval] transfer functions. *)

open Flow
open Manager
open Option


(*==========================================================================*)
(**                       {2 Optional evaluations}                          *)
(*==========================================================================*)

let return x = Some x

let singleton (evc: 'a eval_case) : 'a eval_output option =
  Some (eval_singleton evc)

let map
    (f: 'a eval_case -> 'b eval_case)
    (oevl: 'a eval_output option) : 'b eval_output option
  =
  option_lift1 (eval_map f) oevl


let apply2
    (f1: 'a eval_output -> 'c)
    (f2: 'b eval_output -> 'c)
    (f12: 'a eval_output -> 'b eval_output -> 'c)
    (none: unit -> 'c)
    (oevl1: 'a eval_output option) (oevl2: 'b eval_output option) : 'c =
  option_apply2 f1 f2 f12 none oevl1 oevl2

let collapse
    (f: 'a eval_case -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (none: unit -> 'b)
    (oevl: 'a eval_output option) : 'b
  =
  option_dfl1 none (Dnf.substitute f join meet) oevl


let substitute
    (f: 'a eval_case -> 'b eval_output option)
    (oevl: 'a eval_output option) : 'b eval_output option =
  collapse f
    (apply2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_join evl1 evl2))
       (fun () -> None)
    )
    (apply2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_meet evl1 evl2))
       (fun () -> None)
    )
    (fun () -> None)
    oevl


let flatten (oevl: 'a eval_output option) : ('a eval_case) list =
  option_dfl1 (fun () -> []) eval_flatten oevl

let join
    (oevl1: 'a eval_output option)
    (oevl2: 'a eval_output option) : 'a eval_output option
  =
  option_neutral2 eval_join oevl1 oevl2

let meet
    (oevl1: 'a eval_output option)
    (oevl2: 'a eval_output option) : 'a eval_output option
  =
  option_neutral2 eval_meet oevl1 oevl2

(*==========================================================================*)
                    (** {2 Combination with [eval]} *)
(*==========================================================================*)

(**
   [compose_eval e eval empty man ctx flow] performs a chain composition
   of an evaluator [eval] after the top-level evaluator [man.eval].
   In other words, it evaluates an expression [e] using [man.eval], then it
   applies the argument function [eval] on each result.
   [empty] is called when [man.eval] returns an empty expression.
*)
let compose_eval exp eval empty man ctx flow : 'a flow eval_output option =
  Some (man.eval exp ctx flow) |>
  substitute
    (fun (exp', flow, clean) ->
       let evl =
         match exp' with
         | Some exp' -> eval exp' flow
         | None -> empty flow
       in
       (* we ensure that we always return an evaluation *)
       match evl with
       | None -> None
       | Some evl ->
         eval_map (fun (exp'', flow, clean') ->
             (exp'', flow, clean @ clean')
           ) evl |>
         return
    )

(**
   [compose_eval_list el eval empty man ctx flow] evaluates a list 
   of expressions [el] in chain and applies the argument function [eval] 
   on each combination of the results.
   [empty] is called when [man.eval] returns an empty expression.
*)
let compose_eval_list expl eval empty (man: ('a, 'b) manager) ctx flow
  : 'a flow eval_output option =
  let rec aux expl flow clean = function
    | [] ->
      eval (List.rev expl) flow |>
      option_lift1 (
        eval_map (fun (exp, flow, clean') ->
            (exp, flow, clean @ clean')
          )
      )
    | exp :: tl ->
      Some (man.eval exp ctx flow) |>
      substitute
        (fun (exp', flow, clean') ->
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty flow
        )
  in
  aux [] flow [] expl


(*==========================================================================*)
(**                       {2 Combination with [exec]}                       *)
(*==========================================================================*)

(* Execute post-eval clean statements pushed by evaluators.*)
let exec_cleaner_stmts stmtl man ctx flow =
  stmtl |> List.fold_left (fun acc stmt ->
      man.exec stmt ctx acc
    ) flow

(**
   [compose_exec e f empty man ctx flow] evaluates an expression [e], executes
   the transfer function [f] on the result, and applies the cleaner
   statements pushed by the evaluations.
   [empty] is called when [man.eval e] returns an empty expression.
*)
let compose_exec exp f empty man ctx flow =
  Some (man.eval exp ctx flow) |>
  collapse
    (fun (exp', flow, clean) ->
       let flow' =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty @@ exec_cleaner_stmts clean man ctx flow
       in
       match flow' with
       | None -> None
       | Some flow' ->
         Some (exec_cleaner_stmts clean man ctx flow')
    )
    (fun a b ->
       match a, b with
       | None, x | x, None -> x
       | Some a, Some b -> Some (man.flow.join a b)
    )
    (fun a b ->
       match a, b with
       | None, x | x, None -> x
       | Some a, Some b -> Some (man.flow.meet a b)
    )
    (fun () -> None)

(**
   Same as [compose_exec] but folded on a list of expressions.
*)
let compose_exec_list expl f empty man ctx flow =
  let rec aux expl flow clean = function
    | [] ->
      begin
        let flow = f (List.rev expl) flow in
        match flow with
        | None -> None
        | Some flow ->
          Some (exec_cleaner_stmts clean man ctx flow)
      end
    | exp :: tl ->
      Some (man.eval exp ctx flow) |>
      collapse
        (fun (exp', flow, clean') ->
           match exp' with
           | None -> empty @@ exec_cleaner_stmts clean man ctx flow
           | Some exp' -> aux (exp' :: expl) flow (clean @ clean') tl
        )
        (fun a b ->
           match a, b with
           | None, x | x, None -> x
           | Some a, Some b -> Some (man.flow.join a b)
        )
        (fun a b ->
           match a, b with
           | None, x | x, None -> x
           | Some a, Some b -> Some (man.flow.meet a b)
        )
        (fun () -> None)

  in
  aux [] flow [] expl


(*==========================================================================*)
(**                     {2 Top-level re-evaluator}                          *)
(*==========================================================================*)

(**
   [re_eval_singleton man ctx evc] re-evaluates a singleton evaluation case
   [evc], starting from the top-level domain.
   The cleaner statements of the new evaluations are automatically added to
   the result.
*)
let re_eval_singleton man ctx (exp, flow, clean) =
  match exp with
  | None -> singleton (exp, flow, clean)
  | Some exp ->
    man.eval exp ctx flow |>
    eval_map
      (fun (exp', flow', clean') ->
         (exp', flow', clean @ clean')
      ) |>
    return

(**
   Same as [re_eval_singleton], but applied to a set of previous evaluation
   results.
*)
let re_eval_all man ctx evals =
  substitute (re_eval_singleton man ctx) evals


(*==========================================================================*)
(**                           {2 Conditionals}                              *)
(*==========================================================================*)

let if_eval
    (true_cond: 'a flow)
    (false_cond: 'a flow)
    (true_branch: 'a flow -> 'a flow eval_output option)
    (false_branch: 'a flow -> 'a flow eval_output option)
    (bottom_branch: unit -> 'a flow eval_output option)
    man flow
  : 'a flow eval_output option =
  if_flow
    true_cond false_cond
    true_branch false_branch bottom_branch
    (fun true_flow false_flow -> join (true_branch true_flow) (false_branch false_flow))
    man flow
