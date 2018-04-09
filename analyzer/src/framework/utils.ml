open Ast
open Pp
open Flow
open Manager
open Domains.Global

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

module Total_var_map = Lattices.Total_map.Make(Var)

let return x = Some x

let fail = None

(*==========================================================================*)
(**                         {2 Utility functions}                           *)
(*==========================================================================*)

let if_flow
    (true_cond: 'a flow -> 'a flow)
    (false_cond: 'a flow -> 'a flow)
    (true_branch: 'a flow -> 'b)
    (false_branch: 'a flow -> 'b)
    (bottom_branch: unit -> 'b)
    (merge: 'a flow -> 'a flow -> 'b)
    man flow
  : 'b =
  let true_flow = true_cond flow and false_flow = false_cond flow in
  debug "true cond:@\n  @[%a@]@\nfalse cond:@\n  @[%a@]"
    man.flow.print true_flow
    man.flow.print false_flow
  ;
  match man.flow.is_cur_bottom true_flow, man.flow.is_cur_bottom false_flow with
  | false, true -> debug "true branch"; true_branch true_flow
  | true, false -> debug "true branch"; false_branch false_flow
  | false, false -> debug "merge branch"; merge true_flow false_flow
  | true, true -> debug "bottom branch"; bottom_branch ()

let if_flow_eval
    true_flow false_flow
    true_case false_case man flow
    ?(bottom_case=(fun () -> oeval_singleton (None, flow, [])))
    ?(merge_case=(fun flow1 flow2 -> oeval_join (true_case flow1) (false_case flow2)))
    () =
  if_flow true_flow false_flow true_case false_case bottom_case merge_case man flow

let eval_compose
    (eval: 'a -> 'b flow -> ('c, 'd) evals option)
    ?(empty = (fun flow -> oeval_singleton (None, flow, [])))
    (evl: ('a, 'b) evals)
  : ('c, 'd) evals option  =
  Some evl |> oeval_substitute
    (fun (x, flow, clean) ->
       let oevl' =
         match x with
         | Some x -> eval x flow
         | None -> empty flow
       in
       oeval_map
         (fun (x', flow, clean') ->
            (x', flow, clean @ clean')
         ) oevl'
    )

let oeval_compose eval ?(empty = (fun flow -> oeval_singleton (None, flow, []))) evl =
  match evl with
  | None -> None
  | Some evl -> eval_compose eval ~empty evl

let eval_list
    (l: 'a list)
    (eval: 'a -> 'b flow -> ('c, 'b) evals option)
    ?(empty = (fun flow -> oeval_singleton (None, flow, [])))
    (flow: 'b flow)
  : ('c list, 'b) evals option =
  let rec aux expl flow clean = function
    | [] ->
      oeval_singleton (Some (List.rev expl), flow, clean)
    | exp :: tl ->
      eval exp flow |>
      oeval_substitute
        (fun (exp', flow, clean') ->
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty flow
        )
  in
  aux [] flow [] l

let man_eval_list
    (l: 'a list)
    ?(empty = (fun flow -> oeval_singleton (None, flow, [])))
    man ctx (flow: 'b flow)
  : ('a list, 'b) evals option =
  eval_list l (fun exp flow -> Some (man.eval exp ctx flow)) ~empty flow

(**
   [re_eval_singleton ev eval] re-evaluates a singleton evaluation case
   [ev], starting from the top-level domain.
   The cleaner statements of the new evaluations are automatically added to
   the result.
*)
let re_eval_singleton (exp, flow, clean) man ctx =
  match exp with
  | None -> oeval_singleton (exp, flow, clean)
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
let re_eval evals man ctx =
  oeval_substitute (fun case -> re_eval_singleton case man ctx) evals



(* Execute post-eval clean statements pushed by evaluators.*)
let exec_cleaner_stmts stmtl man ctx flow =
  stmtl |> List.fold_left (fun acc stmt ->
      man.exec stmt ctx acc
    ) flow

(**
   [eval_to_exec f man ctx evl] executes the transfer function [f] 
   on a set of evaluations [evl], and applies the cleaner
   statements pushed by the evaluations.
   [empty] is called when [evl] contains an empty expression.
*)
let eval_to_exec
    (f: 'a -> 'b flow -> 'b flow option)
    ?(empty = (fun flow -> Some flow))
    man ctx
    (eval: ('a, 'b) evals)
  : 'b flow option =
  Some eval |> oeval_merge
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

let oeval_to_exec 
    (f: 'a -> 'b flow -> 'b flow option)
    ?(empty = (fun flow -> Some flow))
    man ctx
    (oeval: ('a, 'b) evals option)
  : 'b flow option =
  match oeval with
  | None -> None
  | Some eval -> eval_to_exec f ~empty man ctx eval
