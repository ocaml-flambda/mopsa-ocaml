open Flow
open Manager
open Eval

let return x = Some x

let fail = None

let oflow_extract = Option.none_to_exn

let oflow_extract_dfl dfl = function
  | None -> dfl
  | Some flow -> flow

let oflow_join man =
  Option.option_neutral2 man.flow.join


let oflow_map f flow = Option.option_lift1 f flow

let oflow_merge f1 f2 f12 none flow1 flow2 = Option.option_apply2 f1 f2 f12 none flow1 flow2

(* Execute post-eval clean statements pushed by evaluators.*)
let exec_cleaner_stmts exec stmtl flow =
  stmtl |> List.fold_left (fun acc stmt ->
      exec stmt acc
    ) flow

(**
   [eval_to_exec f man ctx evl] executes the transfer function [f] 
   on a set of evaluations [evl], and applies the cleaner
   statements pushed by the evaluations.
   [empty] is called when [evl] contains an empty expression.
*)
let eval_to_exec
    (f: 'a -> 'b flow -> 'b flow option)
    (exec: Ast.stmt -> 'b flow -> 'b flow)
    (man: 'b flow_manager)
    ?(empty = (fun flow -> Some flow))
    (eval: ('a, 'b) evals)
  : 'b flow option =
  Some eval |> oeval_merge
    (fun (exp', flow, clean) ->
       let flow' =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty @@ exec_cleaner_stmts exec clean flow
       in
       match flow' with
       | None -> None
       | Some flow' ->
         Some (exec_cleaner_stmts exec clean flow')
    )
    (fun a b ->
       match a, b with
       | None, x | x, None -> x
       | Some a, Some b -> Some (man.join a b)
    )
    (fun a b ->
       match a, b with
       | None, x | x, None -> x
       | Some a, Some b -> Some (man.meet a b)
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
