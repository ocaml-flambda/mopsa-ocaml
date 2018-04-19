open Flow
open Manager
open Eval

let oflow_extract = Option.none_to_exn

let oflow_extract_dfl dfl = function
  | None -> dfl
  | Some flow -> flow

let oflow_join man =
  Option.option_neutral2 man.flow.join


let oflow_map f flow = Option.option_lift1 f flow

let oflow_merge f1 f2 f12 none flow1 flow2 = Option.option_apply2 f1 f2 f12 none flow1 flow2

(**
   [eval_to_exec f man ctx evl] executes the transfer function [f] 
   on a set of evaluations [evl], and applies the cleaner
   statements pushed by the evaluations.
   [empty] is called when [evl] contains an empty expression.
*)
let eval_to_exec
    (f: 'a -> 'b flow -> 'b flow)
    (exec: Ast.stmt -> 'b flow -> 'b flow)
    (man: 'b flow_manager)
    ?(empty = (fun flow -> flow))
    (eval: ('a, 'b) evals)
  : 'b flow =
  eval |> eval_substitute
    (fun (exp', flow, clean) ->
       let flow =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty flow
       in
       clean |> List.fold_left (fun acc stmt ->
           exec stmt acc
         ) flow
    )
    (man.join)
    (man.meet)

let eval_to_oexec
    (f: 'a -> 'b flow -> 'b flow option)
    (exec: Ast.stmt -> 'b flow -> 'b flow)
    (man: 'b flow_manager)
    ?(empty = (fun flow -> Some flow))
    (eval: ('a, 'b) evals)
  : 'b flow option =
  eval |> eval_substitute
    (fun (exp', flow, clean) ->
       let flow =
         match exp' with
         | Some exp' -> f exp' flow
         | None -> empty flow
       in
       match flow with
       | None -> None
       | Some flow ->
         let flow' = clean |> List.fold_left (fun acc stmt ->
             exec stmt acc
           ) flow
         in
         Some flow'
    )
    (fun flow1 flow2 ->
       match flow1, flow2 with
       | None, x | x, None -> x
       | Some flow1, Some flow2 -> Some (man.join flow1 flow2)
    )
    (fun flow1 flow2 ->
       match flow1, flow2 with
       | None, x | x, None -> x
       | Some flow1, Some flow2 -> Some (man.meet flow1 flow2)
    )


let oeval_to_oexec f exec man ?(empty = (fun flow -> Some flow)) oeval =
  match oeval with
  | None -> None
  | Some eval -> eval_to_oexec f exec man ~empty eval
