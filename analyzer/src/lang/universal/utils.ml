open Framework.Manager
open Framework.Flow
open Framework.Ast
open Ast

let assume_to_exec cond true_case false_case man ctx flow
    ?(bottom_case=(fun () -> flow))
    ?(merge_case=(fun flow1 flow2 -> man.flow.join (true_case flow1) (false_case flow2)))
    () =
  Framework.Utils.if_flow
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx)
    true_case false_case bottom_case merge_case
    man flow

let assume_to_eval cond
    true_case false_case man ctx flow
    ?(bottom_case=(fun () -> Framework.Domains.Global.oeval_singleton (None, flow, [])))
    ?(merge_case=(fun flow1 flow2 -> Framework.Domains.Global.oeval_join (true_case flow1) (true_case flow2))) () =
  Framework.Utils.if_flow
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx)
    true_case false_case bottom_case merge_case
    man flow


let compose_alloc_exec f addr_kind range manager ctx flow =
  let exp = mk_expr (E_alloc_addr(addr_kind, range)) range in
  manager.eval exp ctx flow |>
  Framework.Utils.eval_to_exec
    (fun exp flow ->
       match ekind exp with
       | E_addr(addr) -> f addr flow
       | _ -> assert false
    )
    manager ctx
