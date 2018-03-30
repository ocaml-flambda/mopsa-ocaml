open Framework.Manager
open Framework.Flow
open Framework.Ast
open Ast

let assume_and_exec cond true_branch false_branch bottom_branch man ctx flow =
  Framework.Manager.if_flow
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx flow)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx flow)
    true_branch false_branch bottom_branch (fun flow1 flow2 -> man.flow.join (true_branch flow1) (false_branch flow2))
    man flow

let assume_and_eval cond eval_true eval_false eval_bottom man ctx flow =
  Framework.Manager.if_flow
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx flow)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx flow)
    eval_true eval_false eval_bottom (fun flow1 flow2 ->
        Framework.Domains.Global.oeval_join (eval_true flow1) (eval_false flow2)
      )
    man flow


let compose_alloc_exec f addr_kind range manager ctx flow =
  let exp = mk_expr (E_alloc_addr(addr_kind, range)) range in
  Some (manager.eval exp ctx flow) |>
  Framework.Domains.Global.oeval_compose
    (fun exp flow ->
       match ekind exp with
       | E_addr(addr) -> f addr flow
       | _ -> assert false
    )
