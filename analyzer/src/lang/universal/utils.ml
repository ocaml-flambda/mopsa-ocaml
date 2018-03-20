open Framework.Manager
open Framework.Ast
open Ast

let cond_flow cond true_branch false_branch bottom_branch merge man ctx flow =
  Framework.Manager.if_flow
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx flow)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx flow)
    true_branch false_branch bottom_branch merge
    man flow

let cond_eval cond true_branch false_branch bottom_branch man ctx flow =
  Framework.Domains.Eval.if_eval
    (man.exec (mk_assume cond (tag_range cond.erange "true assume")) ctx flow)
    (man.exec (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")) ctx flow)
    true_branch false_branch bottom_branch
    man flow


let compose_alloc_exec f empty addr_kind range manager ctx flow =
  let exp = mk_expr (E_alloc_addr(addr_kind, range)) range in
  Framework.Domains.Eval.compose_exec
    exp
    (fun exp flow ->
       match ekind exp with
       | E_addr(addr) -> f addr flow
       | _ -> assert false
    )
    empty
    manager ctx flow
