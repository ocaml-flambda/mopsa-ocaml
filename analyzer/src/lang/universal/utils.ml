open Framework.Manager
open Framework.Flow
open Framework.Lattice
open Framework.Ast
open Ast

let assume_to_exec cond true_case false_case man ctx flow
    ?(bottom_case=(fun () -> man.flow.set TCur man.env.bottom flow))
    ?(merge_case=(fun flow1 flow2 -> man.flow.join (true_case flow1) (false_case flow2)))
    () =
  if_flow
    (man.exec ctx (mk_assume cond (tag_range cond.erange "true assume")))
    (man.exec ctx (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")))
    true_case false_case bottom_case merge_case
    man flow

let assume_to_eval cond
    true_case false_case man ctx flow
    ?(bottom_case=(fun () -> Framework.Eval.oeval_singleton (None, flow, [])))
    ?(merge_case=(fun flow1 flow2 -> Framework.Eval.oeval_join (true_case flow1) (false_case flow2))) () =
  if_flow
    (man.exec ctx (mk_assume cond (tag_range cond.erange "true assume")))
    (man.exec ctx (mk_assume (mk_not cond (tag_range cond.erange "neg")) (tag_range cond.erange "false assume")))
    true_case false_case bottom_case merge_case
    man flow

let switch_exec
    (cases : (((expr * bool) list) * ('a Framework.Flow.flow -> 'a Framework.Flow.flow)) list)
    man ctx flow
  : 'a Framework.Flow.flow =
  match cases with
  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x (tag_range x.erange "true assume"))
            else (mk_assume (mk_not x (tag_range x.erange "neg"))
                    (tag_range x.erange "false assume"))
          in
          man.exec ctx s acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> man.flow.join (one cond t) acc) (one cond t) q
  | [] -> man.flow.bottom


let switch_rexec
    (cases : (((expr * bool) list) * ('a Framework.Flow.flow -> 'a Framework.Domains.Reduce.Domain.rflow)) list)
    man ctx flow
  : 'a Framework.Domains.Reduce.Domain.rflow =
  let module RF = Framework.Domains.Reduce.Domain in
  match cases with
  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x (tag_range x.erange "true assume"))
            else (mk_assume (mk_not x (tag_range x.erange "neg"))
                    (tag_range x.erange "false assume"))
          in
          man.exec ctx s acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> RF.rflow_join man.flow (one cond t) acc) (one cond t) q
  | [] -> man.flow.bottom |> RF.return_flow_no_opt


let compose_alloc_exec f addr_kind range manager ctx flow =
  let exp = mk_expr (E_alloc_addr(addr_kind, range)) range in
  manager.eval ctx exp flow |>
  Framework.Exec.eval_to_exec
    (fun exp flow ->
       match ekind exp with
       | E_addr(addr) -> f addr flow
       | _ -> assert false
    )
    (manager.exec ctx)
    manager.flow

let rec expr_to_z (e: expr) : Z.t option =
  match ekind e with
  | E_constant (C_int n) -> Some n
  | E_unop (O_minus T_int, e') ->
    begin
      match expr_to_z e' with
      | None -> None
      | Some n -> Some (Z.neg n)
    end
  | E_binop(op, e1, e2) ->
    begin
      match expr_to_z e1, expr_to_z e2 with
      | Some n1, Some n2 ->
        begin
          match op with
          | O_plus T_int -> Some (Z.add n1 n2)
          | O_minus T_int -> Some (Z.sub n1 n2)
          | O_mult T_int -> Some (Z.mul n1 n2)
          | O_div T_int -> if Z.equal n2 Z.zero then None else Some (Z.div n1 n2)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None
