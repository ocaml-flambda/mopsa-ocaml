(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Generic domain for non-relational value abstractions. *)

open Value
open Framework.Ast
open Framework.Domains
open Framework.Domains.Global
open Framework.Flow
open Framework.Manager
open Framework.Context
open Ast

module Make(Value: VALUE) =
struct

  (*==========================================================================*)
                          (** {2 Lattice structure} *)
  (*==========================================================================*)


  module VarMap =
    Framework.Lattices.Total_map.Make
      (struct
        type t = var
        let compare = compare_var
        let print = Framework.Pp.pp_var
      end)
    (Value)

  include VarMap

  let debug fmt = Debug.debug ~channel:"universal.nonrel" fmt


  (*==========================================================================*)
                     (** {2 Expression manipulations} *)
  (*==========================================================================*)

  type 'v aexpr =
    | AExpr_var of var
    | AExpr_cst
    | AExpr_unop of operator * 'v aexpr_val
    | AExpr_binop of operator * 'v aexpr_val * 'v aexpr_val
    | AExpr_unsupported

   and 'v aexpr_val = 'v aexpr * 'v
  (** Expressions annotated with abstract values and errors. *)

  type aexpr_i = Value.t aexpr_val

  let to_bool (can_be_true:bool) (can_be_false:bool) : Value.t =
    match can_be_true, can_be_false with
    | true, false -> Value.of_constant C_true
    | false, true -> Value.of_constant C_false
    | true, true -> Value.join (Value.of_constant C_true) (Value.of_constant C_false)
    | false, false -> Value.bottom

  let rec annotate_expr (a:t) (e:expr) : aexpr_i =
    match ekind e with

    | E_var var ->
       let v = VarMap.find var a in
       AExpr_var var, v

    | E_constant(c) ->
       AExpr_cst, Value.of_constant c

    | E_unop (O_log_not, e1) ->
      let (_,v1) as t1 = annotate_expr a e1 in
      let v = to_bool (Value.can_be_false v1) (Value.can_be_true v1)  in
      (* debug "annotate unop %a:@ %a -> %a" Framework.Pp.pp_operator op Value.print v1 Value.print v; *)
      AExpr_unop (O_log_not, t1), v

    | E_unop (op,e1) ->
      let (_,v1) as t1 = annotate_expr a e1 in
      let v = Value.fwd_unop op v1 in
      (* debug "annotate unop %a:@ %a -> %a" Framework.Pp.pp_operator op Value.print v1 Value.print v; *)
      AExpr_unop (op, t1), v

    | E_binop (((O_eq | O_ne | O_lt | O_le | O_gt | O_ge) as op), e1, e2) ->
      (* debug "annotate comparison %a" Framework.Pp.pp_operator op; *)
       let (_,v1) as t1 = annotate_expr a e1 in
       let (_,v2) as t2 = annotate_expr a e2 in
       (* debug "comparison v1 = %a (%a) v2 = %a" Value.print v1 Framework.Pp.pp_operator op Value.print v2; *)
       let neg_op = match op with
         | O_eq -> O_ne
         | O_ne -> O_eq
         | O_lt -> O_ge
         | O_le -> O_gt
         | O_gt -> O_le
         | O_ge -> O_lt
         | _ -> assert false
       in
       let a1, a2 = Value.fwd_filter op v1 v2, Value.fwd_filter neg_op v1 v2 in
       (* debug "comparison %a a1 = %b@ a2 = %b" Framework.Pp.pp_operator op a1 a2; *)
       AExpr_binop (op, t1, t2), to_bool a1 a2

    | E_binop (O_log_and,e1,e2) ->
      let (_,v1) as t1 = annotate_expr a e1 in
      let (_,v2) as t2 = annotate_expr a e2 in
      let v =
        to_bool
          (Value.can_be_true v1 && Value.can_be_true v2)
          (Value.can_be_false v1 || Value.can_be_false v2)  in
      AExpr_binop (O_log_and, t1, t2), v

    | E_binop (O_log_or,e1,e2) ->
      let (_,v1) as t1 = annotate_expr a e1 in
      let (_,v2) as t2 = annotate_expr a e2 in
      let v =
        to_bool
          (Value.can_be_true v1 || Value.can_be_true v2)
          (Value.can_be_false v1 && Value.can_be_false v2)
      in
      AExpr_binop (O_log_or, t1, t2), v

    | E_binop (op,e1,e2) ->
      (* debug "annotate binop %a" Framework.Pp.pp_operator op; *)
       let (_,v1) as t1 = annotate_expr a e1 in
       let (_,v2) as t2 = annotate_expr a e2 in
       (* debug "binop %a v1 = %a@ v2 = %a" Framework.Pp.pp_operator op Value.print v1 Value.print v2; *)
       let v = Value.fwd_binop op v1 v2 in
       (* debug "binop v = %a" Value.print v; *)
       AExpr_binop (op, t1, t2), v

    | _ ->
      (* debug "Unsupported expression %a" Framework.Pp.pp_exp e; *)
       (* unsupported -> ⊤ *)
       AExpr_unsupported, Value.top
  (** Annotates an expression tree with all intermediate abstract results. *)


  let refine_bool1 (r:Value.t) iftrue iffalse x bottom =
    (* debug "refine_bool1 r = %a" Value.print r; *)
    match Value.can_be_true r, Value.can_be_false r with
    | true,  true  -> x
    | true,  false -> iftrue x
    | false, true  -> iffalse x
    | false, false -> bottom
  (* Utility to refine unary logical operations returing 0 or 1. *)

  let refine_bool2 (r:Value.t) iftrue iffalse x1 x2 bottom =
    (* debug "refine_bool2 r = %a" Value.print r; *)
    match Value.can_be_true r, Value.can_be_false r with
    | true,  true  -> x1, x2
    | true,  false -> iftrue x1 x2
    | false, true  -> iffalse x1 x2
    | false, false -> bottom, bottom
  (* Utility to refine binary logical operations returing 0 or 1. *)



  let rec refine_expr (a:t) ((e,r):aexpr_i) (rr:Value.t) : t =
    (* debug "refine_expr:@ a = @[%a@]@ r = @[%a@]@ rr = @[%a@]" print a Value.print r Value.print rr; *)
    let rrr = Value.meet rr r in
    if Value.leq r rrr && Value.leq rrr r then a (* equality so no refinement *)
    else
      match e with
      | AExpr_var var ->
        VarMap.add var rr a

      | AExpr_cst ->
        refine_bool1 rrr (fun a -> a) (fun a -> bottom) a bottom

      | AExpr_unop (O_log_not,((_,a1) as t1)) ->
        let aa1 = refine_bool1 rrr Value.assume_false Value.assume_true a1 Value.bottom in
        refine_expr a t1 aa1

      | AExpr_unop (op,((_,a1) as t1)) ->
        let aa1 = Value.bwd_unop op a1 rrr in
        refine_expr a t1 aa1

      | AExpr_binop (O_log_and,((_,a1) as t1),((_,a2) as t2)) ->
        (* debug "and@ a1 = %a@ a2 = %a" Value.print a1 Value.print a2; *)
        refine_bool1
          rrr
          (fun a ->
             (* both arguments must be non-zero *)
             let aa1 = Value.assume_true a1
             and aa2 = Value.assume_true a2 in
             refine_expr (refine_expr a t1 aa1) t2 aa2
          )
          (fun a ->
             (* at least one argument must be zero *)
             let aa1 = Value.assume_false a1
             and aa2 = Value.assume_false a2 in
             join
               (refine_expr a t1 aa1)
               (refine_expr a t2 aa2)
          )
          a
          bottom

      | AExpr_binop (O_log_or,((_,a1) as t1),((_,a2) as t2)) ->
        refine_bool1
          rrr
          (fun a ->
             (* a least one argument must be non-zero *)
             let aa1 = Value.assume_true a1
             and aa2 = Value.assume_true a2 in
             join
               (refine_expr a t1 aa1)
               (refine_expr a t2 aa2)
          )
          (fun a ->
             (* both arguments must be zero *)
             let aa1 = Value.assume_false a1
             and aa2 = Value.assume_false a2 in
             refine_expr (refine_expr a t1 aa1) t2 aa2
          )
          a
          bottom

      | AExpr_binop ((O_eq | O_ne | O_lt | O_le | O_gt | O_ge) as op,((_,a1) as t1),((_,a2) as t2)) ->
        (* debug "binop %a" Framework.Pp.pp_operator op; *)
        let neg_op = function
         | O_eq -> O_ne
         | O_ne -> O_eq
         | O_lt -> O_ge
         | O_le -> O_gt
         | O_gt -> O_le
         | O_ge -> O_lt
         | _ -> assert false
        in
        let aa1, aa2 = refine_bool2 rrr (Value.bwd_filter op) (Value.bwd_filter (neg_op op)) a1 a2 Value.bottom in
        (* debug "op = %a@ aa1 = @[%a@]@ aa2 = @[%a@]" Framework.Pp.pp_operator op Value.print aa1 Value.print aa2; *)
        refine_expr (refine_expr a t1 aa1) t2 aa2

      | AExpr_binop (op,((_,a1) as t1),((_,a2) as t2)) ->
         let aa1, aa2 = Value.bwd_binop op a1 a2 rrr in
         refine_expr (refine_expr a t1 aa1) t2 aa2

      | AExpr_unsupported ->
        debug "Unsupported2";
         a
  (** Refines the abstract element given the target value of the expression.
      Can raise Found_BOT.
   *)



  let eval_value (a:t) (e:expr) : Value.t =
    let _,v = annotate_expr a e in
    v
  (** Evaluates the expression to a non-relational value, *)



  (*==========================================================================*)
                         (** {2 Transfer function} *)
  (*==========================================================================*)


  let init prog man flow =
    set_domain_cur top man flow

  let rec exec stmt man ctx flow =
    match skind stmt with
    | S_expression(e) ->
      Eval.compose_exec
        e
        (fun e flow -> Exec.return flow)
        (fun flow -> Exec.return flow)
        man ctx flow

    | S_remove_var v ->
      map_domain_cur (VarMap.remove v) man flow |>
      Exec.return

    | S_project_vars vars ->
      map_domain_cur (fun a ->
          VarMap.fold (fun v _ acc ->
              if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
            ) a a
        ) man flow |>
      Exec.return

    | S_rename_var (var1, var2) ->
      map_domain_cur (fun a ->
          let v = VarMap.find var1 a in
          VarMap.remove var1 a |> VarMap.add var2 v
        ) man flow |>
      Exec.return


    | S_assign({ekind = E_var var}, e, STRONG) ->
      Eval.compose_exec
        e
        (fun e flow ->
           map_domain_cur (fun a ->
               let v = eval_value a e in
               VarMap.add var v a
             ) man flow |>
           Exec.return
        )
        (fun flow -> Exec.return flow)
        man ctx flow

    | S_assign({ekind = E_var var}, e, _) ->
      assert false

    | S_assume e ->
      Eval.compose_exec
        e
        (fun e flow ->
           map_domain_cur (fun a ->
               let (_,r) as t = annotate_expr a e in
               let rr = Value.assume_true r in
               if Value.is_bottom rr then bottom else refine_expr a t rr
             ) man flow |>
           Exec.return
        )
        (fun flow -> Exec.return flow)
        man ctx flow

    | _ ->
      None


  type _ Framework.Query.query +=
    | QEval : expr -> Value.t Framework.Query.query

  let ask : type r. r Framework.Query.query -> ('a, t) manager -> context -> 'a flow -> r option =
    fun query man ctx flow ->
      match query with
      | QEval e ->
        let a = get_domain_cur man flow in
        let value = eval_value a e in
        Some value

      | _ ->
        None

  let eval exp man ctx flow =
    match ekind exp with
    | E_constant _
    | E_var _ ->
      Eval.singleton (Some exp, flow, [])

    | E_unop(op, e) ->
      Eval.compose_eval
        e
        (fun e flow ->
           let exp' = {exp with ekind = E_unop(op, e)} in
           Eval.singleton (Some exp', flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | E_binop(op, e1, e2) ->
      Eval.compose_eval_list
        [e1; e2]
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           Eval.singleton (Some exp', flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | _ -> None


end
