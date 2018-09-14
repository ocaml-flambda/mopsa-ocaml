(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Generic domain for creating non-relational value abstractions. *)

open Value
open Essentials

module Make(Value: VALUE) =
struct


  (*==========================================================================*)
                          (** {2 Lattice structure} *)
  (*==========================================================================*)

  (** Map with variables as keys. Absent bindings are assumed to point to ⊤. *)
  module VarMap =
    Lattices.Total_map.Make
      (struct
        type t = var
        let compare = compare_var
        let print = pp_var
      end)
    (Value)

  include VarMap

  type _ domain += D_nonrel : t domain

  let name = "framework.domains.nonrel"
  let id = D_nonrel
  let identify : type a. a domain -> (t, a) Domain.eq option =
    function
    | D_nonrel -> Some Eq
    | _ -> None


  let print fmt a =
    Format.fprintf fmt "%s:@\n  @[%a@]@\n" (snd @@ Value.name) VarMap.print a

  let debug fmt = Debug.debug ~channel:name fmt

  (*==========================================================================*)
  (**                    {2 Evaluation of expressions}                        *)
  (*==========================================================================*)

  (** Expressions annotated with abstract values; useful for assignment and compare. *)
  type aexpr =
    | A_var of var * Value.t
    | A_cst of constant * Value.t
    | A_unop of operator * aexpr * Value.t
    | A_binop of operator * aexpr * Value.t * aexpr * Value.t
    | A_unsupported

  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  let rec eval (e:expr) (a:t) : (aexpr * Value.t) with_channel =
    match ekind e with

    | E_var var ->
      let v = VarMap.find var a in
      (A_var (var, v), v) |>
      Channel.return

    | E_constant(c) ->
      let v = Value.of_constant c in
      (A_cst (c, v), v) |>
      Channel.return

    | E_unop (op,e1) ->
      eval e1 a |> Channel.bind @@ fun (ae1, v1) ->
      Value.unop op v1 |> Channel.bind @@ fun v ->
      Channel.return (A_unop (op, ae1, v1), v)

    | E_binop (op,e1,e2) ->
      eval e1 a |> Channel.bind @@ fun (ae1, v1) ->
      eval e2 a |> Channel.bind @@ fun (ae2, v2) ->
      Value.binop op v1 v2 |> Channel.bind @@ fun v ->
      Channel.return (A_binop (op, ae1, v1, ae2, v2), v)

    | _ ->
      (* unsupported -> ⊤ *)
      Channel.return (A_unsupported, Value.top)


   (** Forward evaluation of boolean expressions *)
  let rec fwd_compare (e:expr) (a:t) : (aexpr * Value.t) with_channel =
    match ekind e with

    | E_var var ->
      let v = VarMap.find var a in
      Channel.return (A_var (var, v), v)

    | E_constant(c) ->
      let v = Value.of_constant c in
      Channel.return (A_cst (c, v), v)

    | E_unop (op,e1) ->
      eval e1 a |>
      Channel.bind @@ fun (ae1, v1) ->
      Value.unop op v1 |>
      Channel.bind @@ fun v ->
      Channel.return (A_unop (op, ae1, v1), v)

    | E_binop (op,e1,e2) ->
      eval e1 a |>
      Channel.bind @@ fun (ae1, v1) ->
      eval e2 a |>
      Channel.bind @@ fun (ae2, v2) ->
      Value.binop op v1 v2 |>
      Channel.bind @@ fun v ->
      Channel.return (A_binop (op, ae1, v1, ae2, v2), v)

    | _ ->
      (* unsupported -> ⊤ *)
      Channel.return (A_unsupported, Value.top)

  (** Backward refinement of expressions; given an annotated tree, and
     a target value, refine the environment using the variables in the
     expression *)
  let rec refine (ae:aexpr) (v:Value.t) (r:Value.t) (a:t) : t with_channel =
    let v' = Value.meet Annotation.empty v r in
    debug "refine_expr:@ a = @[%a@]@ r = @[%a@]@ rr = @[%a@]" print a Value.print r Value.print v;
    match ae with
    | A_var (var, _) ->
      if Value.is_bottom v'
      then Channel.return bottom
      else Channel.return (VarMap.add var v' a)

    | A_cst(_) ->
      if Value.is_bottom v'
      then Channel.return bottom
      else Channel.return a

    | A_unop (op, ae1, v1) ->
      Value.bwd_unop op v' r |> Channel.bind @@ fun w ->
      refine ae1 v1 w a

    | A_binop (op, ae1, v1, ae2, v2) ->
      Value.bwd_binop op v1 v2 r |> Channel.bind @@ fun (w1, w2) ->
      refine ae1 v1 w1 a |> Channel.bind @@ fun a1 ->
      refine ae2 v2 w2 a1

    | A_unsupported ->
      Channel.return a

  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let filter (annot: 'a annot) (e:expr) (r:bool) (a:t) : t with_channel =
    (* recursive exploration of the expression *)
    let rec doit (e:expr) (r:bool) (a:t) : t with_channel =
      match ekind e with

      | E_unop (O_log_not, e) ->
        doit e (not r) a

      | E_binop (O_log_and, e1, e2) ->
        doit e1 r a |> Channel.bind @@ fun a1 ->
        doit e2 r a |> Channel.bind @@ fun a2 ->
        (if r then meet else join) annot a1 a2 |>
        Channel.return

      | E_binop (O_log_or, e1, e2) ->
        doit e1 r a |> Channel.bind @@ fun a1 ->
        doit e2 r a |> Channel.bind @@ fun a2 ->
        (if r then join else meet) annot a1 a2 |>
        Channel.return

      | E_constant c ->
        let v = Value.of_constant c in
        Value.filter v r |> Channel.bind @@ fun w ->
        (if Value.is_bottom w then bottom else a) |>
        Channel.return

      | E_var var ->
        let v = find var a in
        Value.filter v r |> Channel.bind @@ fun w ->
        (if Value.is_bottom w then bottom else add var w a) |>
        Channel.return

      (* arithmetic comparison part, handled by Value *)
      | E_binop (op, e1, e2) ->
        (* utility function to negate the comparison, when r=false *)
        let inv = function
          | O_eq -> O_ne
          | O_ne -> O_eq
          | O_lt -> O_ge
          | O_le -> O_gt
          | O_gt -> O_le
          | O_ge -> O_lt
          | _ -> assert false
        in
        let op = if r then op else inv op in
        (* evaluate forward each argument expression *)
        eval e1 a |> Channel.bind @@ fun (ae1,v1) ->
        eval e2 a |> Channel.bind @@ fun (ae2,v2) ->
        (* apply comparison *)
        Value.compare op v1 v2 |> Channel.bind @@ fun (r1, r2) ->
        (* propagate backward on both argument expressions *)
        refine ae1 v1 r1 a |> Channel.bind @@ fun a1 ->
        refine ae2 v2 r2 a1

      | _ -> assert false

    in
    doit e r a



  (*==========================================================================*)
                         (** {2 Transfer function} *)
  (*==========================================================================*)


  let init prog man flow =
    Some (
      let flow' = Flow.set_domain_env T_cur top man flow in
      debug "init %a" (Flow.print man) flow';
      flow'
    )

  let exec_interface = Domain.{
    import = [];
    export = [Value.zone];
  }

  let eval_interface = Domain.{
    import = [Zone.top, Value.zone];
    export = [Zone.top, Value.zone];
  }

  let rec exec zone stmt man flow =
    match skind stmt with
    | S_remove_var v ->
      Some (
        let flow' = Flow.map_domain_env T_cur (VarMap.remove v) man flow in
        Post.of_flow flow'
      )

    | S_project_vars vars ->
      Some (
        let flow' = Flow.map_domain_env T_cur (fun a ->
            VarMap.fold (fun v _ acc ->
                if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
              ) a a
          ) man flow
        in
        Post.of_flow flow'
      )

    | S_rename_var (var1, var2) ->
      Some (
        let flow' = Flow.map_domain_env T_cur (fun a ->
            let v = VarMap.find var1 a in
            VarMap.remove var1 a |> VarMap.add var2 v
          ) man flow
        in
        Post.of_flow flow'
      )

    | S_assign({ekind = E_var var}, e, mode) ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e flow |> Post.bind man @@ fun e flow ->
        let flow', channels = Channel.map_domain_env T_cur (fun a ->
            eval e a |> Channel.bind @@ fun (_,v) ->
            let a' = VarMap.add var v a in
            let a'' = match mode with
              | STRONG | EXPAND -> a'
              | WEAK -> join (Flow.get_all_annot flow) a a'
            in
            Channel.return a''
          ) man flow
        in
        Post.of_flow flow' |>
        Post.add_channels channels
      )

    | S_assume e ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e flow |> Post.bind man @@ fun e flow ->
        let flow', channels = Channel.map_domain_env T_cur (fun a ->
            filter (Flow.get_all_annot flow) e true a
          ) man flow
        in
        Post.of_flow flow' |>
        Post.add_channels channels
      )

    | _ -> None


  let ask : type r. r Query.query -> _ -> _ -> r option =
    fun query man flow ->
      let a = Flow.get_domain_env T_cur man flow in
      Value.ask query (fun exp -> let v = eval exp a in snd v.value)


  let eval zpath exp man flow =
    match ekind exp with
    | E_binop(op, e1, e2) ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e1 flow |> Eval.bind @@ fun e1 flow ->
        man.eval ~zone:(Zone.top, Value.zone) e2 flow |> Eval.bind @@ fun e2 flow ->
        let exp' = {exp with ekind = E_binop(op, e1, e2)} in
        Eval.singleton exp' flow
      )

    | E_unop(op, e) ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e flow |> Eval.bind @@ fun e flow ->
        let exp' = {exp with ekind = E_unop(op, e)} in
        Eval.singleton exp' flow
      )

    | _ -> None


end
