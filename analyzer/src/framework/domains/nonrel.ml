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
  let rec eval (a:t) (e:expr) : aexpr * Value.t =
    match ekind e with

    | E_var var ->
      let v = VarMap.find var a in
      A_var (var, v), v

    | E_constant(c) ->
      let v = Value.of_constant c in
      A_cst (c, v), v

    | E_unop (op,e1) ->
      let ae1, v1 = eval a e1 in
      let v = Value.unop op v1 in
      A_unop (op, ae1, v1), v

    | E_binop (op,e1,e2) ->
      let ae1, v1 = eval a e1 in
      let ae2, v2 = eval a e2 in
      let v = Value.binop op v1 v2 in
      A_binop (op, ae1, v1, ae2, v2), v

    | _ ->
      (* unsupported -> ⊤ *)
      A_unsupported, Value.top


   (** Forward evaluation of boolean expressions *)
  let rec fwd_compare (a:t) (e:expr) : aexpr * Value.t =
    match ekind e with

    | E_var var ->
      let v = VarMap.find var a in
      A_var (var, v), v

    | E_constant(c) ->
      let v = Value.of_constant c in
      A_cst (c, v), v

    | E_unop (op,e1) ->
      let ae1, v1 = eval a e1 in
      let v = Value.unop op v1 in
      A_unop (op, ae1, v1), v

    | E_binop (op,e1,e2) ->
      let ae1, v1 = eval a e1 in
      let ae2, v2 = eval a e2 in
      let v = Value.binop op v1 v2 in
      A_binop (op, ae1, v1, ae2, v2), v

    | _ ->
      (* unsupported -> ⊤ *)
      A_unsupported, Value.top



  (** Backward refinement of expressions; given an annotated tree, and
     a target value, refine the environment using the variables in the
     expression *)
  let rec refine (a:t) (ae:aexpr) (v:Value.t) (r:Value.t) : t =
    let v' = Value.meet Annotation.empty v r in
    debug "refine_expr:@ a = @[%a@]@ r = @[%a@]@ rr = @[%a@]" print a Value.print r Value.print v;
    match ae with
    | A_var (var, _) ->
      if Value.is_bottom v'
      then bottom
      else VarMap.add var v' a

    | A_cst(_) ->
      if Value.is_bottom v'
      then bottom
      else a

    | A_unop (op, ae1, v1) ->
      refine a ae1 v1 (Value.bwd_unop op v' r)

    | A_binop (op, ae1, v1, ae2, v2) ->
      let w1,w2 = Value.bwd_binop op v1 v2 r in
      refine (refine a ae1 v1 w1) ae2 v2 w2

    | A_unsupported ->
      a

  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let filter (annot: 'a Annotation.t) (a:t) (e:expr) (r:bool) : t =

    (* recursive exploration of the expression *)
    let rec doit a e r =
      match ekind e with

      | E_unop (O_log_not, e) -> 
        doit a e (not r)

      | E_binop (O_log_and, e1, e2) ->
        (if r then meet else join) annot (doit a e1 r) (doit a e2 r)

      | E_binop (O_log_or, e1, e2) -> 
        (if r then join else meet) annot (doit a e1 r) (doit a e2 r)

      | E_constant c ->
        let v = Value.of_constant c in
        let v' = Value.filter v r in
        if Value.is_bottom v' then bottom else a

      | E_var var ->
        let v = find var a in
        let v' = Value.filter v r in
        if Value.is_bottom v' then bottom else add var v' a

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
        let ae1,v1 = eval a e1
        and ae2,v2 = eval a e2 in
        (* apply comparison *)
        let r1,r2 = Value.compare op v1 v2 in
        (* propagate backward on both argument expressions *)
        refine (refine a ae1 v1 r1) ae2 v2 r2

      | _ -> assert false

    in
    doit a e r



  (*==========================================================================*)
                         (** {2 Transfer function} *)
  (*==========================================================================*)


  let init prog man flow = Some (set_local T_cur top man flow)

  let exec_interface = Domain.{
    import = [];
    export = [Value.zone];
  }

  let eval_interface = Domain.{
    import = [Zone.top, Value.zone];
    export = [Value.zone, Value.zone];
  }

  let rec exec zone stmt man flow =
    match skind stmt with
    | S_remove_var v ->
      Some (
        let flow' = map_local T_cur (VarMap.remove v) man flow in
        Post.singleton flow'
      )

    | S_project_vars vars ->
      Some (
        let flow' = map_local T_cur (fun a ->
            VarMap.fold (fun v _ acc ->
                if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
              ) a a
          ) man flow
        in
        Post.singleton flow'
      )

    | S_rename_var (var1, var2) ->
      Some (
        let flow' = map_local T_cur (fun a ->
            let v = VarMap.find var1 a in
            VarMap.remove var1 a |> VarMap.add var2 v
          ) man flow
        in
        Post.singleton flow'
      )

    | S_assign({ekind = E_var var}, e, mode) ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e flow |> Post.bind man @@ fun e flow ->
        let flow' = map_local T_cur (fun a ->
            let _, v = eval a e in
            let a' = VarMap.add var v a in
            match mode with
            | STRONG | EXPAND -> a'
            | WEAK -> join (get_annot flow) a a'
          ) man flow
        in
        Post.singleton flow'
      )

    | S_assume e ->
      Some (
        man.eval ~zone:(Zone.top, Value.zone) e flow |> Post.bind man @@ fun e flow ->
        let flow' = map_local T_cur (fun a ->
            filter (get_annot flow) a e true
          ) man flow
        in
        Post.singleton flow'
      )

    | _ -> None



  type _ Query.query +=
    | QEval : expr -> Value.t Query.query

  let () =
    Query.(register_reply_manager {
        domatch = (let check : type a. a query -> (a, Value.t) eq option =
                     function
                     | QEval _ -> Some Eq
                     | _ -> None
                   in
                   check
                  );
        join = (fun v1 v2 -> Value.join Annotation.empty v1 v2);
        meet = (fun v1 v2 -> Value.meet Annotation.empty v1 v2);
      };
      )


  let ask : type r. r Query.query -> _ -> _ -> r option =
    fun query man flow ->
      match query with
      | QEval e ->
        let a = get_local T_cur man flow in
        let _, v = eval a e in
        Some v

      | _ -> None


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
