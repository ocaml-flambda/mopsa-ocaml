(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** [Nonrel ∈ 𝒱 → 𝒟] lifts a non-relational value abstraction into an
    abstract domain of partial environments from variables to values.
*)

open Ast.All
open Core.All
open Sig.Domain.Value


(** {2 Identifier for the non-relation domain} *)
(** ****************************************** *)

type _ id += D_nonrel : (module VALUE with type t = 'v) -> (var,'v) Lattices.Partial_map.map id

let () =
  let open Eq in
  register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a, b) eq option = fun next id1 id2 ->
        match id1, id2 with
        | D_nonrel v1, D_nonrel v2 ->
          begin
            let module V1 = (val v1) in
            let module V2 = (val v2) in
            match equal_id V1.id V2.id with
            | Some Eq -> Some Eq
            | None -> None
          end
        | _ -> next.eq id1 id2
      in
      f
    );
  }


(** {2 Variable bounds} *)
(** ******************* *)

(** Context for saving invariants of variables bounds *)
let var_bounds_ctx =
  let module K = Context.GenUnitKey(struct
      type t = constant VarMap.t
      let print fmt m =
        Format.fprintf fmt "variables bounds: %a"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (v,b) -> Format.fprintf fmt "%a: %a" pp_var v pp_constant b)
          ) (VarMap.bindings m)
    end)
  in
  K.key


(** Add the bounds of a variable to context *)
let add_var_bounds_ctx v b uctx =
  let m = try Context.ufind var_bounds_ctx uctx with Not_found -> VarMap.empty in
  Context.uadd var_bounds_ctx (VarMap.add v b m) uctx


(** Add the bounds of a variable to flow *)
let add_var_bounds_flow v b flow =
  let ctx = add_var_bounds_ctx v b (Flow.get_unit_ctx flow) in
  Flow.set_unit_ctx ctx flow


(** Remove the bounds of a variable from context *)
let remove_var_bounds_ctx v ctx =
  try
    let m = Context.ufind var_bounds_ctx ctx in
    let mm = VarMap.remove v m in
    Context.uadd var_bounds_ctx mm ctx
  with Not_found -> ctx


(** Remove the bounds of a variable from flow *)
let remove_var_bounds_flow v flow =
  let ctx = remove_var_bounds_ctx v (Flow.get_unit_ctx flow) in
  Flow.set_unit_ctx ctx flow


(** Find the bounds of a variable in context *)
let find_var_bounds_ctx_opt v uctx =
  try
    let m = Context.ufind var_bounds_ctx uctx in
    try Some (VarMap.find v m)
    with Not_found -> None
  with Not_found -> None


(** {2 Non-relational domain} *)
(** ************************* *)

module Make(Value: VALUE) =
struct


  (** {2 Domain header} *)
  (** ***************** *)

  (** Map with variables as keys. *)
  module VarMap =
    Lattices.Partial_map.Make
      (Var)
      (Value)

  include VarMap

  let id = D_nonrel (module Value)

  let widen uctx a1 a2 =
    let open Bot_top in
    if a1 == a2 then a1 else
    match a1, a2 with
      | BOT, x | x, BOT -> x
      | TOP, x | x, TOP -> TOP
      | Nbt m1, Nbt m2 ->
        Nbt (
          MapExtPoly.map2zo
            (fun _ v1 -> v1)
            (fun _ v2 -> v2)
            (fun var v1 v2 ->
               let w = Value.widen v1 v2 in
               (* In order to apply the bounds constraints of variable [var] on
                  result [w], we need to ensure that both [v1] and [v2] are
                  within these bounds. Otherwise, applying directly the
                  constraints makes the widening unsound (the operands are
                  not included in the result).
               *)
               match find_var_bounds_ctx_opt var uctx with
               | None -> w
               | Some bounds ->
                 match Value.constant var.vtyp bounds with
                 | None -> w
                 | Some vv ->
                   if Value.subset v1 vv && Value.subset v2 vv
                   then Value.meet w vv
                   else w
            )
            m1 m2
        )



  let name = "framework.abstraction.combiners.value.nonrel"

  let debug fmt = Debug.debug ~channel:name fmt

  let merge pre (a1, log1) (a2, log2) =
    debug "generic_nonrel_merge:@.pre = %a@.a1 = %a@.log1 = %a@.a2 = %a@.log2 = %a"
      VarMap.print pre
      VarMap.print a1 pp_block log1
      VarMap.print a2 pp_block log2;
    let a1', a2' = Log.generic_domain_merge ~add ~remove ~find (a1, log1) (a2, log2) in
    try VarMap.map2zo
      (fun _ v1 -> v1)
      (fun _ v2 -> v2)
      (fun _ v1 v2 ->
         let v = Value.meet v1 v2 in
         if Value.is_bottom v then raise Bot.Found_BOT else v
      ) a1' a2'
    with Bot.Found_BOT -> VarMap.bottom


  (* Constrain the value of a variable with its bounds *)
  let add_bound_constraints ctx var v =
    match find_var_bounds_ctx_opt var ctx with
    | None        -> v
    | Some bounds ->
      match Value.constant var.vtyp bounds with
      | None    -> v
      | Some vv -> Value.meet v vv


  let add ctx var v a =
    let vv = add_bound_constraints ctx var v in
    VarMap.add var vv a


  let print fmt a =
    Format.fprintf fmt "%s:@,@[   %a@]@\n" Value.display VarMap.print a


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Expressions annotated with abstract values; useful for assignment and compare. *)
  type aexpr =
    | A_var   of Value.t
    | A_cst   of Value.t
    | A_unop  of aexpr * Value.t
    | A_binop of aexpr * Value.t * aexpr * Value.t
    | A_unsupported


  (** Pretty printer of annotated expressions *)
  let rec pp_aexp fmt = function
    | A_var v -> Format.fprintf fmt "(var, %a)" Value.print v
    | A_cst v -> Format.fprintf fmt "(cst, %a)" Value.print v
    | A_unop (ae,v) -> Format.fprintf fmt "(unop, %a, %a)" pp_aexp ae Value.print v
    | A_binop (ae1,v1,ae2,v2) -> Format.fprintf fmt "(binop, %a, %a, %a, %a)" pp_aexp ae1 Value.print v1 pp_aexp ae2 Value.print v2
    | A_unsupported -> Format.pp_print_string fmt "?"


  (** Value manager *)
  let rec value_man (ev:(expr*aexpr*Value.t) option) (map:t) : Value.t value_man = {
    eval = (fun e ->
        (* check if expression e has been evaluated by searching withing the annotated value-expression ev *)
        match ev with
        | None ->
          eval e map |>
          OptionExt.default (A_unsupported,Value.top) |>
          snd

        | Some (ee,ae,v) ->
          let rec aux ee ae v =
            if compare_expr ee e = 0 then v
            else match ee.ekind,ae with
              | E_unop(_,e1),A_unop(ae1,v1) ->
                aux e1 ae1 v1

              | E_binop(_,e1,e2),A_binop(ae1,v1,ae2,v2) ->
                (try aux e1 ae1 v1 with Not_found -> aux e2 ae2 v2)

              | _ -> raise Not_found
          in
          try aux ee ae v
          with Not_found ->
            eval e map |>
            OptionExt.default (A_unsupported,Value.top) |>
            snd         
      );
    ask = (fun q -> match Value.ask (value_man ev map) q with Some r -> r | _ -> raise Not_found);
  }


  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  and eval (e:expr) (a:t) : (aexpr * Value.t) option =
    match ekind e with
    | E_var(var, mode) ->
      let v = find var a in
      (A_var v, v) |>
      OptionExt.return

    | E_constant(c) ->
      Value.constant e.etyp c |> OptionExt.lift @@ fun v ->
      (A_cst v, v)

    | E_unop(O_cast, e1) ->
      eval e1 a |> OptionExt.bind @@ fun (ae1, v1) ->
      (* Keep the evaluation of e1 in the manager to let the domain query it *)
      Value.cast (value_man (Some (e1,ae1,v1)) a) e.etyp e1 |> OptionExt.lift @@ fun v ->
      (A_unop (ae1,v1), v)

    | E_unop (op,e1) ->
      eval e1 a |> OptionExt.lift @@ fun (ae1, v1) ->
      let v = Value.unop op e.etyp v1 in
      (A_unop (ae1, v1), v)

    | E_binop (op,e1,e2) ->
      eval e1 a |> OptionExt.bind @@ fun (ae1, v1) ->
      eval e2 a |> OptionExt.lift @@ fun (ae2, v2) ->
      let v = Value.binop op e.etyp v1 v2 in
      (A_binop (ae1, v1, ae2, v2), v)

    | _ ->
      (* unsupported -> ⊤ *)
      (* A_unsupported, Value.top *)
      None



  (** Backward refinement of expressions; given an annotated tree, and
      a target value, refine the environment using the variables in the
      expression *)
  let rec refine ctx (e:expr) (ae:aexpr) (v:Value.t) (r:Value.t) (a:t) : t =
    let r' = Value.meet v r in
    match e.ekind,ae with
    | E_var(var,mode), A_var _ ->
      if Value.is_bottom r'
      then bottom

      else
      if var_mode var mode = STRONG
      then add ctx var r' a

      else a

    | E_constant _, A_cst _ ->
      if Value.is_bottom r'
      then bottom
      else a

    | E_unop(O_cast,e1), A_unop(ae1, v1) ->
      (* Keep the refined evaluation of [r'] of [e] in the manager so
         that it can be used by the domain to refine value [v1] *)
      let w = Value.bwd_cast (value_man (Some (e,ae,r')) a) e.etyp e1 v1 in
      refine ctx e1 ae1 v1 w a

    | E_unop(op,e1), A_unop (ae1, v1) ->
      let w = Value.bwd_unop op e.etyp v1 r' in
      refine ctx e1 ae1 v1 w a

    | E_binop(op,e1,e2), A_binop (ae1, v1, ae2, v2) ->
      let w1, w2 = Value.bwd_binop op e.etyp v1 v2 r' in
      let a1 = refine ctx e1 ae1 v1 w1 a in
      refine ctx e2 ae2 v2 w2 a1

    | _ -> a


  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let rec filter ctx (e:expr) (b:bool) (a:t) : t option =
    match ekind e with

    | E_unop (O_log_not, e) ->
      filter ctx e (not b) a

    | E_binop (O_log_and, e1, e2) ->
      filter ctx e1 b a |> OptionExt.bind @@ fun a1 ->
      filter ctx e2 b a |> OptionExt.bind @@ fun a2 ->
      (if b then meet else join) a1 a2 |>
      OptionExt.return

    | E_binop (O_log_or, e1, e2) ->
      filter ctx e1 b a |> OptionExt.bind @@ fun a1 ->
      filter ctx e2 b a |> OptionExt.bind @@ fun a2 ->
      (if b then join else meet) a1 a2 |>
      OptionExt.return

    | E_constant c ->
      Value.constant e.etyp c |> OptionExt.bind @@ fun v ->
      let w = Value.filter b e.etyp v in
      (if Value.is_bottom w then bottom else a) |>
      OptionExt.return

    | E_var(var, mode) ->
      let v = find var a in
      let w = Value.filter b e.etyp v in
      (if Value.is_bottom w then bottom else
       if var_mode var mode = STRONG then add ctx var w a
       else a
      ) |>
      OptionExt.return

    (* arithmetic comparison part, handled by Value *)
    | E_binop (op, e1, e2) ->
      (* evaluate forward each argument expression *)
      eval e1 a |> OptionExt.bind @@ fun (ae1,v1) ->
      eval e2 a |> OptionExt.bind @@ fun (ae2,v2) ->

      (* apply comparison *)
      let r1, r2 = Value.compare op b e1.etyp v1 v2 in (* FIXME: both types should be given to Value.compare? *)

      (* propagate backward on both argument expressions *)
      refine ctx e2 ae2 v2 r2 @@ refine ctx e1 ae1 v1 r1 a |>
      OptionExt.return

    (* unary boolean predicate, handled by Value *)
    | E_unop (op, e) ->
      (* evaluate forward the expression *)
      eval e a |> OptionExt.bind @@ fun (ae,v) ->

      (* apply the predicate *)
      let r = Value.predicate op b e.etyp v in

      (* propagate backward on the argument *)
      refine ctx e ae v r a |>
      OptionExt.return

    | _ -> assert false


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog = empty

  let exec stmt man ctx (map:t) : t option =
    match skind stmt with
    | S_remove { ekind = E_var (v, _) }  ->
      VarMap.remove v map |>
      OptionExt.return

    | S_add { ekind = E_var (v, _) } ->
      (* Check of the variable is already present *)
      if VarMap.mem v map
      then OptionExt.return map
      else OptionExt.return @@ add ctx v Value.top map


    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars ->
      let vars = List.map (function
            | { ekind = E_var (v, _) } -> v
            | _ -> assert false
          ) vars
      in
      VarMap.fold (fun v _ acc ->
          if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
        ) map map |>
      OptionExt.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let v = find var1 map in
      remove var1 map |>
      add ctx var2 v |>
      OptionExt.return

    | S_forget { ekind = E_var (var, _) } ->
      add ctx var Value.top map |>
      OptionExt.return

    | S_assign ({ ekind= E_var (var, mode) }, e)  ->
      eval e map |> OptionExt.lift @@ fun (_, v) ->
      let map' = add ctx var v map in
      begin
        match var_mode var mode with
        | STRONG -> map'
        | WEAK -> join map map'
      end

    | S_expand ({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let value = find v map in
      List.fold_left (fun acc v' ->
          add ctx v' value acc
        ) map vl |>
      OptionExt.return

    | S_fold ({ekind = E_var (v, mode)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      (* Collect values of variables vl before removing them from the map *)
      let value,map' = List.fold_left
          (fun (accv,accm) -> function
             | { ekind = E_var (vv, _) } ->
               let accv' = find vv map |>
                           Value.join accv
               in
               let accm' = remove vv accm in
               accv',accm'
             | _ -> assert false
        ) (Value.bottom,map) vl
      in
      let value' =
        if mem v map then
          Value.join value (find v map)
        else
          value
      in
      add ctx v value' map' |>
      OptionExt.return

    | S_assume e ->
      filter ctx e true map

    | _ -> None



  let ask query man ctx map =
    Value.ask (value_man None map) query


end
