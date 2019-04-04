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

(** [Nonrel ∈ 𝒱 → 𝒟] lifts a non-relational value abstraction into a (leaf)
    abstract domain of partial environments from variables to values.
*)

open Ast.All
open Core.All
open Core.Sig.Lowlevel.Value


module Make(Value: VALUE) =
struct


  (*==========================================================================*)
                          (** {2 Lattice structure} *)
  (*==========================================================================*)

  (** Map with variables as keys. *)
  module VarMap =
    Lattices.Partial_map.Make
      (Var)
      (Value)

  include VarMap

  include GenDomainId(struct
      type typ = t
      let name = Value.name
    end)

  let merge pre (post1, log1) (post2, log2) =
    assert false

  let print fmt a =
    Format.fprintf fmt "%s:@ @[   %a@]@\n" Value.display VarMap.print a

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

  (** Value manager *)
  let rec vman (a:t) : (Value.t,Value.t) vman = {
    vget = (fun v -> v);
    vset = (fun v _ -> v);
    veval = (fun e -> eval e a |> snd);
    vask = (fun query ->
        let r = Value.EvalQuery.handle query (fun e -> eval e a |> snd) in
        match r with
        | Some rr -> rr
        | None -> Exceptions.panic "query with no reply"
      );
  }


  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  and eval (e:expr) (a:t) : aexpr * Value.t =
    match ekind e with
      | E_var(var, _) ->
        let v = VarMap.find var a in
        (A_var (var, v), v)

      | E_constant(c) ->
        let v = Value.of_constant c in
        (A_cst (c, v), v)

      | E_unop (op,e1) ->
        let (ae1, v1) = eval e1 a in
        let v = Value.unop (vman a) op v1 in
        (A_unop (op, ae1, v1), v)

      | E_binop (op,e1,e2) ->
        let (ae1, v1) = eval e1 a in
        let (ae2, v2) = eval e2 a in
        let v = Value.binop (vman a) op v1 v2 in
        A_binop (op, ae1, v1, ae2, v2), v

      | _ ->
        (* unsupported -> ⊤ *)
        A_unsupported, Value.top



  (** Backward refinement of expressions; given an annotated tree, and
      a target value, refine the environment using the variables in the
      expression *)
  let rec refine (ae:aexpr) (v:Value.t) (r:Value.t) (a:t) : t =
    let r' = Value.meet v r in
    match ae with
    | A_var (var, _) ->
      if Value.is_bottom r'
      then bottom
      else VarMap.add var r' a

    | A_cst(_) ->
      if Value.is_bottom r'
      then bottom
      else a

    | A_unop (op, ae1, v1) ->
      let w = Value.bwd_unop (vman a) op v1 r' in
      refine ae1 v1 w a

    | A_binop (op, ae1, v1, ae2, v2) ->
      let w1, w2 = Value.bwd_binop (vman a) op v1 v2 r' in
      let a1 = refine ae1 v1 w1 a in
      refine ae2 v2 w2 a1

    | A_unsupported ->
      a

  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let rec filter (e:expr) (r:bool) (a:t) : t =
    match ekind e with

    | E_unop (O_log_not, e) ->
      filter e (not r) a

    | E_binop (O_log_and, e1, e2) ->
      let a1 = filter e1 r a in
      let a2 = filter e2 r a in
      (if r then meet else join) a1 a2

    | E_binop (O_log_or, e1, e2) ->
      let a1 = filter e1 r a in
      let a2 = filter e2 r a in
      (if r then join else meet) a1 a2

    | E_constant c ->
      let v = Value.of_constant c in
      let w = Value.filter (vman a) v r in
      if Value.is_bottom w then bottom else a

    | E_var(var, _) ->
      let v = find var a in
      let w = Value.filter (vman a) v r in
      if Value.is_bottom w then bottom else add var w a

    (* arithmetic comparison part, handled by Value *)
    | E_binop (op, e1, e2) ->
      (* evaluate forward each argument expression *)
      let ae1,v1 = eval e1 a in
      let ae2,v2 = eval e2 a in
      (* apply comparison *)
      let r1, r2 = Value.compare (vman a) op v1 v2 r in
      (* propagate backward on both argument expressions *)
      let a1 = refine ae1 v1 r1 a in
      refine ae2 v2 r2 a1

    | _ -> assert false



  (*==========================================================================*)
                         (** {2 Transfer function} *)
  (*==========================================================================*)


  let init prog = empty

  let zone = Value.zone

  let rec exec stmt (map:t) : t =
    match skind stmt with
    | S_remove { ekind = E_var (v, _) }  ->
      VarMap.remove v map

    | S_add { ekind = E_var (v, _) } ->
      VarMap.add v Value.top map

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars ->
      let vars = List.map (function
            | { ekind = E_var (v, _) } -> v
            | _ -> assert false
          ) vars
      in
      VarMap.fold (fun v _ acc ->
          if List.exists (fun v' -> compare_var v v' = 0) vars then acc else VarMap.remove v acc
        ) map map

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let v = VarMap.find var1 map in
      VarMap.remove var1 map |> VarMap.add var2 v

    | S_forget { ekind = E_var (var, _) } ->
      add var Value.top map

    | S_assign ({ ekind= E_var (var, mode) }, e)  ->
      let _, v = eval e map in
      let map' = VarMap.add var v map in
      begin
        match mode with
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
          add v' value acc
        ) map vl

    (* FIXME: check weak variables in rhs *)
    | S_assume e ->
      filter e true map

    | _ -> top



  (** Evaluation query *)
  (** **************** *)

  let ask : type r. r Query.query -> t -> r option =
    fun query map ->
      Value.EvalQuery.handle query (fun exp ->
          eval exp map |>
          snd
        )

end
