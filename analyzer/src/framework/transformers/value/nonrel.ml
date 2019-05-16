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
open Sig.Value.Lowlevel


(** {2 Variable maps} *)
(** ***************** *)

module VarMap = Lattices.Partial_map.MakePolymorph(Var)


(** {2 Identifier for the non-relation domain} *)
(** ****************************************** *)

type _ domain +=
  | D_nonrel_id : 'v vmodule -> 'v VarMap.t domain


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

  let id = D_nonrel_id (module Value)

  let () =
    let open Eq in
    register_domain_id {
      eq = (
        let f : type a. a domain -> (a, t) eq option =
          function
          | D_nonrel_id vmodule ->
            begin
              let module V = (val vmodule) in
              match Core.Id.value_id_eq V.id Value.id with
              | Some Eq -> Some Eq
              | None -> None
            end
          | _ -> None
        in
        f
      );
    }


  let name = Value.name

  let debug fmt = Debug.debug ~channel:name fmt

  let merge ctx pre (a1, log1) (a2, log2) =
    debug "@[<v>merging:@, pre-condition: %a@, post-condition #1: %a@, log #1: %a@, post-condition #2: %a@, log #2: %a@]"
      VarMap.print pre
      VarMap.print a1
      pp_block log1
      VarMap.print a2
      pp_block log2
    ;

    let patch stmt a acc =
      match skind stmt with
      | S_forget { ekind = E_var (var, _) }
      | S_add { ekind = E_var (var, _) } ->
        add var Value.top acc

      | S_remove { ekind = E_var (var, _) } ->
        remove var acc

      | S_assign({ ekind = E_var (var, _)}, _) ->
        let v = find var a in
        add var v acc

      | S_assume e ->
        let vars = expr_vars e in
        vars |> List.fold_left (fun acc var ->
            let v = find var a in
            add var v acc
          ) acc

      | _ -> Exceptions.panic ~loc:__LOC__ "merge: unsupported statement %a" pp_stmt stmt
    in
    let acc = List.fold_left (fun acc stmt -> patch stmt a1 acc) a2 log1 in
    List.fold_left (fun acc stmt -> patch stmt a2 acc) acc log2


  let print fmt a =
    Format.fprintf fmt "%s:@,@[   %a@]@\n" Value.display VarMap.print a

  let widen ctx = VarMap.widen

  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Expressions annotated with abstract values; useful for assignment and compare. *)
  type aexpr =
    | A_var of var * Value.t
    | A_cst of constant * Value.t
    | A_unop of operator * typ  * aexpr * Value.t
    | A_binop of operator * typ * aexpr * Value.t * aexpr * Value.t
    | A_unsupported

  (** Value manager *)
  let rec man (a:t) : (Value.t,Value.t) man = {
    get = (fun v -> v);
    set = (fun v _ -> v);
    eval = (fun e ->
        eval e a |>
        Option.default (A_unsupported,Value.top) |>
        snd
      );
    cast = (fun id v ->
        match Value.get (man a) id v with
        | Some rr -> rr
        | None -> Exceptions.panic "cast not handled"
      );
  }


  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  and eval (e:expr) (a:t) : (aexpr * Value.t) option =
    match ekind e with
    | E_var(var, _) ->
      let v = VarMap.find var a in
      (A_var (var, v), v) |>
      Option.return

    | E_constant(c) ->
      let v = Value.of_constant e.etyp c in
      (A_cst (c, v), v) |>
      Option.return

    | E_unop (op,e1) ->
      eval e1 a |> Option.bind @@ fun (ae1, v1) ->
      let v = Value.unop (man a) e.etyp op v1 in
      (A_unop (op, e.etyp, ae1, v1), v) |>
      Option.return

    | E_binop (op,e1,e2) ->
      eval e1 a |> Option.bind @@ fun (ae1, v1) ->
      eval e2 a |> Option.bind @@ fun (ae2, v2) ->
      let v = Value.binop (man a) e.etyp op v1 v2 in
      (A_binop (op, e.etyp, ae1, v1, ae2, v2), v) |>
      Option.return

    | _ ->
      (* unsupported -> ⊤ *)
      (* A_unsupported, Value.top *)
      None



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

    | A_unop (op, typ, ae1, v1) ->
      let w = Value.bwd_unop (man a) typ op v1 r' in
      refine ae1 v1 w a

    | A_binop (op, typ, ae1, v1, ae2, v2) ->
      let w1, w2 = Value.bwd_binop (man a) typ op v1 v2 r' in
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
  let rec filter (e:expr) (r:bool) (a:t) : t option =
    match ekind e with

    | E_unop (O_log_not, e) ->
      filter e (not r) a

    | E_binop (O_log_and, e1, e2) ->
      filter e1 r a |> Option.bind @@ fun a1 ->
      filter e2 r a |> Option.bind @@ fun a2 ->
      (if r then meet else join) a1 a2 |>
      Option.return

    | E_binop (O_log_or, e1, e2) ->
      filter e1 r a |> Option.bind @@ fun a1 ->
      filter e2 r a |> Option.bind @@ fun a2 ->
      (if r then join else meet) a1 a2 |>
      Option.return

    | E_constant c ->
      let v = Value.of_constant e.etyp c in
      let w = Value.filter (man a) v r in
      (if Value.is_bottom w then bottom else a) |>
      Option.return

    | E_var(var, _) ->
      let v = find var a in
      let w = Value.filter (man a) v r in
      (if Value.is_bottom w then bottom else add var w a) |>
      Option.return

    (* arithmetic comparison part, handled by Value *)
    | E_binop (op, e1, e2) ->
      (* evaluate forward each argument expression *)
      eval e1 a |> Option.bind @@ fun (ae1,v1) ->
      eval e2 a |> Option.bind @@ fun (ae2,v2) ->

      (* apply comparison *)
      let r1, r2 = Value.compare (man a) e1.etyp op v1 v2 r in

      (* propagate backward on both argument expressions *)
      refine ae2 v2 r2 @@ refine ae1 v1 r1 a |>
      Option.return

    | _ -> assert false



  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog ctx = empty, ctx

  let zones = Value.zones

  let rec exec stmt (map:t) : t option =
    match skind stmt with
    | S_remove { ekind = E_var (v, _) }  ->
      VarMap.remove v map |>
      Option.return

    | S_add { ekind = E_var (v, _) } ->
      (* Check of the variable is already present *)
      if VarMap.mem v map
      then Option.return map
      else Option.return @@ VarMap.add v Value.top map


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
      Option.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let v = VarMap.find var1 map in
      VarMap.remove var1 map |>
      VarMap.add var2 v |>
      Option.return

    | S_forget { ekind = E_var (var, _) } ->
      add var Value.top map |>
      Option.return

    | S_assign ({ ekind= E_var (var, mode) }, e)  ->
      eval e map |> Option.lift @@ fun (_, v) ->
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
        ) map vl |>
      Option.return

    (* FIXME: check weak variables in rhs *)
    | S_assume e ->
      filter e true map

    | _ -> None


  let exec stmt ctx map =
    exec stmt map |> Option.lift @@ fun a' -> (a',ctx)


  let ask query ctx map =
    Value.ask (man map) query


  let refine channel a =
    assert false


end
