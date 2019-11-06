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
open Common.Var_bounds



(** {2 Identifier for the non-relation domain} *)
(** ****************************************** *)

type _ id += D_nonrel : 'v vmodule -> (var,'v) Lattices.Partial_map.map id



(** Generic merge operation for non-relational domains *)
let generic_nonrel_merge ~top ~add ~remove ~find ~meet pre (a1, log1) (a2, log2) =
  let patch stmt a acc =
    match skind stmt with
    | S_forget { ekind = E_var (var, _) }
    | S_add { ekind = E_var (var, _) }
    | S_assign({ ekind = E_var (var, _)}, _) ->
      add var top acc

    | S_rename ( {ekind = E_var (var1, _)}, {ekind = E_var (var2, _)} ) ->
      let v = find var2 a in
      remove var1 acc |>
      add var2 v

    | S_remove { ekind = E_var (var, _) } ->
      remove var acc

    | S_assume e -> acc

    | _ -> Exceptions.panic ~loc:__LOC__ "merge: unsupported statement %a" pp_stmt stmt
  in
  let a2' = List.fold_left (fun acc stmt -> patch stmt a1 acc) a2 log1 in
  let a1' = List.fold_left (fun acc stmt -> patch stmt a2 acc) a1 log2 in
  meet a1' a2'



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

  let () =
    let open Eq in
    register_id {
      eq = (
        let f : type a. a id -> (a, t) eq option =
          function
          | D_nonrel v ->
            begin
              let module V = (val v) in
              match equal_id V.id Value.id with
              | Some Eq -> Some Eq
              | None -> None
            end
          | _ -> None
        in
        f
      );
    }


  (** Constrain the value of a variable with its bounds *)
  let constrain_var_with_bounds uctx v a =
    match find_var_bounds_ctx_opt v uctx with
    | None -> a
    | Some bounds ->
      let aa = Value.of_constant v.vtyp bounds in
      Value.meet a aa


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
            (fun v v1 v2 -> Value.widen uctx v1 v2 |>
                            constrain_var_with_bounds uctx v
            )
            m1 m2
        )



  let name = Value.name

  let debug fmt = Debug.debug ~channel:name fmt

  let find v a =
    try find v a
    with Not_found -> Exceptions.warn "variable %a not found" pp_var v; raise Not_found

  let merge pre (a1, log1) (a2, log2) = generic_nonrel_merge ~top:Value.top ~add ~remove ~find ~meet pre (a1, log1) (a2, log2)

  let print fmt a =
    Format.fprintf fmt "%s:@,@[   %a@]@\n" Value.display VarMap.print a


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Expressions annotated with abstract values; useful for assignment and compare. *)
  type aexpr =
    | A_var of var * mode * Value.t
    | A_cst of constant * Value.t
    | A_unop of operator * typ  * aexpr * Value.t
    | A_binop of operator * typ * aexpr * Value.t * aexpr * Value.t
    | A_unsupported


  (** Pretty printer of annotated expressions *)
  let rec pp_aexp fmt = function
    | A_var (var,mode,v) -> Format.fprintf fmt "<%a:%a>" pp_var var Value.print v
    | A_cst (c,v) -> Format.fprintf fmt "<%a:%a>" pp_constant c Value.print v
    | A_unop (op,t,ae,v) -> Format.fprintf fmt "%a<%a:%a>" pp_operator op pp_aexp ae Value.print v
    | A_binop (op,t,ae1,v1,ae2,v2) -> Format.fprintf fmt "<%a:%a> %a <%a:%a>" pp_aexp ae1 Value.print v1 pp_operator op pp_aexp ae2 Value.print v2
    | A_unsupported -> Format.pp_print_string fmt "?"


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
    | E_var(var, mode) ->
      let v = find var a in
      (A_var (var, mode, v), v) |>
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
    | A_var (var, mode, _) ->
      if Value.is_bottom r'
      then bottom

      else
      if mode = STRONG
      then VarMap.add var r' a

      else a

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

    | E_var(var, mode) ->
      let v = find var a in
      let w = Value.filter (man a) v r in
      (if Value.is_bottom w then bottom else
       if mode = STRONG then add var w a
       else a
      ) |>
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

  let init prog = empty

  let zones = Value.zones

  let exec ctx stmt man (map:t) : t option =
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
      let v = find var1 map in
      VarMap.remove var1 map |>
      VarMap.add var2 v |>
      Option.return

    | S_forget { ekind = E_var (var, _) } ->
      add var Value.top map |>
      Option.return

    | S_assign ({ ekind= E_var (var, mode) }, e)  ->
      eval e map |> Option.lift @@ fun (_, v) ->
      let vv = constrain_var_with_bounds ctx var v in
      let map' = VarMap.add var vv map in
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



  let ask query map =
    Value.ask (man map) query


  let refine channel a =
    assert false


end
