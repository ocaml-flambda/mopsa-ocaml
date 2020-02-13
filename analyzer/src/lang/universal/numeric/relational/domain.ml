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

(** Relational numeric abstract domain, based on APRON. *)

open Mopsa
open Rounding
open Ast
open Apron_manager
open Apron_transformer



(** Query to retrieve relational variables *)

type _ query +=
    | Q_related_vars : var -> var list query
    | Q_constant_vars : var list query


let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_related_vars _ -> a @ b
          | Q_constant_vars -> a @ b
          | _ -> next.join_query query a b
        in
        f
      );
      meet = (
        let f : type r. query_pool -> r query -> r -> r -> r =
          fun next query a b ->
            match query with
            | Q_related_vars _ -> a @ b
            | Q_constant_vars -> a @ b
            | _ -> next.meet_query query a b
        in
        f
      );
    }


(** Factory functor *)

module Make(ApronManager : APRONMANAGER) =
struct


  include ApronTransformer(ApronManager)


  type t =
    ApronManager.t Apron.Abstract1.t (** Abstract element *) *
    Binding.t (** Bindings between Mopsa and Apron variables *)


  include GenDomainId(struct
      type nonrec t = t
      let name = ApronManager.name
    end)



  (** {2 Command-line options} *)
  (** ************************ *)
  let () =
    import_standalone_option Rounding.name ~into:name


  (** {2 Environment utility functions} *)
  (** ********************************* *)

  let unify abs1 abs2 =
    let env1 = Apron.Abstract1.env abs1 and env2 = Apron.Abstract1.env abs2 in
    let env = Apron.Environment.lce env1 env2 in
    (Apron.Abstract1.change_environment ApronManager.man abs1 env false),
    (Apron.Abstract1.change_environment ApronManager.man abs2 env false)

  let add_missing_vars (a,bnd) lv =
    let env = Apron.Abstract1.env a in
    let lv = List.sort_uniq compare lv in
    let lv = List.filter (fun v -> not (Apron.Environment.mem_var env (Binding.mk_apron_var v))) lv in

    let int_vars, bnd =
      List.filter (fun v -> vtyp v = T_int || vtyp v = T_bool) lv |>
      Binding.vars_to_apron bnd
    in

    let float_vars, bnd =
      List.filter (function { vtyp = T_float _} -> true | _ -> false) lv |>
      Binding.vars_to_apron bnd
    in


    let env' = Apron.Environment.add env
        (Array.of_list int_vars)
        (Array.of_list float_vars)
    in
    Apron.Abstract1.change_environment ApronManager.man a env' false,
    bnd


  (** {2 Lattice operators} *)
  (** ********************* *)

  let top = Apron.Abstract1.top ApronManager.man empty_env, Binding.empty

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env, Binding.empty

  let is_bottom (abs,_) =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let subset (abs1,_) (abs2,_) =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.is_leq ApronManager.man abs1' abs2'

  let join (abs1,bnd1) (abs2,bnd2) =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.join ApronManager.man abs1' abs2', Binding.concat bnd1 bnd2

  let meet (abs1,bnd1) (abs2,bnd2) =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.meet ApronManager.man abs1' abs2', Binding.concat bnd1 bnd2

  let widen bnd (abs1,bnd1) (abs2,bnd2) =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.widening ApronManager.man abs1' abs2', Binding.concat bnd1 bnd2

  let print fmt (abs,_) =
    Format.fprintf fmt "%s:@,  @[%a@]@\n"
      ApronManager.name
      Apron.Abstract1.print abs


  (** {2 Transfer functions} *)
  (** ********************** *)

  let zones = [Zone.Z_u_num; Zone.Z_u_int; Zone.Z_u_float]

  let init prog = top

  let forget_var v (a,bnd) =
    let env = Apron.Abstract1.env a in
    let vars, bnd =
      List.filter (fun v -> is_env_var v a) [v] |>
      Binding.vars_to_apron bnd
    in
    let env = Apron.Environment.remove env (Array.of_list vars) in
    (Apron.Abstract1.change_environment ApronManager.man a env true, bnd)


  let merge (pre,bnd) ((a1,bnd1),log1) ((a2,bnd2),log2) =
    let bnd = Binding.concat bnd1 bnd2 in
    let patch stmt a acc =
      match skind stmt with
      | S_forget { ekind = E_var (var, _) }
      | S_add { ekind = E_var (var, _) }
      | S_remove { ekind = E_var (var, _) }
      | S_assign({ ekind = E_var (var, _)}, _) ->
        let acc', _ = forget_var var (acc,bnd) in
        acc'
      | S_rename ( {ekind = E_var (var1, _)}, {ekind = E_var (var2, _)} ) ->
        let acc', _ = forget_var var1 (acc,bnd) |>
                      forget_var var2
        in
        acc'

      | S_expand({ekind = E_var(var,_)}, vl) ->
        let vars = List.map (function { ekind = E_var(v,_) } -> v | _ -> assert false) vl in
        let acc', _ = List.fold_left (fun acc v -> forget_var v acc) (acc,bnd) vars in
        acc'


      | S_assume _ ->
        acc

      | _ -> panic ~loc:__LOC__ "merge: unsupported statement %a" pp_stmt stmt
    in
    let a1' = List.fold_left (fun acc stmt -> patch stmt a1 acc) a2 (List.rev log1) in
    let a2' = List.fold_left (fun acc stmt -> patch stmt a2 acc) a1 (List.rev log2) in
    meet (a1',bnd) (a2',bnd)


  let rec exec ctx stmt man (a,bnd) =
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      add_missing_vars (a,bnd) [var] |>
      OptionExt.return

    | S_remove { ekind = E_var (var, _) }
    | S_forget { ekind = E_var (var, _) } ->
      forget_var var (a,bnd) |>
      OptionExt.return


    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let a, bnd = add_missing_vars (a,bnd) [var1] in
      let a, bnd = forget_var var2 (a,bnd) in
      let v1, bnd = Binding.var_to_apron bnd var1 in
      let v2, bnd = Binding.var_to_apron bnd var2 in
      (Apron.Abstract1.rename_array ApronManager.man a [| v1  |] [| v2 |], bnd) |>
      OptionExt.return

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars
      ->
      let vars = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vars
      in
      let env = Apron.Abstract1.env a in
      let vars, bnd = Binding.vars_to_apron bnd vars in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Some (
        Apron.Abstract1.change_environment ApronManager.man a new_env true,
        bnd
      )

    | S_assign({ ekind = E_var (var, mode) }, e) when var_mode var mode = STRONG ->
      let a, bnd = add_missing_vars (a,bnd) (var :: (Visitor.expr_vars e)) in
      let v = Binding.mk_apron_var var in
      begin try
          let e, a, bnd, l = exp_to_apron e (a,bnd) [] in
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv e in
          let a' = Apron.Abstract1.assign_texpr ApronManager.man a v texp None |>
                   remove_tmp l
          in
          Some (a', bnd)
        with UnsupportedExpression ->
          exec ctx (mk_remove_var var stmt.srange) man (a,bnd)
      end

    | S_assign({ ekind = E_var (var, mode) } as lval, e) when var_mode var mode = WEAK ->
      let lval' = { lval with ekind = E_var(var, Some STRONG) } in
      exec ctx {stmt with skind = S_assign(lval', e)} man (a,bnd) |>
      OptionExt.lift @@ fun (a',bnd') ->
      join (a,bnd) (a', bnd')

    | S_fold( {ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl ->
      begin
        let vl = List.map (function
            | { ekind = E_var (v, _) } -> v
            | _ -> assert false
          ) vl
        in
        match vl with
        | [] -> Exceptions.panic "Can not fold list of size 0"
        | p::q ->
          let vars, bnd = Binding.vars_to_apron bnd vl in
          let abs = Apron.Abstract1.fold ApronManager.man a
              (Array.of_list vars)
          in
          let pp, bnd = Binding.var_to_apron bnd p in
          let vv, bnd = Binding.var_to_apron bnd v in
          let abs' = Apron.Abstract1.rename_array ApronManager.man abs
              [| pp |] [| vv |]
          in
          Some (abs', bnd)
      end

    | S_expand({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let v, bnd = Binding.var_to_apron bnd v in
      let vl, bnd = Binding.vars_to_apron bnd vl in
      let abs = Apron.Abstract1.expand ApronManager.man a
          v (Array.of_list vl) in
      let env = Apron.Environment.remove (Apron.Abstract1.env abs) [| v |] in
      let abs' = Apron.Abstract1.change_environment ApronManager.man abs env false in
      Some (abs', bnd)

    | S_assume(e) -> begin
        let a, bnd = add_missing_vars (a,bnd) (Visitor.expr_vars e) in
        let env = Apron.Abstract1.env a in

        let join_list l = List.fold_left
            (Apron.Abstract1.join ApronManager.man)
            (Apron.Abstract1.bottom ApronManager.man env)
            l
        in
        let meet_list l = tcons_array_of_tcons_list env l |>
                          Apron.Abstract1.meet_tcons_array ApronManager.man a
        in

        try
          let dnf, a, bnd, l = bexp_to_apron e (a,bnd) [] in
          let a' = Dnf.apply_list
            (fun (op,e1,typ1,e2,typ2) ->
               let typ =

                 let is_integer = function
                   | T_int | T_bool -> true
                   | _ -> false
                 in

                 let is_float = function
                   | T_float _ -> true
                   | _ -> false
                 in

                 if is_float typ1 || is_float typ2
                 then Apron.Texpr1.Real

                 else if is_integer typ1 && is_integer typ2
                 then Apron.Texpr1.Int

                 else
                   Exceptions.panic_at (srange stmt)
                     "Unsupported case (%a, %a) in stmt @[%a@]"
                     pp_typ typ1 pp_typ typ2 pp_stmt stmt
               in
               let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
               let diff_texpr = Apron.Texpr1.of_expr env diff in
               Apron.Tcons1.make diff_texpr op
            )
            join_list meet_list dnf |>
                   remove_tmp l
          in
          Some (a', bnd)
        with UnsupportedExpression -> OptionExt.return (a,bnd)
      end

    | _ -> None

  let vars (abs,bnd) =
    fold_env (fun v acc ->
        let vv = Binding.apron_to_var bnd v in
        vv :: acc
      ) (Apron.Abstract1.env abs) []

  let bound_var v (abs,bnd) =
    if is_env_var v abs then
      let vv = Binding.mk_apron_var v in
      Apron.Abstract1.bound_variable ApronManager.man abs vv |>
      Values.Intervals.Integer.Value.of_apron
    else
      Values.Intervals.Integer.Value.top


  let eval_interval e (abs,bnd) =
    match ekind e with
    | E_var (v,_) -> bound_var v (abs,bnd)
    | _ ->
      try
        let abs, bnd = add_missing_vars (abs,bnd) (Visitor.expr_vars e) in
        let e, abs, bnd, _ = exp_to_apron e (abs,bnd) [] in
        let env = Apron.Abstract1.env abs in
        let e = Apron.Texpr1.of_expr env e in
        Apron.Abstract1.bound_texpr ApronManager.man abs e |>
        Values.Intervals.Integer.Value.of_apron
      with UnsupportedExpression ->
        Values.Intervals.Integer.Value.top


  let ask : type r. r query -> t -> r option =
    fun query (abs,bnd) ->
      match query with
      | Common.Q_int_interval e ->
        eval_interval e (abs,bnd) |>
        OptionExt.return

      | Common.Q_int_congr_interval e ->
        (eval_interval e (abs,bnd), Common.C.minf_inf) |>
        OptionExt.return

      | Q_related_vars v ->
        related_vars v (abs,bnd) |>
        OptionExt.return

      | Q_constant_vars ->
        constant_vars (abs,bnd) |>
        OptionExt.return


      | _ -> None

  let refine channel a = Channel.return a

end
