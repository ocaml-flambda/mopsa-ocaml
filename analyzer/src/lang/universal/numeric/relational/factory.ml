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
open Var_binding

(** Query to retrieve relational variables *)

type _ query +=
    | Q_related_vars : var -> var list query
    | Q_constant_vars : var list query


let () =
  register_query {
    query_join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_related_vars _ -> a @ b
          | Q_constant_vars -> a @ b
          | _ -> next.join query a b
        in
        f
      );
      query_meet = (
        let f : type r. query_pool -> r query -> r -> r -> r =
          fun next query a b ->
            match query with
            | Q_related_vars _ -> a @ b
            | Q_constant_vars -> a @ b
            | _ -> next.meet query a b
        in
        f
      );
    }


(** Factory functor *)

module Make(ApronManager : APRONMANAGER) =
struct

  include ApronTransformer(ApronManager)

  type t = ApronManager.t Apron.Abstract1.t

  include GenDomainId(struct
      type typ = t
      let name = "universal.numeric.relational." ^ ApronManager.name
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

  let add_missing_vars ctx abs lv =
    let env = Apron.Abstract1.env abs in
    let lv = List.sort_uniq compare lv in
    let lv = List.filter (fun v -> not (Apron.Environment.mem_var env (mk_apron_var v))) lv in

    let int_vars, ctx =
      List.filter (fun v -> vtyp v = T_int || vtyp v = T_bool) lv |>
      vars_to_apron ctx
    in

    let float_vars, ctx =
      List.filter (function { vtyp = T_float _} -> true | _ -> false) lv |>
      vars_to_apron ctx
    in
    

    let env' = Apron.Environment.add env
        (Array.of_list int_vars)
        (Array.of_list float_vars)
    in
    Apron.Abstract1.change_environment ApronManager.man abs env' false, ctx


  (** {2 Lattice operators} *)
  (** ********************* *)

  let top = Apron.Abstract1.top ApronManager.man empty_env

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env

  let is_bottom abs =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let subset abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.is_leq ApronManager.man abs1' abs2'

  let join abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.join ApronManager.man abs1' abs2'

  let meet abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.meet ApronManager.man abs1' abs2'

  let widen ctx abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.widening ApronManager.man abs1' abs2'

  let merge pre (post1,log1) (post2,log2) =
    assert false

  let print fmt abs =
    Format.fprintf fmt "%s:@,  @[%a@]@\n"
      ApronManager.name
      Apron.Abstract1.print abs




  (** {2 Transfer functions} *)
  (** ********************** *)

  let zones = [Zone.Z_u_num]

  let init prog ctx =
    top, init_ctx ctx

  let rec exec stmt ctx a =
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      add_missing_vars ctx a [var] |>
      Option.return

    | S_remove { ekind = E_var (var, _) } ->
      let env = Apron.Abstract1.env a in
      let vars, ctx =
        List.filter (fun v -> is_env_var v a) [var] |>
        vars_to_apron ctx
      in
      let env = Apron.Environment.remove env (Array.of_list vars) in
      (Apron.Abstract1.change_environment ApronManager.man a env true, ctx) |>
      Option.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let v1, ctx = var_to_apron ctx var1 in
      let v2, ctx = var_to_apron ctx var2 in 
      (Apron.Abstract1.rename_array ApronManager.man a [| v1  |] [| v2 |], ctx) |>
      Option.return

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars
      ->
      let vars = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vars
      in
      let env = Apron.Abstract1.env a in
      let vars, ctx = vars_to_apron ctx vars in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Apron.Abstract1.change_environment ApronManager.man a new_env true |>
      with_context ctx |>
      Option.return

    | S_assign({ ekind = E_var (var, STRONG) }, e) ->
      let a, ctx = add_missing_vars ctx a (var :: (Visitor.expr_vars e)) in
      let v = mk_apron_var var in
      let e, a, ctx, l = strongify_rhs e ctx a [] in
      begin try
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv e in
          Apron.Abstract1.assign_texpr ApronManager.man a v texp None |>
          remove_tmp l |>
          with_context ctx |>
          Option.return
        with UnsupportedExpression ->
          exec (mk_remove_var var stmt.srange) ctx a
      end

    | S_assign({ ekind = E_var (var, WEAK) } as lval, e) ->
      let lval' = { lval with ekind = E_var(var, STRONG) } in
      exec {stmt with skind = S_assign(lval', e)} ctx a |>
      Option.lift @@ fun (a',ctx) ->
      join a a', ctx

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
          let vars, ctx = vars_to_apron ctx vl in
          let abs = Apron.Abstract1.fold ApronManager.man a
              (Array.of_list vars)
          in
          let pp, ctx = var_to_apron ctx p in
          let vv, ctx = var_to_apron ctx v in
          Apron.Abstract1.rename_array ApronManager.man abs
            [| pp |] [| vv |] |>
          with_context ctx |>
          Option.return
      end

    | S_expand({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let v, ctx = var_to_apron ctx v in
      let vl, ctx = vars_to_apron ctx vl in
      let abs = Apron.Abstract1.expand ApronManager.man a
          v (Array.of_list vl) in
      let env = Apron.Environment.remove (Apron.Abstract1.env abs) [| v |] in
      Apron.Abstract1.change_environment ApronManager.man abs env false |>
      with_context ctx |>
      Option.return

    | S_assume(e) -> begin
        let a, ctx = add_missing_vars ctx a (Visitor.expr_vars e) in
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
          let dnf, ctx = bexp_to_apron ctx e in
          Dnf.apply_list
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
          with_context ctx |>
          Option.return
        with UnsupportedExpression -> Option.return (a,ctx)
      end

    | _ -> None

  
  let ask : type r. r query -> Context.uctx -> t -> r option =
    fun query ctx abs ->
      match query with
      | Values.Intervals.Integer.Value.Q_interval e ->
        let e, ctx = exp_to_apron ctx e in
        let env = Apron.Abstract1.env abs in
        let e = Apron.Texpr1.of_expr env e in
        Apron.Abstract1.bound_texpr ApronManager.man abs e |>
        Values.Intervals.Integer.Value.of_apron |>
        Option.return

      | Q_related_vars v ->
        related_vars v ctx abs |>
        Option.return

      | Q_constant_vars ->
        constant_vars ctx abs |>
        Option.return


      | _ -> None

  let refine channel a = Channel.return a

end
