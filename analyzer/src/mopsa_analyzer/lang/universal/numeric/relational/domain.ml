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
open Sig.Abstraction.Simplified
open Rounding
open Ast
open Apron_manager
open Apron_transformer



(** Query to retrieve relational variables *)

type ('a,_) query +=
    | Q_related_vars : var -> ('a,var list) query
    | Q_constant_vars : ('a,var list) query


let () =
  register_query {
    join = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_related_vars _ -> a @ b
          | Q_constant_vars -> a @ b
          | _ -> next.pool_join query a b
        in
        f
      );
      meet = (
        let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
          fun next query a b ->
            match query with
            | Q_related_vars _ -> a @ b
            | Q_constant_vars -> a @ b
            | _ -> next.pool_meet query a b
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
    import_shared_option Rounding.name name


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
    let lv = List.filter (fun v -> not (Apron.Environment.mem_var env (Binding.mopsa_to_apron_var v bnd |> fst))) lv in

    let int_vars, bnd =
      let lv' = List.filter (fun v -> vtyp v = T_int || vtyp v = T_bool) lv in
      Binding.mopsa_to_apron_vars lv' bnd
    in

    let float_vars, bnd =
      let lv' = List.filter (function { vtyp = T_float _} -> true | _ -> false) lv in
      Binding.mopsa_to_apron_vars lv' bnd
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


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog = top

  let remove_var (v:var) (a,bnd) =
    let env = Apron.Abstract1.env a in
    if is_env_var v (a,bnd) then
      let vv,bnd = Binding.mopsa_to_apron_var v bnd in
      let env = Apron.Environment.remove env [|vv|] in
      let bnd = Binding.remove_apron_var vv bnd in
      (Apron.Abstract1.change_environment ApronManager.man a env true, bnd)
    else
      (a,bnd)


  let forget_var v (a,bnd) =
    let v,bnd = Binding.mopsa_to_apron_var v bnd in
    Apron.Abstract1.forget_array ApronManager.man a [|v|] false, bnd


  let merge (pre,bnd) ((a1,bnd1),e1) ((a2,bnd2),e2) =
    let bnd = Binding.concat bnd1 bnd2 in
    (* FIXME: the use here the generic merge provided by [Framework.Core.Effect]
       and we do not provide a [find] function. Instead, we forget all modified
       and removed variables, which is sound, but inefficient. This should be
       improved by return intervals in [find] and use them in [add]. *)
    let x1,x2 =
      generic_merge
        ~add:(fun v () x -> add_missing_vars x [v] |> forget_var v)
        ~find:(fun v x -> ())
        ~remove:(fun v x -> remove_var v x)
        ((a1,bnd),e1) ((a2,bnd),e2)
    in
    meet x1 x2


  let rec exec stmt man ctx (a,bnd) =
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      add_missing_vars (a,bnd) [var] |>
      OptionExt.return

    | S_remove { ekind = E_var (var, _) } ->
      remove_var var (a,bnd) |>
      OptionExt.return

    | S_forget { ekind = E_var (var, _) } ->
      forget_var var (a,bnd) |>
      OptionExt.return


    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) ->
      let a, bnd' = add_missing_vars (a,bnd) [var1] in
      let a, bnd' = remove_var var2 (a,bnd') in
      let v1, _ = Binding.mopsa_to_apron_var var1 bnd in
      let v2, _ = Binding.mopsa_to_apron_var var2 bnd in
      (Apron.Abstract1.rename_array ApronManager.man a [| v1  |] [| v2 |], bnd') |>
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
      let vars, bnd = Binding.mopsa_to_apron_vars vars bnd in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let bnd = Binding.remove_apron_vars to_remove bnd in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Some (
        Apron.Abstract1.change_environment ApronManager.man a new_env true,
        bnd
      )

    | S_assign({ ekind = E_var (var, mode) }, e) when var_mode var mode = STRONG ->
      let a, bnd = add_missing_vars (a,bnd) (var :: (Visitor.expr_vars e)) in
      let v, bnd = Binding.mopsa_to_apron_var var bnd in
      begin try
          let e, a, bnd, l = exp_to_apron e (a,bnd) [] in
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv e in
          let a' = Apron.Abstract1.assign_texpr ApronManager.man a v texp None |>
                   remove_tmp l
          in
          Some (a', bnd)
        with ImpreciseExpression -> exec (mk_forget_var var stmt.srange) man ctx (a,bnd)
           | UnsupportedExpression -> None
      end

    | S_assign({ ekind = E_var (var, mode) } as lval, e) when var_mode var mode = WEAK ->
      let lval' = { lval with ekind = E_var(var, Some STRONG) } in
      exec {stmt with skind = S_assign(lval', e)} man ctx (a,bnd) |>
      OptionExt.lift @@ fun (a',bnd') ->
      join (a,bnd) (a', bnd')


    | S_expand({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let v, bnd = Binding.mopsa_to_apron_var v bnd in
      let vl, bnd = Binding.mopsa_to_apron_vars vl bnd in
      let abs' = Apron.Abstract1.expand ApronManager.man a v (Array.of_list vl) in
      Some (abs', bnd)

    | S_fold({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let v, bnd = Binding.mopsa_to_apron_var v bnd in
      let vl, bnd = Binding.mopsa_to_apron_vars vl bnd in
      let bnd = Binding.remove_apron_vars vl bnd in
      let abs' = Apron.Abstract1.fold ApronManager.man a (Array.of_list (v::vl)) in
      Some (abs', bnd)

    | S_assume(e) -> begin
        let a, bnd = add_missing_vars (a,bnd) (Visitor.expr_vars e) in
        let env = Apron.Abstract1.env a in

        try
          let dnf, a, bnd, l = bexp_to_apron e (a,bnd) [] in
          let a' =
            Dnf.reduce_conjunction
              (fun conj ->
                 let tcons_list =
                   List.map
                     (fun (op,e1,typ1,e2,typ2) ->
                        let typ =
                          if is_float_type typ1 || is_float_type typ2 then Apron.Texpr1.Real else
                          if is_int_type typ1 && is_int_type typ2 then Apron.Texpr1.Int
                          else Exceptions.panic_at (srange stmt)
                              "Unsupported case (%a, %a) in stmt @[%a@]"
                              pp_typ typ1 pp_typ typ2 pp_stmt stmt
                        in
                        let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
                        let diff_texpr = Apron.Texpr1.of_expr env diff in
                        Apron.Tcons1.make diff_texpr op
                     ) conj
                 in
                 tcons_array_of_tcons_list env tcons_list |>
                 Apron.Abstract1.meet_tcons_array ApronManager.man a
              ) ~join:(Apron.Abstract1.join ApronManager.man) dnf |>
            remove_tmp l
          in
          Some (a', bnd)
        with ImpreciseExpression -> Some (a,bnd)
           | UnsupportedExpression -> None
      end

    | _ -> None

  let vars (abs,bnd) =
    fold_env (fun v acc ->
        let vv = Binding.apron_to_mopsa_var v bnd in
        vv :: acc
      ) (Apron.Abstract1.env abs) []

  let bound_var v (abs,bnd) =
    if is_env_var v (abs,bnd) then
      let vv,_ = Binding.mopsa_to_apron_var v bnd in
      Apron.Abstract1.bound_variable ApronManager.man abs vv |>
      Values.Intervals.Integer.Value.of_apron
    else
      Values.Intervals.Integer.Value.top


  let eval_interval e (abs,bnd) =
    match ekind e with
    | E_var (v,_) -> Some (bound_var v (abs,bnd))
    | _ ->
      try
        let abs, bnd = add_missing_vars (abs,bnd) (Visitor.expr_vars e) in
        let e, abs, bnd, _ = exp_to_apron e (abs,bnd) [] in
        let env = Apron.Abstract1.env abs in
        let e = Apron.Texpr1.of_expr env e in
        Apron.Abstract1.bound_texpr ApronManager.man abs e |>
        Values.Intervals.Integer.Value.of_apron |>
        OptionExt.return
      with ImpreciseExpression -> Some (Values.Intervals.Integer.Value.top)
         | UnsupportedExpression -> None


  let ask : type r. ('a,r) query -> ('a,t) simplified_man -> 'a ctx -> t -> r option =
    fun query man ctx (abs,bnd) ->
      match query with
      | Q_avalue(e, Common.V_int_interval) ->
        eval_interval e (abs,bnd)

      | Q_related_vars v ->
        related_vars v (abs,bnd) |>
        OptionExt.return

      | Q_constant_vars ->
        constant_vars (abs,bnd) |>
        OptionExt.return


      | _ -> None


  let print_state printer (a,_) =
    unformat Apron.Abstract1.print printer a
      ~path:[Key "numeric-relations"]

  let print_expr man ctx a printer exp =
    print_state printer a

end
