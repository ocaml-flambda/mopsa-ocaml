(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Relational numeric abstract domain, based on APRON. *)

open Framework.Lattice
open Framework.Domains
open Framework.Domains.Stateful
open Framework.Manager
open Framework.Ast
open Framework.Exec
open Ast

let name = "universal.numeric.domains.relational"

let debug fmt = Debug.debug ~channel:name fmt

module type APRONMANAGER =
sig
  type t
  val man: t Apron.Manager.t
end

module Make(ApronManager : APRONMANAGER) =
struct

  type t = ApronManager.t Apron.Abstract1.t
  let name = name

  exception Unsupported

  let default_rounding = Apron.Texpr1.Near

  (** {2 Environment utility functions} *)
  let empty_env = Apron.Environment.make [||] [||]

  let unify abs1 abs2 =
    let env1 = Apron.Abstract1.env abs1 and env2 = Apron.Abstract1.env abs2 in
    let env = Apron.Environment.lce env1 env2 in
    (Apron.Abstract1.change_environment ApronManager.man abs1 env false),
    (Apron.Abstract1.change_environment ApronManager.man abs2 env false)

  let var_to_apron v =
    (* assert (v.vtyp <> TAny); *)
    Format.fprintf Format.str_formatter "%s:%a" (var_uniq_name v) Framework.Pp.pp_typ v.vtyp;
    let name = Format.flush_str_formatter () in
    Apron.Var.of_string name

  let is_env_var v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env (var_to_apron v)

  let add_missing_vars  abs lv =
    let env = Apron.Abstract1.env abs in
    let lv = List.sort_uniq compare lv in
    let lv = List.filter (fun v -> not (Apron.Environment.mem_var env (var_to_apron v))) lv in
    let env' = Apron.Environment.add env
        (
          Array.of_list @@
          List.map var_to_apron @@
          List.filter (function {vtyp = T_int} -> true | _ -> false) lv
        )
        (
          Array.of_list @@
          List.map var_to_apron @@
          List.filter (function {vtyp = T_float} -> true | _ -> false) lv
        )
    in
    Apron.Abstract1.change_environment ApronManager.man abs env' false


  (** {2 Lattice operators} *)
  let top = Apron.Abstract1.top ApronManager.man empty_env

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env

  let is_bottom abs =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let is_top abs =
    Apron.Abstract1.is_top ApronManager.man abs

  let leq abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.is_leq ApronManager.man abs1' abs2'

  let join  abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.join ApronManager.man abs1' abs2'

  let meet  abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.meet ApronManager.man abs1' abs2'

  let widening ctx abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.widening ApronManager.man abs1' abs2'

  let print fmt abs =
    Format.fprintf fmt "rel: @[%a@]"
      Apron.Abstract1.print abs

  let refine_var_type var =
    match var.vtyp with
    | T_int | T_float ->
      [var]
    | T_any ->
      [{var with vtyp = T_int}; {var with vtyp = T_float}]
    | _ -> assert false

  (* {2 Transfer functions} *)
  let init man ctx prog flow =
    ctx, set_domain_cur top man flow

  let rec exec man ctx stmt flow =
    let abs = get_domain_cur man flow in
    let return_cur abs =
      set_domain_cur abs man flow |>
      return
    in
    match skind stmt with
    | S_remove_var var ->
      let vars = refine_var_type var in
      let env = Apron.Abstract1.env abs in
      let vars =
        List.filter (fun v -> is_env_var v abs) vars |>
        List.map var_to_apron
      in
      let env = Apron.Environment.remove env (Array.of_list vars) in
      Apron.Abstract1.change_environment ApronManager.man abs env true |>
      return_cur

    | S_project_vars vars ->
      let vars = List.fold_left (fun acc v -> acc @ refine_var_type v) [] vars in
      let env = Apron.Abstract1.env abs in
      let vars = List.map var_to_apron vars in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Apron.Abstract1.change_environment ApronManager.man abs new_env true |>
      return_cur

    | S_assign({ekind = E_var v}, ({etyp = T_int | T_float} as e), STRONG) -> begin
        let v = {v with vtyp = e.etyp} in
        let abs = add_missing_vars abs (v :: (Framework.Visitor.expr_vars e)) in
        try
          let aenv = Apron.Abstract1.env abs in
          let texp = Apron.Texpr1.of_expr aenv (exp_to_apron e) in
          Apron.Abstract1.assign_texpr ApronManager.man abs (var_to_apron v) texp None |>
          return_cur
        with Unsupported ->
          exec man ctx {stmt with skind = S_remove_var v} flow
      end

    | S_assign({ekind = E_var v}, ({etyp = T_int | T_float}), WEAK) ->
      assert false

    | S_assign(({ekind = E_var x}), {ekind = E_var ({vtyp = T_int | T_float} as x0)}, EXPAND) ->
      let abs = add_missing_vars abs [x0] in
      let abs = set_domain_cur abs man flow |>
                exec man ctx (mk_stmt (S_remove_var x) stmt.srange) |>
                oflow_extract |>
                get_domain_cur man
      in
      let x = {x with vtyp = x0.vtyp} in
      Apron.Abstract1.expand ApronManager.man abs (var_to_apron x0) [| var_to_apron x |] |>
      return_cur

    | S_assign({ekind = E_var v}, _, _) ->
      exec man ctx {stmt with skind = S_remove_var v} flow

    | S_assume(e) -> begin
        let abs = add_missing_vars  abs (Framework.Visitor.expr_vars e) in
        let env = Apron.Abstract1.env abs in
        try
          let e' = bexp_to_apron e in
          e' |> Dnf.to_list |> List.map (fun c ->
              c |> List.map ( fun (op,e1,typ1,e2,typ2) ->
                  let typ =
                    match typ1, typ2 with
                    | T_int, T_int -> Apron.Texpr1.Int
                    | T_float, T_int
                    | T_int, T_float
                    | T_float, T_float -> Apron.Texpr1.Real
                    | _ -> Debug.fail "Unsupported case (%a, %a) in stmt @[%a@]"
                             Framework.Pp.pp_typ typ1
                             Framework.Pp.pp_typ typ2
                             Framework.Pp.pp_stmt stmt
                  in
                  let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, default_rounding) in
                  let diff_texpr = Apron.Texpr1.of_expr env diff in
                  let cons = Apron.Tcons1.make diff_texpr op in
                  (* let abs' = Apron.Abstract1.meet_tcons_array ApronManager.man abs (tcons_array_of_tcons_list env [cons]) in *)
                  cons
                ) |> tcons_array_of_tcons_list env
              |> Apron.Abstract1.meet_tcons_array ApronManager.man abs
            ) |> List.fold_left (fun acc abs ->
              Apron.Abstract1.join ApronManager.man acc abs
            ) (Apron.Abstract1.bottom ApronManager.man env) |>
          return_cur
        with Unsupported ->
          return_cur abs
      end

    | S_rename_var( v, v') ->
        Apron.Abstract1.rename_array ApronManager.man abs
          [| var_to_apron v |]
          [| var_to_apron v' |] |>
        return_cur

    | _ -> None

  (** {2 Queries} *)
  and ask : type r. ('a, t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man ctx query flow ->
      match query with
      | Query.QIntInterval exp ->
        begin
          let abs = get_domain_cur man flow in
          try
            let lv =  Framework.Visitor.expr_vars exp in
            let abs = add_missing_vars abs lv in
            let env = Apron.Abstract1.env abs in
            let texp = Apron.Texpr1.of_expr env (exp_to_apron exp) in
            let itv = Apron.Abstract1.bound_texpr ApronManager.man abs texp in
            Some (Values.Int.of_apron itv)
          with
          | Unsupported -> Some (Values.Int.top)
        end
      | _ ->
        None
  (** {2 Transformers to Apron syntax} *)

  and top_by_type = function
    | T_int | T_float -> assert false
    | T_bool ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_int 0)
          (Apron.Scalar.of_int 1)
      )
    | _ ->
      Apron.Texpr1.Cst(Apron.Coeff.Interval Apron.Interval.top)

  and binop_to_apron = function
    | O_plus -> Apron.Texpr1.Add
    | O_minus -> Apron.Texpr1.Sub
    | O_mult -> Apron.Texpr1.Mul
    | O_div -> Apron.Texpr1.Div
    | O_mod -> Apron.Texpr1.Mod
    | _ -> raise Unsupported

  and exp_to_apron exp =
    match ekind exp with
    | E_constant(C_int_interval (a,b)) ->
       Apron.Texpr1.Cst(
         Apron.Coeff.i_of_scalar
           (Apron.Scalar.of_float @@ Z.to_float a)
           (Apron.Scalar.of_float @@ Z.to_float b)
       )

    | E_constant(C_float_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float a)
          (Apron.Scalar.of_float b)
      )

    | E_constant(C_int n) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float @@ Z.to_float n))

    | E_constant(C_float f) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float f))

    | E_var ({vtyp = T_int | T_float} as x) -> Apron.Texpr1.Var(var_to_apron x)

    (* | Var x -> top_by_type x.vtyp *)

    | E_binop(binop, e1, e2) ->
       let binop' = binop_to_apron binop in
       let e1' = exp_to_apron e1 and e2' = exp_to_apron e2 in
       let typ' = typ_to_apron exp.etyp in
       Apron.Texpr1.Binop(binop', e1', e2', typ', default_rounding)

    | E_unop(O_minus, e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Neg, e', typ', default_rounding)

    | E_unop(O_sqrt, e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron T_float in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', default_rounding)

    | _ -> raise Unsupported

  and typ_to_apron = function
    | T_int -> Apron.Texpr1.Int
    | T_float -> Apron.Texpr1.Real
    | _ -> assert false

  and bexp_to_apron exp =
    match ekind exp with
    | E_constant(C_int n) when Z.to_int n = 0 -> Dnf.mk_false

    | E_constant(C_int _) -> Dnf.mk_true

    | E_binop(O_gt, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUP, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_ge, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUPEQ, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_lt, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUP, e1', e1.etyp, e0', e0.etyp)

    | E_binop(O_le, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUPEQ, e1', e1.etyp, e0', e0.etyp)

    | E_binop(O_eq, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.EQ, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_ne, e0, e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.mk_or
         (Dnf.singleton (Apron.Tcons1.SUP, e0', e0.etyp, e1', e1.etyp))
         (Dnf.singleton (Apron.Tcons1.SUP, e1', e1.etyp, e0', e0.etyp))

    | E_binop(O_log_or, e1, e2) ->
      Dnf.mk_or (bexp_to_apron e1) (bexp_to_apron e2)

    | E_binop(O_log_and,e1, e2) ->
      Dnf.mk_and (bexp_to_apron e1) (bexp_to_apron e2)

    | E_unop(O_log_not, exp') ->
      bexp_to_apron exp' |>
      Dnf.mk_neg (fun (op, e1, t1, e2, t2) ->
          match op with
          | Apron.Tcons1.EQ ->
            Dnf.mk_or
              (Dnf.singleton (Apron.Tcons1.SUP, e1, t1, e2, t2))
              (Dnf.singleton (Apron.Tcons1.SUP, e2, t2, e1, t1))
          | Apron.Tcons1.SUP ->
            Dnf.singleton (Apron.Tcons1.SUPEQ, e2, t2, e1, t1)
          | Apron.Tcons1.SUPEQ ->
            Dnf.singleton (Apron.Tcons1.SUP, e2, t2, e1, t1)
          | _ -> assert false
        )

    | _ ->
       let e0' = exp_to_apron exp in
       let e1' = Apron.Texpr1.Cst(Apron.Coeff.s_of_int 0) in
       Dnf.mk_or
         (Dnf.singleton (Apron.Tcons1.SUP, e0', exp.etyp, e1', T_int))
         (Dnf.singleton (Apron.Tcons1.SUP, e1', T_int, e0', exp.etyp))

  and tcons_array_of_tcons_list env l =
    let n = List.length l in
    let cond_array = Apron.Tcons1.array_make env n in
    let () = List.iteri (fun i c ->
      Apron.Tcons1.array_set cond_array i c;
    ) l in
    cond_array

  let eval _ _ _ _ = None

end



let setup () =
  let module Oct = Make(struct type t = Oct.t let name = "oct" let man = Oct.manager_alloc () end) in
  let module Poly = Make(struct type t = Polka.strict Polka.t let name = "poly" let man = Polka.manager_alloc_strict () end) in

  register_domain (name ^ ".octagon") (module Oct);
  register_domain (name ^ ".polyhedra") (module Poly);
