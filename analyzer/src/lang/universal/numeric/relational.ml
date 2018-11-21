(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Relational numeric abstract domain, based on APRON. *)

open Framework.Essentials
open Framework.Domains.Leaf
open Rounding
open Ast


(****************************************************************************)
(**                      {2 Command line options}                           *)
(****************************************************************************)


type _ Framework.Query.query +=
  | Q_sat  : Framework.Ast.expr -> bool Framework.Query.query
  | Q_fold : var list -> var list list Framework.Query.query

let opt_float_rounding = ref Apron.Texpr1.Near

let () =
  register_option (
    "-float-rounding-mode",
    Arg.String (function
        | "near" -> opt_float_rounding := Apron.Texpr1.Near
        | "zero" -> opt_float_rounding := Apron.Texpr1.Zero
        | "up"   -> opt_float_rounding := Apron.Texpr1.Up
        | "down" -> opt_float_rounding := Apron.Texpr1.Down
        | "rnd"  -> opt_float_rounding := Apron.Texpr1.Rnd
        | x -> Exceptions.panic "Unknown rounding mode %s" x
      ),
    "selects the rounding mode of floating-point computations. Possible values: near, zero, up, down, and rnd (default: near)."
  )


(****************************************************************************)
(**                         {2 Abstract domain}                             *)
(****************************************************************************)

(** Module to encapsulate a manager of Apron and the type of its abstraction *)
module type APRONMANAGER =
sig
  type t
  val man : t Apron.Manager.t
  val name : string
end

module OctMan = struct type t = Oct.t let name = "octagon" let man = Oct.manager_alloc () end
module PolyMan = struct type t = Polka.strict Polka.t let name = "polyhedra" let man = Polka.manager_alloc_strict () end

(** ToolBox module for Apron interfacing *)
module ApronTransformer(ApronManager : APRONMANAGER) =
struct

  (* let tcons_vars (tc: Apron.Tcons1.t) =
   *   let expr = tc |> Apron.Tcons1.get_texpr1 |> Apron.Texpr1.to_expr in
   *   let open Apron.Texpr1 in
   *   let module SVar = Set.Make(Apron.Var) in
   *   let rec aux expr cont = match expr with
   *     | Cst _ -> cont SVar.empty
   *     | Var v -> cont (SVar.singleton v)
   *     | Unop(_, e, _, _) -> aux e cont
   *     | Binop(_ , e, e', _, _) -> aux e (fun r -> aux e' (fun r' -> cont (SVar.union r r')))
   *   in
   *   aux expr (fun x -> x) |> SVar.elements *)
  let var_to_apron v =
    (match v.vtyp with
     | T_int -> Format.fprintf Format.str_formatter "%s:%d" v.vname v.vuid;
     | T_float _ -> Format.fprintf Format.str_formatter "%s@%d" v.vname v.vuid;
     | _ -> panic "relational: unsupported variable type %a" pp_typ v.vtyp);
    let name = Format.flush_str_formatter () in
    Apron.Var.of_string name

  let get_interval (v:var) (a: ApronManager.t Apron.Abstract1.t) : (Values.Intervals.Value.t) =
    Apron.Abstract1.bound_variable ApronManager.man a (var_to_apron v) |>
    Values.Intervals.Value.of_apron

  let is_numerical_var (v: var): bool =
    match v.vtyp with
    | T_int | T_float _ -> true
    | _ -> false
  let empty_env = Apron.Environment.make [| |] [| |]

  let print_env = Apron.Environment.print
      ~first:("[")
      ~sep:(",")
      ~last:("]")

  let filter_env int_filter real_filter env =
    let int_var, real_var = Apron.Environment.vars env in
    let list_int_var_to_keep =
      Array.fold_left (fun list_int_var_to_keep int_var ->
          if int_filter int_var then
            int_var :: list_int_var_to_keep
          else list_int_var_to_keep
        ) [] int_var
    in
    let list_real_var_to_keep =
      Array.fold_left (fun list_real_var_to_keep real_var ->
          if real_filter real_var then
            real_var :: list_real_var_to_keep
          else list_real_var_to_keep
        ) [] real_var
    in
    let array_int_res = Array.of_list list_int_var_to_keep in
    let array_real_res = Array.of_list list_real_var_to_keep in
    Apron.Environment.make array_int_res array_real_res

  let fold_env f env acc =
    let int_var, real_var = Apron.Environment.vars env in
    let acc' = Array.fold_left (fun acc x -> f x acc) acc int_var in
    Array.fold_left (fun acc x -> f x acc) acc' real_var

  let exists_env f env =
    let exception Exists in
    try
      let () = fold_env (fun e () -> if f e then raise Exists) env () in
      false
    with
    | Exists -> true

  let gce a b =
    filter_env
      (fun int_var -> Apron.Environment.mem_var b int_var)
      (fun int_var -> Apron.Environment.mem_var b int_var)
      a

  let diff a b =
    filter_env
      (fun int_var -> not (Apron.Environment.mem_var b int_var))
      (fun int_var -> not (Apron.Environment.mem_var b int_var))
      a

  let earray_of_array env a =
    let n = Array.length a in
    let rep = Apron.Lincons1.array_make env n in
    let _ = Array.fold_left (fun i cont ->
        let () = Apron.Lincons1.array_set rep i cont in
        (i+1)
      ) 0 a
    in
    rep

  let to_lincons_list a =
    let earray = Apron.Abstract1.to_lincons_array ApronManager.man a in
    let rec iter i =
      if i = Apron.Lincons1.array_length earray then []
      else (Apron.Lincons1.array_get earray i) :: (iter (i + 1))
    in
    iter 0

  let vars_in_lincons (lc: Apron.Lincons1.t) =
    let rep = ref [] in
    let () = Apron.Lincons1.iter (fun c v -> if not (Apron.Coeff.is_zero c) then rep := v::(!rep)) lc in
    let env = Apron.Lincons1.get_env lc in
    filter_env (fun x -> List.mem x !rep) (fun x -> List.mem x !rep) env


  exception UnsupportedExpression

  let apron_to_var v =
    let v = Apron.Var.to_string v in
    debug "apron_to_var %s" v;
    if Str.string_match (Str.regexp "\\([^:]+\\):\\([0-9]+\\)") v 0 then
      let vname = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      {vname; vuid; vtyp = T_int}
    else
    if Str.string_match (Str.regexp "\\([^@]+\\)@\\([0-9]+\\)") v 0 then
      let vname = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      (* TODO: replace F_REAL with real type *)
      {vname; vuid; vtyp = T_float F_REAL}
    else raise (Invalid_argument v)

  let rec binop_to_apron = function
    | O_plus  -> Apron.Texpr1.Add
    | O_minus -> Apron.Texpr1.Sub
    | O_mult  -> Apron.Texpr1.Mul
    | O_div   -> Apron.Texpr1.Div
    | O_mod   -> Apron.Texpr1.Mod
    | _ -> raise UnsupportedExpression

  and strongify_rhs exp abs l =
    match ekind exp with
    | E_constant(C_int_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float @@ Z.to_float a)
          (Apron.Scalar.of_float @@ Z.to_float b)
      ), abs, l

    | E_constant(C_float_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float a)
          (Apron.Scalar.of_float b)
      ), abs, l

    | E_constant(C_int n) ->
      Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float @@ Z.to_float n)),
      abs, l

    | E_constant(C_float f) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float f)), abs, l

    | E_var(x, STRONG) ->
      Apron.Texpr1.Var(var_to_apron x), abs, l
    | E_var(x, WEAK) ->
      let x' = mk_tmp ~vtyp:(x.vtyp) () in
      let x_apr = var_to_apron x in
      let x_apr' = var_to_apron x' in
      let abs = Apron.Abstract1.expand ApronManager.man abs x_apr [| x_apr' |] in
      (Apron.Texpr1.Var x_apr, abs, x_apr' :: l)

    | E_binop(binop, e1, e2) ->
      let binop' = binop_to_apron binop in
      let e1', abs, l = strongify_rhs e1 abs l in
      let e2', abs, l = strongify_rhs e2 abs l in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Binop(binop', e1', e2', typ', !opt_float_rounding), abs, l

    | E_unop (O_plus, e) ->
      strongify_rhs e abs l

    | E_unop(O_cast, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Cast, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_minus, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Neg, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_sqrt, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_wrap(g, d), e) ->
      let r = erange e in
      mk_binop (mk_z g r) O_plus (mk_binop
                                    (mk_binop e O_minus (mk_z g r) r)
                                    O_mod
                                    (mk_z (Z.(d-g+one)) r)
                                    r
                                 ) r
      |> fun x -> strongify_rhs x abs l

    | _ ->
      Debug.warn "[strongify rhs] : failed to transform %a of type %a" pp_expr exp pp_typ (etyp exp);
      raise UnsupportedExpression

  and is_env_var v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env (var_to_apron v)

  and is_env_var_apron v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env v

  and remove_tmp tmpl abs =
    let env = Apron.Abstract1.env abs in
    let vars =
      List.filter (fun v -> is_env_var_apron v abs) tmpl
    in
    let env = Apron.Environment.remove env (Array.of_list vars) in
    Apron.Abstract1.change_environment ApronManager.man abs env true

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

    | E_var (x, _) -> Apron.Texpr1.Var(var_to_apron x)

    | E_binop(binop, e1, e2) ->
      let binop' = binop_to_apron binop in
      let e1' = exp_to_apron e1 and e2' = exp_to_apron e2 in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Binop(binop', e1', e2', typ', !opt_float_rounding)

    | E_unop(O_minus , e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Neg, e', typ', !opt_float_rounding)

    | E_unop(O_sqrt, e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', !opt_float_rounding)

    | E_unop(O_wrap(g, d), e) ->
      let r = erange e in
      mk_binop (mk_z g r) O_plus (mk_binop
                                    (mk_binop e O_minus (mk_z g r) r)
                                    O_mod
                                    (mk_z (Z.(d-g+one)) r)
                                    r
                                 ) r
      |> exp_to_apron

    | _ ->
      Debug.warn "[exp_to_apron] : failed to transform %a of type %a" pp_expr exp pp_typ (etyp exp);
      raise UnsupportedExpression

  and typ_to_apron = function
    | T_int -> Apron.Texpr1.Int
    | T_float F_SINGLE -> Apron.Texpr1.Single
    | T_float F_DOUBLE -> Apron.Texpr1.Double
    | T_float F_LONG_DOUBLE -> Apron.Texpr1.Extended
    | T_float F_REAL -> Apron.Texpr1.Real
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

  let get_interval_expr (e:expr) (a: ApronManager.t Apron.Abstract1.t) : (Values.Intervals.Value.t) =
    Apron.Abstract1.bound_texpr ApronManager.man a
      (exp_to_apron e |> Apron.Texpr1.of_expr (Apron.Abstract1.env a)) |>
      Values.Intervals.Value.of_apron

end


module Make(ApronManager : APRONMANAGER) =
struct

  include ApronTransformer(ApronManager)

  type t = ApronManager.t Apron.Abstract1.t

  type _ domain += D_universal_relational : t domain

  let id = D_universal_relational
  let name = "universal.numeric.relational." ^ ApronManager.name

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_relational -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** {2 Environment utility functions} *)
  (** ********************************* *)

  let unify abs1 abs2 =
    let env1 = Apron.Abstract1.env abs1 and env2 = Apron.Abstract1.env abs2 in
    let env = Apron.Environment.lce env1 env2 in
    (Apron.Abstract1.change_environment ApronManager.man abs1 env false),
    (Apron.Abstract1.change_environment ApronManager.man abs2 env false)

  let add_missing_vars abs lv =
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
          List.filter (function {vtyp = T_float _} -> true | _ -> false) lv
        )
    in
    Apron.Abstract1.change_environment ApronManager.man abs env' false


  (** {2 Lattice operators} *)
  (** ********************* *)

  let top = Apron.Abstract1.top ApronManager.man empty_env

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env

  let is_bottom abs =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let subset abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.is_leq ApronManager.man abs1' abs2'

  let join_ abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.join ApronManager.man abs1' abs2'

  let join annot abs1 abs2 = join_ abs1 abs2

  let meet annot abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.meet ApronManager.man abs1' abs2'

  let widen annot abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.widening ApronManager.man abs1' abs2'

  let print fmt abs =
    Format.fprintf fmt "%s:@\n  @[%a@]@\n"
      ApronManager.name
      Apron.Abstract1.print abs


  (** {2 Transformers to Apron syntax} *)
  (** ******************************** *)



  (** {2 Transfer functions} *)
  (** ********************** *)

  let zone = Zone.Z_u_num

  let init prog = top

  let rec exec stmt a =
    let () = debug "input: %a" pp_stmt stmt in
    match skind stmt with
    | S_remove_var var ->
      let env = Apron.Abstract1.env a in
      let vars =
        List.filter (fun v -> is_env_var v a) [var] |>
        List.map var_to_apron
      in
      let env = Apron.Environment.remove env (Array.of_list vars) in
      Apron.Abstract1.change_environment ApronManager.man a env true |>
      return

    | S_rename_var(v, v') ->
      Apron.Abstract1.rename_array ApronManager.man a
        [| var_to_apron v  |]
        [| var_to_apron v' |] |>
      return

    | S_project_vars vars ->
      let env = Apron.Abstract1.env a in
      let vars = List.map var_to_apron vars in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Apron.Abstract1.change_environment ApronManager.man a new_env true |>
      return

    | S_assign({ekind = E_var(v, STRONG)}, e) ->
      let a = add_missing_vars a (v :: (Framework.Visitor.expr_vars e)) in
      let e, a, l = strongify_rhs e a [] in
      begin try
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv e in
          Apron.Abstract1.assign_texpr ApronManager.man a (var_to_apron v) texp None |>
          remove_tmp l |>
          return
        with UnsupportedExpression ->
          exec {stmt with skind = S_remove_var v} a
      end

    | S_fold(v, vl) ->
      begin
        debug "Starting fold";
        match vl with
        | [] -> Exceptions.panic "Can not fold list of size 0"
        | p::q ->
          let abs = Apron.Abstract1.fold ApronManager.man a
              (List.map var_to_apron vl |> Array.of_list) in
          let abs = Apron.Abstract1.rename_array ApronManager.man abs
              [|var_to_apron p|] [|var_to_apron v|] in
          abs |> return
      end

    | S_expand(v, vl) ->
      begin
        debug "Starting expand";
        let abs = Apron.Abstract1.expand ApronManager.man a
            (var_to_apron v) (List.map var_to_apron vl |> Array.of_list) in
        let env = Apron.Environment.remove (Apron.Abstract1.env abs) [|var_to_apron v|] in
        let abs = Apron.Abstract1.change_environment ApronManager.man abs env false in
        abs |> return
      end

    | S_assign({ekind = E_var(v, WEAK)} as lval, e) ->
      exec {stmt with skind = S_assign(lval, e)} a |> bind @@ fun a' ->
      join_ a a' |>
      return

    | S_assume(e) -> begin
        let a = add_missing_vars a (Framework.Visitor.expr_vars e) in
        let env = Apron.Abstract1.env a in

        let join_list l = List.fold_left (Apron.Abstract1.join ApronManager.man) (Apron.Abstract1.bottom ApronManager.man env) l in
        let meet_list l = tcons_array_of_tcons_list env l |>
                          Apron.Abstract1.meet_tcons_array ApronManager.man a
        in

        try
          bexp_to_apron e |>
          Dnf.apply
            (fun (op,e1,typ1,e2,typ2) ->
               let typ =
                 match typ1, typ2 with
                 | T_int, T_int -> Apron.Texpr1.Int
                 | T_float _, T_int
                 | T_int, T_float _
                 | T_float _, T_float _ -> Apron.Texpr1.Real
                 | _ -> Exceptions.panic_at (srange stmt)
                          "Unsupported case (%a, %a) in stmt @[%a@]"
                          pp_typ typ1 pp_typ typ2 pp_stmt stmt
               in
               let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
               let diff_texpr = Apron.Texpr1.of_expr env diff in
               Apron.Tcons1.make diff_texpr op
            )
            meet_list join_list |>
          return
        with UnsupportedExpression ->
          return a
      end

    | _ -> return top

  and satisfy (abs: t) (e: expr): bool =
    let a = add_missing_vars abs (Framework.Visitor.expr_vars e) in
    let env = Apron.Abstract1.env a in
    bexp_to_apron e
    |> Dnf.substitute
      (fun (op, e1, t1, e2, t2) ->
         let typ =
           match t1, t2 with
           | T_int, T_int -> Apron.Texpr1.Int
           | T_float _, T_int
           | T_int, T_float _
           | T_float _, T_float _ -> Apron.Texpr1.Real
           | _ -> Exceptions.panic "Unsupported case (%a, %a)" pp_typ t1 pp_typ t2 pp_stmt
         in
         let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
         let diff_texpr = Apron.Texpr1.of_expr env diff in
         let tcons = Apron.Tcons1.make diff_texpr op in
         Apron.Abstract1.sat_tcons ApronManager.man a tcons
      ) (||) (&&)

  and ask : type r. r Framework.Query.query -> t -> r option =
    fun query abs ->
      match query with
      | Q_sat e ->
        Some (satisfy abs e)
      | Values.Intervals.Value.Q_interval e ->
        let e = exp_to_apron e in
        let env = Apron.Abstract1.env abs in
        let e = Apron.Texpr1.of_expr env e in
        Apron.Abstract1.bound_texpr ApronManager.man abs e |>
        Values.Intervals.Value.of_apron |>
        OptionExt.return
      | _ -> None

  let var_relations v a =
    (* Get the linear constraints *)

    let lincons_list = to_lincons_list a in

    let rel1 = List.fold_left (fun acc lincons ->
        let t_involved = ref false in
        Apron.Lincons1.iter (fun c v' ->
            t_involved := !t_involved || ((compare_var v (apron_to_var v') = 0) && not (Apron.Coeff.is_zero c))
          ) lincons;
        (* If lincons is involved in the constraint, we keep all other variables with non null coefficients *)
        if !t_involved then
          let vars = ref [] in
          Apron.Lincons1.iter (fun c v' ->
              let v' = apron_to_var v' in
              if compare_var v v' <> 0 && not (Apron.Coeff.is_zero c) then
                vars := v' :: !vars
            ) lincons;
          !vars @ acc
        else
          acc
      ) [] lincons_list in

    (* Add also constant variables *)
    let rel2 = List.fold_left (fun acc lincons ->
        let nb_non_zero_coeff = ref 0 in
        Apron.Lincons1.iter (fun c v' ->
            if compare_var v (apron_to_var v') = 0 || Apron.Coeff.is_zero c then
              ()
            else
              nb_non_zero_coeff := !nb_non_zero_coeff + 1
          ) lincons;
        if !nb_non_zero_coeff = 1 then
          let vars = ref [] in
          Apron.Lincons1.iter (fun c v' ->
              let v' = apron_to_var v' in
              if compare_var v v' <> 0 && not (Apron.Coeff.is_zero c) then
                vars := v' :: !vars
            ) lincons;
          !vars @ acc
        else
          acc
      ) [] lincons_list
    in

    List.sort_uniq compare_var (rel1 @ rel2)


  let set_interval v i a =
    let env = Apron.Abstract1.env a in
    let a' = Apron.Abstract1.of_box ApronManager.man env [|var_to_apron v|] [|Values.Intervals.Value.to_apron i|] in
    let a, a' = unify a a' in
    Apron.Abstract1.meet ApronManager.man a a'


end

module Oct = Make(OctMan)
module Poly = Make(PolyMan)

let () =
  register_domain (module Oct);
  register_domain (module Poly);
