(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Relational numeric abstract domain, based on APRON.
*)

open Framework.Essentials
open Framework.Domains.Leaf
open Rounding
open Ast
open Relational
open Top
open Apron
module AVar = Apron.Var
module Var = Framework.Essentials.Var

module Make(ApronManager : APRONMANAGER) : Framework.Domains.Leaf.S =
struct

  include ApronTransformer(ApronManager)

  module VarSet = Set.Make(Framework.Essentials.Var)

  (** [type t = 〈p, inf, sup〉] where [p] is a numerical domain with
     environment greater that [sup], [inf] and [sup] are sets of
     variables such that [inf] \subseteq [sup]. γ(〈p, inf, sup〉) =
     {f \in \wp(A \rightarrow R) | inf \subseteq A \subseteq sup
     \wedge f \in γ_0(proj(P, A))} where γ_0 is the usual numerical
     concretization function and proj(P, A) is the projection of
     polyhedra P onto the set variable A.*)

  type u =
    {
      num : ApronManager.t Abstract1.t;
      inf : VarSet.t;
      sup : VarSet.t
    }

  let print_u fmt (u: u) = Format.fprintf fmt "@[<v>%s: %a in {%a} ⊆ {%a}@,@]"
      ApronManager.name
      Abstract1.print u.num
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Var.print)
      (VarSet.elements u.inf)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Var.print)
      (VarSet.elements u.sup)

  type t = u with_top
  let print = Top.top_fprint print_u

  let zone = Zone.Z_u_num

  type _ domain += D_universal_relational2 : t domain

  let id = D_universal_relational2
  let name = "universal.numeric.relational2." ^ ApronManager.name

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_relational2 -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  let bottom =
    Nt
      {
        num = Abstract1.bottom ApronManager.man empty_env;
        inf = VarSet.empty;
        sup = VarSet.empty
      }

  let top = TOP

  let is_bottom =
    Top.top_apply (fun u ->
        not (VarSet.subset u.inf u.sup) ||
        Abstract1.is_bottom ApronManager.man u.num
      ) false

  let is_top = Top.top_apply (fun _ -> false) true

  let apron_env_of_varset (vs: VarSet.t) =
    let list_int_var = VarSet.filter (function {vtyp = T_int} -> true | _ -> false) vs
                       |> VarSet.elements |> List.map var_to_apron in
    let list_real_var = VarSet.filter (function {vtyp = T_int} -> false | _ -> true) vs
                        |> VarSet.elements |> List.map var_to_apron in
    Environment.make (Array.of_list list_int_var) (Array.of_list list_real_var)

  let join_and_widen merger (u: t) (v: t) =
    top_apply2 TOP TOP
      (fun u v ->
         let () = debug "asked to join: %a @,and %a" print_u u print_u v in
         if is_bottom (Nt u) then Nt v
         else if is_bottom (Nt v) then Nt u
         else
           begin
             (* U on {c \/ u} and V on {c \/ v}*)
             let u_env = Abstract1.env u.num
             and v_env = Abstract1.env v.num in
             let diff_u_env = diff u_env v_env                    (* u              *)
             and diff_v_env = diff v_env u_env in                 (* v              *)
             let common_env = gce u_env v_env in                  (* c              *)
             let full_env = Environment.lce u_env v_env in  (* c \/ u \/ v = f*)

             let u_to_common = Abstract1.change_environment
                 ApronManager.man u.num common_env false in
             (* proj(U, c)  *)
             let v_to_common = Abstract1.change_environment
                 ApronManager.man v.num common_env false in
             (* proj(V, c)  *)
             let join_common_part = merger
                 ApronManager.man u_to_common v_to_common in
             (* proj(V, c) |_| proj(U, c) *)
             let join_common_part_extended_full = Abstract1.change_environment
                 ApronManager.man join_common_part full_env false in
             (* ext(proj(V, c) |_| proj(U, c), f) = C *)

             let u_to_diff_u = Abstract1.change_environment ApronManager.man u.num diff_u_env false in
             (* proj(U, u)  *)
             let diff_u_to_full = Abstract1.change_environment ApronManager.man u_to_diff_u full_env false in
             (* ext(proj(U, u), f) = B *)
             let v_to_diff_v = Abstract1.change_environment ApronManager.man v.num diff_v_env false in
             (* proj(V, v)  *)
             let diff_v_to_full = Abstract1.change_environment ApronManager.man v_to_diff_v full_env false in
             (* ext(proj(V, v), f) = A*)

             let meet_on_full_env = Abstract1.meet_array ApronManager.man
                 [| join_common_part_extended_full ; diff_v_to_full ; diff_u_to_full|]
             in
             (* A meet B meet C *)

             let u_constraints = to_lincons_list u.num in
             (* constraints from U *)

             let v_constraints = to_lincons_list v.num in
             (* constraints from V *)


             let useful_u_constraints = List.filter (fun lincons ->
                 let lincons_env = vars_in_lincons lincons in
                 exists_env (fun v -> Environment.mem_var common_env v) lincons_env
                 && exists_env (fun v -> Environment.mem_var diff_u_env v) lincons_env
               ) u_constraints
             in
             (* keep only constraints from U that mention a variable from c and from u *)
             let useful_v_constraints = List.filter (fun lincons ->
                 let lincons_env = vars_in_lincons lincons in
                 exists_env (fun v -> Environment.mem_var common_env v) lincons_env
                 && exists_env (fun v -> Environment.mem_var diff_v_env v) lincons_env
               ) v_constraints
             in
             (* keep only constraints from V that mention a variable from c and from v *)

             let useful_constraints = useful_v_constraints @ useful_u_constraints in
             (* List of all usefull constraints from U and V *)

             let env_test_u = apron_env_of_varset u.sup in
             (* Environment on which the projection of the result of the
                join can not be smaller than the projection of U *)
             let env_test_v = apron_env_of_varset v.sup in
             (* Environment on which the projection of the result of the
                join can not be smaller than the projection of C *)
             let u_inf_bound = Abstract1.change_environment
                 ApronManager.man u.num env_test_u false in
             (* Projection of U onto that environment *)
             let v_inf_bound = Abstract1.change_environment
                 ApronManager.man v.num env_test_v false in
             (* Projection of V onto that environment *)

             let num = List.fold_left (fun meet_with_constraints lincons ->
                 let lincons_to_full_env = Lincons1.extend_environment lincons full_env in
                 let with_new_constraint = Abstract1.meet_lincons_array ApronManager.man
                     meet_with_constraints (earray_of_array full_env [| lincons_to_full_env |])
                 in
                 let with_new_constraint_to_u_sup = Abstract1.change_environment
                     ApronManager.man with_new_constraint env_test_u false in
                 if Abstract1.is_leq ApronManager.man u_inf_bound with_new_constraint_to_u_sup then
                   let with_new_constraint_to_v_sup = Abstract1.change_environment
                       ApronManager.man with_new_constraint env_test_v false in
                   if Abstract1.is_leq ApronManager.man v_inf_bound with_new_constraint_to_v_sup then
                     with_new_constraint
                   else
                     meet_with_constraints
                 else
                   meet_with_constraints
               ) meet_on_full_env (List.rev useful_constraints)
             in
             (* We test all useful constraints from U and V to check
                whether adding them would result in a numerical element
                that does not contain the inputs once projected onto their
                respective "safe" environment, if adding the constraint
                does not break the soundness condition, constraint is
                added and we move on to the next constraint. Note that the
                order in which the constraints are added modifies the
                precision of the output *)
             let res = Nt ({num = num ; inf = VarSet.inter u.inf v.inf ; sup = VarSet.union u.sup v.sup}) in
             let () = debug "res: %a" print res in
             res
           end
      ) u v

  let join_  = join_and_widen Abstract1.join
  let join _ = join_and_widen Abstract1.join
  let widen _ a b =
    let () = debug "entering numerical widening" in
    let () = debug "a: %a" print a in
    let () = debug "b: %a" print b in
    let res = join_and_widen Abstract1.widening a b in
    let () = debug "res:%a" print res in
    res

  let subset (u: t) (v: t) = match u, v with
    | _   , TOP  -> true
    | TOP , Nt _ -> false
    | Nt u, Nt v ->
      begin
        if VarSet.subset v.inf u.inf
        && VarSet.subset u.inf u.sup
        && VarSet.subset u.sup v.sup
        then
          (* we project u and v onto u.sup and check the inclusion *)
          let env_test = apron_env_of_varset u.sup in
          let u_to_env_test = Abstract1.change_environment ApronManager.man
              u.num env_test false in
          let v_to_env_test = Abstract1.change_environment ApronManager.man
              v.num env_test false in
          Abstract1.is_leq ApronManager.man u_to_env_test v_to_env_test
        else
          Abstract1.is_bottom ApronManager.man u.num
      end

  let meet _ u v = top_apply2 v u
      (fun u v ->
         (* meet is performed on the common environment of u and v *)
         let u_env = Abstract1.env u.num
         and v_env = Abstract1.env v.num in
         let common_env = gce u_env v_env in
         let u_to_common_env = Abstract1.change_environment
             ApronManager.man u.num common_env false in
         let v_to_common_env = Abstract1.change_environment
             ApronManager.man v.num common_env false in
         let num = Abstract1.meet ApronManager.man
             u_to_common_env v_to_common_env
         in
         Nt { num = num ; inf = VarSet.union u.inf v.inf ; sup = VarSet.union u.sup v.sup }
      ) u v

  let assert_env env1 env2 =
    let comp = Environment.compare env1 env2 in
    if comp <> -1 && comp <> 0 then
      Debug.fail "Environment %a is not contained in environment %a"
        print_env env1
        print_env env2

  let rec exec stmt a =
    let () = debug "I was asked %a" pp_stmt stmt in
    match skind stmt with
    | S_add_var ({vtyp = T_int | T_float _} as var)->
      top_apply (fun u ->
          let var_apron = var_to_apron var in
          let add_i, add_r = match var.vtyp with | T_int -> [| var_apron |], [| |] | _ -> [| |], [| var_apron |] in
          let env = Environment.add (Abstract1.env u.num) add_i add_r in
          Nt {num = Abstract1.change_environment ApronManager.man u.num env false;
              inf = VarSet.add var u.inf;
              sup = VarSet.add var u.sup
             }
        ) TOP a
      |> return

    | S_remove_var var ->
      top_apply (fun u ->
          let u_apron = var_to_apron var in
          let env = filter_env
              (fun v -> not (AVar.compare u_apron v = 0))
              (fun v -> not (AVar.compare u_apron v = 0))
              (Abstract1.env u.num)
          in
          Nt {num = Abstract1.change_environment ApronManager.man u.num env false;
              inf = VarSet.remove var u.inf;
              sup = VarSet.remove var u.sup
             }
        ) TOP a
      |> return

    (* weak semantic here: rename(v, v') does not cut traces where v
       is not present *)
    | S_rename_var(var, var') ->
       top_apply (fun u ->
           let must_be = VarSet.mem var u.inf in
           let var_apron  = var_to_apron var  in
           let var_apron' = var_to_apron var' in
           Nt {num = Abstract1.rename_array ApronManager.man u.num
                       [| var_apron |] [| var_apron' |];
               inf = if must_be then u.inf |> VarSet.remove var |> VarSet.add var' else u.inf ;
               sup = u.sup |> VarSet.remove var |> VarSet.add var'
             }
         ) TOP a
       |> return

    | S_assume(e) ->
      top_apply (fun u ->
          let vars = Framework.Visitor.expr_vars e |> VarSet.of_list in
          let env = Abstract1.env u.num in
          let env_expr = apron_env_of_varset vars in
          assert_env env_expr env;
          let join_list l = List.fold_left (Apron.Abstract1.join ApronManager.man) (Apron.Abstract1.bottom ApronManager.man env) l in
          let meet_list l = tcons_array_of_tcons_list env l |>
                            Apron.Abstract1.meet_tcons_array ApronManager.man u.num
          in
          try
            let num_res =
              bexp_to_apron e |>
              Dnf.apply
                (fun (op, e1, typ1, e2, typ2) ->
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
                meet_list join_list
            in
            Nt {u with num = num_res; inf = VarSet.union u.inf vars}
          with UnsupportedExpression ->
            Nt {u with inf = VarSet.union u.inf vars}
        ) TOP a
      |> return

    | S_assign({ekind = E_var(v, STRONG)}, e) ->
      top_apply (fun u ->
          let vars = v:: (Framework.Visitor.expr_vars e) |> VarSet.of_list in
          let env = Abstract1.env u.num in
          let env_expr = apron_env_of_varset vars in
          let v_apron = var_to_apron v in
          assert_env env_expr env;
          (* let () = debug "toto" in *)
          let e, u_num, l = strongify_rhs e u.num [] in
          let u_num =
            try
              let texp = Apron.Texpr1.of_expr env e in
              Apron.Abstract1.assign_texpr ApronManager.man u_num v_apron texp None |>
              remove_tmp l
            with UnsupportedExpression ->
              Abstract1.forget_array ApronManager.man u.num [| v_apron |] false
          in
          Nt {u with num = u_num ; inf = VarSet.union u.inf vars}
        ) TOP a
      |> return

    | S_fold(v, vl) ->
      begin
        top_apply (fun u ->
            let all_must_present = List.for_all (fun v -> VarSet.mem v u.inf) vl in
            let () = debug "I am performing a fold of: %a" print (Nt u) in
            let () = debug "%a <- {%a}" pp_var v (Format.pp_print_list pp_var) vl in
            match vl with
            | [] -> Debug.fail "Can not fold list of size 0"
            | p::q ->
              let vs = VarSet.of_list vl in
              let abs = Apron.Abstract1.fold ApronManager.man u.num
                  (List.map var_to_apron vl |> Array.of_list) in
              let abs = Apron.Abstract1.rename_array ApronManager.man abs
                  [|var_to_apron p|] [|var_to_apron v|] in
              let rep =
                Nt { num = abs ;
                     inf = if all_must_present then
                             VarSet.add v (VarSet.diff u.inf vs)
                           else (VarSet.diff u.inf vs);
                     sup = VarSet.add v (VarSet.diff u.sup vs)
                  }
              in
              let () = debug "Answer is: %a" print (rep) in
              rep
          ) TOP a
        |> return
      end

    | S_expand(v, vl) ->
       begin
         match vl with
         | [ v' ] when Var.compare v v' = 0 -> a |> return
         | [ v' ] ->  exec (mk_rename v v' (srange stmt)) a
         | _ ->
            top_apply (fun u ->
                let vs = VarSet.of_list vl in
                let abs = Apron.Abstract1.expand ApronManager.man u.num
                            (var_to_apron v) (List.map var_to_apron vl |> Array.of_list) in
                let env = Apron.Environment.remove (Apron.Abstract1.env abs) [|var_to_apron v|] in
                let abs = Apron.Abstract1.change_environment ApronManager.man abs env false in
                Nt { num = abs ;
                     inf = if VarSet.mem v u.inf then
                             VarSet.union vs (VarSet.remove v u.inf)
                           else (VarSet.remove v u.inf);
                     sup = VarSet.union vs (VarSet.remove v u.sup)
                  }
              ) TOP a
            |> return
       end

    | S_assign({ekind = E_var(v, WEAK)} as lval, e) ->
      exec {stmt with skind = S_assign(lval, e)} a |> bind @@ fun a' ->
      join_ a a' |>
      return

    | _ -> return top

  and my_fold man abs (x, y) =
    let env = Abstract1.env abs in
    let env1 = filter_env (fun a -> Apron.Var.compare a x != 0) (fun a -> Apron.Var.compare a x != 0) env in
    let env2 = filter_env (fun a -> Apron.Var.compare a y != 0) (fun a -> Apron.Var.compare a y != 0) env in
    let a0 = Abstract1.change_environment man abs env1 false in
    let a1 = Abstract1.rename_array man a0 [|y|] [|x|] in
    let a2 = Abstract1.change_environment man abs env2 false in
    let res = Abstract1.join man a1 a2 in
    res

  (* This is experimental *)
  and find_foldable_variables lv abs =
    top_apply (fun u ->
        let rec cross l acc = match l with
          | p::q ->
             let reste, acc' = find_foldable p q [] [] in
             cross reste ((p :: acc'):: acc)
          | []   -> acc
        and find_foldable x l reste acc = match l with
          | y::q when compare_typ x.vtyp y.vtyp = 0 ->
             let abs = u.num in
             let tmp = mk_tmp ~vtyp:x.vtyp () in
             let () = debug "We try to fold: %a and %a using tmp %a" Var.print x Var.print y Var.print tmp in
             let ax = var_to_apron x and ay = var_to_apron y and atmp = var_to_apron tmp in
             let () = debug "Folding on %a and %a@,%a" Apron.Var.print ax Apron.Var.print ay (Apron.Abstract1.print) abs in
             (* let abs1 = Abstract1.fold ApronManager.man abs [| ax ; ay |] in *)
             let abs1 = my_fold ApronManager.man abs (ax, ay) in
             let () = debug "result is: %a" Apron.Abstract1.print abs1 in
             let abs2 = Abstract1.rename_array ApronManager.man abs1 [| ax |] [| atmp |] in
             (* let () = debug "Folded: %a" Apron.Abstract1.print abs2 in *)
             let abs3 = Abstract1.expand ApronManager.man abs2 (atmp) [| ax ; ay |] in
             let env = Apron.Environment.remove (Apron.Abstract1.env abs3) [| atmp |] in
             let abs4 = Apron.Abstract1.change_environment ApronManager.man abs3 env false in
             (* let () = debug "FoldExpand input : %a" Apron.Abstract1.print abs  in
              * let () = debug "FoldExpand output: %a" Apron.Abstract1.print abs4 in *)
             if Abstract1.is_leq ApronManager.man abs4 abs then
               let () = debug "added to foldable" in
               find_foldable x q reste (y::acc)
             else
               let () = debug "not added to foldable" in
               find_foldable x q (y :: reste) acc
          | y::q -> find_foldable x q (y :: reste) acc
          | _ -> reste, acc
        in
        List.filter (fun cl -> match cl with | p::q::r -> true | _ -> false) (cross lv [])
      ) [] abs

  and ask : type r. r Framework.Query.query -> t -> r option =
    fun query abs ->
    match query with
    | Q_fold vl -> Some (find_foldable_variables vl abs)
    | Values.Intervals.Value.Q_interval e ->
       top_apply
         (fun x -> Some (get_interval_expr e x.num))
         (Some Values.Intervals.Value.top) abs
    | _ -> None


  (* TODO: This should not be done in this domain: *)
  let init p =
    match p with
    | Ast.P_universal pu ->
      let vs = VarSet.of_list (pu.universal_gvars |> List.filter is_numerical_var) in
      let env = apron_env_of_varset vs in
      Nt {num = Abstract1.top ApronManager.man env ; inf = vs ; sup = vs }
    | _ -> Nt { num = Abstract1.top ApronManager.man empty_env ; inf = VarSet.empty ; sup = VarSet.empty }
end

module Oct = Make(OctMan)
module Poly = Make(PolyMan)

let () =
  register_domain (module Oct);
  register_domain (module Poly);
