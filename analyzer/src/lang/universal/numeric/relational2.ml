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

module Make(ApronManager : APRONMANAGER) =
struct

  include ApronTransformer(ApronManager)

  module VarSet = Set.Make(Var)

  (** [type t = 〈p, inf, sup〉] where [p] is a numerical domain with
     environment greater that [sup], [inf] and [sup] are sets of
     variables such that [inf] \subseteq [sup]. γ(〈p, inf, sup〉) =
     {f \in \wp(A \rightarrow R) | inf \subseteq A \subseteq sup
     \wedge f \in γ_0(proj(P, A))} where γ_0 is the usual numerical
     concretization function and proj(P, A) is the projection of
     polyhedra P onto the set variable A.*)

  type u =
    {
      num : ApronManager.t Apron.Abstract1.t;
      inf : VarSet.t;
      sup : VarSet.t
    }
  let print_u fmt (u: u) = Format.fprintf fmt "@[%a in {%a} ⊆ {%a}@]"
      Apron.Abstract1.print u.num
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Var.print)
      (VarSet.elements u.inf)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Var.print)
      (VarSet.elements u.sup)

  type t = u with_top
  let print = Top.top_fprint print_u

  let bottom =
    Nt
      {
        num = Apron.Abstract1.bottom ApronManager.man empty_env;
        inf = VarSet.empty;
        sup = VarSet.empty
      }

  let top = TOP

  let is_bottom =
    Top.top_apply (fun u ->
        not (VarSet.subset u.inf u.sup) ||
        Apron.Abstract1.is_bottom ApronManager.man u.num
      ) false

  let is_top = Top.top_apply (fun _ -> false) true

  let apron_env_of_varset (vs: VarSet.t) =
    let list_int_var = VarSet.filter (function {vtyp = T_int} -> true | _ -> false) vs
                       |> VarSet.elements |> List.map var_to_apron in
    let list_real_var = VarSet.filter (function {vtyp = T_int} -> false | _ -> true) vs
                        |> VarSet.elements |> List.map var_to_apron in
    Apron.Environment.make (Array.of_list list_int_var) (Array.of_list list_real_var)

  let join_and_widen merger (u: t) (v: t) =
    top_apply2 TOP TOP
      (fun u v ->
         (* U on {c \/ u} and V on {c \/ v}*)
         let u_env = Apron.Abstract1.env u.num
         and v_env = Apron.Abstract1.env v.num in
         let diff_u_env = diff u_env v_env                    (* u              *)
         and diff_v_env = diff v_env u_env in                 (* v              *)
         let common_env = gce u_env v_env in                  (* c              *)
         let full_env = Apron.Environment.lce u_env v_env in  (* c \/ u \/ v = f*)

         let u_to_common = Apron.Abstract1.change_environment
             ApronManager.man u.num common_env false in
         (* proj(U, c)  *)
         let v_to_common = Apron.Abstract1.change_environment
             ApronManager.man v.num common_env false in
         (* proj(V, c)  *)
         let join_common_part = merger
             ApronManager.man u_to_common v_to_common in
         (* proj(V, c) |_| proj(U, c) *)
         let join_common_part_extended_full = Apron.Abstract1.change_environment
             ApronManager.man join_common_part full_env false in
         (* ext(proj(V, c) |_| proj(U, c), f) = C *)

         let u_to_diff_u = Apron.Abstract1.change_environment ApronManager.man u.num diff_u_env false in
         (* proj(U, u)  *)
         let diff_u_to_full = Apron.Abstract1.change_environment ApronManager.man u_to_diff_u full_env false in
         (* ext(proj(U, u), f) = B *)
         let v_to_diff_v = Apron.Abstract1.change_environment ApronManager.man v.num diff_v_env false in
         (* proj(V, v)  *)
         let diff_v_to_full = Apron.Abstract1.change_environment ApronManager.man v_to_diff_v full_env false in
         (* ext(proj(V, v), f) = A*)

         let meet_on_full_env = Apron.Abstract1.meet_array ApronManager.man
             [| join_common_part_extended_full ; diff_v_to_full ; diff_u_to_full|]
         in
         (* A meet B meet C *)

         let u_constraints = to_lincons_list u.num in
         (* constraints from U *)

         let v_constraints = to_lincons_list v.num in
         (* constraints from V *)

         let useful_u_constraints = List.filter (fun lincons ->
             let lincons_env = vars_in_lincons lincons in
             exists_env (fun v -> Apron.Environment.mem_var common_env v) lincons_env
             && exists_env (fun v -> Apron.Environment.mem_var diff_u_env v) lincons_env
           ) u_constraints
         in
         (* keep only constraints from U that mention a variable from c and from u *)
         let useful_v_constraints = List.filter (fun lincons ->
             let lincons_env = vars_in_lincons lincons in
             exists_env (fun v -> Apron.Environment.mem_var common_env v) lincons_env
             && exists_env (fun v -> Apron.Environment.mem_var diff_v_env v) lincons_env
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
         let u_inf_bound = Apron.Abstract1.change_environment
             ApronManager.man u.num env_test_u false in
         (* Projection of U onto that environment *)
         let v_inf_bound = Apron.Abstract1.change_environment
             ApronManager.man v.num env_test_v false in
         (* Projection of V onto that environment *)

         let num = List.fold_left (fun meet_with_constraints lincons ->
             let with_new_constraint = Apron.Abstract1.meet_lincons_array ApronManager.man
                 meet_with_constraints (earray_of_array full_env [| lincons |])
             in
             let with_new_constraint_to_u_sup = Apron.Abstract1.change_environment
                 ApronManager.man with_new_constraint env_test_u false in
             if Apron.Abstract1.is_leq ApronManager.man u_inf_bound with_new_constraint_to_u_sup then
               let with_new_constraint_to_v_sup = Apron.Abstract1.change_environment
                   ApronManager.man with_new_constraint env_test_v false in
               if Apron.Abstract1.is_leq ApronManager.man v_inf_bound with_new_constraint_to_v_sup then
                 with_new_constraint
               else
                 meet_with_constraints
             else
               meet_with_constraints
           ) meet_on_full_env useful_constraints
         in
         (* We test all useful constraints from U and V to check
            whether adding them would result in a numerical element
            that does not contain the inputs once projected onto their
            respective "safe" environment, if adding the constraint
            does not break the soundness condition, constraint is
            added and we move on to the next constraint. Note that the
            order in which the constraints are added modifies the
            precision of the output *)
         Nt ({num = num ; inf = VarSet.inter u.inf v.inf ; sup = VarSet.union u.inf v.inf})
      ) u v

  let join = join_and_widen Apron.Abstract1.join
  let widen = join_and_widen Apron.Abstract1.widening

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
          let u_to_env_test = Apron.Abstract1.change_environment ApronManager.man
              u.num env_test false in
          let v_to_env_test = Apron.Abstract1.change_environment ApronManager.man
              v.num env_test false in
          Apron.Abstract1.is_leq ApronManager.man u_to_env_test v_to_env_test
        else
          Apron.Abstract1.is_bottom ApronManager.man u.num
      end

  let meet u v = top_apply2 v u
      (fun u v ->
         (* meet is performed on the common environment of u and v *)
         let u_env = Apron.Abstract1.env u.num
         and v_env = Apron.Abstract1.env v.num in
         let common_env = gce u_env v_env in
         let u_to_common_env = Apron.Abstract1.change_environment
             ApronManager.man u.num common_env false in
         let v_to_common_env = Apron.Abstract1.change_environment
             ApronManager.man v.num common_env false in
         let num = Apron.Abstract1.meet ApronManager.man
             u_to_common_env v_to_common_env
         in
         Nt { num = num ; inf = VarSet.union u.inf v.inf ; sup = VarSet.union u.sup v.sup }
      ) u v
end
