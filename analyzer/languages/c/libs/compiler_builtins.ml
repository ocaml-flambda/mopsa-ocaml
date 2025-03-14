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


(** Evaluation of compiler's builtin functions *)


open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.compiler"
    end)

  let checks = []
  
  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow = None

  let exec stmt man flow = None


  (** {2 Evaluation entry point} *)
  (** ========================== *)

  let eval exp man flow =
    match ekind exp with

    (* 𝔼⟦ __builtin_constant_p(e) ⟧ *)
    | E_c_builtin_call("__builtin_constant_p", [e]) ->

      (* __builtin_constant_ determines if [e] is known 
         to be constant at compile time *)
      let ret =
        match remove_casts e |> ekind with
        | E_constant _ -> mk_one exp.erange
        | _ -> mk_z_interval Z.zero Z.one exp.erange
      in
      Eval.singleton ret flow |>
      OptionExt.return

    (* 𝔼⟦ __builtin_expect(e,v) ⟧ *)
    | E_c_builtin_call("__builtin_expect", [e;v]) ->
      man.eval e flow |>
      OptionExt.return

    (* 𝔼⟦ __builtin_isfinite(e) ⟧ *)
    | E_c_builtin_call("__builtin_isfinite", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_valid e exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange)) (* should be !=0 *)
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isnan(e) ⟧ *)
    | E_c_builtin_call("__builtin_isnan", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_nan e exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange)) (* should be !=0 *)
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isnormal(e) ⟧ *)
    | E_c_builtin_call("__builtin_isnormal", [e]) ->
       (* !valid => !normal but valid =/=> normal *)
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_valid e exp.erange)
         ~fthen:(Eval.singleton (mk_top s32 exp.erange))
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isinf_sign(e) ⟧ *)
    | E_c_builtin_call("__builtin_isinf_sign", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_inf e exp.erange)
         ~fthen:(
           switch [
               [mk_gt e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_one exp.erange);
               [mk_lt e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_minus_one exp.erange);
               [mk_eq e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_zero exp.erange)
             ] man
         )
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_val() ⟧,
       𝔼⟦ __builtin_inff() ⟧ *)
    | E_c_builtin_call("__builtin_huge_val", []) ->
       Eval.singleton (mk_float infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_valf() ⟧ *)
    | E_c_builtin_call("__builtin_huge_valf", [])
    | E_c_builtin_call("__builtin_inff", []) ->
       Eval.singleton (mk_float ~prec:F_SINGLE infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_vall() ⟧ *)
    | E_c_builtin_call("__builtin_huge_vall", []) ->
       Eval.singleton (mk_float ~prec:F_LONG_DOUBLE infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_nanf("")  ⟧ *)
    | E_c_builtin_call("__builtin_nanf", [_]) ->
       Eval.singleton (mk_float ~prec:F_SINGLE nan exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_signbit(e) ⟧ *)
    | E_c_builtin_call("__builtin_signbit", [e]) ->
       man.eval e flow >>$? fun e flow ->
       switch [
           [mk_ge e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton (mk_zero exp.erange);
           [mk_le e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton (mk_top s32 exp.erange); (* should be !=0 *)
           (* 0. should indeed return top because it can represent +0. or -0. *)
         ] man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_fpclassify(...) ⟧ *)
    | E_c_builtin_call("__builtin_fpclassify", [fp_nan;fp_infinite;fp_normal;fp_subnormal;fp_zero;e]) ->
       man.eval fp_nan flow >>$? fun fp_nan flow ->
       man.eval fp_infinite flow >>$? fun fp_infinite flow ->
       man.eval fp_normal flow >>$? fun fp_normal flow ->
       man.eval fp_subnormal flow >>$? fun fp_subnormal flow ->
       man.eval fp_zero flow >>$? fun fp_zero flow ->
       man.eval e flow >>$? fun e flow ->
       switch [
           (* FP_NAN *)
           [mk_float_class float_nan e exp.erange],
           Eval.singleton fp_nan;
           (* FP_INFINITE *)
           [mk_float_class float_inf e exp.erange],
           Eval.singleton fp_infinite;
           (* FP_ZERO *)
           [mk_eq e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton fp_zero;
           (* FP_SUBNORMAL *)
           [mk_float_class float_valid e exp.erange], (* should also be != 0 *)
           Eval.singleton fp_subnormal;
           (* FP_NORMAL *)
           [mk_float_class float_valid e exp.erange], (* should also be != 0 *)
           Eval.singleton fp_normal
         ] man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isgreater(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isgreater", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_gt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isgreaterequal(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isgreaterequal", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_ge e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isless(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isless", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_lt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_islessequal(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_islessequal", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_gt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_islessgreater(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_islessgreater", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_ne e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isunordered(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isunordered", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_one exp.erange))
             ~felse:(Eval.singleton (mk_zero exp.erange))
             man
         ) man flow |>
         OptionExt.return

    | E_c_builtin_call(
        (
          "__builtin_sadd_overflow" |
          "__builtin_saddl_overflow" |
          "__builtin_saddll_overflow" |
          "__builtin_uadd_overflow" |
          "__builtin_uaddl_overflow" |
          "__builtin_uaddll_overflow"
        ), [e1;e2;res]) ->
      let range = erange exp in
      man.eval ~translate:"Universal" (mk_binop ~etyp:T_int e1 O_plus e2 range) flow >>$? fun eadd flow ->
      man.exec (mk_assign (mk_c_deref res range) {eadd with etyp = under_pointer_type (etyp res)} range) flow >>%? fun flow ->
      let rmin, rmax = rangeof (under_pointer_type (etyp res)) flow in
      assume (
        log_and
          (le (mk_z rmin range) eadd range)
          (le eadd (mk_z rmax range) range)
          range
      ) man flow
        ~fthen:(fun flow ->
            let () = debug "%a" (format @@ man.lattice.print) (Flow.get T_cur man.lattice flow) in Eval.singleton (mk_zero range) flow)
        ~felse:(Eval.singleton (mk_one range))
      |> OptionExt.return

    | E_c_builtin_call(
        (
          "__builtin_smul_overflow" |
          "__builtin_smull_overflow" |
          "__builtin_smulll_overflow" |
          "__builtin_umul_overflow" |
          "__builtin_umull_overflow" |
          "__builtin_umulll_overflow"
        ), [e1;e2;res]) ->
      let range = erange exp in
      man.eval ~translate:"Universal" (mk_binop ~etyp:T_int e1 O_mult e2 range) flow >>$? fun emul flow ->
      man.exec (mk_assign (mk_c_deref res range) {emul with etyp = under_pointer_type (etyp res)} range) flow >>%? fun flow ->
      let rmin, rmax = rangeof (under_pointer_type (etyp res)) flow in
      assume (
        log_and
          (le (mk_z rmin range) emul range)
          (le emul (mk_z rmax range) range)
          range
      ) man flow
        ~fthen:(Eval.singleton (mk_zero range))
        ~felse:(Eval.singleton (mk_one range))
      |> OptionExt.return

    | E_c_builtin_call("__builtin___memset_chk", [s;c;n;_]) ->
      man.eval (mk_c_call (find_c_fundec_by_name "__builtin_memset" flow) [s;c;n] exp.erange) flow |> OptionExt.return

    | E_c_builtin_call("__builtin___memmove_chk", [s;c;n;_]) ->
      man.eval (mk_c_call (find_c_fundec_by_name "__builtin_memmove" flow) [s;c;n] exp.erange) flow |> OptionExt.return

    | E_c_builtin_call("__builtin___memcpy_chk", [s;c;n;_]) ->
      man.eval (mk_c_call (find_c_fundec_by_name "__builtin_memcpy" flow) [s;c;n] exp.erange) flow |> OptionExt.return

    | E_c_builtin_call("__builtin___strcpy_chk", [s;c;n]) ->
      man.eval (mk_c_call (find_c_fundec_by_name "__builtin_strcpy" flow) [s;c] exp.erange) flow |> OptionExt.return




    | _ -> None

  let ask _ _ _  = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
