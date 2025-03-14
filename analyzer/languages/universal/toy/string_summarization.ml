(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2022 The MOPSA Project.                               *)
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

open Mopsa
open Sig.Abstraction.Stateless
open Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "universal.toy.string_summarization"
    end)

  let mk_ord_string ?(mode=WEAK) s = mk_attr_var ~mode:mode s "ord" T_int

  let checks = []

  let init (prog:program) man flow = None

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_subscript({ekind = E_var (s, _)}, i) ->
       Eval.singleton (mk_var (mk_ord_string s) range) flow |>
         OptionExt.return

    (* This rewrite rule is in both domains, otherwise the effect-based reduction will lose precision. I guess this should be moved out of both domains and put upwards in the configuration *)
    | E_len {ekind = E_binop(O_concat, e1, e2)} ->
      Eval.singleton (mk_binop ~etyp:T_int (mk_expr (E_len e1) range) O_plus (mk_expr (E_len e2) range) range) flow 
      |> OptionExt.return

    | E_len ({etyp = T_string}) ->
      Eval.singleton (mk_int_general_interval (ItvUtils.IntBound.Finite Z.zero) ItvUtils.IntBound.PINF  range) flow |>
      OptionExt.return

    | _ -> None


  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_add ({ekind = E_var (s, _); etyp = T_string}) ->
      man.exec (mk_add (mk_var (mk_ord_string s) range) range) flow
      |> OptionExt.return

    | S_assign ({ekind = E_var (s, _); etyp=T_string}, {ekind = E_constant (C_string str)}) ->
       let min, max = Seq.fold_left (fun (mini, maxi) c ->
           let ic = int_of_char c in
           min mini ic, max maxi ic) (256,0) (String.to_seq str) in
       man.exec (mk_assign (mk_var ~mode:(Some STRONG) (mk_ord_string s) range) (mk_int_interval min max range) range) flow
       |> OptionExt.return

    | S_assign ({ekind = E_var (s, _); etyp=T_string}, {ekind = E_var (t, _)}) ->
      man.exec (mk_assign (mk_var ~mode:(Some STRONG) (mk_ord_string s) range) (mk_var (mk_ord_string t) range) range) flow
       |> OptionExt.return

    | S_assign ({ekind = E_var (s, _); etyp=T_string}, {ekind = E_binop (O_concat, {ekind = E_var (v1, _)}, {ekind = E_var (v2, _)})}) ->
       Some (man.exec (mk_assign (mk_var (mk_ord_string s) range) (mk_var (mk_ord_string v1) range) range) flow >>% fun flow1 ->
       man.exec (mk_assign (mk_var (mk_ord_string s) range) (mk_var (mk_ord_string v2) range) range) flow >>% fun flow2 ->
        Post.return (Flow.join man.lattice flow1 flow2))


    | S_assign ({ekind = E_subscript ({ekind = E_var (s, _)}, i); etyp=T_int}, e) ->
       assume (mk_log_and (mk_le (mk_zero range) e range) (mk_le e (mk_int 127 range) range) range) man flow
         ~fthen:(fun flow ->
           man.exec (mk_assign (mk_var (mk_ord_string s) range) e range) flow)
         ~felse:(Cases.empty)
       |> OptionExt.return

    | _ -> None


  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> ('a, r) cases option =
    fun query man flow -> None

  let print_expr man flow printer exp =
    match ekind exp, etyp exp with
    | E_var (v, om), T_string ->
      man.print_expr flow printer (mk_var (mk_ord_string v) exp.erange)

    | _ -> () 

end

let () =
  register_stateless_domain (module Domain)
