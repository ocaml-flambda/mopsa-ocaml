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

(** Domain for supporting variable-length arrays *)

open Mopsa
open Sig.Domain.Stateless
open Universal.Ast
open Universal.Zone
open Stubs.Ast
open Ast
open Zone


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId
      (struct let name = "c.memory.variable_length_array" end)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = [
        Z_c_low_level;
        Z_c_scalar;
        Z_u_num
      ];
    };
    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c,Z_c_scalar;
        Z_c,Z_u_num
      ];
    }
  }

  let alarms = []


  (** {2 Initialization} *)
  (** ****************** *)

  let init prog man flow = flow


  (** Variable-length auxiliary variable *)
  (** ********************************** *)

  type var_kind +=
    | V_c_variable_length of var


  let pp_variable_length fmt arr =
    Format.fprintf fmt "vlength(%a)" pp_var arr

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_variable_length arr ->
            pp_variable_length fmt arr
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_variable_length a1, V_c_variable_length a2 ->
            compare_var a1 a2
          | _ -> next v1 v2
        );
    }


  let mk_variable_length_var arr =
    let name =
      let () = pp_variable_length Format.str_formatter arr in
      Format.flush_str_formatter ()
    in
    mkv name (V_c_variable_length arr) ul



  (** {2 Execution of statements} *)
  (** *************************** *)

  (** Get the length expression *)
  let get_array_length_expr v =
    match remove_typedef_qual v.vtyp with
    | T_c_array(_, C_array_length_expr e) -> e
    | _ -> assert false


  (** 𝕊⟦ t arr[n]; ⟧ *)
  let exec_declare arr range man flow =
    (* FIXME: we do not support multi dimensional arrays *)
    if is_c_variable_length_array_type (under_array_type arr.vtyp) then
      panic_at range "multi-dimensional variable length arrays not supported"
    ;

    (* Add the length variable to the environment *)
    let len = mk_variable_length_var arr in
    man.post ~zone:Z_c_scalar (mk_add_var len range) flow >>$ fun () flow ->

    (* Initialize it with the length expression *)
    man.eval ~zone:(Z_c,Z_u_num) (get_array_length_expr arr) flow >>$ fun e flow ->
    let ee = mul e
        (under_array_type arr.vtyp |> void_to_char |> sizeof_type |> (fun z -> mk_z z range))
        range
    in
    man.post ~zone:Z_c_scalar (mk_assign (mk_var len range) ee range) flow >>$ fun () flow ->

    (* Add arr as a base in the underlying memory abstraction *)
    man.post ~zone:Z_c_low_level (mk_add_var arr range) flow


  (** 𝕊⟦ remove arr; ⟧ *)
  let exec_remove arr range man flow =
    (* Remove the base arr from the underlying memory abstraction *)
    man.post ~zone:Z_c_low_level (mk_remove_var arr range) flow >>$ fun () flow ->

    (* Remove the length variable from the environment *)
    let len = mk_variable_length_var arr in
    man.post ~zone:Z_c_scalar (mk_remove_var len range) flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration(v,None,_) when is_c_variable_length_array_type v.vtyp ->
      exec_declare v stmt.srange man flow |>
      Option.return

    | S_remove { ekind = E_var(v,_) } when is_c_variable_length_array_type v.vtyp ->
      exec_remove v stmt.srange man flow |>
      Option.return

    | _ -> None


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** 𝔼⟦ bytes(arr) ⟧ *)
  let eval_bytes arr range man flow =
    let len = mk_variable_length_var arr in
    Eval.singleton (mk_var len range) flow


  let eval zone exp man flow =
    match ekind exp with
    | E_stub_builtin_call(BYTES, { ekind = E_var(v,_) }) when is_c_variable_length_array_type v.vtyp ->
      eval_bytes v exp.erange man flow |>
      Option.return

    | _ -> None


  (** {2 Handler of queries} *)
  (** ********************** *)

  let ask query man flow = None


end

let () =
  Sig.Domain.Stateless.register_domain (module Domain)
