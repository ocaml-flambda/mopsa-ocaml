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

(** String length abstraction.

    This abstract domain implements the technique presented in [1]. It
    abstracts C strings by the position of the first `\0` character within the
    base memory block.

    The length is kept in an underlying numeric domain. Therefore, the domain
    is implemented as a stack domain, to allow sharing the underlying domain
    with others.

    The domain is stateless, because abstraction is performed by rewriting
    statements/expressions in C into equivalent ones in Universal over
    the length variable. Not internal state is required.

    [1] M. Journault, A. Miné, A. Ouadjaout. Modular static analysis
    of string manipulations in C programs. SAS 2018. LNCS, vol. 11002.
*)


open Mopsa
open Universal.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  let name = "c.memory.strings.length"

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = [
        Z_u_num;
        Z_c_scalar
      ];
    };
    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c, Z_u_num;
        Z_c_scalar, Z_u_num;
        Z_c, Z_c_low_level;
        Z_c, Z_c_scalar;
        Z_c_low_level, Z_c_points_to;
      ];
    }
  }

  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow


  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Create a length variable.

      The returned variable is a mathematical integer, not a
      C variable (it has type [T_int])
  *)
  let mk_length_var base range =
    let org_vname =
      let () = Format.fprintf Format.str_formatter "%a_length" pp_base base in
      Format.flush_str_formatter ()
    in
    let vuid = base_uid base in
    let uniq_vname = org_vname ^ ":" ^ (string_of_int vuid) in
    let v = mkv org_vname uniq_vname vuid T_int in
    mk_var v range



  (* Find the position of the first zero, or reach the end of the
       allocation space *)
  let find_zero typ init =
    let size = sizeof_type typ in
    match init with
    | C_init_expr {ekind = E_constant(C_c_string (s, _))} ->
      let rec aux j =
        if Z.equal j size
        then Z.pred size
        else if Z.lt j (Z.of_int @@ String.length s)
        then
          if int_of_char (String.get s (Z.to_int j)) = 0
          then j
          else aux Z.(j + one)
        else j
      in
      aux Z.zero

    | _ -> panic ~loc:__LOC__
             "find_zero: initialization %a not supported"
             Pp.pp_c_init init


  (** Declaration of a C variable *)
  let declare_variable v man sman flow =
    let scope, init, range =
      match v.vkind with
      | V_c { var_scope; var_init; var_range } -> var_scope, var_init, var_range
      | _ -> assert false
    in

    let length = mk_length_var (V v) range in

    match scope, init with
    (** Uninitialized global variable *)
    | Variable_global, None | Variable_file_static _, None ->
      (* The variable is filled with 0 (C99 6.7.8.10) *)
      sman.post (mk_assign length (mk_zero range) range) flow

    (** Uninitialized local variable *)
    | Variable_local _, None | Variable_func_static _, None ->
      (* The value of the variable is undetermined (C99 6.7.8.10), so
         the first zero can be in offsets [0, size]
      *)
      let size = sizeof_type v.vtyp in
      sman.post (mk_assign length (mk_z_interval Z.zero size range) range) flow

    | _, Some init ->
      (* Find the first zero byte *)
      let zero_offset = find_zero v.vtyp init in
      sman.post (mk_assign length (mk_z zero_offset range) range) flow

    | _ -> assert false


  (** Assignment to a base and an offset *)
  let assign_base base offset rhs typ range man sman flow =
    let length = mk_length_var base range in

    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c, Z_u_num) offset flow |>
    post_eval man @@ fun offset flow ->

    (* Compute the offset in bytes *)
    let elm_size = sizeof_type typ in
    let offset =
      (* Pointer to void => return size in bytes *)
      if is_c_void_type typ then offset
      else
      if Z.equal elm_size Z.one
      then offset
      else div size (of_z elm_size range) range
    in

    (* Utility function to assign an interval *)
    let assign_interval l u flow =
      man.post ~zone:Z_u_num (mk_forget length range) flow |>
      Post.bind (
        man.post ~zone:Z_u_num (mk_assume ((mk_in length l u range)) range)
      )
    in

    (* Check that offset ∈ [0, size - elm_size] *)
    assume_post (mk_in offset (mk_zero range) (sub size (mk_z elm_size range) range) range)
      ~fthen:(fun flow ->
          if Z.gt elm_size Z.one
          then
            (* FIXME: assignments of multi-bytes not supported for the moment *)
            assign_interval (mk_zero range) size flow
          else
          switch_post [
            (* set0 case *)
            (* Offset condition: offset ∈ [0, length] *)
            (* RHS condition: rhs = 0 *)
            (* Transformation: length := 0; *)
            [
              mk_binop offset O_ge (mk_zero range) range, true;
              mk_binop offset O_le length range, true;
              mk_binop rhs O_eq (mk_zero range) range, true;
            ],
            (fun flow -> man.post ~zone:Z_u_num (mk_assign length offset range) flow)
            ;

            (* setnon0 case *)
            (* Offset condition: offset = length *)
            (* RHS condition: rhs ≠ 0 *)
            (* Transformation: length := [offset + 1, size]; *)
            [
              mk_binop offset O_eq length range, true;
              mk_binop rhs O_ne (mk_zero range) range, true;
            ],
            (fun flow -> assign_interval (add offset (one range) range) size flow)
            ;

            (* First unchanged case *)
            (* Offset condition: offset ∈ [0, length[ *)
            (* RHS condition: rhs ≠ 0 *)
            (* Transformation: nop; *)
            [
              mk_binop offset O_ge (mk_zero range) range, true;
              mk_binop offset O_lt length range, true;
              mk_binop rhs O_ne (mk_zero range) range, true;
            ],
            (fun flow -> Post.return flow)
            ;

            (* Second unchanged case *)
            (* Offset condition: offset > length *)
            (* RHS condition: ⊤ *)
            (* Transformation: nop; *)
            [
              mk_binop offset O_gt length range, true;
            ],
            (fun flow -> Post.return flow)


          ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man flow in
          Post.return flow'
        )
      ~zone:Z_u_num man flow


  (** Assignment abstract transformer *)
  let assign lval rhs range man sman flow =
    let p =
      match ekind lval with
      | E_c_deref p -> p
      | _ -> assert false
    in

    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    post_eval man @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man flow |>
      Post.return

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man flow |>
      Post.return

    | E_c_points_to (P_block (base, offset)) ->
      assign_base base offset rhs lval.etyp range man sman flow

    | _ -> assert false



  (** Transformers entry point *)
  let exec zone stmt man sman flow =
    match skind stmt with
    | S_c_declaration v when is_c_array_type v.vtyp ->
      declare_variable v man sman flow |>
      Option.return

    | S_assign({ ekind = E_var _} as lval, rval) when is_c_scalar_type lval.etyp ->
      Some (
        man.eval ~zone:(Z_c,Z_c_scalar) rval flow |>
        post_eval man @@ fun rval flow ->

        let stmt = { stmt with skind = S_assign (lval, rval) } in
        sman.post ~zone:Z_c_scalar stmt flow
      )


    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Z_c,Z_c_low_level) lval flow |> Option.return |> Option.lift @@
      post_eval man @@ fun lval flow ->

      man.eval ~zone:(Z_c,Z_u_num) rval flow |>
      post_eval man @@ fun rval flow ->

      assign lval rval stmt.srange man sman flow


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Dereference a base and an offset *)
  let deref_base base offset typ range man flow =
    let length = mk_length_var base range in

    eval_base_size base range man flow |>
    Eval.bind @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    Eval.bind @@ fun size flow ->

    man.eval ~zone:(Z_c, Z_u_num) offset flow |>
    Eval.bind @@ fun offset flow ->

    (* Compute the offset in bytes *)
    let elm_size = sizeof_type typ in
    let offset =
      (* Pointer to void => return size in bytes *)
      if is_c_void_type typ then offset
      else
      if Z.equal elm_size Z.one
      then offset
      else div size (of_z elm_size range) range
    in

    (* Check that offset ∈ [0, size - elm_size] *)
    assume_eval (mk_in offset (mk_zero range) (sub size (mk_z elm_size range) range) range)
      ~fthen:(fun flow ->
          if Z.gt elm_size Z.one
          then
            let l,u = rangeof typ in
            Eval.singleton (mk_z_interval l u ~typ range) flow
          else
            switch_eval [
              (* before case *)
              (* Offset condition: offset ∈ [0, length[ *)
              (* Evaluation: [1; 255] if unsigned, [-128, 127] otherwise *)
              [
                mk_binop offset O_ge (mk_zero range) range, true;
                mk_binop offset O_lt length range, true;
              ],
              (fun flow ->
                 if is_signed typ = false then
                   Eval.singleton (mk_int_interval 1 255 ~typ range) flow
                 else
                   Eval.singleton (mk_int_interval (-128) 127 ~typ range) flow
              );

              (* at case *)
              (* Offset condition: offset = length *)
              (* Evaluation: 0 *)
              [
                mk_binop offset O_eq length range, true;
              ],
              (fun flow -> Eval.singleton (mk_zero ~typ range)flow)
              ;

              (* After case *)
              (* Offset condition: offset > length *)
              (* Evaluation: [0; 255] if unsigned, [-128, 127] otherwise *)
              [
                mk_binop offset O_gt length range, true;
              ],
              (fun flow ->
                 if is_signed typ = false then
                   Eval.singleton (mk_int_interval 0 255 ~typ range) flow
                 else
                   Eval.singleton (mk_int_interval (-128) 127 ~typ range) flow
              );

            ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          raise_alarm Alarms.AOutOfBound range ~bottom:true man flow |>
          Eval.empty_singleton
        )
      ~zone:Z_u_num man flow


  (** Abstract evaluation of a dereference *)
  let deref p range man flow =
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref range ~bottom:true man flow |>
      Eval.empty_singleton

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref range ~bottom:true man flow |>
      Eval.empty_singleton

    | E_c_points_to (P_block (base, offset)) ->
      deref_base base offset (under_pointer_type p.etyp) range man flow

    | _ -> assert false


  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p ->
      deref p exp.erange man flow |>
      Option.return

    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
