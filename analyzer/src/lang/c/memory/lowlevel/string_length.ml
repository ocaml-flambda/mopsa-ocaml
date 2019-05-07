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
open Core.Sig.Stacked.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  let name = "c.memory.lowlevel.string_length"

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [
        Z_u_num;
        Z_c_scalar
      ];
    };
    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c_low_level, Z_u_num;
        Z_c_scalar, Z_u_num;
        Z_c_low_level, Z_c_scalar;
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
      let () = Format.fprintf Format.str_formatter "length(%a)" pp_base base in
      Format.flush_str_formatter ()
    in
    let vuid = base_uid base in
    let uniq_vname = org_vname ^ ":" ^ (string_of_int vuid) in
    let v = mkv org_vname uniq_vname vuid T_int in
    mk_var v range



  (* Find the position of the first zero in an initialization expression *)
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
  let declare_variable v init man flow =
    let scope, range =
      match v.vkind with
      | V_c { var_scope; var_range } -> var_scope, var_range
      | _ -> assert false
    in

    let length = mk_length_var (V v) range in

    (* Add the length variable to the numeric domain *)
    let post = man.exec_sub (mk_add length range) flow in

    match scope, init with
    (** Uninitialized global variable *)
    | Variable_global, None | Variable_file_static _, None ->
      (* The variable is filled with 0 (C99 6.7.8.10) *)
      Post.bind (man.exec_sub (mk_assign length (mk_zero range) range)) post

    (** Uninitialized local variable *)
    | Variable_local _, None | Variable_func_static _, None ->
      (* The value of the variable is undetermined (C99 6.7.8.10), so
         the first zero can be in offsets [0, size]
      *)
      let size = sizeof_type v.vtyp in
      Post.bind (man.exec_sub (mk_assign length (mk_z_interval Z.zero size range) range)) post

    | _, Some init ->
      (* Find the first zero byte *)
      let zero_offset = find_zero v.vtyp init in
      Post.bind (man.exec_sub (mk_assign length (mk_z zero_offset range) range)) post

    | _ -> assert false


  (** Cases of the assignment abstract transformer *)
  let assign_cases base offset rhs typ range man flow =
    let length = mk_length_var base range in

    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) offset flow |>
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
          let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow in
          Post.return flow'
        )
      ~zone:Z_u_num man flow


  (** Assignment abstract transformer *)
  let assign lval rhs range man flow =
    let p =
      match ekind lval with
      | E_c_deref p -> p
      | _ -> assert false
    in

    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    post_eval man @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to (P_block (base, offset)) ->
      assign_cases base offset rhs lval.etyp range man flow

    | _ -> assert false


  (** Compute symbolic boundaries of a quantified offset. *)
  (* FIXME: works only for linear expressions *)
  let rec bound_quantified_offset offset : expr * expr =
    match ekind offset with
     | E_constant _ -> offset, offset

     | E_var (v, _) -> offset, offset

     | E_stub_quantified(FORALL, _, S_interval(l, u)) -> l, u

     | E_unop (O_minus, e) ->
       let l, u = bound_quantified_offset e in
       { offset with ekind = E_unop (O_minus, u)},
       { offset with ekind = E_unop (O_minus, l)}

     | E_binop (O_plus, e1, e2) ->
       let l1, u1 = bound_quantified_offset e1 in
       let l2, u2 = bound_quantified_offset e2 in
       { offset with ekind = E_binop (O_plus, l1, l2)},
       { offset with ekind = E_binop (O_plus, u1, u2)}

     | E_binop (O_minus, e1, e2) ->
       let l1, u1 = bound_quantified_offset e1 in
       let l2, u2 = bound_quantified_offset e2 in
       { offset with ekind = E_binop (O_minus, l1, u2)},
       { offset with ekind = E_binop (O_minus, u1, l2)}

     | E_binop (O_mult, e, ({ ekind = E_constant (C_int c) } as const))
     | E_binop (O_mult, ({ ekind = E_constant (C_int c) } as const), e) ->
       let l, u = bound_quantified_offset e in
       if Z.geq c Z.zero then
         { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })},
         { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })}
       else
         { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })},
         { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })}


     | _ -> panic ~loc:__LOC__
              "bound_quantified_offset called on a non linear expression %a"
              pp_expr offset


  (** Cases of the abstract transformer for tests *(p + ∀i) ? 0 *)
  let assume_quantified_zero_cases op base offset range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = bound_quantified_offset offset in

    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) min flow |>
    post_eval man @@ fun min flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) max flow |>
    post_eval man @@ fun max flow ->

    let length = mk_length_var base range in

    let mk_bottom flow = Flow.set T_cur man.lattice.bottom man.lattice flow in

    (* Safety condition: [min, max] ⊆ [0, size[ *)
    assume_post (
      mk_binop
        (mk_in min (mk_zero range) size range)
        O_log_and
        (mk_in max (mk_zero range) size range)
        range
    )
      ~fthen:(fun flow ->
          switch_post [
            (* nonzero case *)
            (* Range condition: max < length

               |--------|***********|---------|--------|->
               0       min         max     length     size

                     ∀ i ∈ [min, max] : s[i] != 0
            *)
            (* Transformation: ⊥ if op = O_eq, nop if op = O_ne *)
            [
              mk_binop max O_lt length range, true;
            ],
            (fun flow ->
               match op with
               | O_eq -> Post.return (mk_bottom flow)
               | O_ne -> Post.return flow
               | _ -> assert false
            )
            ;

            (* zero case *)
            (* Range condition: length ≤ max

               |--------|***********|********|-------|->
               0       min        length    max     size

                      ∃ i ∈ [min, max] : s[i] == 0
            *)
            (* Transformation: nop if op = O_eq, ⊥ if op = O_ne *)
            [
              mk_in max length size range, true;
            ],
            (fun flow ->
               match op with
               | O_eq -> Post.return flow
               | O_ne -> Post.return (mk_bottom flow)
               | _ -> assert false
            )
            ;
          ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow in
          Post.return flow'
        )
      ~zone:Z_u_num man flow




  (** Abstract transformer for tests *(p + ∀i) ? 0 *)
  let assume_quantified_zero op lval range man flow =
    let p =
      let rec find_pointer e =
        match ekind e with
        | E_c_deref p -> p
        | E_c_cast(ee, _) -> find_pointer ee
        | _ -> assert false
      in
      find_pointer lval
    in

    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    post_eval man @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to (P_block (base, offset)) ->
      assume_quantified_zero_cases op base offset range man flow

    | _ -> assert false



  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init) ->
      (* We need to simplify initialization expression before passing
         the statement to the underlying scalar domains *)
      assert false

    | S_add { ekind = E_addr addr } ->
      (* Add the length of the address @ to the numeric domain and
         initialize it with the interval [0, size(@)] *)

      Some (
        eval_base_size (A addr) stmt.srange man flow |>
        post_eval man @@ fun size flow ->

        man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
        post_eval man @@ fun size flow ->

        let length = mk_length_var (A addr) stmt.srange in

        man.exec_sub ~zone:Z_u_num (mk_add length stmt.srange) flow |>
        Post.bind (man.exec_sub ~zone:Z_u_num (mk_assume (mk_in length (mk_zero stmt.srange) size stmt.srange) stmt.srange))
      )

    | S_assign({ ekind = E_var _} as lval, rval) when is_c_scalar_type lval.etyp ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_scalar) rval flow |>
        post_eval man @@ fun rval flow ->

        let stmt = { stmt with skind = S_assign (lval, rval) } in
        man.exec_sub ~zone:Z_c_scalar stmt flow
      )


    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_u_num) rval flow |>
        post_eval man @@ fun rval flow ->

        assign lval rval stmt.srange man flow
      )

    (* 𝕊⟦ *(p + ∀i) != 0 ⟧ *)
    | S_assume({ekind = E_binop(O_ne, lval, {ekind = E_constant(C_int n)})})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, lval, {ekind = E_constant(C_int n)})})})
      when is_expr_quantified lval &&
           Z.equal n Z.zero
      ->
      Some (
        assume_quantified_zero O_ne lval stmt.srange man flow
      )

    (* 𝕊⟦ *(p + ∀i) == 0 ⟧ *)
    | S_assume({ekind = E_binop(O_eq, lval, {ekind = E_constant(C_int n)})})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, lval, {ekind = E_constant(C_int n)})})})
      when is_expr_quantified lval &&
           Z.equal n Z.zero
      ->
      Some (
        assume_quantified_zero O_eq lval stmt.srange man flow
      )

    | S_assume(e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow |>
        post_eval man @@ fun e flow ->

        let stmt = { stmt with skind = S_assume (e) } in
        man.exec_sub ~zone:Z_c_scalar stmt flow
      )


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Cases of the abstraction evaluations *)
  let eval_cases base offset typ range man flow =
    let length = mk_length_var base range in

    eval_base_size base range man flow |>
    Eval.bind @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    Eval.bind @@ fun size flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) offset flow |>
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
          raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow |>
          Eval.empty_singleton
        )
      ~zone:Z_u_num man flow


  (** Abstract evaluation of a dereference *)
  let deref p range man flow =
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref range ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref range ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to (P_block (base, offset)) ->
      eval_cases base offset (under_pointer_type p.etyp) range man flow


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
