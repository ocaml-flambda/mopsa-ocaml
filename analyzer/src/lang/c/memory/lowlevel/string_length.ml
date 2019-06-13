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

  include GenStatelessDomainId(struct
      let name = "c.memory.lowlevel.string_length"
    end)

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


  (** {2 Variable of string lengths} *)
  (** ****************************** *)

  (** Registration of a new var kind for length variables *)
  type var_kind +=
    | V_c_string_length of base * bool (* is it primed? *)

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_string_length (base,primed) ->
            Format.fprintf fmt "length(%a)%s"
              pp_base base
              (if primed then "'" else "")

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_string_length(b1,p1), V_c_string_length(b2,p2) ->
            Compare.compose [
              (fun () -> compare_base b1 b2);
              (fun () -> compare p1 p2);
            ]

          | _ -> next v1 v2
        );
    }

  (** Create a length variable. The returned variable is a
      mathematical integer, not a C variable.
  *)
  let mk_length_var base ?(primed=false) ?(mode=base_mode base) range =
    let name =
      let () = match base with
        | V v ->
          Format.fprintf Format.str_formatter "length(%s)%s"
            v.vname
            (if primed then "'" else "")

        | _ ->
          Format.fprintf Format.str_formatter "length(%a)%s"
            pp_base base
            (if primed then "'" else "")
      in
      Format.flush_str_formatter ()
    in
    let v = mkv name (V_c_string_length (base,primed)) T_int in
    mk_var v ~mode range


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow


  (** {2 Abstract transformers} *)
  (** ************************* *)


  let is_memory_base base =
    match base with
    | V v -> is_c_type v.vtyp &&
             not (is_c_scalar_type v.vtyp)

    | A { addr_kind = A_stub_resource "Memory" }
    | A { addr_kind = A_stub_resource "ReadOnlyMemory" }
    | A { addr_kind = A_stub_resource "String"}
    | A { addr_kind = A_stub_resource "ReadOnlyString" }
      -> true

    | _ -> false


  (** Declaration of a C variable *)
  let declare_variable v init scope range man flow =
    if not (is_memory_base (V v))
    then
      Post.return flow

    else
      (* Since we are in the Z_low_level zone, we assume that init has
         been translated by a structured domain into a flatten
         initialization *)
      let flat_init =
        match init with
        | Some (C_init_flat l) -> l
        | _ -> assert false
      in

      let is_global =
        match scope with
        | Variable_func_static _ | Variable_local _ | Variable_parameter _ -> false
        | _ -> true
      in

      let size = sizeof_type v.vtyp in

      let length = mk_length_var (V v) range in

      (* Find the position of the first zero *)
      let rec aux o l =
        match l with
        | [] -> o, o

        | C_flat_none _ :: tl when is_global -> o, o

        | C_flat_none _ :: tl -> o, size

        | C_flat_expr (e,t) :: tl ->
          begin
            match expr_to_z e with
            | Some e ->
              if Z.equal e Z.zero
              then o, o
              else aux (Z.add o (sizeof_type t)) tl

            | None ->
              (* FIXME: test the value of the expression *)
              o, size
          end

        | C_flat_fill (e,t,n) :: tl ->
          begin
            match expr_to_z e with
            | Some e ->
              if Z.equal e Z.zero
              then o, o
              else aux (Z.add o (Z.mul n (sizeof_type t))) tl

            | None ->
              (* FIXME: test the value of the expression *)
              o, size
          end
      in
      let o1, o2 = aux Z.zero flat_init in

      let init' =
        if Z.equal o1 o2
        then mk_z o1 range
        else mk_z_interval o1 o2 range
      in

      man.exec_sub ~zone:Z_u_num (mk_add length range) flow |>
      Post.bind (man.exec_sub ~zone:Z_u_num (mk_assign length init' range))



  (** Cases of the assignment abstract transformer *)
  let assign_cases base offset rhs typ range man flow =
    let length = mk_length_var base range in

    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    (* Compute the offset in bytes *)
    let elm_size = sizeof_type typ in

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
          if not (is_memory_base base)
          then
            Post.return flow

          else if Z.gt elm_size Z.one
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
                mk_in offset (mk_zero range) length range, true;
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
                mk_in ~right_strict:true offset (mk_zero range) length range, true;
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


  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    post_eval man @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_valid ->
      warn_at range "unsound assignment to valid pointer %a" pp_expr p;
      Post.return flow

    | E_c_points_to (P_block (base, offset)) ->
      man.eval ~zone:(Z_c_low_level,Z_u_num) rval flow |>
      post_eval man @@ fun rval flow ->

      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      post_eval man @@ fun offset flow ->

      let post = assign_cases base offset rval (under_type p.etyp) range man flow in
      debug "Post *%a = %a : %a" pp_expr p pp_expr rval  (Post.print man.lattice) post;
      post

    | _ -> assert false



  (** Cases of the abstract transformer for tests *("..." + ∀i) ? 0 *)
  let assume_quantified_string_zero_cases op str offset range man flow =
    debug "quantified string %s" str;
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset in

    man.eval ~zone:(Z_c_low_level, Z_u_num) min flow |>
    post_eval man @@ fun min flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) max flow |>
    post_eval man @@ fun max flow ->

    let length = mk_z (Z.of_int @@ String.length str) range in

    let mk_bottom flow = Flow.set T_cur man.lattice.bottom man.lattice flow in

    (* Safety condition: [min, max] ⊆ [0, length] *)
    assume_post (
      mk_binop
        (mk_in min (mk_zero range) length range)
        O_log_and
        (mk_in max (mk_zero range) length range)
        range
    )
      ~fthen:(fun flow ->
          switch_post [
            (* nonzero case *)
            (* Range condition: max < length

               |--------|***********|--------| '\0' |----->
               0       min         max    length   size

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
            (* Range condition: max = length

                       min                 max
               |--------|*******************| '\0' |-------|->
               0                         length    size

                      ∃ i ∈ [min, max] : s[i] == 0
            *)
            (* Transformation: nop if op = O_eq, ⊥ if op = O_ne *)
            [
              mk_binop max O_eq length range, true;
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


  (** Cases of the abstract transformer for tests *(p + ∀i) ? 0 *)
  let assume_quantified_zero_cases op base offset primed range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset in

    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) min flow |>
    post_eval man @@ fun min flow ->

    man.eval ~zone:(Z_c_low_level, Z_u_num) max flow |>
    post_eval man @@ fun max flow ->

    let length = mk_length_var base ~primed range in

    let mk_bottom flow = Flow.set T_cur man.lattice.bottom man.lattice flow in

    (* Safety condition: [min, max] ⊆ [0, size[ *)
    assume_post (
      mk_binop
        (mk_in min (mk_zero range) size ~right_strict:true range)
        O_log_and
        (mk_in max (mk_zero range) size ~right_strict:true range)
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
    let p, primed =
      let rec doit e =
        match ekind e with
        | E_c_deref p -> p, false
        | E_c_cast(ee, _) -> doit ee
        | E_stub_primed e -> fst (doit e), true
        | _ -> panic_at range "assume_quantified_zero: invalid argument %a" pp_expr lval;
      in
      doit lval
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

    | E_c_points_to (P_block (S str, offset)) ->
      man.eval ~zone:(Z_c_low_level, Z_u_num) offset flow |>
      post_eval man @@ fun offset flow ->
      assume_quantified_string_zero_cases op str offset range man flow

    | E_c_points_to (P_block (base, offset)) ->
      man.eval ~zone:(Z_c_low_level, Z_u_num) offset flow |>
      post_eval man @@ fun offset flow ->
      assume_quantified_zero_cases op base offset primed range man flow

    | E_c_points_to P_top | E_c_points_to P_valid ->
      Post.return flow

    | _ -> assert false


  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    (* Add the length of the base to the numeric domain and
       initialize it with the interval [0, size(@)]
    *)
    eval_base_size base range man flow |>
    post_eval man @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    post_eval man @@ fun size flow ->

    let length = mk_length_var base range in

    man.exec_sub ~zone:Z_u_num (mk_add length range) flow |>
    Post.bind (man.exec_sub ~zone:Z_u_num (mk_assume (mk_in length (mk_zero range) size range) range))


  (** Rename primed variables introduced by a stub *)
  let rename_primed target offsets range man flow =
    (* FIXME: since we do not keep track of bases that have a length
       representation, we can not determine whether the length
       variable exists in the pre-condition (so we do a weak update)
       or not (so we rename the primed one).

       Therefore, we add the length variable of the precondition and
       we perform a weak update. This may cause loss of precision.
    *)
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow |>
    post_eval man @@ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) ->
      let length = mk_length_var base range ~primed:false ~mode:WEAK in
      let length' = mk_length_var base range ~primed:true in
      man.exec_sub ~zone:Z_u_num (mk_add length range) flow |> Post.bind @@ fun flow ->
      man.exec_sub ~zone:Z_u_num (mk_add length' range) flow |> Post.bind @@ fun flow ->
      man.exec_sub ~zone:Z_u_num (mk_assign length length' range) flow |> Post.bind @@ fun flow ->
      man.exec_sub ~zone:Z_u_num (mk_remove length' range) flow



    | E_c_points_to (P_null | P_invalid) ->
      Post.return flow


    | E_c_points_to (P_top | P_valid) ->
      warn_at range "unsound: rename of %a not supported because it can not be resolved"
        pp_expr target
      ;
      Post.return flow


    | _ -> assert false


  (** Rename the length variable associated to a base *)
  let rename_base base1 base2 range man flow =
    let length1 = mk_length_var base1 range in
    let length2 = mk_length_var base2 range in
    man.exec_sub ~zone:Z_u_num (mk_rename length1 length2 range) flow


  let rec is_deref_expr e =
    match ekind e with
    | E_c_deref _ -> true
    | E_stub_primed ee -> is_deref_expr ee
    | E_c_cast (ee, _) -> is_deref_expr ee
    | _ -> false


  let rec is_zero_expr e =
    match ekind e with
    | E_constant (C_int n) when Z.equal n Z.zero -> true
    | E_c_cast (ee, _) -> is_zero_expr ee
    | _ -> false



  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when not (is_c_scalar_type v.vtyp) ->
      declare_variable v init scope stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_var (v, _) } when not (is_c_scalar_type v.vtyp) ->
      add_base (V v) stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_addr addr } when is_memory_base (A addr) ->
      add_base (A addr) stmt.srange man flow |>
      Option.return


    | S_rename ({ ekind = E_var (v1,_) }, { ekind = E_var (v2,_) })
      when is_memory_base (V v1) &&
           is_memory_base (V v2)
      ->
      rename_base (V v1) (V v2) stmt.srange man flow |>
      Option.return


    | S_rename ({ ekind = E_addr addr1 }, { ekind = E_addr addr2 })
      when is_memory_base (A addr1) &&
           is_memory_base (A addr2)
      ->
      rename_base (A addr1) (A addr2) stmt.srange man flow |>
      Post.bind (man.exec_sub ~zone:Z_c_scalar stmt) |>
      Option.return

    | S_assign({ ekind = E_c_deref p}, rval) when under_type p.etyp |> is_c_num_type ->
      assign_deref p rval stmt.srange man flow |>
      Option.return


    (* 𝕊⟦ *(p + ∀i) != 0 ⟧ *)
    | S_assume({ekind = E_binop(O_ne, lval, n)})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, lval, n)})})
      when is_expr_quantified lval &&
           is_deref_expr lval &&
           is_zero_expr n
      ->
      assume_quantified_zero O_ne lval stmt.srange man flow |>
      Option.return

    (* 𝕊⟦ *(p + ∀i) == 0 ⟧ *)
    | S_assume({ekind = E_binop(O_eq, lval, n)})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, lval, n)})})
      when is_expr_quantified lval &&
           is_deref_expr lval &&
           is_zero_expr n
      ->
      assume_quantified_zero O_eq lval stmt.srange man flow |>
      Option.return

    | S_stub_rename_primed (target, offsets) when is_c_type target.etyp &&
                                                  not (is_c_num_type target.etyp) &&
                                                  (not (is_c_pointer_type target.etyp) || List.length offsets > 0)
      ->
      rename_primed target offsets stmt.srange man flow |>
      Option.return

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Cases of the abstraction evaluation of s[i] where s is a string *)
  let eval_deref_string_cases str offset typ range man flow =

    let length = mk_z (Z.of_int @@ String.length str) range in

    (* Check that offset ∈ [0, length] *)
    assume_eval (mk_in offset (mk_zero range) length range)
      ~fthen:(fun flow ->
          switch_eval [
            (* before case *)
            (* Offset condition: offset < length *)
            (* Evaluation: [1; 255] if unsigned, [-128, -1] U [1, 127] otherwise *)
            [
              mk_binop offset O_lt length range, true;
            ],
            (fun flow ->
               if is_signed typ = false then
                 Eval.singleton (mk_int_interval 1 255 ~typ range) flow
               else
                 Eval.join
                   (Eval.singleton (mk_int_interval (-128) (-1) ~typ range) flow)
                   (Eval.singleton (mk_int_interval 1 127 ~typ range) flow)
            );

            (* at case *)
            (* Offset condition: offset = length *)
            (* Evaluation: 0 *)
            [
              mk_binop offset O_eq length range, true;
            ],
            (fun flow -> Eval.singleton (mk_zero ~typ range)flow)
            ;

          ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow |>
          Eval.empty_singleton
        )
      ~zone:Z_u_num man flow



  (** Cases of the abstraction evaluations *)
  let eval_deref_cases base offset typ primed range man flow =
    eval_base_size base range man flow |>
    Eval.bind @@ fun size flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
    Eval.bind @@ fun size flow ->

    let elm_size = sizeof_type typ in

    debug "deref %a%s" pp_base base (if primed then "'" else "");

    (* Check that offset ∈ [0, size - elm_size] *)
    assume_eval (mk_in offset (mk_zero range) (sub size (mk_z elm_size range) range) range)
      ~fthen:(fun flow ->
          if not (Z.equal elm_size Z.one &&
                  is_memory_base base
                 )
          then
            Eval.singleton (mk_top typ range) flow
          else
            let length = mk_length_var base ~primed range in
            switch_eval [
              (* before case *)
              (* Offset condition: offset ∈ [0, length[ *)
              (* Evaluation: [1; 255] if unsigned, [-128, -1] U [1, 127] otherwise *)
              [
                mk_binop offset O_ge (mk_zero range) range, true;
                mk_binop offset O_lt length range, true;
              ],
              (fun flow ->
                 if is_signed typ = false then
                   Eval.singleton (mk_int_interval 1 255 ~typ range) flow
                 else
                   Eval.join
                     (Eval.singleton (mk_int_interval (-128) (-1) ~typ range) flow)
                     (Eval.singleton (mk_int_interval 1 127 ~typ range) flow)
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
  let eval_deref exp primed range man flow =
    let p = match ekind exp with E_c_deref p -> p | _ -> assert false in
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to (P_block (S str, offset)) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->
      eval_deref_string_cases str offset (under_pointer_type p.etyp) range man flow

    | E_c_points_to (P_block (base, offset)) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->
      eval_deref_cases base offset (under_pointer_type p.etyp) primed range man flow

    | E_c_points_to P_top | E_c_points_to P_valid ->
      Eval.singleton (mk_top (under_pointer_type p.etyp) range) flow

    | _ -> assert false


  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p when is_c_num_type exp.etyp ->
      eval_deref exp false exp.erange man flow |>
      Option.return

    | E_stub_primed e when is_deref_expr e ->
      eval_deref e true exp.erange man flow |>
      Option.return


    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
