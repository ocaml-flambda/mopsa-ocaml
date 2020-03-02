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

(** Abstraction of pointer arrays by segmentation using a NULL/INVALID sentinel.

    This abstract domain divides an array of pointers into three segments:

       +-------------------+-+-------------+
       |       before      |S|    after    |
       +-------------------+-+-------------+

    - The segment "before" represents pointers before the sentinel. All these
    pointers are different than NULL. The domains maintains a smash variable
    "before-sentinel" for approximating these values.

    - The array cell "S" represents a cell that *may be* NULL. The domain
    uses the numeric variable "sentinel" and the pointer variable "at-sentinel"
    to represent the position and the value of the sentinel respectively.

    - The segment "after" represents pointers after the sentinel. The domain
    keeps no information about it.
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
open Common.Alarms


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId(struct
      let name = "c.memory.lowlevel.pointer_sentinel"
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
        Z_c, Z_u_num;
        Z_c_low_level, Z_u_num;
        Z_c_scalar, Z_u_num;
        Z_c_low_level, Z_c_scalar;
        Z_c_low_level, Z_c_points_to;
        Z_c_scalar, Z_c_points_to;
      ];
    }
  }

  let alarms = []

  (** {2 Auxiliary variables} *)
  (** *********************** *)

  (** Registration of a new var kinds for auxiliary variables *)
  type var_kind +=
    | V_c_sentinel        of base
    | V_c_before_sentinel of base
    | V_c_at_sentinel     of base


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_sentinel (base) ->
            Format.fprintf fmt "sentinel(%a)" pp_base base

          | V_c_before_sentinel (base) ->
            Format.fprintf fmt "before-sentinel(%a)" pp_base base

          | V_c_at_sentinel (base) ->
            Format.fprintf fmt "at-sentinel(%a)" pp_base base

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_sentinel(b1), V_c_sentinel(b2)
          | V_c_before_sentinel(b1), V_c_before_sentinel(b2)
          | V_c_at_sentinel(b1), V_c_at_sentinel(b2) ->
            compare_base b1 b2

          | _ -> next v1 v2
        );
    }


  (** void* type *)
  let void_ptr = T_c_pointer T_c_void


  (** Size of a pointer cell *)
  let ptr_size = sizeof_type void_ptr


  (** Create the auxiliary sentinel(base) variable representing the
      position of the first NULL/INVALID pointer in the base.

      Note that the returned variable has a mathematical integer type,
      not a C int type.
  *)
  let mk_sentinel_var base ?(mode=None) range : expr =
    let name = "sentinel(" ^ (base_uniq_name base) ^ ")" in
    let v = mkv name (V_c_sentinel (base)) T_int ~mode:(base_mode base) in
    mk_var v ~mode range


  (** Create the auxiliary before-sentinel(base) variable representing
      valid pointers before sentinel(base).
  *)
  let mk_before_var base ?(mode=None) range : expr =
    let name = "before-sentinel(" ^ (base_uniq_name base) ^ ")" in
    let v = mkv name (V_c_before_sentinel (base)) (T_c_pointer T_c_void) ~mode:WEAK in
    mk_var v ~mode range


  (** Create the auxiliary at-sentinel(base) variable representing the
     pointer at the sentinel.
  *)
  let mk_at_var base ?(mode=None) range : expr =
    let name = "at-sentinel(" ^ (base_uniq_name base) ^ ")" in
    let v = mkv name (V_c_at_sentinel (base)) (T_c_pointer T_c_void) ~mode:(base_mode base) in
    mk_var v ~mode range



  (* Execute [exists] when the set of pointers before the sentinel is non-empty, [empty] otherwise *)
  let before_cases ~exists ~empty sentinel range man flow =
    assume ~zone:Z_u_num
      (mk_binop sentinel O_eq (mk_zero range) range)
      ~fthen:empty
      ~felse:exists
      man flow


  (* Execute [exists] when the sentinel cell exits, [empty] otherwise. *)
  let at_cases ~exists ~empty sentinel size range man flow =
    assume ~zone:Z_u_num
      (mk_binop sentinel O_eq size range)
      ~fthen:empty
      ~felse:exists
      man flow


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow



  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Get the base and offset pointed by ptr. Since we do not track invalid
     dereferences, we ignore invalid pointers.
  *)
  let eval_pointed_base_offset ptr range man flow =
    man.eval ptr ~zone:(Zone.Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to P_null
    | E_c_points_to P_invalid
    | E_c_points_to (P_block ({ base_valid = false }, _, _))
    | E_c_points_to P_top ->
      Cases.empty_singleton flow

    | E_c_points_to (P_block (base, offset, mode)) ->
      Cases.singleton (base, offset, mode) flow

    | _ -> assert false

  (** Predicate defining interesting bases for which the domain will
      track the sentinel position.
  *)
  let is_interesting_base base =
    match base with
    | { base_kind = Var v; base_valid = true } when is_c_type v.vtyp && is_c_array_type v.vtyp ->
      (* Accept only arrays with pointers or records of pointers *)
      let rec aux t =
        match remove_typedef_qual t with
        | T_c_pointer _ -> true
        | T_c_array(tt,_) -> aux tt
        | T_c_record { c_record_fields } ->
          List.for_all (fun field -> aux field.c_field_type) c_record_fields
        | _ -> false
      in
      aux v.vtyp

    | { base_kind = Addr { addr_kind = A_stub_resource "argv" }; base_valid = true } -> true

    | _ -> false



  (** Partition [flow] depending whether e is a sentinel or not *)
  let is_sentinel_expr ?(zone=Z_c_low_level) e man flow =
    (* Try static checks *)
    match ekind (remove_casts e) with
    | E_c_address_of _            -> Cases.singleton false flow
    | E_constant (C_c_string _)   -> Cases.singleton false flow
    | E_var (v,_)
      when is_c_array_type v.vtyp -> Cases.singleton false flow
    | E_constant C_c_invalid      -> Cases.singleton true flow
    | _ ->
      match c_expr_to_z (remove_casts e) with
      | Some e -> Cases.singleton (Z.equal e Z.zero) flow
      | None ->
        (* If the above heuristics fails, fall back to dynamic evaluations *)
        man.eval e ~zone:(zone,Z_c_points_to) flow >>$ fun pt flow ->
        match ekind pt with
        | E_c_points_to P_top -> Cases.join (Cases.singleton true flow) (Cases.singleton false flow)
        | E_c_points_to (P_null | P_invalid) -> Cases.singleton true flow
        | E_c_points_to (P_block _ | P_fun _) -> Cases.singleton false flow
        | _ -> assert false


  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      let sentinel = mk_sentinel_var base range in
      let at = mk_at_var base range in
      (* Put the sentinel at postion 0 with value ⊤ *)
      man.post ~zone:Z_u_num (mk_add sentinel range) flow >>$ fun _ flow ->
      man.post ~zone:Z_u_num (mk_assign sentinel (mk_zero range) range) flow >>$ fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_add at range) flow


  (** Remove the auxiliary variables of a base *)
  let remove_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      let sentinel = mk_sentinel_var base range in
      let at = mk_at_var base range in
      let before = mk_before_var base range in
      man.post ~zone:Z_u_num (mk_remove sentinel range) flow >>$ fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_remove at range) flow >>$ fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_remove before range) flow


  (** Rename the auxiliary variables associated to a base *)
  let rename_base base1 base2 range man flow =
    if not (is_interesting_base base1) then Post.return flow else
    if not (is_interesting_base base2) then remove_base base1 range man flow
    else
      let sentinel1 = mk_sentinel_var base1 range in
      let sentinel2 = mk_sentinel_var base2 range in
      man.post ~zone:Z_u_num (mk_rename sentinel1 sentinel2 range) flow >>$ fun _ flow ->

      before_cases sentinel2 range man flow
        ~exists:(fun flow ->
            let before1 = mk_before_var base1 range in
            let before2 = mk_before_var base2 range in
            man.post ~zone:Z_c_scalar (mk_rename before1 before2 range) flow
          )
        ~empty:(fun flow -> Post.return flow)
      >>$ fun _ flow ->

      (* FIXME: check if at-sentinel exists *)
      let at1 = mk_at_var base1 range in
      let at2 = mk_at_var base2 range in
      man.post ~zone:Z_c_scalar (mk_rename at1 at2 range) flow


  (** Expand the auxiliary variables of a base *)
  let expand_base base1 bases range man flow =
    if not (is_interesting_base base1) then Post.return flow else
    if List.exists (fun b -> not (is_interesting_base b)) bases then panic_at range "expand %a not supported" pp_base base1
    else
      let mk_aux_list f = List.map (fun b -> f b range) bases in
      let sentinel1 = mk_sentinel_var base1 range in
      let sentinel2 = mk_aux_list (mk_sentinel_var ~mode:None) in
      man.post ~zone:Z_u_num (mk_expand sentinel1 sentinel2 range) flow >>$ fun _ flow ->

      before_cases sentinel1 range man flow
        ~exists:(fun flow ->
            let before1 = mk_before_var base1 range in
            let before2 = mk_aux_list (mk_before_var ~mode:None) in
            man.post ~zone:Z_c_scalar (mk_expand before1 before2 range) flow
          )
        ~empty:(fun flow -> Post.return flow)
      >>$ fun _ flow ->

      (* FIXME: check if at-sentinel exists *)
      let at1 = mk_at_var base1 range in
      let at2 = mk_aux_list (mk_at_var ~mode:None) in
      man.post ~zone:Z_c_scalar (mk_expand at1 at2 range) flow


  (** Forget the value of auxiliary variables of a base *)
  let forget e range man flow =
    (* Get the pointed base *)
    let ptr = match ekind e with
      | E_var _   -> mk_c_address_of e range
      | E_c_deref(p) -> p
      | _ -> assert false
    in
    man.eval ptr ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun p flow ->
    match ekind p with
    | E_c_points_to(P_block(base,offset,mode)) when is_interesting_base base ->
      let sentinel = mk_sentinel_var base range in
      let at = mk_at_var base range in
      man.post ~zone:Z_u_num (mk_assign sentinel (mk_zero range) range) flow >>$ fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_forget at range) flow


    | _ -> Post.return flow




  (** Declaration of a C variable *)
  let declare_variable v scope range man flow =
    let base = mk_var_base v in
    if not (is_interesting_base base)
    then Post.return flow
    else add_base base range man flow



  (** Cases of the abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_cases base offset mode rval range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar,Z_u_num) size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size]. This test is
       optional as the domain does not raise out-of-bound alarms *)
    assume ~zone:Z_u_num
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Post.return flow

          else
            let sentinel = mk_sentinel_var base ~mode range in
            let at = mk_at_var base ~mode range in
            let before = mk_before_var base ~mode range in
            let ptr = mk_z ptr_size range in

            switch ~zone:Z_u_num [
              (* Case 1: set after
                                                     offset
                 -----|------------------#-------------?------|--->
                      0               sentinel              size
                 offset condition: offset >= sentinel + |ptr|
                 transformation: nop;
              *)
              [
                mk_binop offset O_ge (add sentinel ptr range) range;
              ],
              (fun flow -> Post.return flow);


              (* Case 2: set before
                               offset
                 -----|-----------?------------#-------------|--->
                 0                         sentinel        size
                 offset condition: offset ∈ [0, sentinel - |ptr|]
              *)
              [
                mk_binop offset O_le (sub sentinel ptr range) range;
              ],
              (fun flow ->
                 (* Test if the rval is a sentinel *)
                 is_sentinel_expr ~zone:Z_c_scalar rval man flow >>$ fun ok flow ->
                 if ok then
                   (* Case 2.1: set sentinel before
                                    offset
                      -----|-----------#------------#-------------|--->
                           0                      sentinel       size
                      rval condition: rval == SENTINEL
                      transformation: if offset = 0 then remove before;
                                      if sentinel = size then add at;
                                      at = rval;
                                      sentinel = offset;
                   *)
                   before_cases offset range man flow
                     ~exists:(fun flow -> Post.return flow)
                     ~empty:(fun flow -> man.post ~zone:Z_c_scalar (mk_remove before range) flow)
                   >>$ fun _ flow ->
                   at_cases sentinel size range man flow
                     ~exists:(fun flow -> Post.return flow)
                     ~empty:(fun flow -> man.post ~zone:Z_c_scalar (mk_add at range) flow)
                   >>$ fun _ flow ->
                   man.post ~zone:Z_u_num (mk_assign sentinel offset range) flow >>$ fun _ flow ->
                   man.post ~zone:Z_c_scalar (mk_assign at rval range) flow
                 else
                   (* Case 2.2: set non-sentinel before
                                    offset
                      -----|-----------#------------#-------------|--->
                           0                      sentinel       size
                      rval condition: rval != SENTINEL
                      transformation: weak(before) = rval;
                   *)
                   man.post ~zone:Z_c_scalar (mk_assign (weaken_var_expr before) rval range) flow
              );

              (* Case 3: set at sentinel
                                             offset
                 -----|------------------------?#-------------|--->
                      0                      sentinel        size
                 offset condition: offset = sentinel
              *)
              [
                mk_binop offset O_eq sentinel range;
              ],
              (fun flow ->
                 (* Test if the rval is a sentinel *)
                 is_sentinel_expr ~zone:Z_c_scalar rval man flow >>$ fun ok flow ->
                 if ok then
                   (* Case 3: set sentinel at sentinel
                                                  offset
                      -----|-------------------------#-------------|--->
                           0                      sentinel        size
                      offset condition: at = rval
                   *)
                   man.post ~zone:Z_c_scalar (mk_assign at rval range) flow
                 else
                   (* Case 2.2: set non-sentinel at sentinel
                                                  offset
                      -----|------------------------@-------------|--->
                           0                      sentinel       size
                      rval condition: rval != SENTINEL
                      transformation: if sentinel = 0 then before = rval else weak(before) = rval;
                                      sentinel = sentinel + |ptr|;
                                      if sentinel = size then remove at else at = ⊤;
                   *)
                   before_cases sentinel range man flow
                     ~exists:(fun flow -> man.post ~zone:Z_c_scalar (mk_assign (weaken_var_expr before) rval range) flow)
                     ~empty:(fun flow -> man.post ~zone:Z_c_scalar (mk_assign before rval range) flow)
                   >>$ fun _ flow ->
                   man.post ~zone:Z_u_num (mk_assign sentinel (add sentinel ptr range) range) flow >>$ fun _ flow ->
                   at_cases sentinel size range man flow
                     ~exists:(fun flow -> man.post ~zone:Z_c_scalar (mk_assign at (mk_top void_ptr range) range) flow)
                     ~empty:(fun flow -> man.post ~zone:Z_c_scalar (mk_remove at range) flow)
              );


            ] man flow

        )
      ~felse:(fun flow ->
          (* Unsafe case. No alarm is raised as this should be done by other domains *)
          Flow.set_bottom T_cur flow |>
          Post.return
        ) man flow


  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    eval_pointed_base_offset p range man flow >>$ fun (base,offset,mode) flow ->
    man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow >>$ fun offset flow ->
    man.eval ~zone:(Z_c_low_level,Z_c_scalar) rval flow >>$ fun rval flow ->
    assign_cases base offset mode rval range man flow


  (** Cases of the transfer function of quantified tests 𝕊⟦ *(base + ∀offset) op q ⟧ *)
  let assume_quantified_cases op base offset mode q range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset in

    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow >>$ fun size flow ->
    man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
    man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

    let sentinel = mk_sentinel_var base ~mode range in
    let at = mk_at_var base ~mode range in
    let before = mk_before_var base ~mode range in
    let ptr = mk_z ptr_size range in

    debug "min = %a, max = %a" pp_expr min pp_expr max;
    debug "cur = %a" man.lattice.print (Flow.get T_cur man.lattice flow);

    (* Safety condition: [min, max] ⊆ [0, size - ptr [ *)
    assume
      (
        mk_binop
          (mk_in min (mk_zero range) (sub size ptr range) range)
          O_log_and
          (mk_in max (mk_zero range) (sub size ptr range) range)
          range
      )
      ~fthen:(fun flow ->
          is_sentinel_expr q man flow >>$ fun ok flow ->

          (* q is a sentinel *)
          if ok then
            switch [
              [
                mk_binop (add sentinel ptr range) O_le min range
              ],
              (fun flow ->
                 debug "case 1";
                 Post.return flow
              );

              [
                mk_binop min O_eq sentinel range
              ],
              (fun flow ->
                 debug "case 2";
                 man.post ~zone:Z_c_scalar (mk_assume (mk_binop at O_eq q range) range) flow
              );

              [
                mk_binop min O_le (sub sentinel ptr range) range
              ],
              (fun flow ->
                 debug "case 3";
                 Flow.set T_cur man.lattice.bottom man.lattice flow |>
                 Post.return
              )
            ] ~zone:Z_u_num man flow

          (* q is not a sentinel *)
          else
            switch [
              [
                mk_binop (add sentinel ptr range) O_le min range
              ],
              (fun flow ->
                 debug "case 4";
                 Post.return flow
              );

              [
                mk_binop min O_eq sentinel range
              ],
              (fun flow ->
                 debug "case 5";
                 man.post ~zone:Z_u_num (mk_assign sentinel (add max ptr range) range) flow >>$ fun _ flow ->
                 before_cases min range man flow
                   ~exists:(fun flow -> man.post ~zone:Z_c_scalar (mk_assign (weaken_var_expr before) q range) flow)
                   ~empty:(fun flow -> man.post ~zone:Z_c_scalar (mk_assign before q range) flow)
              );

              [
                mk_binop max O_le (sub sentinel ptr range) range
              ],
              (fun flow ->
                 debug "case 6";
                 Post.return flow
              )

            ]
              ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          Flow.set_bottom T_cur flow |>
          Post.return
        )
      ~zone:Z_u_num man flow




  (** Entry point of the transfer function of quantified tests 𝕊⟦ *(p + ∀i) op q ⟧ *)
  let assume_quantified op p q range man flow =
    let pp =
      let rec doit e =
        match ekind e with
        | E_c_deref pp -> pp
        | E_c_cast(ee, _) -> doit ee
        | _ -> panic_at range "assume_quantified_zero: invalid argument %a" pp_expr p;
      in
      doit p
    in

    eval_pointed_base_offset pp range man flow >>$ fun (base,offset,mode) flow ->
    if is_interesting_base base then
      assume_quantified_cases op base offset mode q range man flow
    else
      Post.return flow



  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,None,scope) when is_interesting_base (mk_var_base v) ->
      declare_variable v scope stmt.srange man flow |>
      OptionExt.return

    | S_add (e) when is_base_expr e->
      add_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_rename (e1,e2) when is_base_expr e1 && is_base_expr e2 ->
      rename_base (expr_to_base e1) (expr_to_base e2) stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_base_expr e && List.for_all is_base_expr el ->
      expand_base (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) ->
      forget e stmt.srange man flow |>
      OptionExt.return

    | S_remove(e) when is_base_expr e ->
      remove_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_assign({ ekind = E_c_deref p} as lval, rval) when is_c_pointer_type lval.etyp ->
      assign_deref p rval stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ *(p + ∀i) = q ⟧ *)
    | S_assume({ekind = E_binop(O_eq, lval, q)})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, lval, q)})})
      when is_c_pointer_type lval.etyp &&
           is_lval_offset_forall_quantified lval &&
           not (is_expr_forall_quantified q) &&
           is_c_deref lval
      ->
      assume_quantified O_eq lval q stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ *(p + ∀i) != q ⟧ *)
    | S_assume({ekind = E_binop(O_ne, lval, q)})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, lval, q)})})
      when is_c_pointer_type lval.etyp &&
           is_lval_offset_forall_quantified lval &&
           not (is_lval_offset_forall_quantified q) &&
           is_c_deref lval
      ->
      assume_quantified O_ne lval q stmt.srange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Cases of the abstraction evaluations *)
  let eval_deref_cases base offset mode typ range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar,Z_u_num) size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size] *)
    assume ~zone:Z_u_num
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Eval.singleton (mk_top typ range) flow

          else
            let sentinel = mk_sentinel_var base ~mode range in
            let before = mk_before_var base ~mode range in
            let at = mk_at_var base ~mode range in
            let ptr = mk_z ptr_size range in
            let top = mk_top void_ptr range in


            switch ~zone:Z_c_scalar [
              (* Case 1: before sentinel
                 Offset condition: offset <= sentinel - |ptr|
                 Transformation: weak(before)
              *)
              [
                mk_binop offset O_le (sub sentinel ptr range) range;
              ],
              (fun flow ->
                 Eval.singleton (weaken_var_expr before) flow
              );

              (* Case 2: at sentinel
                 Offset condition: offset == sentinel
                 Transformation: at
              *)
              [
                mk_binop offset O_eq sentinel range;
              ],
              (fun flow ->
                 Eval.singleton at flow
              );

              (* Case 2: after sentinel
                 Offset condition: offset >= sentinel + |ptr|
                 Transformation: at
              *)
              [
                mk_binop offset O_ge (add sentinel ptr range) range;
              ],
              (fun flow ->
                 Eval.singleton top flow
              );

            ] man flow

        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          Flow.set_bottom T_cur flow |>
          Eval.empty_singleton
        ) man flow



  (** Cases of the abstraction evaluations of *(p + ∀i) *)
  let eval_quantified_deref_cases base offset mode typ  range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar,Z_u_num) size flow  >>$ fun size flow ->

    let min, max = Common.Quantified_offset.bound offset in

    man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
    man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

    let ptr = mk_z ptr_size range in

    (* Safety condition: [min, max] ⊆ [0, size - ptr [ *)
    assume
      (
        mk_binop
          (mk_in min (mk_zero range) (sub size ptr range) range)
          O_log_and
          (mk_in max (mk_zero range) (sub size ptr range) range)
          range
      )
      ~fthen:(fun flow ->
          let sentinel = mk_sentinel_var base ~mode range in
          let before = mk_before_var base ~mode range in
          let top = mk_top void_ptr range in

          switch ~zone:Z_c_scalar [
              (* Case 1: before sentinel
                 Offset condition: max <= sentinel - |ptr|
                 Transformation: weak(before)
              *)
              [
                mk_binop max O_le (sub sentinel ptr range) range;
              ],
              (fun flow ->
                 Eval.singleton (weaken_var_expr before) flow
              );

              (* Case 2: after sentinel
                 Offset condition: max >= sentinel
                 Transformation: ⊤
              *)
              [
                mk_binop max O_ge sentinel range;
              ],
              (fun flow ->
                 Eval.singleton top flow
              );

            ] man flow

        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          Flow.set_bottom T_cur flow |>
          Eval.empty_singleton
        ) man flow



  (** Abstract evaluation of a dereference *)
  let eval_deref exp range man flow =
    let p = match ekind exp with E_c_deref p -> p | _ -> assert false in
    eval_pointed_base_offset p range man flow >>$ fun (base,offset,mode) flow ->
    if is_interesting_base base &&
       not (is_expr_forall_quantified offset)
    then
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->
      eval_deref_cases base offset mode (under_type p.etyp) range man flow
    else if is_interesting_base base &&
            is_expr_forall_quantified offset
    then
      eval_quantified_deref_cases base offset mode (under_type p.etyp) range man flow
    else
      Eval.singleton (mk_top (under_type p.etyp |> void_to_char) range) flow



  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p
      when is_c_pointer_type exp.etyp &&
           under_type p.etyp |> void_to_char |> is_c_scalar_type
      ->
      eval_deref exp exp.erange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
