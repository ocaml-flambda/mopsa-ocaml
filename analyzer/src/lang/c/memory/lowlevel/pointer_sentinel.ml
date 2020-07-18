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
open Sig.Domain.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
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

  let scalar = mk_semantic "C/Scalar" ~domain:name
  let numeric = mk_semantic "U/Numeric" ~domain:name
  
  let dependencies = [ scalar;
                       numeric ]

  let alarms = []

  (** {2 Auxiliary variables} *)
  (** *********************** *)

  (** Registration of a new var kinds for auxiliary variables *)
  type var_kind +=
    | V_c_sentinel_pos    of base
    | V_c_before_sentinel of base
    | V_c_sentinel        of base


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_sentinel_pos (base) ->
            Format.fprintf fmt "sentinel-pos(%a)" pp_base base

          | V_c_before_sentinel (base) ->
            Format.fprintf fmt "before-sentinel(%a)" pp_base base

          | V_c_sentinel (base) ->
            Format.fprintf fmt "sentinel(%a)" pp_base base

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_sentinel_pos(b1), V_c_sentinel_pos(b2)
          | V_c_before_sentinel(b1), V_c_before_sentinel(b2)
          | V_c_sentinel(b1), V_c_sentinel(b2) ->
            compare_base b1 b2

          | _ -> next v1 v2
        );
    }


  (** void* type *)
  let void_ptr = T_c_pointer T_c_void


  (** Size of a pointer cell *)
  let ptr_size = sizeof_type void_ptr


  (** Create the auxiliary variable sentinel(base) *)
  let mk_sentinel_pos_var base : var =
    let name = "sentinel-pos(" ^ (base_uniq_name base) ^ ")" in
    mkv name (V_c_sentinel_pos (base)) T_int ~mode:(base_mode base)


  let mk_sentinel_pos_var_expr base ?(mode=None) range : expr =
    mk_var (mk_sentinel_pos_var base) ~mode range


  (** Create the auxiliary variable before-sentinel(base) *)
  let mk_before_var base : var =
    let name = "before-sentinel(" ^ (base_uniq_name base) ^ ")" in
    mkv name (V_c_before_sentinel (base)) (T_c_pointer T_c_void) ~mode:WEAK


  let mk_before_var_expr base ?(mode=None) range : expr =
    mk_var (mk_before_var base) ~mode range


  (** Create the auxiliary variable at-sentinel(base) *)
  let mk_sentinel_var base : var =
    let name = "sentinel(" ^ (base_uniq_name base) ^ ")" in
    mkv name (V_c_sentinel (base)) (T_c_pointer T_c_void) ~mode:(base_mode base)


  let mk_sentinel_var_expr base ?(mode=None) range : expr =
    mk_var (mk_sentinel_var base) ~mode range



  (* Execute [exists] when the set of pointers before the sentinel is non-empty, [empty] otherwise *)
  let before_cases ~exists ~empty sentinel_pos range man flow =
    assume ~semantic:numeric
      (mk_binop sentinel_pos O_eq (mk_zero range) range)
      ~fthen:empty
      ~felse:exists
      man flow


  (* Execute [exists] when the sentinel cell exits, [empty] otherwise. *)
  let sentinel_cases ~exists ~empty sentinel_pos size range man flow =
    assume ~semantic:numeric
      (mk_binop sentinel_pos O_eq size range)
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
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    | P_null
    | P_invalid
    | P_block ({ base_valid = false }, _, _)
    | P_top ->
      Cases.empty_singleton flow

    | P_block (base, offset, mode) ->
      Cases.singleton (base, offset, mode) flow

    | P_fun _ -> assert false

  (** Predicate defining interesting bases for which the domain will
      track the sentinel position.
  *)
  let rec is_interesting_base base =
    match base with
    | { base_kind = Var {vkind = Cstubs.Aux_vars.V_c_primed_base base}; base_valid = true } -> is_interesting_base base

    (* | { base_kind = Var v; base_valid = true } when is_c_type v.vtyp && is_c_array_type v.vtyp ->
     *   (\* Accept only arrays with pointers or records of pointers *\)
     *   let rec aux t =
     *     match remove_typedef_qual t with
     *     | T_c_pointer _ -> true
     *     | T_c_array(tt,_) -> aux tt
     *     | T_c_record { c_record_fields } ->
     *       List.for_all (fun field -> aux field.c_field_type) c_record_fields
     *     | _ -> false
     *   in
     *   aux v.vtyp *)

    | { base_kind = Addr { addr_kind = A_stub_resource "argv" }; base_valid = true } -> true

    | _ -> false



  (** Partition [flow] depending whether e is a sentinel or not *)
  let is_sentinel_expr e man flow =
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
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_top -> Cases.join (Cases.singleton true flow) (Cases.singleton false flow)
        | P_null | P_invalid -> Cases.singleton true flow
        | P_block _ | P_fun _ -> Cases.singleton false flow


  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      let sentinel_pos = mk_sentinel_pos_var base in
      let sentinel = mk_sentinel_var base in
      (* Put the sentinel at postion 0 with value ⊤ *)
      man.post ~semantic:numeric (mk_add_var sentinel_pos range) flow >>$ fun _ flow ->
      man.post ~semantic:numeric (mk_assign (mk_var sentinel_pos range) (mk_zero range) range) flow >>$ fun _ flow ->
      man.post ~semantic:scalar (mk_add_var sentinel range) flow


  (** Remove the auxiliary variables of a base *)
  let remove_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      let sentinel_pos = mk_sentinel_pos_var base in
      let sentinel = mk_sentinel_var base in
      let before = mk_before_var base in
      man.post ~semantic:numeric (mk_remove_var sentinel_pos range) flow >>$ fun _ flow ->
      man.post ~semantic:scalar (mk_remove_var sentinel range) flow >>$ fun _ flow ->
      man.post ~semantic:scalar (mk_remove_var before range) flow


  (** Rename the auxiliary variables associated to a base *)
  let rename_base base1 base2 range man flow =
    if not (is_interesting_base base1) then Post.return flow else
    if not (is_interesting_base base2) then remove_base base1 range man flow
    else
      let sentinel_pos1 = mk_sentinel_pos_var base1 in
      let sentinel_pos2 = mk_sentinel_pos_var base2 in
      let before1 = mk_before_var base1 in
      let before2 = mk_before_var base2 in
      let sentinel1 = mk_sentinel_var base1 in
      let sentinel2 = mk_sentinel_var base2 in

      man.post ~semantic:numeric (mk_rename_var sentinel_pos1 sentinel_pos2 range) flow >>$ fun () flow ->
      before_cases (mk_var sentinel_pos2 range) range man flow
        ~exists:(fun flow ->
            man.post ~semantic:scalar (mk_rename_var before1 before2 range) flow
          )
        ~empty:(fun flow ->
            man.post ~semantic:scalar (mk_remove_var before1 range) flow
          )
      >>$ fun _ flow ->
      (* FIXME: check if at-sentinel exists *)
       man.post ~semantic:scalar (mk_rename_var sentinel1 sentinel2 range) flow


  (** Expand the auxiliary variables of a base *)
  let expand_base base1 bases range man flow =
    if not (is_interesting_base base1) then
      List.fold_left
        (fun acc b -> Post.bind (add_base b range man) acc)
        (Post.return flow) bases
    else
      let sentinel_pos1 = mk_sentinel_pos_var base1 in
      let before1 = mk_before_var base1 in
      let sentinel1 = mk_sentinel_var base1 in
      let sentinel_pos2,before2,sentinel2 = List.fold_left
          (fun (acc1,acc2,acc3) b ->
             if is_interesting_base b then
               let sentinel_pos2 = mk_sentinel_pos_var b in
               let before2 = mk_before_var b in
               let sentinel2 = mk_sentinel_var b in
               (sentinel_pos2::acc1),
               (before2::acc2),
               (sentinel2::acc3)
             else
               (acc1,acc2,acc3)
          ) ([],[],[]) bases in
      if sentinel_pos2 = [] then
        Post.return flow
      else
        man.post ~semantic:numeric (mk_expand_var sentinel_pos1 sentinel_pos2 range) flow >>$ fun _ flow ->
        before_cases (mk_var sentinel_pos1 range) range man flow
          ~exists:(fun flow ->
              man.post ~semantic:scalar (mk_expand_var before1 before2 range) flow
            )
          ~empty:(fun flow -> Post.return flow)
        >>$ fun _ flow ->
        (* FIXME: check if sentinel exists *)
        man.post ~semantic:scalar (mk_expand_var sentinel1 sentinel2 range) flow


  (** Fold the auxiliary variables of a set of bases *)
  let fold_bases base1 bases range man flow =
    if not (is_interesting_base base1) then Post.return flow
    else
      let sentinel_pos1 = mk_sentinel_pos_var base1 in
      let before1 = mk_before_var base1 in
      let sentinel1 = mk_sentinel_var base1 in
      let sentinel_pos2,before2,sentinel2 = List.fold_left
          (fun (acc1,acc2,acc3) b ->
             if is_interesting_base b then
               let sentinel_pos2 = mk_sentinel_pos_var b in
               let before2 = mk_before_var b in
               let sentinel2 = mk_sentinel_var b in
               (sentinel_pos2::acc1),
               (before2::acc2),
               (sentinel2::acc3)
             else
               (acc1,acc2,acc3)
          ) ([],[],[]) bases in
      if sentinel_pos2 = [] then
        assert false
      else
        man.post ~semantic:numeric (mk_fold_var sentinel_pos1 sentinel_pos2 range) flow >>$ fun _ flow ->
        before_cases (mk_var sentinel_pos1 range) range man flow
          ~exists:(fun flow ->
              man.post ~semantic:scalar (mk_fold_var before1 before2 range) flow
            )
          ~empty:(fun flow -> Post.return flow)
        >>$ fun _ flow ->
        (* FIXME: check if sentinel exists *)
        man.post ~semantic:scalar (mk_fold_var sentinel1 sentinel2 range) flow

  
  (** Forget the value of auxiliary variables of a base *)
  let forget e range man flow =
    let ptr = mk_c_address_of e range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
    | P_block(base,offset,mode) when is_interesting_base base ->
      let sentinel_pos = mk_sentinel_pos_var_expr base ~mode range in
      let sentinel = mk_sentinel_var_expr base ~mode range in
      man.post ~semantic:numeric (mk_assign sentinel_pos (mk_zero range) range) flow >>$ fun _ flow ->
      man.post ~semantic:scalar (mk_forget sentinel range) flow

    | _ -> Post.return flow


  let forget_quant quants e range man flow =
    let ptr = mk_c_address_of e range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
    | P_block(base,offset,mode) when is_interesting_base base ->
      let sentinel_pos = mk_sentinel_pos_var_expr base ~mode range in
      let sentinel = mk_sentinel_var_expr base ~mode range in
      man.post ~semantic:numeric (mk_assign sentinel_pos (mk_zero range) range) flow >>$ fun _ flow ->
      man.post ~semantic:scalar (mk_forget sentinel range) flow

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
    man.eval ~semantic:scalar size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size]. This test is
       optional as the domain does not raise out-of-bound alarms *)
    assume ~semantic:numeric
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Post.return flow

          else
            let sentinel_pos = mk_sentinel_pos_var_expr base ~mode range in
            let sentinel = mk_sentinel_var_expr base ~mode range in
            let before = mk_before_var_expr base ~mode range in
            let ptr = mk_z ptr_size range in

            switch ~semantic:numeric [
              (* Case 1: set after
                                                     offset
                 -----|------------------#-------------?------|--->
                      0               sentinel              size
                 offset condition: offset >= sentinel_pos + |ptr|
                 transformation: nop;
              *)
              [
                mk_binop offset O_ge (add sentinel_pos ptr range) range;
              ],
              (fun flow -> Post.return flow);


              (* Case 2: set before
                               offset
                 -----|-----------?------------#-------------|--->
                 0                         sentinel        size
                 offset condition: offset ∈ [0, sentinel_pos - |ptr|]
              *)
              [
                mk_binop offset O_le (sub sentinel_pos ptr range) range;
              ],
              (fun flow ->
                 (* Test if the rval is a sentinel *)
                 is_sentinel_expr rval man flow >>$ fun ok flow ->
                 if ok then
                   (* Case 2.1: set sentinel before
                                    offset
                      -----|-----------#------------#-------------|--->
                           0                      sentinel       size
                      rval condition: rval == SENTINEL
                      transformation: if offset = 0 then remove before;
                                      if sentinel_pos = size then add sentinel;
                                      sentinel = rval;
                                      sentinel_pos = offset;
                   *)
                   before_cases offset range man flow
                     ~exists:(fun flow -> Post.return flow)
                     ~empty:(fun flow -> man.post ~semantic:scalar (mk_remove before range) flow)
                   >>$ fun _ flow ->
                   sentinel_cases sentinel_pos size range man flow
                     ~exists:(fun flow -> Post.return flow)
                     ~empty:(fun flow -> man.post ~semantic:scalar (mk_add sentinel range) flow)
                   >>$ fun _ flow ->
                   man.post ~semantic:numeric (mk_assign sentinel_pos offset range) flow >>$ fun _ flow ->
                   man.post ~semantic:scalar (mk_assign sentinel rval range) flow
                 else
                   (* Case 2.2: set non-sentinel before
                                    offset
                      -----|-----------#------------#-------------|--->
                           0                      sentinel       size
                      rval condition: rval != SENTINEL
                      transformation: weak(before) = rval;
                   *)
                   man.post ~semantic:scalar (mk_assign (weaken_var_expr before) rval range) flow
              );

              (* Case 3: set at sentinel
                                             offset
                 -----|------------------------?#-------------|--->
                      0                      sentinel        size
                 offset condition: offset = sentinel_pos
              *)
              [
                mk_binop offset O_eq sentinel_pos range;
              ],
              (fun flow ->
                 (* Test if the rval is a sentinel *)
                 is_sentinel_expr rval man flow >>$ fun ok flow ->
                 if ok then
                   (* Case 3: set sentinel at sentinel
                                                  offset
                      -----|-------------------------#-------------|--->
                           0                      sentinel        size
                      offset condition: sentinel = rval
                   *)
                   man.post ~semantic:scalar (mk_assign sentinel rval range) flow
                 else
                   (* Case 2.2: set non-sentinel at sentinel
                                                  offset
                      -----|------------------------@-------------|--->
                           0                      sentinel       size
                      rval condition: rval != SENTINEL
                      transformation: if sentinel_pos = 0 then before = rval else weak(before) = rval;
                                      sentinel_pos = sentinel_pos + |ptr|;
                                      if sentinel_pos = size then remove sentinel else sentinel = ⊤;
                   *)
                   before_cases sentinel_pos range man flow
                     ~exists:(fun flow -> man.post ~semantic:scalar (mk_assign (weaken_var_expr before) rval range) flow)
                     ~empty:(fun flow -> man.post ~semantic:scalar (mk_assign (strongify_var_expr before) rval range) flow)
                   >>$ fun _ flow ->
                   man.post ~semantic:numeric (mk_assign sentinel_pos (add sentinel_pos ptr range) range) flow >>$ fun _ flow ->
                   sentinel_cases sentinel_pos size range man flow
                     ~exists:(fun flow -> man.post ~semantic:scalar (mk_assign sentinel (mk_top void_ptr range) range) flow)
                     ~empty:(fun flow -> man.post ~semantic:scalar (mk_remove sentinel range) flow)
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
    man.eval ~semantic:scalar offset flow >>$ fun offset flow ->
    man.eval rval flow >>$ fun rval flow ->
    assign_cases base offset mode rval range man flow


  (** Cases of the transfer function of quantified tests 𝕊⟦ ∀i ∈ [lo,hi]: *(base + i) op q ⟧ *)
  let assume_quantified_cases i lo hi op base offset mode q range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset [FORALL,i,S_interval(lo,hi)] in

    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~semantic:scalar size flow >>$ fun size flow ->
    man.eval ~semantic:scalar min flow >>$ fun min flow ->
    man.eval ~semantic:scalar max flow >>$ fun max flow ->

    let sentinel_pos = mk_sentinel_pos_var_expr base ~mode range in
    let sentinel = mk_sentinel_var_expr base ~mode range in
    let before = mk_before_var_expr base ~mode range in
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
                mk_binop (add sentinel_pos ptr range) O_le min range
              ],
              (fun flow ->
                 debug "case 1";
                 Post.return flow
              );

              [
                mk_binop min O_eq sentinel_pos range
              ],
              (fun flow ->
                 debug "case 2";
                 man.post ~semantic:scalar (mk_assume (mk_binop sentinel O_eq q range) range) flow
              );

              [
                mk_binop min O_le (sub sentinel_pos ptr range) range
              ],
              (fun flow ->
                 debug "case 3";
                 Flow.set T_cur man.lattice.bottom man.lattice flow |>
                 Post.return
              )
            ] ~semantic:numeric man flow

          (* q is not a sentinel *)
          else
            switch [
              [
                mk_binop (add sentinel_pos ptr range) O_le min range
              ],
              (fun flow ->
                 debug "case 4";
                 Post.return flow
              );

              [
                mk_binop min O_eq sentinel_pos range
              ],
              (fun flow ->
                 debug "case 5";
                 man.post ~semantic:numeric (mk_assign sentinel_pos (add max ptr range) range) flow >>$ fun _ flow ->
                 before_cases min range man flow
                   ~exists:(fun flow -> man.post ~semantic:scalar (mk_assign (weaken_var_expr before) q range) flow)
                   ~empty:(fun flow -> man.post ~semantic:scalar (mk_assign (strongify_var_expr before) q range) flow)
              );

              [
                mk_binop max O_le (sub sentinel_pos ptr range) range
              ],
              (fun flow ->
                 debug "case 6";
                 Post.return flow
              )

            ]
              ~semantic:numeric man flow
        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          Flow.set_bottom T_cur flow |>
          Post.return
        )
      ~semantic:numeric man flow


  (** Entry point of the transfer function of quantified tests 𝕊⟦ ∀i ∈ [lo,hi]: *(p + ∀i) op q ⟧ *)
  let assume_quantified i lo hi op p q range man flow =
    eval_pointed_base_offset (mk_c_address_of p range) range man flow >>$ fun (base,offset,mode) flow ->
    man.eval q flow >>$ fun q flow ->
    if is_interesting_base base then
      assume_quantified_cases i lo hi op base offset mode q range man flow
    else
      Post.return flow



  (** Transformers entry point *)
  let exec stmt man flow =
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

    | S_fold(e,el) when is_base_expr e && List.for_all is_base_expr el ->
      fold_bases (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) when is_c_deref e  ->
      forget e stmt.srange man flow |>
      OptionExt.return

    | S_forget({ ekind = E_stub_quantified_formula(quants, e) }) when is_c_deref e ->
      forget_quant quants e stmt.srange man flow |>
      OptionExt.return

    | S_remove(e) when is_base_expr e ->
      remove_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_assign({ ekind = E_c_deref p} as lval, rval) when is_c_pointer_type lval.etyp ->
      assign_deref p rval stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ ∀i ∈ [a,b] : *(p + i) == q ⟧ *)
    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], {ekind = E_binop(O_eq, lval, q)}) })
    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], {ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, lval, q)})}) })
      when is_c_pointer_type lval.etyp &&
           is_var_in_expr i lval &&
           not (is_var_in_expr i q) &&
           is_c_deref lval
      ->
      assume_quantified i a b O_eq lval q stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ ∀i ∈ [a,b] : *(p + i) != q ⟧ *)
    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], {ekind = E_binop(O_ne, lval, q)}) })
    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], {ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, lval, q)})}) })
      when is_c_pointer_type lval.etyp &&
           is_var_in_expr i lval &&
           not (is_var_in_expr i q) &&
           is_c_deref lval
      ->
      assume_quantified i a b O_ne lval q stmt.srange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Cases of the abstraction evaluations *)
  let eval_deref_cases base offset mode typ range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~semantic:scalar size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size] *)
    assume ~semantic:numeric
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Eval.singleton (mk_top typ range) flow

          else
            let sentinel_pos = mk_sentinel_pos_var_expr base ~mode range in
            let before = mk_before_var_expr base ~mode range in
            let sentinel = mk_sentinel_var_expr base ~mode range in
            let ptr = mk_z ptr_size range in
            let top = mk_top void_ptr range in


            switch ~semantic:scalar [
              (* Case 1: before sentinel
                 Offset condition: offset <= sentinel_pos - |ptr|
                 Transformation: weak(before)
              *)
              [
                mk_binop offset O_le (sub sentinel_pos ptr range) range;
              ],
              (fun flow ->
                 Eval.singleton (weaken_var_expr before) flow
              );

              (* Case 2: at sentinel
                 Offset condition: offset == sentinel_pos
                 Transformation: sentinel
              *)
              [
                mk_binop offset O_eq sentinel_pos range;
              ],
              (fun flow ->
                 Eval.singleton sentinel flow
              );

              (* Case 2: after sentinel
                 Offset condition: offset >= sentinel_pos + |ptr|
                 Transformation: sentinel
              *)
              [
                mk_binop offset O_ge (add sentinel_pos ptr range) range;
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
    if is_interesting_base base
    then
      man.eval ~semantic:scalar offset flow >>$ fun offset flow ->
      eval_deref_cases base offset mode (under_type p.etyp) range man flow
    else
      Eval.singleton (mk_top (under_type p.etyp |> void_to_char) range) flow



  (** Evaluations entry point *)
  let eval exp man flow =
    match ekind exp with
    | E_c_deref p
      when is_c_pointer_type exp.etyp &&
           under_type p.etyp |> void_to_char |> is_c_scalar_type
      ->
      eval_deref exp exp.erange man flow |>
      Rewrite.forward_eval ~semantic:scalar |>
      OptionExt.return

    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  register_stateless_domain (module Domain)
