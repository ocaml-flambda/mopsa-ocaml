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

(** Abstraction of pointer arrays with a NULL/INVALID sentinel.

    This abstract domain keeps track of the position of the first
    NULL/INVALID pointer in arrays or dynamically allocated
    blocks. Four auxiliary variables are used:

    a. The variable sentinel(b) represents the position in bytes of
    the first sentinel in the block b.

    b. The variable before-sentinel(b) is a smash of all valid
    pointers before sentinel(b).

    c. The variable after-sentinel(b) is a smash of all pointers after
    sentinel(b), including the sentinel.
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
      ];
    }
  }


  (** {2 Auxiliary variables} *)
  (** *********************** *)

  (** Registration of a new var kinds for auxiliary variables *)
  type var_kind +=
    | V_c_sentinel        of base * bool
    | V_c_before_sentinel of base * bool
    | V_c_after_sentinel  of base * bool


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_sentinel (base,primed) ->
            Format.fprintf fmt "sentinel(%a)%s" pp_base base (if primed then "'" else "")

          | V_c_before_sentinel (base,primed) ->
            Format.fprintf fmt "before-sentinel(%a)%s" pp_base base (if primed then "'" else "")

          | V_c_after_sentinel (base,primed) ->
            Format.fprintf fmt "after-sentinel(%a)%s" pp_base base (if primed then "'" else "")

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_sentinel(b1,p1), V_c_sentinel(b2,p2)
          | V_c_before_sentinel(b1,p1), V_c_before_sentinel(b2,p2)
          | V_c_after_sentinel(b1,p1), V_c_after_sentinel(b2,p2) ->
            Compare.compose [
              (fun () -> compare_base b1 b2);
              (fun () -> compare p1 p2);
            ]

          | _ -> next v1 v2
        );
    }

  (** Create the auxiliary sentinel(base) variable representing the
      position of the first NULL/INVALID pointer in the base.

      Note that the returned variable has a mathematical integer type,
      not a C int type.
  *)
  let mk_sentinel_var base ?(primed=false) ?(mode=base_mode base) range : expr =
    let name = "sentinel(" ^ (base_uniq_name base) ^ ")" ^ (if primed then "'" else "") in
    let v = mkv name (V_c_sentinel (base,primed)) T_int in
    mk_var v ~mode range


  (** Create the auxiliary before-sentinel(base) variable representing
      valid pointers before sentinel(base).
  *)
  let mk_before_var base ?(primed=false) ?(mode=WEAK) range : expr =
    let name = "before-sentinel(" ^ (base_uniq_name base) ^ ")" ^ (if primed then "'" else "") in
    let v = mkv name (V_c_before_sentinel (base,primed)) (T_c_pointer T_c_void) in
    mk_var v ~mode range


  (** Create the auxiliary after-sentinel(base) variable representing
      valid pointers after sentinel(base), including the sentinel.
  *)
  let mk_after_var base ?(primed=false) ?(mode=WEAK) range : expr =
    let name = "after-sentinel(" ^ (base_uniq_name base) ^ ")" ^ (if primed then "'" else "") in
    let v = mkv name (V_c_after_sentinel (base,primed)) (T_c_pointer T_c_void) in
    mk_var v ~mode range


  (** Create a weak copy of a smash variable *)
  let weaken v =
    match ekind v with
    | E_var (vv,WEAK) -> v
    | E_var (vv, STRONG) -> { v with ekind = E_var (vv,WEAK) }
    | _ -> assert false

  (** Create a strong copy of a smash variable *)
  let strongify v =
    match ekind v with
    | E_var (vv,STRONG) -> v
    | E_var (vv, WEAK) -> { v with ekind = E_var (vv,STRONG) }
    | _ -> assert false



  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow



  (** {2 Abstract transformers} *)
  (** ************************* *)


  (** Predicate defining interesting bases for which the domain will
      track the sentinel position.
  *)
  let is_interesting_base base =
    match base with
    | V v -> is_c_type v.vtyp && not (is_c_scalar_type v.vtyp)

    | A { addr_kind = A_stub_resource "Memory" } -> true

    | _ -> false


  (** void* type *)
  let void_ptr = T_c_pointer T_c_void


  (** Size of a pointer cell *)
  let ptr_size = sizeof_type void_ptr


  (** Partition [flow] depending whether e is a sentinel or not *)
  let is_sentinel_expr e man flow =
    (* Try static checks *)
    match ekind (remove_casts e) with
    | E_c_address_of _            -> Result.singleton false flow
    | E_constant (C_c_string _)   -> Result.singleton false flow
    | E_var (v,_)
      when is_c_array_type v.vtyp -> Result.singleton false flow
    | E_constant C_c_invalid      -> Result.singleton true flow
    | _ ->
      match c_expr_to_z (remove_casts e) with
      | Some e -> Result.singleton (Z.equal e Z.zero) flow
      | None ->
        (* If the above heuristics fails, fall back to dynamic evaluations *)
        man.eval e ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
        match ekind pt with
        | E_c_points_to (P_null | P_invalid) -> Result.singleton true flow
        | E_c_points_to (P_block _ | P_valid | P_fun _) -> Result.singleton false flow
        | _ -> assert false


  (** Declaration of a C variable *)
  let declare_variable v init scope range man flow =
    if not (is_interesting_base (V v))
    then Post.return flow

    else
      (* Since we are in the Z_low_level zone, we assume that init has
         been translated by a structured domain into a flatten
         initialization *)
      let flat_init = match init with
        | Some (C_init_flat l) -> l
        | _ -> assert false
      in

      let size = sizeof_type v.vtyp in

      (* Exception raised when a non-pointer initializer is found *)
      let exception NonPointerFound in

      (* Check if an initializer has a pointer type *)
      let is_non_pointer = function
        | C_flat_none (_,t)
        | C_flat_expr(_,t)
        | C_flat_fill(_,t,_) ->
          not (is_c_pointer_type t)
      in

      let is_global =
        match scope with
        | Variable_func_static _
        | Variable_local _
        | Variable_parameter _ -> false
        | _ -> true
      in

      (* Find the position of the sentinel and accumulate pointers before and after it *)
      let rec aux o found init flow : ('a,Z.t*Z.t*expr list*expr list) result =
        match init with
        | [] -> Result.singleton (size, size, [], []) flow

        | hd :: _ when is_non_pointer hd -> raise NonPointerFound

        | C_flat_none (n,_) :: tl  ->
          let step = Z.mul n ptr_size in
          let sentinel = if is_global then mk_c_null range else mk_c_invalid_pointer range in
          aux (Z.add o step) true tl flow >>$ fun (o1,o2,before,after) flow ->
          Result.singleton (o,o,before,sentinel::after) flow

        | (C_flat_fill (e,_,_) as hd):: tl
        | (C_flat_expr (e,_) as hd):: tl ->
          let step =
            match hd with
            | C_flat_expr _ -> ptr_size
            | C_flat_fill (_,_,n) -> Z.mul n ptr_size
            | _ -> assert false
          in
          is_sentinel_expr e man flow >>$ fun b flow ->
          if b then
              aux (Z.add o step) true tl flow >>$ fun (o1,o2,before,after) flow ->
              Result.singleton (o,o,before,e::after) flow
            else
              aux (Z.add o step) found tl flow >>$ fun (o1,o2,before,after) flow ->
              let before = if found then before else e :: before in
              let after = if found then e :: after else after in
              Result.singleton (o1,o2,before,after) flow
      in
      (* Initialize the sentinel variable *)
      let sentinel = mk_sentinel_var (V v) range in
      man.post ~zone:Z_u_num (mk_add sentinel range) flow >>= fun _ flow ->
      try
        aux Z.zero false flat_init flow >>$ fun (o1,o2,before,after) flow ->
        let pos =
          if Z.equal o1 o2
          then mk_z o1 range
          else mk_z_interval o1 o2 range
        in
        man.post ~zone:Z_u_num (mk_assign sentinel pos range) flow >>= fun _ flow ->

        (* Function to initialize an auxiliary variable with a list of expressions *)
        let init_aux_var v l flow =
          match l with
          | [] -> Post.return flow
          | hd :: tl ->
            man.post ~zone:Z_c_scalar (mk_add v range) flow >>= fun _ flow ->
            man.post ~zone:Z_c_scalar (mk_assign (strongify v) hd range) flow >>= fun _ flow ->
            List.fold_left (fun acc ptr ->
                acc >>= fun _ flow ->
                man.post ~zone:Z_c_scalar (mk_assign v ptr range) flow
              ) (Post.return flow) tl
        in
        init_aux_var (mk_before_var (V v) range) before flow >>= fun _ flow ->
        init_aux_var (mk_after_var (V v) range) after flow
      with NonPointerFound ->
        (* Non-pointer value found in the initializers *)
        man.post ~zone:Z_u_num (mk_assign sentinel (mk_z_interval Z.zero size range) range) flow >>= fun _ flow ->
        man.post ~zone:Z_c_scalar (mk_add (mk_before_var (V v) range) range) flow >>= fun _ flow ->
        man.post ~zone:Z_c_scalar (mk_add (mk_after_var (V v) range) range) flow



  (** Add a pointer to the smash after-sentinel *)
  let add_after base sentinel size range man flow =
    assume ~zone:Z_u_num
      (mk_binop sentinel O_eq size range)
      ~fthen:(fun flow ->
          let after = mk_after_var base range |>
                      strongify
          in
          man.post ~zone:Z_c_scalar (mk_add after range) flow >>= fun _ flow ->
          Result.singleton STRONG flow
        )
      ~felse:(fun flow ->
          Result.singleton WEAK flow
        ) man flow

  (** Add a pointer to the smash before-sentinel *)
  let add_before base sentinel range man flow =
    assume ~zone:Z_u_num
      (mk_binop sentinel O_eq (mk_zero range) range)
      ~fthen:(fun flow ->
          let before = mk_before_var base range |>
                      strongify
          in
          man.post ~zone:Z_c_scalar (mk_add before range) flow >>= fun _ flow ->
          Result.singleton STRONG flow
        )
      ~felse:(fun flow ->
          Result.singleton WEAK flow
        ) man flow


  (** Assign to [v] only valid addresses pointed by [w] *)
  let assign_valid v w range man flow =
    man.post ~zone:Z_c_scalar (mk_assign v w range) flow >>= fun _ flow' ->
    man.post ~zone:Z_c_scalar (mk_assume (mk_binop (strongify v) O_ne (mk_c_null range) range) range) flow' >>= fun _ flow' ->
    man.post ~zone:Z_c_scalar (mk_assume (mk_binop (strongify v) O_ne (mk_c_invalid_pointer range) range) range) flow' >>* fun _ flow' log cleaners ->
    if Flow.get T_cur man.lattice flow' |> man.lattice.is_bottom then
      Post.return flow
    else
      Result.return (Some ()) flow' ~log ~cleaners

  (** Cases of the abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_cases base offset rval range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar,Z_u_num) size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size] *)
    assume ~zone:Z_u_num
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Post.return flow

          else
            let sentinel = mk_sentinel_var base range in
            let zero = mk_zero range in
            let ptr_size = mk_z ptr_size range in

            (* Fact that is always true: sentinel(a) ∈ [0, size]. 
               Useful when a coarse widening looses precision
               on the sentinel variable. 
            *)
            man.post ~zone:Z_u_num (mk_assume (mk_in sentinel zero size range) range) flow >>= fun _ flow ->


            switch ~zone:Z_u_num [
              (* Case 1: set after
                                                     offset
                 -----|------------------#-------------?------|--->
                      0               sentinel              size
                 Offset condition: offset => sentinel + ptr_size
                 Transformation: after = after U { rval };
              *)
              [
                mk_binop offset O_ge (add sentinel ptr_size range) range;
              ],
              (fun flow ->
                 add_after base sentinel size range man flow >>$ fun mode flow ->
                 let after = mk_after_var base range ~mode in
                 man.post ~zone:Z_c_scalar (mk_assign after rval range) flow
              );

              (* Case 2: set before
                               offset
                 -----|-----------?------------#-------------|--->
                 0                         sentinel        size
                 Offset condition: offset ∈ [0, sentinel]
              *)
              [
                mk_binop offset O_le sentinel range;
              ],
              (fun flow ->
                 is_sentinel_expr rval man flow >>$ fun b flow ->
                 (* Case 2.1: rval is a sentinel *)
                 if b then
                   switch ~zone:Z_u_num [
                     (* Case 2.1.1: set sentinel at sentinel
                                              offset
                          -----|-----------------#-----------------|--->
                               0              sentinel            size
                          Offset condition: offset = sentinel <= size - 2*ptr_size
                          Transformation: after = after U { rval }
                     *)
                     [
                       mk_binop offset O_eq sentinel range;
                       mk_binop sentinel O_le (sub size (mul (mk_int 2 range) ptr_size range) range) range;
                     ],
                     (fun flow ->
                        add_after base sentinel size range man flow >>$ fun mode flow ->
                        let after = mk_after_var base range ~mode in
                        man.post ~zone:Z_c_scalar (mk_assign after rval range) flow
                     );

                     (* Case 2.1.2: set sentinel at sentinel tl
                                                          offset size
                        -----|---------------------------------#|--->
                        0                                  sentinel
                        Offset condition: offset = sentinel = size - ptr_size
                        Transformation: after = rval
                     *)
                     [
                       mk_binop offset O_eq sentinel range;
                       mk_binop sentinel O_eq (sub size ptr_size range) range;
                     ],
                     (fun flow ->
                        let after = mk_after_var base range ~mode:STRONG in
                        man.post ~zone:Z_c_scalar (mk_assign after rval range) flow
                     );

                     (* Case 2.1.3: set sentinel at hd
                           offset
                        -----#-----------------#-----------------|--->
                             0              sentinel            size
                        Offset condition: 0 = offset <= sentinel - ptr_size
                        Transformation: sentinel = 0
                        after = after U { rval } U before
                        remove before
                     *)
                     [
                       mk_binop offset O_eq zero range;
                       mk_binop offset O_le (sub sentinel ptr_size range) range;                       
                     ],
                     (fun flow ->
                        add_after base sentinel size range man flow >>$ fun mode flow ->
                        let before = mk_before_var base range in
                        let after = mk_after_var base range ~mode in
                        man.post ~zone:Z_c_scalar (mk_assign after rval range) flow >>= fun _ flow ->
                        man.post ~zone:Z_c_scalar (mk_assign (weaken after) before range) flow >>= fun _ flow ->
                        man.post ~zone:Z_c_scalar (mk_remove before range) flow >>= fun _ flow ->
                        man.post ~zone:Z_u_num (mk_assign sentinel zero range) flow
                     );



                     (* Case 2.1.4: set sentinel before sentinel
                                    offset
                        -----|--------#--------#-----------------|--->
                             0              sentinel            size
                        Offset condition: ptr_size <= offset <= sentinel - ptr_size
                        Transformation: sentinel = offset
                                        after = after U { rval } U before
                     *)
                     [
                       mk_binop offset O_ge ptr_size range;
                       mk_binop offset O_le (sub sentinel ptr_size range) range;
                     ],
                     (fun flow ->
                        add_after base sentinel size range man flow >>$ fun mode flow ->
                        let before = mk_before_var base range in
                        let after = mk_after_var base range ~mode in
                        man.post ~zone:Z_c_scalar (mk_assign after rval range) flow >>= fun _ flow ->
                        man.post ~zone:Z_c_scalar (mk_assign (weaken after) before range) flow >>= fun _ flow ->
                        man.post ~zone:Z_u_num (mk_assign sentinel offset range) flow
                     );


                   ] man flow
                 else
                   (* Case 2.2: rval is not a sentinel *)
                   switch ~zone:Z_u_num [
                     (* Case 2.2.1: set non-sentinel at sentinel
                                              offset
                          -----|-----------------@-----------------|--->
                               0              sentinel            size
                          Offset condition: offset = sentinel
                          Transformation: sentinel = [offset + ptr_size, size]
                                          before = before U { rval } U after
                     *)
                     [
                       mk_binop offset O_eq sentinel range;
                     ],
                     (fun flow ->
                        add_before base sentinel range man flow >>$ fun mode flow ->
                        let before = mk_before_var base ~mode range in
                        let after = mk_after_var base range in
                        man.post ~zone:Z_c_scalar (mk_assign before rval range) flow >>= fun _ flow ->
                        assign_valid (weaken before) after range man flow >>= fun _ flow ->
                        man.post ~zone:Z_u_num (mk_forget sentinel range) flow >>= fun _ flow ->                        
                        man.post ~zone:Z_u_num (mk_assume (mk_in sentinel (add offset ptr_size range) size range) range) flow
                     );

                     (* Case 2.2.2: set non-sentinel before sentinel
                                    offset
                        -----|--------@--------#-----------------|--->
                             0              sentinel            size
                        Offset condition: offset <= sentinel - ptr_size
                        Transformation: before = before U { rval }
                     *)
                     [
                       mk_binop offset O_le (sub sentinel ptr_size range) range;
                     ],
                     (fun flow ->
                        add_before base sentinel range man flow >>$ fun mode flow ->
                        let before = mk_before_var base ~mode range in
                        man.post ~zone:Z_c_scalar (mk_assign before rval range) flow
                     );

                   ] man flow
              );

            ] man flow

        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow in
          Post.return flow'
        ) man flow


  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow >>$ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Post.return

    | E_c_points_to P_valid ->
      warn_at range "unsound assignment to valid pointer %a" pp_expr p;
      raise_alarm Alarms.AOutOfBound p.erange ~bottom:false man.lattice flow |>
      Post.return

    | E_c_points_to (P_block (base, offset)) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow >>$ fun offset flow ->
      assign_cases base offset rval range man flow

    | _ -> assert false




  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar, Z_u_num) size flow >>$ fun size flow ->

    let sentinel = mk_sentinel_var base range in
    let before = mk_before_var base range in
    let after = mk_after_var base range in

    man.post ~zone:Z_u_num (mk_add sentinel range) flow >>= fun _ flow ->
    man.post ~zone:Z_u_num (mk_assume (mk_in sentinel (mk_zero range) size range) range) flow >>= fun _ flow ->
    man.post ~zone:Z_c_scalar (mk_add before range) flow >>= fun _ flow ->
    man.post ~zone:Z_c_scalar (mk_add after range) flow



  (** Declare a block as assigned by adding the primed length variable *)
  let stub_assigns target offsets range man flow =
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) when is_interesting_base base ->
      let sentinel' = mk_sentinel_var base range ~primed:true in
      let before' = mk_before_var base range ~primed:true in
      let after' = mk_after_var base range ~primed:true ~mode:STRONG in
      man.post (mk_add sentinel' range) ~zone:Z_u_num flow >>= fun _ flow ->
      man.post (mk_add before' range) ~zone:Z_c_scalar flow >>= fun _ flow ->
      man.post (mk_add after' range) ~zone:Z_c_scalar flow

    | _ -> Post.return flow



  (** Rename primed variables introduced by a stub *)
  let rename_primed target offsets range man flow : 'a post =
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) when is_interesting_base base ->
      (* FIXME: not yet implemented *)
      let sentinel = mk_sentinel_var base range in
      let sentinel' = mk_sentinel_var ~primed:true base range in
      man.post ~zone:Z_u_num (mk_assign (weaken sentinel) (weaken sentinel') range) flow >>= fun _ flow ->
      man.post ~zone:Z_u_num (mk_remove sentinel' range) flow  >>= fun _ flow ->

      let before = mk_before_var base range in
      let before' = mk_before_var ~primed:true base range in
      man.post ~zone:Z_c_scalar (mk_assign before (mk_top (T_c_pointer T_c_void) range) range) flow >>= fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_remove before' range) flow  >>= fun _ flow ->

      let after = mk_after_var base range in
      let after' = mk_after_var ~primed:true base range in
      man.post ~zone:Z_c_scalar (mk_assign after (mk_top (T_c_pointer T_c_void) range) range) flow >>= fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_remove after' range) flow


    | E_c_points_to (P_null | P_invalid) ->
      Post.return flow


    | E_c_points_to P_valid ->
      warn_at range "unsound: rename of %a not supported because it can not be resolved"
        pp_expr target
      ;
      Post.return flow


    | _ -> assert false



  (** Rename the length variable associated to a base *)
  let rename_base base1 base2 range man flow =
    let sentinel1 = mk_sentinel_var base1 range in
    let sentinel2 = mk_sentinel_var base2 range in
    man.post ~zone:Z_u_num (mk_rename sentinel1 sentinel2 range) flow >>= fun _ flow ->

    let before1 = mk_before_var base1 range in
    let before2 = mk_before_var base2 range in
    man.post ~zone:Z_c_scalar (mk_rename before1 before2 range) flow >>= fun _ flow ->

    let after1 = mk_after_var base1 range in
    let after2 = mk_after_var base2 range in
    man.post ~zone:Z_c_scalar (mk_rename after1 after2 range) flow



  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when not (is_c_scalar_type v.vtyp) ->
      declare_variable v init scope stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_var (v, _) } when not (is_c_scalar_type v.vtyp) ->
      add_base (V v) stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_addr addr } when is_interesting_base (A addr) ->
      add_base (A addr) stmt.srange man flow |>
      Option.return


    | S_rename ({ ekind = E_var (v1,_) }, { ekind = E_var (v2,_) })
      when is_interesting_base (V v1) &&
           is_interesting_base (V v2)
      ->
      rename_base (V v1) (V v2) stmt.srange man flow |>
      Option.return


    | S_rename ({ ekind = E_addr addr1 }, { ekind = E_addr addr2 })
      when is_interesting_base (A addr1) &&
           is_interesting_base (A addr2)
      ->
      rename_base (A addr1) (A addr2) stmt.srange man flow >>=? fun _ flow ->
      man.post ~zone:Z_c_scalar stmt flow |>
      Option.return

    | S_assign({ ekind = E_c_deref p}, rval) when is_c_pointer_type rval.etyp ->
      assign_deref p rval stmt.srange man flow |>
      Option.return


    | S_stub_assigns(target, offsets) ->
      stub_assigns target offsets stmt.srange man flow |>
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

  (** Cases of the abstraction evaluations *)
  let eval_deref_cases base offset typ primed range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.eval ~zone:(Z_c_scalar,Z_u_num) size flow  >>$ fun size flow ->

    (* Safety condition: offset ∈ [0, size - pointer_size] *)
    assume ~zone:Z_u_num
      (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
      ~fthen:(fun flow ->
          if not (is_interesting_base base)
          then Eval.singleton (mk_top typ range) flow

          else
            let sentinel = mk_sentinel_var base ~primed range in
            let before = mk_before_var base ~primed range in
            let after = mk_after_var base ~primed range in
            let zero = mk_zero range in
            let ptr_size = mk_z ptr_size range in

            (* Fact that is always true: sentinel(a) ∈ [0, size] *)
            man.post ~zone:Z_u_num (mk_assume (mk_in sentinel zero size range) range) flow >>= fun _ flow ->

            switch ~zone:Z_c_scalar [
              (* Case 1: before sentinel
                 Offset condition: offset <= sentinel - ptr_size
                 Transformation: weak(before)
              *)
              [
                mk_binop offset O_le (sub sentinel ptr_size range) range;
              ],
              (fun flow ->
                 Eval.singleton before flow
              );

              (* Case 2: at sentinel
                 Offset condition: offset >= sentinel
                 Transformation: weak(after)
              *)
              [
                mk_binop offset O_ge sentinel range;
              ],
              (fun flow ->
                 Eval.singleton after flow
              );

            ] man flow

        )
      ~felse:(fun flow ->
          (* Unsafe case *)
          let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow in
          Eval.empty_singleton flow'
        ) man flow



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

    | E_c_points_to (P_block (base, offset)) when is_interesting_base base ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->
      eval_deref_cases base offset (under_type p.etyp) primed range man flow

    | E_c_points_to (P_block _)
    | E_c_points_to P_valid ->
      raise_alarm Alarms.AOutOfBound p.erange ~bottom:false man.lattice flow |>
      Eval.singleton (mk_top (under_pointer_type p.etyp |> void_to_char) range)

    | _ -> assert false



  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p when is_c_pointer_type exp.etyp &&
                       not (is_expr_quantified p)
      ->
      eval_deref exp false exp.erange man flow |>
      Option.return

    | E_stub_primed ({ ekind = E_c_deref _ } as e) when is_c_pointer_type exp.etyp &&
                                                        not (is_expr_quantified e)
      ->
      eval_deref e true exp.erange man flow |>
      Option.return


    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
