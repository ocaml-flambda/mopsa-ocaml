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

    This abstract domain keeps track of the position of the first NULL/INVALID
    pointer in an array of pointers. This position is represented by
    the auxiliary variable sentinel(b), where b represents a base.

    Pointers before sentinel(b) are all valid pointers and are smashed into
    a single auxiliary variable valid(b).
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
      let name = "c.memory.lowlevel.null_invalid_sentinel"
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


  (** {2 Variable of NULL position} *)
  (** ********************************* *)

  (** Registration of a new var kinds for our auxiliary variables *)
  type var_kind +=
    | V_c_sentinel of base * bool (* is it primed? *)
    | V_c_valid    of base * bool


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_sentinel (base,primed) ->
            Format.fprintf fmt "sentinel(%a)%s" pp_base base (if primed then "'" else "")

          | V_c_valid (base,primed) ->
            Format.fprintf fmt "valid(%a)%s" pp_base base (if primed then "'" else "")

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_sentinel(b1,p1), V_c_sentinel(b2,p2)
          | V_c_valid(b1,p1), V_c_valid(b2,p2) ->
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


  (** Create the auxiliary valid(base) variable representing the
      all pointers before sentinel(base).
  *)
  let mk_valid_var base ?(primed=false) ?(mode=WEAK) range : expr =
    let name = "valid(" ^ (base_uniq_name base) ^ ")" ^ (if primed then "'" else "") in
    let v = mkv name (V_c_valid (base,primed)) (T_c_pointer T_c_void) in
    mk_var v ~mode range



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


  (** Declaration of a C variable *)
  let declare_variable v init scope range man flow =
    if not (is_interesting_base (V v))
    then Post.return flow

    else
      (* Since we are in the Z_low_level zone, we assume that init has
         been translated by a structured domain into a flatten
         initialization *)
      let flat_init =
        match init with
        | Some (C_init_flat l) -> l
        | _ -> assert false
      in

      let size = sizeof_type v.vtyp in

      let pointer_size = sizeof_type (T_c_pointer T_c_void) in

      let sentinel = mk_sentinel_var (V v) range in
      let valid = mk_valid_var (V v) range in

      let is_sentinel_expr e =
        match ekind (remove_casts e) with
        | E_c_address_of _          -> Some false
        | E_constant (C_c_string _) -> Some false
        | E_var (v,_) when is_c_array_type v.vtyp -> Some false
        | E_constant C_c_invalid    -> Some true
        | _ ->
          match c_expr_to_z (remove_casts e) with
          | Some e -> Some (Z.equal e Z.zero)
          | None -> None
      in


      (* Exception raised when a non-pointer initializer is found *)
      let exception NonPointerFound in

      (* Check if an initializer has a pointer type *)
      let is_non_pointer = function
        | C_flat_none (_,t)
        | C_flat_expr(_,t)
        | C_flat_fill(_,t,_) ->
          not (is_c_pointer_type t)
      in


      (* Find the position of the first NULL and accumulate valid pointers *)
      let rec aux o l acc =
        match l with
        | [] -> size, size, acc

        | hd :: _ when is_non_pointer hd -> raise NonPointerFound

        | C_flat_none _ :: tl  -> o, o, acc

        | C_flat_expr (e,_) :: tl ->
          debug "init expr %a" pp_expr e;
          begin
            match is_sentinel_expr e with
            | Some true -> o, o, acc
            | Some false -> aux (Z.add o pointer_size) tl (e :: acc)
            | None ->
              let o1,o2,acc = aux (Z.add o pointer_size) tl (e :: acc) in
              o,o2,acc
          end


        | C_flat_fill (e,_,n) :: tl ->
          debug "fill expression %a * %a" pp_expr e Z.pp_print n;
          begin
            match is_sentinel_expr e with
            | Some true -> o, o, acc
            | Some false -> aux (Z.add o (Z.mul n pointer_size)) tl (e :: acc)
            | None ->
              let o1,o2,acc = aux (Z.add o (Z.mul n pointer_size)) tl (e :: acc) in
              o,o2,acc
          end

      in
      man.post ~zone:Z_u_num (mk_add sentinel range) flow >>= fun _ flow ->
      try
        let o1, o2, pointers = aux Z.zero flat_init [] in
        let pos =
          if Z.equal o1 o2
          then mk_z o1 range
          else mk_z_interval o1 o2 range
        in
        man.post ~zone:Z_u_num (mk_assign sentinel pos range) flow >>= fun _ flow ->
        match pointers with
        | [] -> Post.return flow
        | hd :: tl ->
          man.post ~zone:Z_c_scalar (mk_add valid range) flow >>= fun _ flow ->
          let strong_valid = mk_valid_var (V v) range ~mode:STRONG in
          man.post ~zone:Z_c_scalar (mk_assign strong_valid hd range) flow >>= fun _ flow ->
          List.fold_left (fun acc ptr ->
              acc >>= fun _ flow ->
              man.post ~zone:Z_c_scalar (mk_assign valid ptr range) flow
            ) (Post.return flow) tl

      with NonPointerFound ->
        Post.return flow




  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    assert false



  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    assert false


  (** Declare a block as assigned by adding the primed length variable *)
  let stub_assigns target offsets range man flow =
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) when is_interesting_base base ->
      let sentinel' = mk_sentinel_var base range ~primed:true in
      let valid' = mk_valid_var base range ~primed:true in
      man.post (mk_add sentinel' range) ~zone:Z_u_num flow >>= fun _ flow ->
      man.post (mk_add valid' range) ~zone:Z_c_scalar flow

    | _ -> Post.return flow



  (** Rename primed variables introduced by a stub *)
  let rename_primed target offsets range man flow : 'a post =
    assert false


  (** Rename the length variable associated to a base *)
  let rename_base base1 base2 range man flow =
    assert false


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

    | S_assign({ ekind = E_c_deref p}, rval) ->
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
  let eval_deref_cases base offset primed range man flow =
    assert false


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
      eval_deref_cases base offset primed range man flow

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

    | E_stub_primed e when is_c_pointer_type exp.etyp &&
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
