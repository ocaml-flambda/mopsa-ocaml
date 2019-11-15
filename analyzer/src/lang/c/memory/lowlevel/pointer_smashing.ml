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

(** Abstraction of pointer arrays by smashing. *)


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
      let name = "c.memory.lowlevel.pointer_smashing"
    end)

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [
        Z_c_scalar;
        Z_u_num
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

  let alarms = []

  (** {2 Auxiliary variables} *)
  (** *********************** *)

  (** Registration of the smash auxiliary variable *)
  type var_kind += V_c_pointer_smash   of base * bool


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_pointer_smash (base,primed) ->
            Format.fprintf fmt "smash(%a)%s" pp_base base (if primed then "'" else "")

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_pointer_smash(b1,p1), V_c_pointer_smash(b2,p2) ->
            Compare.compose [
              (fun () -> compare_base b1 b2);
              (fun () -> compare p1 p2);
            ]

          | _ -> next v1 v2
        );
    }


  (** void* type *)
  let void_ptr = T_c_pointer T_c_void


  (** Size of a pointer cell *)
  let ptr_size = sizeof_type void_ptr


  (** Create the auxiliary variable smash(base). *)
  let mk_smash_var base ?(primed=false) range : expr =
    let name = "smash(" ^ (base_uniq_name base) ^ ")" ^ (if primed then "'" else "") in
    let v = mkv name (V_c_pointer_smash (base,primed)) (T_c_pointer T_c_void) in
    mk_var v range


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
    | V v when is_c_type v.vtyp && is_c_array_type v.vtyp ->
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

    | _ -> false



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

      (* Collect pointers initializations *)
      let rec aux init flow : ('a,expr list) result =
        match init with
        | [] -> Result.singleton [] flow

        | C_flat_none (n,_) :: tl  ->
          let e = if is_global then mk_c_null range else mk_c_invalid_pointer range in
          aux tl flow >>$ fun el flow ->
          Result.singleton (e::el) flow

        | hd :: _ when is_non_pointer hd -> raise NonPointerFound

        | C_flat_fill (e,_,_):: tl
        | C_flat_expr (e,_) :: tl ->
          aux tl flow >>$ fun el flow ->
          Result.singleton (e::el) flow
      in

      let smash = mk_smash_var (V v) range in
      man.post ~zone:Z_c_scalar (mk_add smash range) flow >>= fun _ flow ->
      try
        aux flat_init flow >>$ fun el flow ->
        match el with
        | [] -> Post.return flow
        | hd :: tl ->
          man.post ~zone:Z_c_scalar (mk_assign smash hd range) flow >>= fun _ flow ->
          let smash_weak = weaken smash in
          List.fold_left (fun acc ptr ->
              acc >>= fun _ flow ->
              man.post ~zone:Z_c_scalar (mk_assign smash_weak ptr range) flow
            ) (Post.return flow) tl

      with NonPointerFound ->
        (* Non-pointer value found in the initializers *)
        Post.return flow



  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    eval_pointed_base_offset p range man flow >>$ fun r flow ->
    match r with
    | None ->
      (* Undetermined base and offset *)
      Soundness.warn_at range "ignoring assignment to undetermined lval *%a = %a;"
        pp_expr p
        pp_expr rval
      ;
      Post.return flow

    | Some (base,offset) ->
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow >>$ fun offset flow ->
      assume
        (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
        ~fthen:(fun flow ->
            if is_interesting_base base then
              man.eval ~zone:(Z_c_low_level,Z_c_scalar) rval flow >>$ fun rval flow ->
              let smash_weak = mk_smash_var base range |> weaken in
              man.post ~zone:Z_c_scalar (mk_assign smash_weak rval range) flow
            else
              Post.return flow
          )
        ~felse:(fun flow ->
            raise_c_out_bound_alarm ~base ~offset ~size range man flow |>
            Post.return
          )
        ~zone:Z_u_num man flow



  (** Add a base to the domain's dimensions *)
  let add_base base range man flow =
    if is_interesting_base base then
      let smash = mk_smash_var base range in
      man.post ~zone:Z_c_scalar (mk_add smash range) flow
    else
      Post.return flow



  (** Declare a block as assigned by adding the primed auxiliary variables *)
  let stub_assigns target offsets range man flow =
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) when is_interesting_base base ->
      let smash' = mk_smash_var base ~primed:true range in
      man.post ~zone:Z_c_scalar (mk_add smash' range) flow

    | _ -> Post.return flow



  (** Rename primed variables introduced by a stub *)
  (* FIXME: not yet implemented *)
  let rename_primed target offsets range man flow : 'a post =
    man.eval target ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block (base, _)) when is_interesting_base base ->
      let smash = mk_smash_var base range ~primed:false |> weaken in
      let smash' = mk_smash_var base range ~primed:true in
      man.post ~zone:Z_c_scalar (mk_assign smash smash' range) flow >>= fun _ flow ->
      man.post ~zone:Z_c_scalar (mk_remove smash' range) flow

    | _ ->
      Post.return flow


  (** Rename the auxiliary variables associated to a base *)
  let rename_base base1 base2 range man flow =
    let smash1 = mk_smash_var base1 range in
    let smash2 = mk_smash_var base2 range in
    man.post ~zone:Z_c_scalar (mk_rename smash1 smash2 range) flow
    

  (** Remove the auxiliary variables of a base *)
  let remove_base base range man flow =
    let smash = mk_smash_var base range in
    man.post ~zone:Z_c_scalar (mk_remove smash range) flow


  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when is_interesting_base (V v) ->
      declare_variable v init scope stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_var (v, _) } when is_interesting_base (V v) ->
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

    | S_assign({ ekind = E_c_deref p} as lval, rval) when is_c_pointer_type lval.etyp ->
      assign_deref p rval stmt.srange man flow |>
      Option.return


    | S_remove { ekind = E_var (v, _) } when is_interesting_base (V v) ->
      remove_base (V v) stmt.srange man flow |>
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


  (** Abstract evaluation of a dereference *)
  let eval_deref exp primed range man flow =
    let p = match ekind exp with E_c_deref p -> p | _ -> assert false in
    let t = exp.etyp in
    eval_pointed_base_offset p range man flow >>$ fun r flow ->
    match r with
    | None ->
      Soundness.warn_at range "ignoring dereference of ⊤ pointer";
      Eval.singleton (mk_top t range) flow

    | Some (base,offset) when not (is_expr_quantified offset) ->
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow >>$ fun offset flow ->
      assume
        (mk_in offset (mk_zero range) (sub size (mk_z ptr_size range) range) range)
        ~fthen:(fun flow ->
            if is_interesting_base base then
              let smash_weak = mk_smash_var base ~primed range |> weaken in
              Eval.singleton smash_weak flow
            else
              Eval.singleton (mk_top t range) flow
          )
        ~felse:(fun flow ->
            raise_c_out_bound_alarm ~base ~offset ~size range man flow |>
            Eval.empty_singleton
          )
        ~zone:Z_u_num man flow

    | Some (base,offset) when is_expr_quantified offset ->
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
      let min, max = Common.Quantified_offset.bound offset in
      man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
      man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->
      (* Safety condition: [min, max] ⊆ [0, size - ptr [ *)
      assume
      (
        mk_binop
          (mk_in min (mk_zero range) (sub size (mk_z ptr_size range) range) range)
          O_log_and
          (mk_in max (mk_zero range) (sub size (mk_z ptr_size range) range) range)
          range
      )
      ~fthen:(fun flow ->
          if is_interesting_base base then
            let smash_weak = mk_smash_var base ~primed range |> weaken in
            Eval.singleton smash_weak flow
          else
            Eval.singleton (mk_top t range) flow
        )
      ~felse:(fun flow ->
          (* FIXME: remove qunatifiers from offset *)
          raise_c_out_bound_alarm ~base ~offset ~size range man flow |>
          Eval.empty_singleton
        ) ~zone:Z_u_num man flow

    | _ -> assert false



  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p
      when is_c_pointer_type exp.etyp &&
           under_type p.etyp |> void_to_char |> is_c_scalar_type
      ->
      eval_deref exp false exp.erange man flow |>
      Option.return

    | E_stub_primed ({ ekind = E_c_deref p } as e)
      when is_c_pointer_type exp.etyp &&
           under_type p.etyp |> void_to_char |> is_c_scalar_type
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
