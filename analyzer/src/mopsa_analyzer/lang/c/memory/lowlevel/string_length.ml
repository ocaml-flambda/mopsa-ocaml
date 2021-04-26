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
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Common.Base
open Common.Points_to
open Common.Alarms
open Universal.Numeric.Common



module Domain =
struct


  (** {2 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId(struct
      let name = "c.memory.lowlevel.string_length"
    end)

  let universal = Semantic "Universal"

  let checks = []


  (** {2 Options} *)
  (** *********** *)

  let opt_track_length = ref true


  let () =
    register_domain_option name {
      key = "-c-track-string-length";
      category = "C";
      doc = " track lengths of dynamic strings";
      spec = ArgExt.Bool (fun b -> opt_track_length := b);
      default = "true";
    }

  (* string length variables count in elem_size byte units
     1 -> regular char strings
     4 -> wide char strings

     TODO should be replaced by an automatic detect of char length
     for each buffer
  *)
  let elem_size = 1


  (** {2 Variable of string lengths} *)
  (** ****************************** *)

  (** Registration of a new var kind for length variables *)
  type var_kind +=
    | V_c_string_length of base * int (* element size *)

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_string_length (base,elemsz) ->
            if elemsz = 1 then Format.fprintf fmt "string-length⦃%a⦄" pp_base base
            else Format.fprintf fmt "string-lengt⦃%a⦄h*%i" pp_base base elemsz

          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_string_length(b1,s1), V_c_string_length(b2,s2) ->
            Compare.pair compare_base compare (b1,s1) (b2,s2)

          | _ -> next v1 v2
        );
    }

  (** Create a length variable. The returned variable is a
      mathematical integer, not a C variable.
  *)
  let mk_length_var base elemsz ?(mode=None) range =
    let name = Format.asprintf "string-length⦃%s⦄" (base_uniq_name base) in
    let v = mkv name (V_c_string_length (base,elemsz)) T_int ~mode:(base_mode base) ~semantic:"Universal" in
    mk_var v ~mode range


  (** {2 Utility functions} *)
  (** ********************* *)

  (** Get the base and offset pointed by ptr. Since we do not track invalid
      dereferences, we ignore invalid pointers.
      Offsets are still counted in bytes (not elem_size).
  *)
  let eval_pointed_base_offset ptr range man flow =
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    | P_null
    | P_invalid
    | P_block ({ base_valid = false }, _, _) ->
      Cases.empty flow

    | P_block (base, offset, mode) ->
      Cases.singleton (Some (base, offset, mode)) flow

    | P_top ->
      Cases.singleton None flow

    | _ -> assert false

  (** Converts from byte offsets to offsets in elem_size units. *)
  let elem_of_offset boffset elem_size range =
    if elem_size = 1 then boffset
    else div boffset (mk_int elem_size range) range


  let is_interesting_resource = function
    | "Memory" | "alloca" | "ReadOnlyMemory" | "String" | "ReadOnlyString" | "arg" -> true
    | _ -> false

  let rec is_scalar_base base =
    match base with
    | { base_kind = Var {vkind = Cstubs.Aux_vars.V_c_primed_base base}; base_valid = true } ->
      is_scalar_base base

    | { base_kind = Var v; base_valid = true } -> is_c_scalar_type v.vtyp

    | _ -> false

  let rec is_interesting_base base =
    match base with
    | { base_kind = Var {vkind = Cstubs.Aux_vars.V_c_primed_base base}; base_valid = true } ->
      is_interesting_base base

    | { base_kind = Var v; base_valid = true } ->
      !opt_track_length &&
      is_c_type v.vtyp &&
      is_c_array_type v.vtyp &&
      under_array_type v.vtyp |> is_c_int_type

    | { base_kind = String _ } -> true

    | { base_kind = Addr { addr_kind = A_stub_resource res }; base_valid = true }
      ->
      !opt_track_length &&
      is_interesting_resource res

    | _ -> false


  let equal_int_types t1 t2 =
    match remove_typedef_qual t1, remove_typedef_qual t2 with
    | T_c_integer _, T_c_integer _ ->
      sizeof_type t1 = sizeof_type t2 &&
      is_signed t1 = is_signed t2
    | _ -> true



  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow


  (** {2 Abstract transformers} *)
  (** ************************* *)


  (** 𝕊⟦ add(base); ⟧ *)
  let exec_add_base base range man flow =
    (* For scalar variables, return NotHandled to let the underlying C/Scalar semantics handle them *)
    if is_scalar_base base then Cases.not_handled flow else
    (* For non-scalar bases but yet not interesting, we just do nothing *)
    if not (is_interesting_base base) then Post.return flow
    else match base.base_kind with
      | String _ ->
        Post.return flow

      | _ ->
        (* Add the length of the base to the numeric domain and
           initialize it with the interval [0, byte-size(@) / elem-size]
        *)
        eval_base_size base range man flow >>$ fun bsize flow ->

        let length = mk_length_var base elem_size range in
        let size = elem_of_offset bsize elem_size range in

        man.exec ~route:universal (mk_add length range) flow >>% fun flow ->
        man.exec (mk_assume (mk_in length (mk_zero range) size range) range) flow


  (** 𝕊⟦ remove(base); ⟧ *)
  let exec_remove_base base range man flow =
    if is_scalar_base base then Cases.not_handled flow else
    if not (is_interesting_base base) then Post.return flow
    else match base.base_kind with
      | String _ ->
        Post.return flow

      | _ ->
        let length = mk_length_var base elem_size range in
        man.exec ~route:universal (mk_remove length range) flow


  (** 𝕊⟦ rename(base1,base2); ⟧ *)
  let exec_rename_base base1 base2 range man flow =
    if is_scalar_base base1 then Cases.not_handled flow else
    if not (is_interesting_base base1) then Post.return flow else
    if not (is_interesting_base base2) then panic_at range "rename %a -> %a not supported" pp_base base1 pp_base base2
    else
      let length1 = mk_length_var base1 elem_size range in
      let length2 = mk_length_var base2 elem_size range in
      man.exec ~route:universal (mk_rename length1 length2 range) flow


  (** 𝕊⟦ expand(base,bases); ⟧ *)
  let exec_expand_base base1 bases range man flow =
    if is_scalar_base base1 then Cases.not_handled flow else
    if not (is_interesting_base base1) then
      List.fold_left
        (fun acc b -> Post.bind (exec_add_base b range man) acc)
        (Post.return flow) bases
    else
      let length1 = mk_length_var base1 elem_size range in
      let lengths = List.fold_left
          (fun acc b ->
             if is_interesting_base b then
               mk_length_var b elem_size range :: acc
             else
               acc
          ) [] bases in
      if lengths = [] then
        Post.return flow
      else
        man.exec ~route:universal (mk_expand length1 lengths range) flow


  (** Fold the length variable of a base *)
  let exec_fold_bases base bases range man flow =
    if is_scalar_base base then Cases.not_handled flow else
    if not (is_interesting_base base) then
      Post.return flow
    else
      let length = mk_length_var base elem_size range in
      let lengths = List.fold_left
          (fun acc b ->
             if is_interesting_base b then
               mk_length_var b elem_size range :: acc
             else
               acc
          ) [] bases in
      if lengths = [] then
         eval_base_size base range man flow >>$ fun bsize flow ->
         let size = elem_of_offset bsize elem_size range in
         man.exec ~route:universal (mk_forget length range) flow >>% fun flow ->
         man.exec (mk_assume (mk_in length (mk_zero range) size range) range) flow
      else
        man.exec ~route:universal (mk_fold length lengths range) flow


  (** 𝕊⟦ forget(e); ⟧ *)
  let exec_forget e range man flow =
    eval_pointed_base_offset (mk_c_address_of e range) range man flow >>$ fun bo flow ->
    match bo with
    | None -> Post.return flow
    | Some (base,boffset,mode) ->
      if is_scalar_base base then Cases.not_handled flow else
      if not (is_interesting_base base) || not (is_c_int_type e.etyp) then
        Post.return flow
      else
        match base.base_kind with
        | String _ -> Post.return flow
        | _ ->
          (* FIXME: we can do better by checking if the offset affects the length of the string *)
          let length = mk_length_var base elem_size range in
          eval_base_size base range man flow >>$ fun bsize flow ->
          let size = elem_of_offset bsize elem_size range in
          man.exec ~route:universal (mk_forget length range) flow >>% fun flow ->
          man.exec (mk_assume (mk_in length (mk_zero range) size range) range) flow


  let exec_forget_quant quants e range man flow =
    eval_pointed_base_offset (mk_c_address_of e range) range man flow >>$ fun bo flow ->
    match bo with
    | None -> Post.return flow
    | Some (base,boffset,mode) ->
    if is_scalar_base base then Cases.not_handled flow else
    if not (is_interesting_base base) || not (is_c_int_type e.etyp) then
      Post.return flow
    else
      match base.base_kind with
      | String _ -> Post.return flow
      | _ ->
        (* FIXME: we can do better by checking if the offset affects the length of the string *)
        let length = mk_length_var base elem_size range in
        eval_base_size base range man flow >>$ fun bsize flow ->
        let size = elem_of_offset bsize elem_size range in
        man.exec ~route:universal (mk_forget length range) flow >>% fun flow ->
        man.exec (mk_assume (mk_in length (mk_zero range) size range) range) flow


  (** 𝕊⟦ type v; ⟧ *)
  let exec_declare_variable v scope range man flow =
    let base = mk_var_base v in
    if is_scalar_base base then Cases.not_handled flow else
    if not (is_interesting_base base)
    then Post.return flow
    else exec_add_base base range man flow


  (** 𝕊⟦ lval = rhs; ⟧ *)
  let exec_assign lval rhs range man flow =
    man.eval rhs flow >>$ fun rhs flow ->
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun bo flow ->
    match bo with
    | None -> Post.return flow
    | Some (base,boffset,mode) ->
      (* Scalar variables are handled by C/Scalar semantics *)
      if is_scalar_base base then Cases.not_handled flow else
      (* For non-interesting bases, we do nothing since their content is always evaluated to ⊤ *)
      if not (is_interesting_base base) || not (is_c_int_type lval.etyp) then
        Post.return flow
      else
      match base.base_kind with
      | String _ -> Post.return flow
      | Var _ | Addr _ ->
        let char_size = sizeof_type lval.etyp in
        let length = mk_length_var base elem_size ~mode range in
        let offset = elem_of_offset boffset elem_size range in
        if char_size = Z.of_int elem_size &&
           Common.Quantified_offset.is_aligned boffset char_size man flow
        then
          man.eval offset flow >>$ fun offset flow ->
          eval_base_size base range man flow >>$ fun bsize flow ->
          let size = elem_of_offset bsize elem_size range in

          (* Utility function to assign an interval to [length] *)
          let assign_length_interval l u flow =
            man.exec ~route:universal (mk_forget length range) flow |>
            Post.bind (
              man.exec (mk_assume ((mk_in length l u range)) range)
            )
          in
          switch [
            (* set0 case *)
            (* Offset condition: offset ∈ [0, length] *)
            (* RHS condition: rhs = 0 *)
            (* Transformation: length := offset; *)
            [ mk_in offset zero length range;
              mk_eq rhs zero range ],
            (fun flow -> man.exec ~route:universal (mk_assign length offset range) flow)
            ;

            (* setnon0 case *)
            (* Offset condition: offset = length *)
            (* RHS condition: rhs ≠ 0 *)
            (* Transformation: length := [offset + 1, size]; *)
            [ mk_eq offset length range;
              mk_ne rhs zero range ],
            (fun flow -> assign_length_interval (add offset one range) size flow)
            ;

            (* First unchanged case *)
            (* Offset condition: offset ∈ [0, length - 1] *)
            (* RHS condition: rhs ≠ 0 *)
            (* Transformation: nop; *)
            [ mk_in offset zero (pred length range) range;
              mk_ne rhs zero range ],
            (fun flow -> Post.return flow)
            ;

            (* Second unchanged case *)
            (* Offset condition: offset >= length + 1 *)
            (* RHS condition: ⊤ *)
            (* Transformation: nop; *)
            [ mk_ge offset (succ length range) range ],
            (fun flow -> Post.return flow)

          ] man flow

        else
          man.exec ~route:universal (mk_forget length range) flow


  (** Transformers entry point *)
  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when not (is_c_scalar_type v.vtyp) ->
      exec_declare_variable v scope stmt.srange man flow |>
      OptionExt.return

    | S_add e when is_base_expr e && is_c_type e.etyp && not (is_var_base_expr e && is_c_scalar_type e.etyp) ->
      exec_add_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_rename (e1,e2) when is_base_expr e1 && is_base_expr e2 && is_c_type e1.etyp && not (is_var_base_expr e1 && is_c_scalar_type e1.etyp) ->
      exec_rename_base (expr_to_base e1) (expr_to_base e2) stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_base_expr e && List.for_all is_base_expr el && is_c_type e.etyp && not (is_c_scalar_type e.etyp) ->
      exec_expand_base (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_fold(e,el) when is_base_expr e && List.for_all is_base_expr el && is_c_type e.etyp && not (is_var_base_expr e && is_c_scalar_type e.etyp) ->
      exec_fold_bases (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) when is_c_type e.etyp ->
      exec_forget e stmt.srange man flow |>
      OptionExt.return

    | S_forget({ ekind = E_stub_quantified_formula(quants, e)}) when is_c_type e.etyp ->
      exec_forget_quant quants e stmt.srange man flow |>
      OptionExt.return

    | S_remove(e) when is_base_expr e && is_c_type e.etyp && not (is_c_scalar_type e.etyp) ->
      exec_remove_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, rval)
      when !opt_track_length && lval.etyp |> is_c_scalar_type
      ->
      exec_assign lval rval stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** 𝔼⟦ *(str + offset) ⟧ *)
  let eval_string_literal_char str t boffset range man flow =
    let char_size = sizeof_type t in
    let length = Z.(div (of_int (String.length str)) char_size) in
    let offset = elem_of_offset boffset (Z.to_int char_size) range in
    man.eval offset flow >>$ fun offset flow ->
    if Common.Quantified_offset.is_aligned boffset char_size man flow then
      switch [
        [ eq offset (mk_z length range) range],
        (fun flow -> Eval.singleton (mk_zero range) flow);

        [mk_in offset zero (mk_z (Z.pred length) range) range],
        (fun flow ->
           (* Get the interval of the offset *)
           let itv = man.ask (mk_int_interval_query offset) flow in
           (* itv should be included in [0,length-1] *)
           let max = I.of_z Z.zero (Z.pred length) in
           begin match I.meet_bot itv (Bot.Nb max) with
             | Bot.BOT -> Eval.empty flow
             | Bot.Nb itv' ->
               (* Get the interval of possible chars *)
               let indexes = I.to_list itv' in
               let char_at i =
                 I.cst (extract_multibyte_integer str (Z.to_int (Z.mul char_size i)) t)
               in
               let chars =
                 List.fold_left (fun acc i -> char_at i :: acc)
                   [char_at (List.hd indexes)] (List.tl indexes)
               in
               let l,u =
                 match I.join_list chars |> Bot.bot_to_exn with
                 | I.B.Finite l, I.B.Finite u -> l,u
                 | _ -> assert false
               in
               if Z.equal l u
               then Eval.singleton (mk_z l range) flow
               else Eval.singleton (mk_z_interval l u range) flow
           end
        )
      ] man flow
    else
      man.eval (mk_top t range) flow


  let eval_deref p range man flow =
    let ctype = under_pointer_type p.etyp in
    eval_pointed_base_offset p range man flow >>$ fun bo flow ->
    match bo with
    | None -> man.eval (mk_top ctype range) flow
    | Some (base,offset,mode) ->
      if is_scalar_base base then Eval.not_handled flow else
      if not (is_interesting_base base) || not (is_c_int_type ctype) then
         man.eval (mk_top ctype range) flow
      else
        match base.base_kind with
        | String (str,_,t) when equal_int_types t ctype ->
          eval_string_literal_char str t offset range man flow
        | _ ->
          man.eval (mk_top ctype range) flow



  let assume_string_literal_char_eq str t boffset mode n range man flow =
    let char_size = Z.to_int (sizeof_type t) in
    let blen = String.length str in
    (* When n = 0, require that byte-offset = byte-length(str) *)
    if Z.(n = zero) then
      man.exec (mk_assume (mk_binop boffset O_eq (mk_int blen range) range) range) flow
    else if char_size = 1 then
      (* Search for the first and last positions of `n` in `str` *)
      let c = Z.to_int n |> Char.chr in
      if not (String.contains str c) then
        Post.return (Flow.remove T_cur flow)
      else
        let l = String.index str c in
        let u = String.rindex str c in
        let pos =
          if l = u then mk_int l range
          else mk_int_interval l u range
        in
        (* Require that offset is equal to pos *)
        man.exec (mk_assume (mk_binop boffset O_eq pos range) range) flow
    else
      (* for wide characters, we only know that offset < len(str) *)
      man.exec (mk_assume (mk_binop boffset O_le (mk_int (blen-char_size) range) range) range) flow

  let assume_string_literal_char_ne str t boffset mode n range man flow =
    (* FIXME TODO *)
    Post.return flow

  (** 𝕊⟦ *(p + i) != n ⟧ *)
  let assume_ne base boffset mode etype n range man flow =
    match base.base_kind, c_expr_to_z n with
    | String (str,_,t), Some c when sizeof_type t = sizeof_type etype ->
      assume_string_literal_char_ne str t boffset mode c range man flow

    | String _, _ ->
      Post.return flow

    | _ when sizeof_type etype = Z.of_int elem_size ->
      let length = mk_length_var base elem_size ~mode range in
      let offset = elem_of_offset boffset elem_size range in
      switch [
        (*         offset    length
                   |---------x---------0----->
        *)
        [ le offset (pred length range) range ],
        (fun flow -> man.exec (mk_assume (eq n zero range) range) flow);

        (*          offset/length
                    |--------------0----------->
        *)
        [ eq offset length range],
        (fun flow -> man.exec (mk_assume (ne n zero range) range) flow);

        (*          length   offset
                    |----------0--------x----->
        *)
        [ ge offset (succ length range) range ],
        (fun flow -> Post.return flow);
      ] man flow
    |_ ->
      Post.return flow

  (** 𝕊⟦ *(p + i) == n ⟧ *)
  let assume_eq base boffset mode etype n range man flow =
    match base.base_kind, c_expr_to_z n with
    | String (str,_,t), Some c when sizeof_type t = sizeof_type etype ->
      assume_string_literal_char_eq str t boffset mode c range man flow

    | String _, _ ->
      Post.return flow

    | _ when sizeof_type etype = Z.of_int elem_size ->
      let length = mk_length_var base elem_size ~mode range in
      let offset = elem_of_offset boffset elem_size range in
      switch [
        (*         offset    length
                   |---------x---------0----->
        *)
        [ le offset (pred length range) range ],
        (fun flow -> man.exec (mk_assume (ne n zero range) range) flow);

        (*          offset/length
                    |--------------0----------->
        *)
        [ eq offset length range],
        (fun flow -> man.exec (mk_assume (eq n zero range) range) flow);

        (*          length   offset
                    |----------0--------x----->
        *)
        [ ge offset (succ length range) range ],
        (fun flow -> Post.return flow);
      ] man flow
    |_ ->
      Post.return flow

  let assume_exists_eq i a b = assume_eq


  (** 𝕊⟦ ∀i ∈ [a,b] : *(p + i) == n ⟧ *)
  (* FIXME: this transfer function is sound only when the offset is an
     affine function with coefficient 1, i.e. of the form ∀i + a *)
  let assume_forall_eq i a b base boffset mode ctype n range man flow =
    let char_size = sizeof_type ctype in
    if char_size <> Z.of_int elem_size then Post.return flow else
    (** Get symbolic bounds of the offset *)
    let quants = [(FORALL,i,S_interval(a,b))] in
    match Common.Quantified_offset.bound_div boffset char_size quants man flow with
    | Top.TOP -> Post.return flow
    | Top.Nt (min,max) ->
    man.eval min flow ~translate:"Universal" >>$ fun min flow ->
    man.eval max flow ~translate:"Universal" >>$ fun max flow ->

    let length = mk_length_var base elem_size ~mode range in
    eval_base_size base range man flow >>$ fun bsize flow ->
    man.eval bsize flow >>$ fun bsize flow ->
    let size = elem_of_offset bsize elem_size range in
    (* Ensure that [min, max] ⊆ [0, size-1] *)
    man.exec (mk_assume (ge min zero range) range) ~route:universal flow >>% fun flow ->
    man.exec (mk_assume (le max (pred size range) range) range) ~route:universal flow >>% fun flow ->
    switch [
      (*          length    min     max
         |-----------0-------|nnnnnnnn|------>
      *)
      [ ge min (succ length range) range ],
      (fun flow -> Post.return flow);

      (*         length/min    max
         |-----------0nnnnnnnnnn|------>
      *)
      [ eq min length range ],
      (fun flow ->
         match c_expr_to_z n with
         | Some n when Z.(n = zero) -> Post.return flow
         | Some n -> Cases.empty flow
         | None ->
           assume (eq n zero range)
             ~fthen:(fun flow -> Post.return flow)
             ~felse:(fun flow -> Cases.empty flow)
             man flow
      );

      (*         min    length    max
         |--------|nnnnnnn0nnnnnnnn|------>
      *)
      [ le min (pred length range) range;
        le length max range ],
      (fun flow -> Cases.empty flow);

      (*     min       max    length
         |----|nnnnnnnnn|-------0------>
      *)
      [ le max (pred length range) range ],
      (fun flow -> man.exec (mk_assume (ne n zero range) range) flow);
    ] man flow


  (** 𝕊⟦ ∀i ∈ [a,b] : *(p + i) != 0 ⟧ *)
  (* FIXME: this transfer function is sound only when the offset is an
     affine function with coefficient 1, i.e. of the form ∀i + a *)
  let assume_forall_ne_zero i a b base boffset ctype mode range man flow =
    (** Get symbolic bounds of the offset *)
    let char_size = sizeof_type ctype in
    let quants = [(FORALL,i,S_interval(a,b))] in
    match Common.Quantified_offset.bound_div boffset char_size quants man flow with
    | Top.TOP -> Post.return flow
    | Top.Nt (min,max) ->
    man.eval min flow ~translate:"Universal" >>$ fun min flow ->
    man.eval max flow ~translate:"Universal" >>$ fun max flow ->

    let length =
      match base.base_kind with
      | String (str,_,t) when sizeof_type t = sizeof_type ctype ->
        Some (mk_z (Z.div (Z.of_int (String.length str)) (sizeof_type t)) range)
        | String _ -> None
      | _ when char_size = Z.of_int elem_size ->
        Some (mk_length_var base elem_size ~mode range)
      | _ ->
        None
    in
    match length with
    | None ->
      Post.return flow
    | Some length ->
      switch [
        (*       min      max   length
           |------|--------|------0------>
        *)
        [ le max (pred length range) range ],
        (fun flow -> Post.return flow);

        (*       min  length     max
           |------|------0--------|------>
        *)
        [ mk_in length min max range ],
        (fun flow -> Cases.empty flow);

        (*       length   min    max
           |------0--------|------|------>
        *)
        [ ge min (succ length range) range ],
        (fun flow -> Post.return flow);
      ] man flow


  let assume_exists_ne i a b  = assume_ne

  let assume_forall_ne i a b base boffset mode ctype n range man flow =
    match c_expr_to_z n with
    | Some n when Z.(n = zero) ->
      assume_forall_ne_zero i a b base boffset ctype mode range man flow
    | Some n -> Post.return flow
    | None ->
      assume (eq n zero range)
        ~fthen:(fun flow -> assume_forall_ne_zero i a b base boffset ctype mode range man flow)
        ~felse:(fun flow -> Post.return flow)
        man flow


  (** 𝕊⟦ ∀i ∈ [a,b] :*(p + i) == *(q + i) ⟧ *)
  (* FIXME: this transfer function is sound only when the offset is an
     affine function with coefficient 1, i.e. of the form ∀i + a *)
  let assume_forall_eq2 i a b base1 boffset1 mode1 ctype1 base2 boffset2 mode2 ctype2 range man flow =
    if not (equal_int_types ctype1 ctype2) ||
       sizeof_type ctype1 <> Z.of_int elem_size then Post.return flow
    else

      (* Get symbolic bounds of quantified offsets *)
      let quants = [(FORALL,i,S_interval(a,b))] in
      match
        Common.Quantified_offset.bound_div boffset1 (Z.of_int elem_size) quants man flow,
        Common.Quantified_offset.bound_div boffset2 (Z.of_int elem_size) quants man flow
      with
      | Top.TOP,_ | _, Top.TOP -> Post.return flow
      | Top.Nt (min1,max1), Top.Nt (min2,max2) ->
        let evl1 = man.eval min1 flow ~translate:"Universal" in
        let evl2 = man.eval max1 flow ~translate:"Universal" in
        let evl3 = man.eval min2 flow ~translate:"Universal" in
        let evl4 = man.eval max2 flow ~translate:"Universal" in

        evl1 >>$ fun min1 flow ->
        evl2 >>$ fun max1 flow ->
        evl3 >>$ fun min2 flow ->
        evl4 >>$ fun max2 flow ->

        let get_length base mode = match base.base_kind with
          | String (str,_,t) when equal_int_types t ctype1 ->
            Some (mk_int ((String.length str) / elem_size) range)
          | String _ -> None
          | _ -> Some (mk_length_var base elem_size ~mode:mode range)
        in

        match get_length base1 mode1, get_length base2 mode2 with
        | None, _ | _, None -> Post.return flow
        | Some length1, Some length2 ->
          let before1 = le max1 (pred length1 range) range in
          let before2 = le max2 (pred length2 range) range in

          let cover1 = mk_in length1 min1 max1 range in
          let cover2 = mk_in length2 min2 max2 range in

          let after1 = ge min1 (succ length1 range) range in
          let after2 = ge min2 (succ length2 range) range in

          switch [
            [ log_or after1 after2 range ],
            (fun flow -> Post.return flow);

            [ log_and before1 before2 range ],
            (fun flow -> Post.return flow);

            [ log_and before1 cover2 range ],
            (fun flow -> Cases.empty flow);

            [ log_and cover1 before2 range ],
            (fun flow -> Cases.empty flow);

            [ log_and cover1 cover2 range ],
            (fun flow -> man.exec (mk_assume (eq (sub length1 min1 range) (sub length2 min2 range) range) range) ~route:universal flow);
          ] man flow

  let assume_exists_ne2 i a b base1 boffset1 mode1 ctype1 base2 boffset2 mode2 ctype2 range man flow =
    (** FIXME: TODO *)
    Post.return flow



  (** 𝔼⟦ *(p + i) == n ⟧ *)
  let eval_eq lval n range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun bo flow ->
    man.eval n flow >>$ fun n flow ->
    match bo with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base,offset,mode) ->
      if not (is_interesting_base base) then
        Eval.singleton (mk_top T_bool range) flow
      else
        let true_post = assume_eq base offset mode lval.etyp n range man flow in
        (* Propagate context to the false branch before executing [assume_ne] *)
        let false_post = assume_ne base offset mode lval.etyp n range man (Flow.set_ctx (Cases.get_ctx true_post) flow) in
        Eval.join
          (true_post >>% Eval.singleton (mk_true range))
          (false_post >>% Eval.singleton (mk_false range))

  let eval_ne lval n range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun bo flow ->
    man.eval n flow >>$ fun n flow ->
    match bo with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base,offset,mode) ->
      if not (is_interesting_base base) then
        Eval.singleton (mk_top T_bool range) flow
      else
        let true_post = assume_ne base offset mode lval.etyp n range man flow in
        let false_post = assume_eq base offset mode lval.etyp n range man (Flow.set_ctx (Cases.get_ctx true_post) flow) in
        Eval.join
          (true_post >>% Eval.singleton (mk_true range))
          (false_post >>% Eval.singleton (mk_false range))

  (** 𝔼⟦ ∃i ∈ [a,b]: *(p + i) == n ⟧ *)
  let eval_exists_eq i a b lval n range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun bo flow ->
    man.eval n flow >>$ fun n flow ->
    match bo with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base,offset,mode) ->
      if not (is_interesting_base base) then
        Eval.singleton (mk_top T_bool range) flow
      else
        Eval.join
          (assume_exists_eq i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_true range) flow)
          (assume_forall_ne i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_false range) flow)

  (** 𝔼⟦ ∀i ∈ [a,b] : *(p + i) == n ⟧ *)
  let eval_forall_eq i a b lval n range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun  bo flow ->
    man.eval n flow >>$ fun n flow ->
    match bo with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base,offset,mode) ->
      if not (is_interesting_base base) then
        Eval.singleton (mk_top T_bool range) flow
      else
        Eval.join
          (assume_forall_eq i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_true range) flow)
          (assume_exists_ne i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_false range) flow)

  (** 𝔼⟦ ∀i ∈ [a,b] : *(p + i) != n ⟧ *)
  let eval_forall_ne i a b lval n range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun  bo flow ->
    man.eval n flow >>$ fun n flow ->
    match bo with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base,offset,mode) ->
      if not (is_interesting_base base) then
        Eval.singleton (mk_top T_bool range) flow
      else
        Eval.join
          (assume_forall_ne i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_true range) flow)
          (assume_exists_eq i a b base offset mode lval.etyp n range man flow >>% fun flow -> Eval.singleton (mk_false range) flow)


  (** 𝔼⟦ ∀i ∈ [a,b] : *(p + i) == *(q + i) ⟧ *)
  let eval_forall_eq2 i a b lval1 lval2 range man flow =
    let ctype1 = (remove_casts lval1).etyp
    and ctype2 = (remove_casts lval2).etyp in
    let evl1 = eval_pointed_base_offset (mk_c_address_of lval1 range) range man flow in
    let evl2 = eval_pointed_base_offset (mk_c_address_of lval2 range) range man flow in

    evl1 >>$ fun bo1 flow ->
    match bo1 with
    | None -> Eval.singleton (mk_top T_bool range) flow
    | Some (base1,offset1,mode1) ->
      if not (is_interesting_base base1) then Eval.singleton (mk_top T_bool range) flow
      else
        evl2 >>$ fun bo2 flow ->
        match bo2 with
        | None -> Eval.singleton (mk_top T_bool range) flow
        | Some (base2,offset2,mode2) ->
          if not (is_interesting_base base2) then Eval.singleton (mk_top T_bool range) flow
          else
            Eval.join
              (assume_forall_eq2 i a b base1 offset1 mode1 ctype1 base2 offset2 mode2 ctype2 range man flow >>% fun flow -> Eval.singleton (mk_true range) flow)
              (assume_exists_ne2 i a b base1 offset1 mode1 ctype1 base2 offset2 mode2 ctype2 range man flow >>% fun flow -> Eval.singleton (mk_false range) flow)


  let eval exp man flow =
    match ekind exp with
    (* 𝔼⟦ *p ⟧ *)
    | E_c_deref p when under_type p.etyp |> void_to_char |> is_c_scalar_type ->
      eval_deref p exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ *(p + i) == n ⟧ *)
    | E_binop(O_eq, lval, n)
    | E_unop(O_log_not, { ekind = E_binop(O_ne, lval, n)})
      when is_c_int_type lval.etyp &&
           is_c_lval (remove_casts lval) &&
           is_c_int_type n.etyp
      ->
      eval_eq (remove_casts lval) n exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ *(p + i) != n ⟧ *)
    | E_binop(O_ne, lval, n)
    | E_unop(O_log_not, { ekind = E_binop(O_eq, lval, n)})
      when is_c_int_type lval.etyp &&
           is_c_lval (remove_casts lval) &&
           is_c_int_type n.etyp
      ->
      eval_ne (remove_casts lval) n exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ ∃i ∈ [a,b]: *(p + i) == n ⟧ *)
    | E_stub_quantified_formula([EXISTS,i,S_interval(a,b)], { ekind = E_binop(O_eq, lval, n) })
    | E_stub_quantified_formula([EXISTS,i,S_interval(a,b)], { ekind = E_unop(O_log_not, { ekind = E_binop(O_ne, lval, n)}) })
      when is_c_int_type lval.etyp &&
           is_c_lval (remove_casts lval) &&
           is_var_in_expr i lval &&
           not (is_var_in_expr i n)
      ->
      eval_exists_eq i a b (remove_casts lval) n exp.erange man flow |>
      OptionExt.return

    (* 𝕊⟦ ∀i ∈ [a,b] : *(p + i) == n ⟧ *)
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_binop(O_eq, lval, n) })
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_unop(O_log_not, { ekind = E_binop(O_ne, lval, n)}) })
      when is_c_int_type lval.etyp &&
           is_c_lval (remove_casts lval) &&
           is_var_in_expr i lval &&
           not (is_var_in_expr i n)
      ->
      eval_forall_eq i a b (remove_casts lval) n exp.erange man flow |>
      OptionExt.return

    (* 𝕊⟦ ∀i ∈ [a,b] : *(p + i) != n ⟧ *)
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_binop(O_ne, lval, n)})
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_unop(O_log_not, { ekind = E_binop(O_eq, lval, n)} )})
      when is_c_int_type lval.etyp &&
           is_c_lval (remove_casts lval) &&
           is_var_in_expr i lval &&
           not (is_var_in_expr i n)
      ->
      eval_forall_ne i a b (remove_casts lval) n exp.erange man flow |>
      OptionExt.return

    (* 𝕊⟦ ∀i ∈ [a,b]: *(p + i) == *(q + i) ⟧ *)
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_binop(O_eq, lval1, lval2)})
    | E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_unop(O_log_not, { ekind = E_binop(O_ne, lval1, lval2)} )})
      when is_c_int_type lval1.etyp &&
           is_c_lval (remove_casts lval1) &&
           is_c_int_type lval2.etyp &&
           is_c_lval (remove_casts lval2) &&
           is_var_in_expr i lval1 &&
           is_var_in_expr i lval2
      ->
      eval_forall_eq2 i a b (remove_casts lval1) (remove_casts lval2) exp.erange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_expr man flow printer exp  =
    let exp = remove_casts exp in
    (* Process only integer C lvalues *)
    if not (is_c_type exp.etyp && is_c_lval exp) then () else
    (* Iterate over bases and offsets *)
    resolve_pointer (mk_c_address_of exp exp.erange) man flow |>
    Cases.iter_result
      (fun pt _ ->
         match pt with
         | P_block(base,offset,_) when base.base_valid
                                    && is_interesting_base base ->
           let len = mk_length_var base elem_size exp.erange in
           pprint ~path:[Key "string-length"; Tail] printer
             (fbox
                "∀ i < %a: %a[i] ≠ 0 ∧ %a[%a] = 0"
                pp_expr len
                pp_base base
                pp_base base
                pp_expr len);
           man.print_expr flow printer len ~route:universal
         | _ -> ()
      )


end

let () =
  register_stateless_domain (module Domain)
