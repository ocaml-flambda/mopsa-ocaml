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

(** This abstract domain destructs the structured C memory into a flat memory
    containing only scalars.
*)

open Mopsa
open Framework.Core.Sig.Stacked.Stateless
open Universal.Ast
open Ast
open Zone
open Universal.Zone


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.memory.structured.flat"
    end)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = [
        Z_c_low_level;
        Z_c
      ]
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [
        Z_c, Z_c_low_level;
        Z_c, Z_c_scalar;
        Z_c, Z_u_num
      ]
    };
  }


  (** {2 Initialization procedure} *)
  (** ============================ *)

  let init _ _ flow = flow


  (** {2 Syntactic simplifications} *)
  (** ============================= *)

  (** a[i] -> *(a + i) *)
  let mk_lowlevel_subscript_access a i t range =
    mk_c_deref (mk_binop a O_plus i ~etyp:(pointer_type t) range) range


  (** s.f -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let mk_lowlevel_member_access s i range =
    let ss = mk_c_address_of s range in

    let st = etyp s in
    let field =
      match remove_typedef_qual st with
      | T_c_record r -> List.nth r.c_record_fields i
      | _ -> panic "member access on type %a" pp_typ st;
    in
    let t = field.c_field_type in
    let align = mk_int (align_byte st i) range in

    mk_c_deref
      (mk_c_cast
         (mk_binop
            (mk_c_cast ss (pointer_type s8) range)
            O_plus
            align
            range
         )
         (pointer_type t)
         range
      )
      range



  (** {2 Abstract transformers} *)
  (** ========================= *)

  (** The following functions flatten the initialization expression
      into a list of scalar initializations *)
  let rec flatten_init init typ range =
    debug "flatten_init: %a (%a)" (Option.print Pp.pp_c_init) init pp_typ typ;
    if is_c_scalar_type typ then flatten_scalar_init init typ range else
    if is_c_array_type typ  then flatten_array_init init typ range else
    if is_c_record_type typ then flatten_record_init init typ range
    else panic_at ~loc:__LOC__ range
        "init %a of type %a not supported"
        (Option.print Pp.pp_c_init) init pp_typ typ

  and flatten_scalar_init init typ range =
    match init with
    | None                 -> [C_flat_none (Z.one, typ)]
    | Some (C_init_expr e) -> [C_flat_expr (e,typ)]
    | Some init -> panic_at range "unsupported scalar initializer %a for type %a" Pp.pp_c_init init pp_typ typ;

  and flatten_array_init init typ range =
    let n = get_array_constant_length typ in
    let under_typ = under_array_type typ in
    match init with
    | None ->
      if is_c_scalar_type under_typ then
        [C_flat_none (n,under_typ)]
      else
        let nn = Z.mul n (sizeof_type under_typ) in
        [C_flat_none (nn,u8)]

    | Some (C_init_list (l, filler)) ->
      let rec aux i =
        if Z.equal (Z.of_int i) n
        then [] else
        if i < List.length l
        then flatten_init (Some (List.nth l i)) under_typ range @ aux (i + 1)
        else
          let remain = Z.sub n (Z.of_int i) in
          match filler with
          | None ->
            if is_c_scalar_type under_typ then
              [C_flat_none (remain,under_typ)]
            else
              let nn = Z.mul remain (sizeof_type under_typ) in
              [C_flat_none (nn,u8)]

          | Some (C_init_list([], Some (C_init_expr e)))
          | Some (C_init_expr e) ->
            [C_flat_fill (e, under_typ, remain)]

          | Some x -> panic_at range "initialization filler %a not supported" Pp.pp_c_init x
      in
      aux 0

    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _))}) ->
      let rec aux i =
        if Z.equal (Z.of_int i) n
        then []

        else if i < String.length s
        then C_flat_expr (mk_c_character (String.get s i) range, under_typ) :: aux (i + 1)

        else if i = String.length s
        then C_flat_expr (mk_c_character (char_of_int 0) range, under_typ) :: aux (i + 1)

        else [C_flat_none (Z.sub n (Z.of_int i), s8)]
      in
      aux 0

    | Some (Ast.C_init_expr e) ->
      let rec aux i =
        if Z.equal (Z.of_int i) n
        then []
        else
          let init' = Some (C_init_expr (mk_lowlevel_subscript_access e (mk_int i range) under_typ range)) in
          flatten_init init' under_typ range @ aux (i + 1)
      in
      aux 0

    | _ -> panic_at range ~loc:__LOC__
             "flatten_array_init: %a is not supported"
             Pp.pp_c_init (Option.none_to_exn init)


  and flatten_record_init init typ range =
    let fields =
      match remove_typedef_qual typ with
      | T_c_record{c_record_fields} -> c_record_fields
      | t -> panic_at ~loc:__LOC__ range "type %a is not a record" pp_typ t
    in
    match init with
    | None ->
      let rec aux = function
        | [] -> []
        | field :: tl ->
          let init = flatten_init None field.c_field_type range in
          init @ aux tl
      in
      aux fields

    | Some (C_init_list(l, None)) ->
      let rec aux l records =
        match records with
        | [] -> []
        | field :: tl ->
          match l with
          | [] ->
            let init = flatten_init None field.c_field_type range in
            init @ aux l tl
          | init :: tll ->
            let init = flatten_init (Some init) field.c_field_type range in
            init @ aux tll tl
      in
      aux l fields

    | Some (C_init_expr e) when is_c_record_type e.etyp ->
      (* Remove unnecessary casts in e *)
      let e =
        let rec aux ee =
          match ekind ee with
          | E_c_cast (eee, false) -> eee
          | _ -> ee
        in
        aux e
      in

      let fields' = match remove_typedef_qual typ with
        | T_c_record{c_record_fields} -> c_record_fields
        | _ -> assert false
      in
      fields' |> List.fold_left (fun acc field ->
          let init = Some (C_init_expr (mk_lowlevel_member_access e field.c_field_index range)) in
          acc @ flatten_init init field.c_field_type range
        ) []


    | _ -> panic_at ~loc:__LOC__ range "initialization %a is not supported"
             Pp.pp_c_init (Option.none_to_exn init)


  let to_lowlevel_expr e man flow =
    (* Check if e is a constant *)
    match c_expr_to_z e with
    | Some z ->
      Result.singleton (mk_z z ~typ:e.etyp e.erange) flow

    | None ->
      (* Expression not simplified statically to a constant, so
         rely on other domains to evaluate it 
      *)
      man.eval ~zone:(Z_c,Z_c_low_level) e flow


  (** Evaluate init expressions into low-level expressions *)
  let rec to_lowlevel_init_opt init range man flow =
    match init with
    | None ->
      Result.singleton None flow

    | Some init ->
      to_lowlevel_init init range man flow >>$ fun init flow ->
      Result.singleton (Some init) flow

  and to_lowlevel_init init range man flow =
    match init with
    | C_init_expr e ->
      to_lowlevel_expr e man flow >>$ fun e flow ->
      Result.singleton (C_init_expr e) flow

    | C_init_list(l, filler) ->
      Result.bind_list l
        (fun init flow -> to_lowlevel_init init range man flow) flow
      >>$ fun l flow ->
      to_lowlevel_init_opt filler range man flow >>$ fun filler flow ->
      Result.singleton (C_init_list(l, filler)) flow

    | _ -> panic_at range
             "initialization expression %a not supported"
             Pp.pp_c_init init


  (** 𝕊⟦ type v = init; ⟧ *)
  let declare v init scope range man flow =
    to_lowlevel_init_opt init range man flow >>$ fun init flow ->
    let flat_init = flatten_init init v.vtyp range in
    let init' = C_init_flat flat_init in
    man.post ~zone:Z_c_low_level (mk_c_declaration v (Some init') scope range) flow


  (** 𝕊⟦ lval = e; ⟧ when lval is scalar *)
  let assign_scalar lval e range man flow =
    man.eval ~zone:(Z_c,Z_c_low_level) lval flow >>$ fun lval flow ->
    man.eval ~zone:(Z_c,Z_c_low_level) e flow >>$ fun e flow ->

    let stmt = mk_assign lval e range in
    man.post ~zone:Z_c_low_level stmt flow


  (** 𝕊⟦ lval = rval; ⟧ when lval is a record *)
  let assign_record lval rval range man flow =
    let rval = remove_casts rval in

    let t1 = lval |> etyp |> remove_typedef_qual
    and t2 = rval |> etyp |> remove_typedef_qual in

    if compare_typ t1 t2 <> 0 then
      panic_at range "[%s] assignment of records with uncompatible \
                      types: %a %a" name pp_typ t1 pp_typ t2
    else
      let fields, record_kind = match t1 with
        | T_c_record{c_record_fields; c_record_kind} -> c_record_fields, c_record_kind
        | _ -> assert false
      in
      match record_kind with
      | C_struct ->
        fields |> List.fold_left (fun acc field ->
            let lval = mk_c_member_access lval field range in
            let rval = mk_c_member_access rval field range in
            if is_c_array_type field.c_field_type
            then begin
              Soundness.warn range "copy of array %a ignored" pp_expr rval;
              acc
            end
            else
              let stmt = mk_assign lval rval range in
              Post.bind (man.post ~zone:Z_c stmt) acc
          ) (Post.return flow)

      | C_union ->
        let fieldopt, _ = List.fold_left (fun (accfield, accsize) field ->
            let size = field.c_field_type |> sizeof_type in
            if Z.geq size accsize then
              (Some field, size)
            else (accfield, accsize)
          ) (None, Z.zero) fields
        in
        match fieldopt with
        | Some field ->
          let lval = mk_c_member_access lval field range in
          let rval = mk_c_member_access rval field range in
          let stmt = mk_assign lval rval range in
          man.post ~zone:Z_c stmt flow

        | None -> panic_at range "[%s] all fields have size 0" name


  (** 𝕊⟦ ?e ⟧ *)
  let assume e range man flow =
    man.eval ~zone:(Z_c,Z_c_low_level) e flow >>$ fun e flow ->
    let stmt = mk_assume e range in
    man.post ~zone:Z_c_low_level stmt flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration(v, init, scope) ->
      declare v init scope stmt.srange man flow |>
      Option.return

    | S_assign(lval, e)
    | S_expression { ekind = E_c_assign (lval, e) } when is_c_scalar_type lval.etyp ->
      assign_scalar lval e stmt.srange man flow |>
      Option.return

    | S_assign(lval, e)
    | S_expression { ekind = E_c_assign (lval, e) } when is_c_record_type lval.etyp ->
      assign_record lval e stmt.srange man flow |>
      Option.return

    | S_assume(e) ->
      assume e stmt.srange man flow |>
      Option.return

    | S_expression e when is_c_num_type e.etyp ->
      Some (
        man.eval ~zone:(Z_c,Z_u_num) e flow >>$ fun e flow ->
        Post.return flow
      )

    | S_expression e when is_c_scalar_type e.etyp ->
      Some (
        man.eval ~zone:(Z_c,Z_c_scalar) e flow >>$ fun e flow ->
        Post.return flow
      )

    | S_expression e when is_c_type e.etyp ->
      Some (
        man.eval ~zone:(Z_c,Z_c_low_level) e flow >>$ fun e flow ->
        Post.return flow
      )

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ======================== *)

  let array_subscript a i exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) a flow |>
    Eval.bind @@ fun a flow ->
    man.eval ~zone:(Z_c, Z_c_low_level) i flow |> Eval.bind @@ fun i flow ->
    let exp' = mk_lowlevel_subscript_access a i exp.etyp range in
    Eval.singleton exp' flow

  (** 𝔼⟦ s.f ⟧ -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let member_access s i f exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) s flow |>
    Eval.bind @@ fun s flow ->
    let exp' = mk_lowlevel_member_access s i range in
    Eval.singleton exp' flow


  (** 𝔼⟦ p->f ⟧ -> *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
  let arrow_access p i f exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) p flow |>
    Eval.bind @@ fun p flow ->

    let st = under_pointer_type p.etyp in
    let t = etyp exp in
    let align = mk_int (align_byte st i) range in

    let exp' =
      mk_c_deref
        (mk_c_cast
           (mk_binop
              (mk_c_cast p (pointer_type s8) range)
              O_plus
              align
              range
           )
           (pointer_type t)
           range
        )
        range
    in
    Eval.singleton exp' flow


  (** 𝔼⟦ &(a[i]) ⟧ = a + i *)
  let address_of_array_subscript a i exp range man flow =
      man.eval ~zone:(Z_c, Z_c_low_level) a flow |>
      Eval.bind @@ fun a flow ->

      man.eval ~zone:(Z_c, Z_c_low_level) i flow |>
      Eval.bind @@ fun i flow ->

      let exp' = { exp with ekind = E_binop(O_plus, a, i) } in
      Eval.singleton exp' flow


  (** 𝔼⟦ &(p->f) ⟧ = ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
  let address_of_arrow_access p i f exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) p flow |>
    Eval.bind @@ fun p flow ->

    let st = under_pointer_type p.etyp in
    let t = etyp exp in
    let align = mk_int (align_byte st i) range in

    let exp' =
      mk_c_cast
        (mk_binop
           (mk_c_cast p (pointer_type s8) range)
           O_plus
           align
           range
        )
        (pointer_type t)
        range
    in
    Eval.singleton exp' flow


  let eval zone exp man flow =
    match ekind exp with
    | E_c_array_subscript(a, i) ->
      array_subscript a i exp exp.erange man flow |>
      Option.return

    | E_c_member_access (s, i, f) ->
      member_access s i f exp exp.erange man flow |>
      Option.return

    | E_c_arrow_access(p, i, f) ->
      arrow_access p i f exp exp.erange man flow |>
      Option.return

    | E_c_address_of { ekind = E_c_array_subscript(a,i) } ->
      address_of_array_subscript a i exp exp.erange man flow |>
      Option.return

    | E_c_address_of { ekind = E_c_arrow_access(p, i, f) } ->
      address_of_arrow_access p i f exp exp.erange man flow |>
      Option.return

    | E_c_assign(lval, rval) ->
      man.eval rval ~zone:(Z_c, Z_c_low_level) flow >>$? fun rval flow ->
      man.eval lval ~zone:(Z_c, Z_c_low_level) flow >>$? fun lval flow ->
      let flow = man.exec ~zone:Z_c_low_level (mk_assign lval rval exp.erange) flow in
      Eval.singleton rval flow |>
      Option.return

    | E_c_statement {skind = S_block l} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (erange exp) in
          let flow' = man.exec stmt' flow in
          man.eval ~zone:(Z_c, Z_c_low_level) e flow' |>
          Option.return

        | _ -> panic "E_c_statement %a not supported" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval ~zone:(Z_c, Z_c_low_level) e flow |>
      Option.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Stacked.Stateless.register_stack (module Domain)
