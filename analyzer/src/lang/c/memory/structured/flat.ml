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


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  let name = "c.memory.structured.flat"

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = [Z_c_low_level]
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [Z_c, Z_c_low_level]
    };
  }


  (** {2 Initialization procedure} *)
  (** ============================ *)

  let init _ _ flow = flow


  (** {2 Abstract transformers} *)
  (** ========================= *)

  (** The following functions flatten the initialization expression into a list of scalar initializations *)
  let rec flatten_init init typ range =
    if is_c_scalar_type typ then flatten_scalar_init init typ range else
    if is_c_array_type typ  then flatten_array_init init typ range else
    if is_c_record_type typ then flatten_record_init init typ range
    else panic_at ~loc:__LOC__ range
        "init %a of type %a not supported"
        (Option.print Pp.pp_c_init) init pp_typ typ

  and flatten_scalar_init init typ range =
    match init with
    | None                 -> [C_flat_none (sizeof_type typ)]
    | Some (C_init_expr e) -> [C_flat_expr (e,typ)]
    | _ -> assert false

  and flatten_array_init init typ range =
    let n = get_array_constant_length typ in
    let under_typ = under_array_type typ in
    match init with
    | None -> [C_flat_none (Z.mul n (sizeof_type under_typ))]

    | Some (C_init_list (l, filler)) ->
      let rec aux i =
        if Z.equal (Z.of_int i) n
        then [] else
        if i < List.length l
        then flatten_init (Some (List.nth l i)) under_typ range @ aux (i + 1)
        else
          let nn = Z.mul (Z.sub n (Z.of_int i)) (sizeof_type under_typ) in
          match filler with
          | None ->
            [C_flat_none nn]

          | Some (C_init_list([], Some (C_init_expr e)))
          | Some (C_init_expr e) ->
            [C_flat_fill (e, under_typ, nn)]

          | Some x -> panic_at range "initialization filler %a not supported" Pp.pp_c_init x
      in
      aux 0

    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _))}) ->
      let rec aux i =
        if Z.equal (Z.of_int i) n then []
        else
          let init =
            if i < String.length s
            then C_flat_expr (mk_c_character (String.get s i) range, under_typ)
            else C_flat_expr (mk_c_character (char_of_int 0) range, under_typ)
          in
          init :: aux (i + 1)
      in
      aux 0

    | _ -> panic_at range ~loc:__LOC__
             "flatten_array_init: %a is not supported"
             Pp.pp_c_init (Option.none_to_exn init)


  and flatten_record_init init typ range =
    let records =
      match remove_typedef_qual typ with
      | T_c_record{c_record_fields} -> c_record_fields
      | t -> panic_at ~loc:__LOC__ range "type %a is not a record" pp_typ t
    in
    match init with
    | None ->
      let rec aux = function
        | [] -> []
        | record :: tl ->
          let init = flatten_init None record.c_field_type range in
          init @ aux tl
      in
      aux records

    | Some (C_init_list(l, None)) ->
      let rec aux l records =
        match records with
        | [] -> []
        | record :: tl ->
          match l with
          | [] ->
            let init = flatten_init None record.c_field_type range in
            init @ aux l tl
          | init :: tll ->
            let init = flatten_init (Some init) record.c_field_type range in
            init @ aux tll tl
      in
      aux l records

    | _ -> panic_at ~loc:__LOC__ range "%a is not supported"
             Pp.pp_c_init (Option.none_to_exn init)



  (** 𝕊⟦ type v = init; ⟧ *)
  let declare v init range man flow =
    let flat_init = flatten_init init v.vtyp range in

    (* Evaluate C expressions into low-level expressions *)
    let rec aux l flow =
      match l with
      | [] -> Eval.singleton [] flow
      | C_flat_expr (e,t) :: tl ->
        man.eval ~zone:(Z_c,Z_c_low_level) e flow |>
        Eval.bind @@ fun e flow ->

        aux tl flow |>
        Eval.bind @@ fun tl flow ->

        Eval.singleton (C_flat_expr (e,t) :: tl) flow

      | x :: tl ->
        aux tl flow |>
        Eval.bind @@ fun tl flow ->

        Eval.singleton (x :: tl) flow
    in

    aux flat_init flow |>
    post_eval man @@ fun init_list flow ->

    let init = C_init_flat init_list in
    man.exec_sub ~zone:Z_c_low_level (mk_c_declaration v (Some init) range) flow


  (** 𝕊⟦ lval = e; ⟧ *)
  let assign lval e range man flow =
    man.eval ~zone:(Z_c,Z_c_low_level) lval flow |>
    post_eval man @@ fun lval flow ->

    man.eval ~zone:(Z_c,Z_c_low_level) e flow |>
    post_eval man @@ fun e flow ->

    let stmt = mk_assign lval e range in
    man.exec_sub ~zone:Z_c_low_level stmt flow

  (** 𝕊⟦ ?e ⟧ *)
  let assume e range man flow =
    man.eval ~zone:(Z_c,Z_c_low_level) e flow |>
    post_eval man @@ fun e flow ->

    let stmt = mk_assume e range in
    man.exec_sub ~zone:Z_c_low_level stmt flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration(v, init) -> declare v init stmt.srange man flow |> Option.return
    | S_assign(lval, e) -> assign lval e stmt.srange man flow |> Option.return
    | S_assume(e) -> assume e stmt.srange man flow |> Option.return
    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ======================== *)

  (** 𝔼⟦ a[i] ⟧ -> *(a + i) *)
  let array_subscript a i exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) a flow |>
    Eval.bind @@ fun a flow ->
    man.eval ~zone:(Z_c, Z_c_low_level) i flow |> Eval.bind @@ fun i flow ->
    let t = exp |> etyp |> Ast.pointer_type in
    let exp' = mk_c_deref (mk_binop a O_plus i ~etyp:t range) range in

    Eval.singleton exp' flow

  (** 𝔼⟦ s.f ⟧ -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let member_access s i f exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) s flow |>
    Eval.bind @@ fun s flow ->

    let ss = mk_c_address_of s range in

    let st = etyp s in
    let t = etyp exp in
    let align = mk_int (align_byte st i) range in

    let exp' =
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
    in
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

  (** 𝔼⟦ &*x ⟧ = x *)
  let address_of_deref x exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) x flow

  (** 𝔼⟦ *&x ⟧ = x *)
  let deref_address_of x exp range man flow =
    man.eval ~zone:(Z_c, Z_c_low_level) x flow


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

    | E_c_address_of { ekind = E_c_deref x } ->
      address_of_deref x exp exp.erange man flow |>
      Option.return

    | E_c_deref { ekind = E_c_address_of x } ->
      deref_address_of x exp exp.erange man flow |>
      Option.return

    | E_c_assign(lval, rval) ->
      man.eval rval ~zone:(Z_c, Z_c_low_level) flow |>
      Eval.bind_some @@ fun rval flow ->

      let flow = man.exec (mk_assign lval rval exp.erange) flow in
      Eval.singleton rval flow

    | E_c_statement {skind = S_block l} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (erange exp) in
          let flow' = man.exec stmt' flow in
          man.eval ~zone:(Z_c, Z_c_low_level) e flow' |>
          Option.return

        | _ ->
          panic "E_c_statement %a" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval ~zone:(Z_c, Z_c_low_level) e flow |>
      Option.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Stacked.Stateless.register_stack (module Domain)
