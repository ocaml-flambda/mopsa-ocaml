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

(** Desugar accesses to aggregates into accesses to scalar arrays using
    pointer arithmetics.

    This domains translates subscript accesses `a[i]` and field accesses `s.f`
    into dereferences of scalar pointers. This is useful for low-level memory
    abstractions to handle the full C language.
*)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Stubs.Ast
open Common.Base
open Common.Points_to


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.memory.aggregates"
    end)

  let dependencies = []

  let checks = []


  (** {2 Command-line options} *)
  (** ======================== *)

  (** Size threshold (in bytes) for using memset to initialize memory blocks
      instead of a sequence of assignments *)
  let opt_init_memset_threshold = ref 50

  let () = register_domain_option name {
      key  = "-c-init-memset-threshold";
      doc  = " Size threshold (in bytes) for using memset to initialize memory blocks instead of a sequence of assignments";
      spec = ArgExt.Set_int opt_init_memset_threshold;
      category = "C";
      default = string_of_int !opt_init_memset_threshold;
    }


  (** {2 Initialization procedure} *)
  (** ============================ *)

  let init _ _ flow = flow


  (** {2 Syntactic simplifications} *)
  (** ============================= *)

  (** a[i] -> *(a + i) *)
  let mk_lowlevel_subscript_access a i t range =
    mk_c_deref (mk_binop a O_plus i ~etyp:(pointer_type t) range) range

  (** 𝔼⟦ &(p->f) ⟧ -> ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
  let mk_lowlevel_field_address p i t range =
    let st = under_type p.etyp in
    let align = mk_int (align_byte st i) range in
    mk_c_cast
      (mk_binop
         (mk_c_cast p (pointer_type s8) range)
         O_plus
         align
         ~etyp:(pointer_type s8)
         range
      )
      (pointer_type t)
      range



  (** s.f -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let mk_lowlevel_member_access s i t range =
    let ss = mk_c_address_of s range in
    mk_c_deref (mk_lowlevel_field_address ss i t range) range

  (** 𝔼⟦ p->f ⟧ -> *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
  let mk_lowlevel_arrow_access p i t range =
    mk_c_deref (mk_lowlevel_field_address p i t range) range




  (** {2 Abstract transformers} *)
  (** ========================= *)

  (** The following functions flatten the initialization expression
      into a list of scalar initializations *)
  let rec flatten_init init offset typ range =
    if is_c_scalar_type typ then flatten_scalar_init init offset typ range else
    if is_c_array_type typ  then flatten_array_init init offset typ range else
    if is_c_record_type typ then flatten_record_init init offset typ range
    else panic_at ~loc:__LOC__ range
        "init %a of type %a not supported"
        (OptionExt.print Pp.pp_c_init) init pp_typ typ

  and flatten_scalar_init init offset typ range =
    match init with
    | None                 -> [],[(None,Z.one, offset, typ)]
    | Some (C_init_expr e) -> [(e,offset, typ)],[]
    (* a scalar initializer can be optionnaly enclosed in braces *)
    | Some (C_init_list ([C_init_expr e], _)) -> [(e,offset, typ)],[]
    | Some init -> panic_at range "unsupported scalar initializer %a for type %a" Pp.pp_c_init init pp_typ typ;

  and flatten_array_init init offset typ range =
    if is_c_no_length_array_type typ
    || is_c_variable_length_array_type typ
    then [],[] else

    let n = get_array_constant_length typ in
    let under_typ = under_array_type typ in
    match init with
    | None ->
      if is_c_scalar_type under_typ then
        [],[(None,n,offset,under_typ)]
      else
        let nn = Z.mul n (sizeof_type under_typ) in
        [],[(None,nn,offset,u8)]

    (* a string literal can be optionally enclosed in braces *)
    | Some (C_init_list ([C_init_expr {ekind = E_constant(C_c_string _); } as i], _))
         when is_c_int_array_type typ ->
       flatten_array_init (Some i) offset typ range

    | Some (C_init_list (l, filler)) ->
      let rec aux i l acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ)) in
        if Z.equal (Z.of_int i) n
        then acc1, acc2
        else
          match l with
          | elem::rest ->
            let l1,l2 = flatten_init (Some elem) o under_typ range in
            aux (i+1) rest (List.rev_append l1 acc1) (List.rev_append l2 acc2)
          | [] ->
            let remain = Z.sub n (Z.of_int i) in
            match filler with
            | None ->
              if is_c_scalar_type under_typ then
                acc1, (None,remain,o,under_typ)::acc2
              else
                let nn = Z.mul remain (sizeof_type under_typ) in
                acc1, (None,nn,o,u8)::acc2

            | Some (C_init_list([], Some (C_init_expr e)))
            | Some (C_init_expr e) ->
              acc1, (Some e, remain, o,under_typ)::acc2

            | Some (C_init_list(l, None)) ->
              aux i [C_init_list (l, None)] acc1 acc2

            | Some x ->
              panic_at range "initialization filler %a not supported" Pp.pp_c_init x
      in
      let l1,l2 = aux 0 l [] [] in
      List.rev l1, List.rev l2

    | Some (C_init_expr {ekind = E_constant(C_c_string (s, C_char_ascii)); etyp = t; erange}) ->
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ)) in
        if Z.equal (Z.of_int i) n
        then acc1, acc2

        else if i < String.length s
        then aux (i+1) ((mk_c_character (String.get s i) erange under_typ, o, under_typ)::acc1) acc2

        else if i = String.length s
        then aux (i+1) ((mk_c_character (char_of_int 0) erange under_typ, o, under_typ)::acc1) acc2

        else acc1,(None,Z.sub n (Z.of_int i), o, s8)::acc2
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    (* wide strings *)
    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _)); etyp = t; erange}) ->
      let char_size = sizeof_type under_typ in
      let nchar_size = Z.to_int char_size in
      let len = String.length s / nchar_size in
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) char_size) in
        if Z.equal (Z.of_int i) n
        then acc1,acc2

        else if i < len
        then aux (i+1) ((mk_c_multibyte_integer s (i * nchar_size) under_typ erange, o, under_typ)::acc1) acc2

        else if i = len
        then aux (i+1) ((mk_zero ~typ:under_typ erange, o, under_typ)::acc1) acc2

        else acc1,(None,Z.sub n (Z.of_int i), o, s8)::acc2
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    | Some (Ast.C_init_expr e) ->
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ)) in
        if Z.equal (Z.of_int i) n
        then acc1,acc2
        else
          let init' = Some (C_init_expr (mk_lowlevel_subscript_access e (mk_int i e.erange) under_typ e.erange)) in
          let l1,l2 = flatten_init init' o under_typ range in
          aux (i+1) (List.rev_append l1 acc1) (List.rev_append l2 acc2)
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    | _ -> panic_at range ~loc:__LOC__
             "flatten_array_init: %a is not supported"
             Pp.pp_c_init (OptionExt.none_to_exn init)


  and flatten_record_init init offset typ range =
    let fields =
      match remove_typedef_qual typ with
      | T_c_record{c_record_fields} -> c_record_fields
      | t -> panic_at ~loc:__LOC__ range "type %a is not a record" pp_typ t
    in
    let pp_record_field fmt f =
      Format.fprintf fmt "%s@+%d,%d:%a" f.c_field_name f.c_field_offset f.c_field_bit_offset pp_typ f.c_field_type in
    debug "fields of %a: %a" pp_typ typ (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_record_field) fields;
    match init with
    | None ->
      let rec aux acc1 acc2 = function
        | [] -> acc1,acc2
        | field :: tl ->
          let o = Z.add offset (Z.of_int field.c_field_offset) in
          let l1,l2 = flatten_init None o field.c_field_type range in
          aux (List.rev_append l1 acc1) (List.rev_append l2 acc2) tl
      in
      let l1,l2 = aux [] [] fields in
      List.rev l1, List.rev l2

    | Some (C_init_list(l, None)) ->
      let rec aux l acc1 acc2 records =
        match records with
        | [] -> acc1,acc2
        | field :: tl ->
          let o = Z.add offset (Z.of_int field.c_field_offset) in
          if is_c_bitfield field.c_field_type then
            (* skip all fields at that offset and put to T *)
            (* FIXME: use bitmasks to actually encode the real value *)
            let rec discard_same_offsets l r =
              match l, r with
              | lhd:: ltl, f :: rtl ->
                if f.c_field_offset = field.c_field_offset then
                  discard_same_offsets ltl rtl
                else
                  let (new_l, new_r) = discard_same_offsets ltl rtl in
                  lhd::new_l, f::new_r
              | [], [] -> [], []
              | _ -> assert false in
            let under_bitfield_typ = match field.c_field_type with
              | T_c_bitfield (t, _) -> t
              | _ -> assert false in
            let new_l, new_r = discard_same_offsets l records in
            let init_top = C_init_expr (mk_top under_bitfield_typ range) in
            let l1,l2 = flatten_init (Some init_top) o under_bitfield_typ range in
            aux new_l (List.rev_append l1 acc1) (List.rev_append l2 acc2) new_r
          else
            match l with
            | [] ->
              let l1,l2 = flatten_init None o field.c_field_type range in
              aux l (List.rev_append l1 acc1) (List.rev_append l2 acc2) tl
            | init :: tll ->
              let l1,l2 = flatten_init (Some init) o field.c_field_type range in
              aux tll (List.rev_append l1 acc1) (List.rev_append l2 acc2) tl
      in
      let l1,l2 = aux l [] [] fields in
      List.rev l1, List.rev l2

    | Some (C_init_expr e) when is_c_record_type e.etyp ->
      [(e,offset,typ)],[]


    | _ -> panic_at ~loc:__LOC__ range "initialization %a is not supported"
             Pp.pp_c_init (OptionExt.none_to_exn init)



  (** 𝕊⟦ type v = init; ⟧ *)
  let declare v init scope range man flow =
    let () = Debug.debug ~channel:"declare" "variable %a: %a" pp_var v pp_typ v.vtyp in
    (* (match init with
    | None -> Debug.debug ~channel:"declare" "no init provided"
    | Some (C_init_expr e) -> Debug.debug ~channel:"declare" "init expression %a" pp_expr e
    | Some (C_init_list _) -> Debug.debug ~channel:"declare" "init list"
    | Some (C_init_implicit ty) -> Debug.debug ~channel:"declare" "init impicit for %a" pp_typ ty
    ); *)
    man.exec (mk_add_var v range) flow >>% fun flow ->
    let initl,fill = flatten_init init Z.zero v.vtyp range in

    (* Scalar variables can be handed directly by the underlying low-level domain *)
    if is_c_scalar_type v.vtyp then
      match initl with
      | [e,o,t] ->
        let stmt = mk_assign (mk_var v range) e range in
        man.exec stmt flow

      | [] when is_c_global_scope scope ->
        let stmt = mk_assign (mk_var v range) (mk_zero ~typ:v.vtyp range) range in
        man.exec stmt flow

      | _ ->
        Post.return flow
    else
      (* Create a sequence of assignments for initializers *)
      List.fold_left
        (fun acc (e,o,t) ->
           (* Execute the assignment on the range of the expression *)
           let erange = e.erange in
           (* *(( t* )( char* )(&v) + o)) = e; *)
           let stmt = mk_assign (mk_c_deref (mk_c_cast
                                               (mk_binop
                                                  (mk_c_cast (mk_c_address_of (mk_var v erange) erange) (pointer_type s8) erange)
                                                  O_plus
                                                  (mk_z o erange)
                                                  ~etyp:(pointer_type s8) erange
                                               )
                                               (pointer_type t) erange
                                            ) erange) e erange
           in
           acc >>% man.exec stmt
        ) (Post.return flow) initl
      >>% fun flow ->

      (* For uninitialized parts, do nothing if the variable is local *)
      if not (is_c_global_scope scope) then
        Post.return flow
      else
        (* Global variable => initialize with zero *)
        fill |> List.fold_left
          (fun acc (filler, n, o, t) ->
             (* Initialisation expression *)
             let init =
               match filler with
               | Some e -> e
               | None -> mk_zero range (* global + uninitialized *)
             in
             let base =
               (* base : (char * )&v + o *)
               add
                 (mk_c_cast (mk_c_address_of (mk_var v range) range) (pointer_type s8) range)
                 (mk_z o range)
                 ~typ:(pointer_type s8) range
             in
             let bytes = Z.(n * sizeof_type t) in
             (* Use memset if the size is greater than the threshold *)
             if Z.(bytes >= of_int !opt_init_memset_threshold) then
               let i = mk_zero range in
               let j = mk_z Z.(pred bytes) range in
               memset base (mk_zero range) i j range man flow
             else
               (* Otherwise, use assignments *)
               let rec aux i acc =
                 if Z.(i = n) then acc
                 else
                   let stmt =
                     (* *(( t* )base + i) = init; *)
                     mk_assign
                       (mk_c_deref
                          (add
                             (mk_c_cast base (pointer_type t) range)
                             (mk_z i range) range) range)
                       init range
                   in
                   acc >>% fun flow ->
                   man.exec stmt flow |>
                   aux (Z.succ i)
               in
               aux Z.zero acc
          ) (Post.return flow)


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

      (* Get the fields to copy *)
      let fields =
        match record_kind with
        | C_struct -> fields
        | C_union ->
          (* In case of union get the field with the greatest size *)
          let fieldopt, _ = List.fold_left (fun (accfield, accsize) field ->
              let size = field.c_field_type |> sizeof_type in
              if Z.geq size accsize then
                (Some field, size)
              else (accfield, accsize)
            ) (None, Z.zero) fields
          in
          match fieldopt with
          | Some field -> [field]
          | None -> panic_at range "[%s] all fields have size 0" name
      in

      (* Now copy the fields *)
      fields |> List.fold_left (fun acc field ->
          let lval' = mk_c_member_access lval field range in
          let rval' = mk_c_member_access rval field range in
          match field.c_field_type |> remove_typedef_qual with

          | T_c_array(t,C_array_length_cst n) ->
            (* Copying cell-by-cell maybe expensive, so use memcpy instead *)
            acc >>% memcpy lval' rval' zero (mk_z Z.(n * sizeof_type t - one) range) range man

          | T_c_array _ ->
            (* Flexible array members are not copied (CC99 6.7.2.1.22) *)
            acc

          | _ ->
            let stmt = mk_assign lval' rval' range in
            acc >>% man.exec stmt
        ) (Post.return flow)


  let assign_array a i t e r range man flow =
    let lval = mk_lowlevel_subscript_access a i t r in
    let stmt = mk_assign lval e range in
    man.exec stmt flow

  let assign_member a i f t e r range man flow =
    let lval = mk_lowlevel_member_access a i t r in
    let stmt = mk_assign lval e range in
    man.exec stmt flow

  let assign_arrow a i f t e r range man flow =
    let lval = mk_lowlevel_arrow_access a i t r in
    let stmt = mk_assign lval e range in
    man.exec stmt flow

  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration(v, init, scope) ->
      declare v init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, e)
    | S_expression { ekind = E_c_assign (lval, e) } when is_c_record_type lval.etyp ->
      assign_record lval e stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ======================== *)

  let array_subscript a i t range = mk_lowlevel_subscript_access a i t range

  (** 𝔼⟦ s.f ⟧ -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let member_access s i f range = mk_lowlevel_member_access s i range

  (** 𝔼⟦ p->f ⟧ -> *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
  let arrow_access p i f t range = mk_lowlevel_arrow_access p i t range

  (** 𝔼⟦ &(a[i]) ⟧ = a + i *)
  let address_of_array_subscript a i t range = mk_binop a O_plus i ~etyp:t range

  (** 𝔼⟦ &(p->f) ⟧ = ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
  let address_of_arrow_access p i f t range =
    let st = under_type p.etyp in
    let align = mk_int (align_byte st i) range in
    mk_c_cast
      (mk_binop
         (mk_c_cast p (pointer_type s8) range)
         O_plus
         align
         range
      )
      (pointer_type t)
      range

  let eval exp man flow =
    match ekind exp with
    | E_c_array_subscript(a, i) ->
      man.eval (array_subscript a i exp.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_member_access (s, i, f) ->
      man.eval (member_access s i f exp.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_arrow_access(p, i, f) ->
      man.eval (arrow_access p i f exp.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_array_subscript(a,i)}) ->
      man.eval (add a i ~typ:a.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_member_access(s,i,f)} as e) ->
      man.eval (mk_lowlevel_field_address (mk_c_address_of s exp.erange) i e.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_arrow_access(p,i,f)} as e) ->
      man.eval (mk_lowlevel_field_address p i e.etyp exp.erange) flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
