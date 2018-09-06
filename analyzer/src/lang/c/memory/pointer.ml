(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of pointer arithmetic *)

open Framework.Essentials
open Universal.Ast
open Ast
open Base

let name = "c.memory.pointer"
let debug fmt = Debug.debug ~channel:name fmt

(** Points-to evaluations *)
type pexpr =
  | E_p_fun of c_fundec
  | E_p_var of base (** base *) * expr (** offset *) * typ (** type *)
  | E_p_null
  | E_p_invalid

let pp_pexpr fmt = function
  | E_p_fun f -> Format.fprintf fmt "<fp %a>" pp_var f.c_func_var
  | E_p_var(base, offset, typ) -> Format.fprintf fmt "<%a, %a, %a>" pp_base base pp_expr offset pp_typ typ
  | E_p_null -> Format.pp_print_string fmt "NULL"
  | E_p_invalid -> Format.pp_print_string fmt "Invalid"

type expr_kind +=
  | E_c_resolve_pointer of expr
  | E_c_points_to of pexpr

type constant +=
  | C_c_invalid (** invalid pointer constant *)

let mk_c_resolve_pointer e range =
  mk_expr (E_c_resolve_pointer e) range

let mk_c_invalid range =
  mk_constant C_c_invalid range ~etyp:(T_c_pointer(T_c_void))

(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

module Domain : Framework.Domain.DOMAIN =
struct

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [
    ];
    import = [
    ];
  }
  let eval_interface = {
    export = [
    ];
    import = [
    ];
  }
  (** points-to elements *)
  module P =
  struct
    type t =
      | F of c_fundec          (* points to a function *)
      | B of base              (* points to a variable *)
      | Null                   (* Null pointer         *)
      | Invalid                (* Invalid pointer      *)
    let print fmt p = match p with
      | F f -> Format.fprintf fmt "%a" pp_var f.c_func_var
      | B b -> pp_base fmt b
      | Null -> Format.fprintf fmt "Null"
      | Invalid -> Format.fprintf fmt "Invalid"
    let compare p p' =
      match p, p' with
      | F f    , F f'    -> compare_var f.c_func_var f'.c_func_var
      | B b    , B b'    -> compare_base b b'
      | _                -> compare p p'
  end


  (** points-to set abstraction *)
  module PSL = struct
    include Framework.Lattices.Top_set.Make(P)
  end

  (** (cell -> pointsto lattice) lattice *)
  module CPML = Framework.Lattices.Total_map.Make(Var)(PSL)


  include CPML

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_memory_pointer : t domain
  let id = D_c_memory_pointer
  let name = "c.memory.pointer"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_memory_pointer -> Some Eq
    | _ -> None

  (** Tool functions *)
  (** ============== *)


  let debug fmt = Debug.debug ~channel:name fmt

  let add annot p psl mode a =
    let a' = add p psl a in
    match mode with
    | STRONG | EXPAND -> a'
    | WEAK -> join annot a a'

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let points_to_fun annot p f ?(mode=STRONG) a =
    add annot p (PSL.singleton (P.F f)) mode a

  let points_to_base annot p b ?(mode=STRONG) a =
    (* (match b with V {vkind = V_orig} -> () | V _ -> assert false | A _ -> ()); *)
    add annot p (PSL.singleton (P.B b)) mode a

  let points_to_var annot p v ?(mode=STRONG) a =
    (* (match v.vkind with V_orig -> () | _ -> assert false); *)
    add annot p (PSL.singleton (P.B (V v))) mode a

  let points_to_null annot p ?(mode=STRONG) a =
    add annot p (PSL.singleton P.Null) mode a

  let points_to_invalid annot p ?(mode=STRONG) a =
    add annot p (PSL.singleton P.Invalid) mode a

  let mk_offset_var p =
    {vname = (Var.var_uniq_name p) ^ "_offset"; vuid = 0; vtyp = T_int}

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    None

  (** Transfer functions *)
  (** ================== *)

  let rec eval_p
      man
      (exp: expr)
      (flow: 'a flow)
    : ('a, pexpr) evl =

    let range = erange exp in
    debug "eval_p %a in@\n@[%a@]" pp_expr exp (Flow.print man) flow;

    match ekind exp with
    | E_constant (C_int n) when Z.equal n Z.zero ->
      Eval.singleton E_p_null flow

    | E_constant C_c_invalid ->
      Eval.singleton E_p_invalid flow

    | E_addr addr ->
      let pt' = E_p_var (A addr, mk_int 0 range, T_c_void) in
      Eval.singleton pt' flow

    | E_var p when is_c_pointer_type p.vtyp ->
      let annot = Flow.get_annot flow in
      debug "pointer var";
      let a = Flow.get_domain_cur man flow in
      let psl = find p a in

      if PSL.is_empty psl then Eval.empty_singleton flow
      else if PSL.is_top psl then  Eval.singleton E_p_invalid flow
      else PSL.fold (fun pt acc ->
          let a = add annot p (PSL.singleton pt) STRONG a in
          let flow = Flow.set_domain_cur a man flow in
          match pt with
          | P.F fundec ->
            Eval.singleton (E_p_fun fundec) flow |>
            Eval.join acc

          | P.B base ->
            let pt' = E_p_var (base, (mk_var (mk_offset_var p) range), under_pointer_type p.vtyp) in
            Eval.singleton pt' flow |>
            Eval.join acc

          | P.Null ->
            Eval.singleton E_p_null flow |>
            Eval.join acc

          | P.Invalid ->
            Eval.singleton E_p_invalid flow |>
            Eval.join acc

        ) psl Eval.empty

    | E_var a when is_c_array_type a.vtyp ->
      let pt = E_p_var (V a, mk_zero range, under_array_type a.vtyp) in
      Eval.singleton pt flow

    | E_c_address_of(e) when is_c_array_type e.etyp || is_c_function_type e.etyp ->
      debug "address of an array or a function";
      man.eval e flow |> Eval.bind @@ eval_p man

    | E_c_address_of({ekind = E_c_array_subscript(e1, e2)} as e) ->
      let pt = pointer_type (etyp e) in
      eval_p man
        (mk_binop
           ({ekind = E_c_cast(e1,false); etyp = pt; erange = tag_range range "t0"})
           O_plus
           e2
           (tag_range range "t1")
        )
        flow

    | E_c_address_of({ekind = E_c_member_access (e, i, f)}) ->
      let pt = pointer_type (T_c_integer C_signed_char) in
      let record = match remove_typedef e.etyp with T_c_record r -> r | _ -> assert false in
      let field = List.nth record.c_record_fields i in
      let offset' = mk_int field.c_field_offset exp.erange in
      eval_p man
        (mk_binop
           ({ekind = E_c_cast(e,false); etyp = pt; erange = tag_range range "t0"})
           (O_plus)
           offset'
           (tag_range range "t1")
        )
        flow

    | E_c_address_of({ekind = E_c_deref (e)}) ->
      eval_p man e flow

    | E_binop(O_plus, e1, e2) when
        ((is_c_pointer_type e1.etyp || is_c_array_type e1.etyp) && (is_c_int_type e2.etyp || is_int_type e2.etyp)) ||
        ((is_c_pointer_type e2.etyp || is_c_array_type e2.etyp) && (is_c_int_type e1.etyp || is_int_type e1.etyp))
      ->
      let p, e = if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp then e1, e2 else e2, e1 in
      man.eval p flow |> Eval.bind @@ eval_p man |> Eval.bind @@ (fun pt flow ->
          match pt with
          | E_p_fun fundec ->
            debug "pointer arithmetic on a pointer to a function";
            assert false
          | E_p_var (base, offset, t) ->
            debug "pointer arithmetics: %a, %a, %a" pp_base base pp_expr offset pp_typ t;
            let size = sizeof_type t in
            let pt = E_p_var (
                base,
                (mk_binop
                   offset
                   O_plus
                   (mk_binop
                      e
                      O_mult
                      (mk_z size range)
                      range ~etyp:T_int
                   ) range ~etyp:T_int
                ), t
              ) in
            Eval.singleton pt flow

          | E_p_null ->
            Eval.singleton E_p_null flow

          | E_p_invalid ->
            Eval.singleton E_p_invalid flow
        )
    | E_c_cast(p', _) ->
      man.eval p' flow |> Eval.bind @@ eval_p man |> Eval.bind @@ (fun pt flow ->
          let pt =
            match pt with
            | E_p_var (base, offset, _) -> E_p_var(base, offset, under_pointer_type exp.etyp)
            | _ -> pt
          in
          Eval.singleton pt flow
        )

    | E_c_function fundec ->
      Eval.singleton (E_p_fun fundec) flow

    | E_c_points_to pexpr ->
      Eval.singleton pexpr flow

    | _ ->
      let exp' = mk_c_resolve_pointer exp (tag_range range "t0") in
      let () = debug "calling on other domain to eval_p %a" pp_expr exp' in
      man.eval exp' flow |> Eval.bind @@ fun e flow ->
      match ekind e with
      | E_c_points_to pe -> Eval.singleton pe flow
      | _ -> panic "eval_p: unsupported expression %a in %a" pp_expr exp pp_range_verbose exp.erange


  let rec exec (zone: Framework.Zone.zone) stmt (man: ('a, t) Framework.Manager.man) (flow :'a flow) : 'a Post.post option=
    debug "exec %a" pp_stmt stmt;
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      begin
        man.eval (mk_var p range) flow |> Post.bind man @@ fun exp flow ->
        match ekind exp with
        | E_var p ->
          Flow.map_domain_cur (points_to_invalid (Flow.get_annot flow) p) man flow |>
          man.exec (mk_remove_var (mk_offset_var p) range) |>
          Post.of_flow
        | _ -> Debug.fail "in %a exec, case [S_c_local_declaration], \
                           evaluation yielded a non variable"
                 Format.pp_print_string name
      end |> Option.return

    | S_assign({ekind = E_var p}, q, mode) when is_c_pointer_type p.vtyp ->
      begin
        eval_p man q flow |> Post.bind man @@ fun pt flow ->
        debug "assign pointer";
        let annot = Flow.get_annot flow in
        match pt with
        | E_p_fun fundec ->
          Flow.map_domain_cur (points_to_fun annot p fundec) man flow |>
          Post.of_flow

        | E_p_var (base, offset, t) ->
          debug "pointer var = %a" pp_var p;
          Flow.map_domain_cur (points_to_base annot p ~mode base) man flow |>
          man.exec (mk_assign (mk_var (mk_offset_var p) range) offset ~mode range) |>
          Post.of_flow

        | E_p_null ->
          Flow.map_domain_cur (points_to_null annot ~mode p) man flow |>
          (* FIXME: this is not precise, but reduces the cases
                  where the offset becomes unbounded when joining
                  defined and undefined pointers *)
          man.exec (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) ~mode range) |>
          Post.of_flow

        | E_p_invalid ->
          Flow.map_domain_cur (points_to_invalid annot ~mode p) man flow |>
          (* FIXME: this is not precise, but reduces the cases
                  where the offset becomes unbounded when joining
                  defined and undefined pointers *)
          man.exec (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) ~mode range) |>
          Post.of_flow
      end |> Option.return

    | S_remove_var(p) when is_c_pointer_type p.vtyp ->
      Flow.map_domain_cur (remove p) man flow |>
      man.exec (mk_remove_var (mk_offset_var p) range) |>
      Post.return

    | _ -> None

  let eval zones exp man flow : 'a option=
    let range = exp.erange in
    match ekind exp with
    | E_c_resolve_pointer p ->
      begin
        eval_p man p flow |> Eval.bind @@ fun pt flow ->
        let exp' = {exp with ekind = E_c_points_to pt} in
        Eval.singleton exp' flow
      end
      |> Option.return

    | E_binop(O_eq, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      begin
        Eval.eval_list [p; q] man.eval flow |> Eval.bind @@ fun el flow ->
        Eval.eval_list el (eval_p man) flow |> Eval.bind @@ fun ptl flow ->
        match ptl with
        | [E_p_var (base1, offset1, t1); E_p_var (base2, offset2, t2)] ->
          if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0
          then
            Eval.singleton (mk_zero range) flow
          else
            Eval.assume
              (mk_binop offset1 O_eq offset2 range ~etyp:T_int)
              ~fthen:(fun true_flow -> Eval.singleton (mk_one range) true_flow)
              ~felse:(fun false_flow -> Eval.singleton (mk_zero range)false_flow)
              ~fboth:(fun _ _ -> Eval.singleton (mk_int_interval 0 1 range) flow)
              man flow
        | [E_p_null; E_p_null] -> Eval.singleton (mk_one range) flow

        | [E_p_invalid; _] | [_; E_p_invalid] ->
          (* FIXME: maybe detect an error here? *)
          Eval.singleton (mk_int_interval 0 1 range) flow

        | _ -> Eval.singleton (mk_zero range) flow
      end |> Option.return

    | E_binop(O_ne, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      begin
        Eval.eval_list [p; q] man.eval flow |> Eval.bind @@ fun el flow ->
        Eval.eval_list el (eval_p man) flow |> Eval.bind @@ fun ptl flow ->
        match ptl with
        | [E_p_var (base1, offset1, t1); E_p_var (base2, offset2, t2)] ->
          if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0 then
            Eval.singleton (mk_one range) flow
          else
            Eval.assume
              (mk_binop offset1 O_ne offset2 range ~etyp:T_int)
              ~fthen:(fun true_flow -> Eval.singleton (mk_one range) true_flow)
              ~felse:(fun false_flow -> Eval.singleton (mk_zero range) false_flow)
              ~fboth:(fun _ _ -> Eval.singleton (mk_int_interval 0 1 range) flow)
              man flow

        | [E_p_null; E_p_null] ->
          Eval.singleton (mk_zero range) flow

        | [E_p_invalid; _] | [_; E_p_invalid] ->
          (* FIXME: maybe detect an error here? *)
          Eval.singleton (mk_int_interval 0 1 range) flow

        | _ ->
          Eval.singleton (mk_one range) flow
      end |> Option.return

    | E_binop(Universal.Ast.O_minus, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp ->
      panic "Pointer substraction not supported"

    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  register_pp_expr (fun next fmt exp ->
      match ekind exp with
      | E_c_resolve_pointer e -> Format.fprintf fmt "resolve %a" pp_expr e
      | E_c_points_to pe -> Format.fprintf fmt "points-to %a" pp_pexpr pe
      | _ -> next fmt exp
    );
  register_pp_constant (fun next fmt c ->
      match c with
      | C_c_invalid -> Format.fprintf fmt "invalid"
      | _ -> next fmt c
    );
  Framework.Visitor.register_expr_visitor (fun next exp ->
      let open Framework.Visitor in
      match ekind exp with
      | E_c_resolve_pointer e ->
        {exprs = [e]; stmts = []},
        (function
          | {exprs = [e]} -> {exp with ekind = E_c_resolve_pointer e}
          | _ -> assert false
        )
      | E_c_points_to _ -> leaf exp
      | _ -> next exp
    )
