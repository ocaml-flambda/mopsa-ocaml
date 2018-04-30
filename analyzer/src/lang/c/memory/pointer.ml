(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of pointer arithmetic *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Stateful
open Framework.Manager
open Framework.Exceptions
open Framework.Ast
open Framework.Visitor
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Utils
open Universal.Ast
open Ast

let name = "c.memory.pointer"
let debug fmt = Debug.debug ~channel:name fmt


(** lv base *)
type base =
  | V of var
  | A of Universal.Ast.addr

let pp_base fmt = function
  | V v -> pp_var fmt v
  | A a -> Universal.Pp.pp_addr fmt a

let compare_base b b' = match b, b' with
  | V v, V v' -> compare_var v v'
  | A a, A a' -> Universal.Ast.compare_addr a a'
  | _ -> 1

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
  | E_p_invalid -> Format.pp_print_string fmt "Invalide"

type expr_kind +=
  | E_c_resolve_pointer of expr
  | E_c_points_to of pexpr

let mk_c_resolve_pointer e range =
  mk_expr (E_c_resolve_pointer e) range


module Domain =
struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)


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
      | F f    , F f'    -> compare f f'
      | B b    , B b'    -> compare_base b b'
      | Null   , Null    -> 0
      | Invalid, Invalid -> 0
      | _                -> 1
  end


  (** points-to set abstraction *)
  module PSL = struct
    include Framework.Lattices.Top_set.Make(P)
  end

  (** (cell -> pointsto lattice) lattice *)
  module CPML = Framework.Lattices.Total_map.Make(Var)(PSL)


  include CPML


  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let points_to_fun p f a =
    add p (PSL.singleton (P.F f)) a

  let points_to_base p b a =
    add p (PSL.singleton (P.B b)) a

  let points_to_var p v a =
    add p (PSL.singleton (P.B (V v))) a

  let points_to_null p a =
    add p (PSL.singleton P.Null) a

  let points_to_invalid p a =
    add p (PSL.singleton P.Invalid) a

  (*==========================================================================*)
  (**                         {2 Transfer functions}                          *)
  (*==========================================================================*)

  let mk_offset_var p =
    {vname = (var_uniq_name p) ^ "_offset"; vuid = 0; vkind = V_orig; vtyp = T_int}
    
  let init (man : ('a, t) manager) ctx prog (flow : 'a flow) =
    ctx, set_domain_cur top man flow

  let rec eval_p
      (man: ('a, t) manager) ctx
      (exp: expr)
      (flow: 'a flow)
    : (pexpr, 'a) evals option =
    let range = erange exp in
    debug "eval_p %a in@\n@[%a@]" pp_expr exp man.flow.print flow;
    match ekind exp with
    | E_constant (C_int n) when Z.equal n Z.zero ->
      oeval_singleton (Some E_p_null, flow, [])

    | E_addr addr ->
      let pt' = E_p_var (A addr, mk_int 0 range, T_c_void) in
      oeval_singleton (Some pt', flow, [])

    | E_var p when is_c_pointer_type p.vtyp ->
      debug "pointer var";
      let a = get_domain_cur man flow in
      let psl = find p a in

      if PSL.is_empty psl then oeval_singleton (None, flow, [])
      else
        PSL.fold (fun pt acc ->
            match pt with
            | P.F fundec ->
              oeval_singleton (Some (E_p_fun fundec), flow, []) |>
              oeval_join acc

            | P.B base ->
              let a = add p (PSL.singleton pt) a in
              let flow = set_domain_cur a man flow in
              let pt' = E_p_var (base, (mk_var (mk_offset_var p) range), under_pointer_type p.vtyp) in
              oeval_singleton (Some pt', flow, []) |>
              oeval_join acc

            | P.Null ->
              oeval_singleton (Some E_p_null, flow, []) |>
              oeval_join acc

            | P.Invalid ->
              oeval_singleton (Some E_p_invalid, flow, []) |>
              oeval_join acc

          ) psl None

    | E_var a when is_c_array_type a.vtyp ->
      debug "points to array var";
      let pt = E_p_var (V a, mk_int 0 range, under_array_type a.vtyp) in
      oeval_singleton (Some pt, flow, [])

    | E_c_address_of(e) when is_c_array_type e.etyp ->
      debug "address of an array";
      man.eval ctx e flow |>
      eval_compose (eval_p man ctx)

    | E_c_address_of(e) when is_c_function_type e.etyp ->
      debug "address of a function";
      man.eval ctx e flow |>
      eval_compose (eval_p man ctx)

    | E_c_address_of(e) ->
      debug "address of a non-array";
      man.eval ctx e flow |>
      eval_compose
        (fun e flow ->
           match ekind e with
           | E_var v when is_c_type v.vtyp ->
             let pt = E_p_var (V v, mk_zero (tag_range range "offset"), v.vtyp) in
             oeval_singleton (Some pt, flow, [])

           | _ -> assert false
        )

    | E_binop(Universal.Ast.O_plus, p, e) when is_c_pointer_type p.etyp || is_c_array_type p.etyp ->
      eval_p man ctx p flow |>
      oeval_compose
        (fun pt flow ->
           match pt with
           | E_p_fun fundec ->
             debug "pointer arithmetic on a pointer to a function";
             assert false
           | E_p_var (base, offset, t) ->
             let size = sizeof_type t in
             let pt = E_p_var (base, (mk_binop offset O_plus (mk_binop e O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int), t) in
             oeval_singleton (Some pt, flow, [])

           | E_p_null ->
             oeval_singleton (Some E_p_null, flow, [])

           | E_p_invalid ->
             oeval_singleton (Some E_p_invalid, flow, [])
        )

    | E_c_cast(p', _) ->
      eval_p man ctx p' flow |>
      oeval_compose
        (fun pt flow ->
           let pt =
             match pt with
             | E_p_var (base, offset, _) -> E_p_var(base, offset, under_pointer_type exp.etyp)
             | _ -> pt
           in
           oeval_singleton (Some pt, flow, [])
        )

    | E_c_function fundec ->
      oeval_singleton (Some (E_p_fun fundec), flow, [])

    | E_binop(Universal.Ast.O_minus, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      panic "Pointer substraction not supported"

    | _ ->
      man.eval ctx exp flow |>
      eval_compose
        (fun exp flow ->
           eval_p man ctx exp flow
        )
        ~empty:
          (fun _ ->
             panic "eval_p: unsupported expression %a in %a" pp_expr exp pp_range_verbose exp.erange
          )

  let rec exec man ctx stmt flow =
    debug "exec %a" pp_stmt stmt;
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      map_domain_cur (points_to_invalid p) man flow |>
      man.exec ctx (mk_remove_var (mk_offset_var p) range) |>
      return

    | S_c_local_declaration(p, Some (C_init_expr e)) when is_c_pointer_type p.vtyp ->
      let stmt' = mk_assign (mk_var p stmt.srange) e stmt.srange in
      exec man ctx stmt' flow

    | S_assign(p, q, k) ->
      debug "assign on type %a (is pointer = %b)" pp_typ p.etyp (is_c_pointer_type p.etyp);
      if not (is_c_pointer_type p.etyp) then None
      else
      eval_p man ctx q flow |>
      oeval_to_oexec
        (fun pt flow ->
           debug "assign pointer";
           match pt with
           | E_p_fun fundec ->
             man.eval ctx p flow |>
             eval_to_exec
               (fun p flow ->
                  let p = match p with
                    | {ekind = E_var p} -> p
                    | _ -> assert false
                  in
                  map_domain_cur (points_to_fun p fundec) man flow
               )
               (man.exec ctx) man.flow |>
             return

           | E_p_var (base, offset, t) ->
             man.eval ctx p flow |>
             eval_to_exec
               (fun p flow ->
                  let p = match p with
                    | {ekind = E_var p} -> p
                    | _ -> assert false
                  in
                  map_domain_cur (points_to_base p base) man flow |>
                  man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) offset range)
               )
               (man.exec ctx) man.flow |>
             return

           | E_p_null ->
             man.eval ctx p flow |>
             eval_to_exec
               (fun p flow ->
                  let p = match p with
                    | {ekind = E_var p} -> p
                    | _ -> assert false
                  in
                  map_domain_cur (points_to_null p) man flow |>
                  (* FIXME: this is not precise, but reduces the cases
                     where the offset becomes unbounded when joining
                     defined and undefined pointers *)
                  man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) range)
               )
               (man.exec ctx) man.flow |>
             return

           | E_p_invalid ->
             man.eval ctx p flow |>
             eval_to_exec
               (fun p flow ->
                  let p = match p with
                    | {ekind = E_var p} -> p
                    | _ -> assert false
                  in
                  map_domain_cur (points_to_invalid p) man flow |>
                  (* FIXME: this is not precise, but reduces the cases
                     where the offset becomes unbounded when joining
                     defined and undefined pointers *)
                  man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) range)
               )
               (man.exec ctx) man.flow |>
             return

        ) (man.exec ctx) man.flow

    | S_remove_var(p) when is_c_pointer_type p.vtyp ->
      map_domain_cur (remove p) man flow |>
      man.exec ctx (mk_remove_var (mk_offset_var p) range) |>
      return

    | _ -> None

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_c_resolve_pointer p ->
      man.eval ctx p flow |>
      eval_compose (eval_p man ctx) |>
      oeval_compose (fun pt flow ->
          let exp' = {exp with ekind = E_c_points_to pt} in
          oeval_singleton (Some exp', flow, [])
        )

    | E_binop(O_eq, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      oeval_list [p; q] (eval_p man ctx) flow |>
      oeval_compose
        (fun ptl flow ->
           match ptl with
           | [E_p_var (base1, offset1, t1); E_p_var (base2, offset2, t2)] ->
             if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0 then
               oeval_singleton (Some (mk_zero range), flow, [])
             else
               Universal.Utils.assume_to_eval
                 (mk_binop offset1 O_eq offset2 range ~etyp:T_int)
                 (fun true_flow -> oeval_singleton (Some (mk_one range), true_flow, []))
                 (fun false_flow -> oeval_singleton (Some (mk_zero range), false_flow, []))
                 ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_int_interval 0 1 range), flow, []))
                 man ctx flow ()

           | [E_p_null; E_p_null] -> oeval_singleton (Some (mk_one range), flow, [])

           | [E_p_var _; E_p_null] | [E_p_null; E_p_var _] -> oeval_singleton (Some (mk_zero range), flow, [])

           | _ -> assert false
        )

    | E_binop(O_ne, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      oeval_list [p; q] (eval_p man ctx) flow |>
      oeval_compose
        (fun ptl flow ->
           match ptl with
           | [E_p_var (base1, offset1, t1); E_p_var (base2, offset2, t2)] ->
             if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0 then
               oeval_singleton (Some (mk_one range), flow, [])
             else
               Universal.Utils.assume_to_eval
                 (mk_binop offset1 O_ne offset2 range ~etyp:T_int)
                 (fun true_flow -> oeval_singleton (Some (mk_one range), true_flow, []))
                 (fun false_flow -> oeval_singleton (Some (mk_zero range), false_flow, []))
                 ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_int_interval 0 1 range), flow, []))
                 man ctx flow ()

           | [E_p_null; E_p_null] -> oeval_singleton (Some (mk_zero range), flow, [])

           | [E_p_var _; E_p_null] | [E_p_null; E_p_var _] -> oeval_singleton (Some (mk_one range), flow, [])

           | _ -> assert false
        )

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain);
  Framework.Pp.register_pp_expr (fun next fmt exp ->
      match ekind exp with
      | E_c_resolve_pointer e -> Format.fprintf fmt "resolve %a" pp_expr e
      | E_c_points_to pe -> Format.fprintf fmt "points-to %a" pp_pexpr pe
      | _ -> next fmt exp
    );
  Framework.Visitor.register_expr_visitor (fun next exp ->
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
