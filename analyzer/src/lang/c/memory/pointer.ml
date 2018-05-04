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

  let add p psl mode a =
    let a' = add p psl a in
    match mode with
    | STRONG | EXPAND -> a'
    | WEAK -> join a a'

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let points_to_fun p f ?(mode=STRONG) a =
    add p (PSL.singleton (P.F f)) mode a

  let points_to_base p b ?(mode=STRONG) a =
    (match b with V {vkind = V_orig} -> () | V _ -> assert false | A _ -> ());
    add p (PSL.singleton (P.B b)) mode a

  let points_to_var p v ?(mode=STRONG) a =
    (match v.vkind with V_orig -> () | _ -> assert false);
    add p (PSL.singleton (P.B (V v))) mode a

  let points_to_null p ?(mode=STRONG) a =
    add p (PSL.singleton P.Null) mode a

  let points_to_invalid p ?(mode=STRONG) a =
    add p (PSL.singleton P.Invalid) mode a

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

    | E_constant C_c_invalid ->
      oeval_singleton (Some E_p_invalid, flow, [])

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
              let a = add p (PSL.singleton pt) STRONG a in
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

    | E_var ({vkind = V_orig} as a) when is_c_array_type a.vtyp ->
      let pt = E_p_var (V a, mk_zero range, under_array_type a.vtyp) in
      oeval_singleton (Some pt, flow, [])

    | E_var a when is_c_array_type a.vtyp ->
      debug "points to array var";
      (
        match man.ask ctx (Query.QExtractVarBase a) flow with
        | Some (b, o) ->
          let pt = E_p_var (b, o, under_array_type a.vtyp) in
          oeval_singleton (Some pt, flow, [])
        | None -> assert false
      )

    | E_c_address_of(e) when is_c_array_type e.etyp || is_c_function_type e.etyp ->
      debug "address of an array or a function";
      man.eval ctx e flow |>
      eval_compose (eval_p man ctx)

    | E_c_address_of(e) ->
      debug "other addresses";
      man.eval ctx e flow |>
      eval_compose
        (fun e flow ->
           match ekind e with
           | E_var v ->
             (
               debug "address of var %a" pp_var v;
               match man.ask ctx (Query.QExtractVarBase v) flow with
               | Some (b, o) ->
                 let pt = E_p_var (b, o, v.vtyp) in
                 oeval_singleton (Some pt, flow, [])

               | None ->
                 assert false
             )

           | _ -> Debug.fail "&(%a) not known" pp_expr e;
        )

    | E_binop(O_plus _, e1, e2) when
        ((is_c_pointer_type e1.etyp || is_c_array_type e1.etyp) && (is_c_int_type e2.etyp || is_int_type e2.etyp)) ||
        ((is_c_pointer_type e2.etyp || is_c_array_type e2.etyp) && (is_c_int_type e1.etyp || is_int_type e1.etyp))
      ->
      let p, e = if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp then e1, e2 else e2, e1 in
      man.eval ctx p flow |>
      eval_compose (eval_p man ctx) |>
      oeval_compose
        (fun pt flow ->
           match pt with
           | E_p_fun fundec ->
             debug "pointer arithmetic on a pointer to a function";
             assert false
           | E_p_var (base, offset, t) ->
             debug "pointer arithmetics: %a, %a, %a" pp_base base pp_expr offset pp_typ t;
             let size = sizeof_type t in
             let pt = E_p_var (base, (mk_binop offset math_plus (mk_binop e math_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int), t) in
             oeval_singleton (Some pt, flow, [])

           | E_p_null ->
             oeval_singleton (Some E_p_null, flow, [])

           | E_p_invalid ->
             oeval_singleton (Some E_p_invalid, flow, [])
        )

    | E_c_cast(p', _) ->
      man.eval ctx p' flow |>
      eval_compose (eval_p man ctx) |>
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

    | _ ->
      panic "eval_p: unsupported expression %a in %a" pp_expr exp pp_range_verbose exp.erange

  let rec exec man ctx stmt flow =
    debug "exec %a" pp_stmt stmt;
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      man.eval ctx (mk_var p range) flow |>
      eval_to_oexec
        (fun exp flow ->
           match ekind exp with
           | E_var p ->
             map_domain_cur (points_to_invalid p) man flow |>
             man.exec ctx (mk_remove_var (mk_offset_var p) range) |>
             return

           | _ -> assert false
        )
        (man.exec ctx) man.flow

    | S_assign({ekind = E_var p}, q, mode) when is_c_pointer_type p.vtyp ->
      eval_p man ctx q flow |>
      oeval_to_oexec
        (fun pt flow ->
           debug "assign pointer";
           match pt with
           | E_p_fun fundec ->
             map_domain_cur (points_to_fun p fundec) man flow |>
             return

           | E_p_var (base, offset, t) ->
             debug "pointer var = %a" pp_var p;
             map_domain_cur (points_to_base p ~mode base) man flow |>
             man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) offset ~mode range) |>
             return

           | E_p_null ->
             map_domain_cur (points_to_null ~mode p) man flow |>
             (* FIXME: this is not precise, but reduces the cases
                     where the offset becomes unbounded when joining
                     defined and undefined pointers *)
             man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) ~mode range) |>
             return

           | E_p_invalid ->
             map_domain_cur (points_to_invalid ~mode p) man flow |>
             (* FIXME: this is not precise, but reduces the cases
                     where the offset becomes unbounded when joining
                     defined and undefined pointers *)
             man.exec ctx (mk_assign (mk_var (mk_offset_var p) range) (mk_zero range) ~mode range) |>
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
      eval_list [p; q] (man.eval ctx) flow |>
      eval_compose (fun el flow -> oeval_list [p; q] (eval_p man ctx) flow) |>
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
      eval_list [p; q] (man.eval ctx) flow |>
      eval_compose (fun el flow -> oeval_list [p; q] (eval_p man ctx) flow) |>
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

    | E_binop(Universal.Ast.O_minus _, p, q) when is_c_pointer_type p.etyp && is_c_pointer_type q.etyp ->
      panic "Pointer substraction not supported"


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
  Framework.Pp.register_pp_constant (fun next fmt c ->
      match c with
      | C_c_invalid -> Format.fprintf fmt "invalid"
      | _ -> next fmt c
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
