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
open Framework.Domains.Global
open Framework.Manager
open Framework.Ast
open Framework.Visitor
open Framework.Pp
open Universal.Ast
open Ast
open Cell

let name = "c.memory.pointer"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** points-to elements *)
  module P =
  struct
    type t =
      | V of var (* points to a variable *)
      | Null                   (* Null pointer         *)
      | Invalid                (* Invalid pointer      *)
    let print fmt p = match p with
      | V v -> pp_var fmt v
      | Null -> Format.fprintf fmt "Null"
      | Invalid -> Format.fprintf fmt "Invalid"
    let compare p p' =
      match p, p' with
      | V x    , V y     -> compare_var x y
      | Null   , Null    -> 0
      | Invalid, Invalid -> 0
      | _                -> 1
  end


  (** points-to set abstraction *)
  module PSL = struct
    include Framework.Lattices.Top_set.Make(P)
  end

  (** (cell -> pointsto lattice) lattice *)
  module CPML = Framework.Utils.Total_var_map(PSL)


  include CPML


  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let add_var p v a =
    add p (PSL.singleton (P.V v)) a

  (*==========================================================================*)
  (**                         {2 Transfer functions}                          *)
  (*==========================================================================*)

  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    set_domain_cur top man flow

  let mk_offset_var p =
    let v = {vname = (var_uniq_name p) ^ "_offset"; vuid = 0; vkind = V_orig; vtyp = T_int} in
    {v with vkind = V_cell {v; o = Z.zero; t = v.vtyp}}

  let under_type base ptr =
    match remove_typedef base, ptr with
    | T_c_array(t, _), _ -> t
    | _, ptr -> Ast.under_pointer_type ptr

  let rec eval_p
      (exp: expr)
      (man: ('a, t) manager) ctx
      (flow: 'a flow)
    : ((var * expr), 'a flow) Eval.xeval_output =
    let range = erange exp in
    match ekind exp with
    | E_var p when is_c_pointer p.vtyp ->
      debug "eval_p %a in@\n@[%a@]" pp_var p man.flow.print flow;
      let a = get_domain_cur man flow in
      let psl = find p a in

      PSL.fold (fun pt acc ->
          match pt with
          | P.V base ->
            let a = add p (PSL.singleton pt) a in
            let flow = set_domain_cur a man flow in
            let pt' = base, (mk_var (mk_offset_var p) range) in
            Eval.xsingleton (Some pt', flow, []) |>
            Eval.xjoin acc

          | P.Null -> assert false
          | P.Invalid -> assert false
        ) psl Eval.xbottom

    | E_var a when is_c_array a.vtyp ->
      let pt = a, mk_int 0 range in
      Eval.xsingleton (Some pt, flow, [])

    | E_c_address_of(e) ->
      Eval.compose_xeval e
        (fun e flow ->
           match ekind e with
           | E_var {vkind = V_cell c} ->
             let pt = (c.v, mk_z c.o (tag_range range "offset")) in
             Eval.xsingleton (Some pt, flow, [])

           | E_var v when is_c_array v.vtyp ->
             let pt = v, mk_int 0 range in
             Eval.xsingleton (Some pt, flow, [])

           | E_var v when is_c_type v.vtyp ->
             let pt = (v, mk_zero (tag_range range "offset")) in
             Eval.xsingleton (Some pt, flow, [])

           | _ -> assert false
        )
        (fun flow -> Eval.xsingleton (None, flow, []))
        man ctx flow

    | E_binop(Universal.Ast.O_plus, p, e) when is_c_pointer p.etyp || is_c_array p.etyp ->
      Eval.compose_xeval p
        (fun p flow ->
           Eval.xcompose_xeval (eval_p p man ctx flow)
             (fun (base, offset) flow ->
                let size = sizeof_type (under_type base.vtyp p.etyp) in
                let pt = base, (mk_binop offset O_plus (mk_binop e O_mult (mk_z size range) range) range) in
                Eval.xsingleton (Some pt, flow, [])
             )
             (fun flow -> Eval.xsingleton (None, flow, []))
        )
        (fun flow -> Eval.xsingleton (None, flow, []))
        man ctx flow


    | E_binop(Universal.Ast.O_minus, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      assert false

    | _ -> panic "Unsupported expression %a in eval_p" pp_expr exp

  let exec stmt man ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_pointer v.vtyp ->
      let flow =
        match init with
        | None -> flow
        | Some (C_init_expr e) -> man.exec (mk_assign (mk_var v stmt.srange) e stmt.srange) ctx flow
        | Some (Ast.C_init_list (_,_)) -> assert false
        | Some (Ast.C_init_implicit _) -> assert false
      in
      Exec.return flow

    | S_assign(p, q, k) when is_c_pointer p.etyp ->
      Eval.xcompose_exec
        (eval_p q man ctx flow)
        (fun (v, offset) flow ->
           Eval.compose_exec p
             (fun p flow ->
                let p = match p with
                  | {ekind = E_var p} -> p
                  | _ -> assert false
                in
                map_domain_cur (add_var p v) man flow |>
                man.exec (mk_assign (mk_var (mk_offset_var p) range) offset range) ctx |>
                Exec.return
             )
             (fun flow -> Exec.return flow)
             man ctx flow
        )
        (fun flow -> Exec.return flow)
        man ctx

    | S_remove_var(p) when is_c_pointer p.vtyp ->
      let p, c = Cell.mk_base_cell p in
      map_domain_cur (remove p) man flow |>
      man.exec (mk_remove_var (mk_offset_var p) range) ctx |>
      man.exec (Cell.mk_remove_cell c stmt.srange) ctx |>
      Exec.return

    | _ -> None

  let eval exp man ctx flow =
    let range = exp.erange in
    match ekind exp with
    | E_c_deref(p) ->
      Eval.compose_eval p
        (fun p flow ->
           Eval.xcompose_eval
             (eval_p p man ctx flow)
             (fun (base, offset) flow ->
                let t = under_type base.vtyp p.etyp in
                let open Universal.Numeric.Integers in
                let itv = man.ask (Domain.Domain.QEval offset) ctx flow in
                match itv with
                | None -> assert false
                | Some itv ->
                  Value.fold (fun acc o ->
                      let c = mk_cell base o t in
                      let exp' = Cell.mk_gen_cell_var c range in
                      let evl = Eval.re_eval_singleton man ctx (Some exp', flow, []) in
                      Eval.join acc evl
                    ) None itv
             )
             (fun flow -> Eval.singleton (None, flow, []))
             man ctx
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | E_binop(O_eq, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      Eval.compose_eval_list [p; q]
        (fun el flow ->
           let p, q = match el with
             | [{ekind = E_var p}; {ekind = E_var q}] -> p, q
             | _ -> assert false
           in
           let a = get_domain_cur man flow in
           let psl1 = find p a in
           let psl2 = find q a in
           let psl = PSL.meet psl1 psl2 in
           if PSL.is_bottom psl then
             Eval.singleton (Some (mk_zero range), flow, [])
           else
           if PSL.cardinal psl = 1 then
             let a = add p psl a in
             let a = add q psl a in
             let flow = set_domain_cur a man flow in
             let true_flow = man.exec (mk_assume (mk_binop (mk_var (mk_offset_var p) range) O_eq (mk_var (mk_offset_var q) range) range) range) ctx flow in
             let false_flow = man.exec (mk_assume (mk_binop (mk_var (mk_offset_var p) range) O_ne (mk_var (mk_offset_var q) range) range) range) ctx flow in
             Eval.join
               (Eval.singleton (Some (mk_one range), true_flow, []))
               (Eval.singleton (Some (mk_zero range), false_flow, []))
           else
             Eval.singleton (Some (mk_int_interval 0 1 range), flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | E_binop(O_ne, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      Eval.compose_eval_list [p; q]
        (fun el flow ->
           let p, q = match el with
             | [{ekind = E_var p}; {ekind = E_var q}] -> p, q
             | _ -> assert false
           in
           let a = get_domain_cur man flow in
           let psl1 = find p a in
           let psl2 = find q a in
           let psl = PSL.meet psl1 psl2 in
           if PSL.is_bottom psl then
             Eval.singleton (Some (mk_one range), flow, [])
           else
             Eval.singleton (Some (mk_int_interval 0 1 range), flow, []) (* TODO: improve precision *)
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
