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
open Framework.Utils
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

  let rec eval_p
      (exp: expr)
      (man: ('a, t) manager) ctx
      (flow: 'a flow)
    : ((var * expr * typ), 'a) evals option =
    if man.flow.is_cur_bottom flow then
      oeval_singleton (None, flow, [])
    else
    let range = erange exp in
    debug "eval_p %a in@\n@[%a@]" pp_expr exp man.flow.print flow;
    match ekind exp with
    | E_var p when is_c_pointer p.vtyp ->
      debug "pointer var";
      let a = get_domain_cur man flow in
      let psl = find p a in

      PSL.fold (fun pt acc ->
          match pt with
          | P.V base ->
            let a = add p (PSL.singleton pt) a in
            let flow = set_domain_cur a man flow in
            let pt' = base, (mk_var (mk_offset_var p) range), under_pointer_type p.vtyp in
            oeval_singleton (Some pt', flow, []) |>
            oeval_join acc

          | P.Null -> assert false
          | P.Invalid -> assert false
        ) psl None

    | E_var {vkind = V_cell c} when is_c_array c.t ->
      debug "points to array cell";
      let pt = c.v, mk_z c.o range, under_array_type c.t in
      oeval_singleton (Some pt, flow, [])

    | E_var a when is_c_array a.vtyp ->
      debug "points to array var";
      let pt = a, mk_int 0 range, under_array_type a.vtyp in
      oeval_singleton (Some pt, flow, [])
  
    | E_c_address_of(e) when is_c_array e.etyp ->
      debug "address of an array";
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           eval_p e man ctx flow
        )

    | E_c_address_of(e) ->
      debug "address of a non-array";
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           match ekind e with
           | E_var {vkind = V_cell c} ->
             let pt = (c.v, mk_z c.o (tag_range range "offset"), c.t) in
             oeval_singleton (Some pt, flow, [])

           | E_var v when is_c_type v.vtyp ->
             let pt = (v, mk_zero (tag_range range "offset"), v.vtyp) in
             oeval_singleton (Some pt, flow, [])

           | _ -> assert false
        )

    | E_binop(Universal.Ast.O_plus, p, e) when is_c_pointer p.etyp || is_c_array p.etyp ->
      man.eval p ctx flow |>
      eval_compose
        (fun p flow ->
           eval_p p man ctx flow |>
           oeval_compose
             (fun (base, offset, t) flow ->
                let size = sizeof_type t in
                let pt = base, (mk_binop offset O_plus (mk_binop e O_mult (mk_z size range) range) range), t in
                oeval_singleton (Some pt, flow, [])
             )
        )


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
      return flow

    | S_assign(p, q, k) when is_c_pointer p.etyp ->
      man.eval q ctx flow |>
      eval_compose (fun q flow ->
          eval_p q man ctx flow
        ) |>
      oeval_to_exec
        (fun (v, offset, t) flow ->
                man.eval p ctx flow |>
                eval_to_exec
                  (fun p flow ->
                     let p = match p with
                       | {ekind = E_var p} -> p
                       | _ -> assert false
                     in
                     map_domain_cur (add_var p v) man flow |>
                     man.exec (mk_assign (mk_var (mk_offset_var p) range) offset range) ctx |>
                     return
                  )
                  man ctx
        ) man ctx

    | S_remove_var(p) when is_c_pointer p.vtyp ->
      let p, c = Cell.mk_base_cell p in
      map_domain_cur (remove p) man flow |>
      man.exec (mk_remove_var (mk_offset_var p) range) ctx |>
      man.exec (Cell.mk_remove_cell c stmt.srange) ctx |>
      return

    | _ -> None

  let eval exp man ctx flow =
    let range = exp.erange in
    match ekind exp with
    | E_c_deref(p) ->
      man.eval p ctx flow |>
      eval_compose
        (fun p flow ->
           eval_p p man ctx flow |>
           oeval_compose
             (fun (base, offset, t) flow ->
                let open Universal.Numeric.Integers in
                let itv = man.ask (Domain.Domain.QEval offset) ctx flow in
                match itv with
                | None -> assert false
                | Some itv ->
                  Value.fold (fun acc o ->
                      let c = mk_cell base o t in
                      let exp' = Cell.mk_gen_cell_var c range in
                      man.eval exp' ctx flow |>
                      eval_compose
                      (fun exp' flow ->
                        oeval_join acc (oeval_singleton (Some exp', flow, []))
                      )
                    ) None itv
             )
        )

    | E_c_arrow_access(p, i, f) ->
      man.eval p ctx flow |>
      eval_compose
        (fun p flow ->
           eval_p p man ctx flow |>
           oeval_compose
             (fun (base, offset, t) flow ->
                let record = match remove_typedef t with T_c_record r -> r | _ -> assert false in
                let field = List.nth record.c_record_fields i in
                let open Universal.Numeric.Integers in
                let itv = man.ask (Domain.Domain.QEval offset) ctx flow in
                match itv with
                | None -> assert false
                | Some itv ->
                  Value.fold (fun acc o ->
                      let o' = Z.add o (Z.of_int field.c_field_offset) in
                      let c = mk_cell base o' field.c_field_type in
                      let exp' = Cell.mk_gen_cell_var c range in
                      man.eval exp' ctx flow |>
                      eval_compose
                      (fun exp' flow ->
                        oeval_join acc (oeval_singleton (Some exp', flow, []))
                      )
                    ) None itv
             )
        )


    | E_binop(O_eq, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      man_eval_list [p; q] man ctx flow |>
      oeval_compose
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
             oeval_singleton (Some (mk_zero range), flow, [])
           else
           if PSL.cardinal psl = 1 then
             let a = add p psl a in
             let a = add q psl a in
             let flow = set_domain_cur a man flow in
             let true_flow = man.exec (mk_assume (mk_binop (mk_var (mk_offset_var p) range) O_eq (mk_var (mk_offset_var q) range) range) range) ctx flow in
             let false_flow = man.exec (mk_assume (mk_binop (mk_var (mk_offset_var p) range) O_ne (mk_var (mk_offset_var q) range) range) range) ctx flow in
             oeval_join
               (oeval_singleton (Some (mk_one range), true_flow, []))
               (oeval_singleton (Some (mk_zero range), false_flow, []))
           else
             oeval_singleton (Some (mk_int_interval 0 1 range), flow, [])
        )

    | E_binop(O_ne, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      man_eval_list [p; q] man ctx flow |>
      oeval_compose
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
             oeval_singleton (Some (mk_one range), flow, [])
           else
             oeval_singleton (Some (mk_int_interval 0 1 range), flow, []) (* TODO: improve precision *)
        )

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
