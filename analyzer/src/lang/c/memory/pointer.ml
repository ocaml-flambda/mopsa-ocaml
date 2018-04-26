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


  (** Points-to evaluations *)
  type pexpr =
    | E_p_fun of c_fundec
    | E_p_var of base (** base *) * expr (** offset *) * typ (** type *)
    | E_p_null
    | E_p_invalid

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let add p pt a =
    CPML.add (Cell.annotate_var p) pt a

  let find p a =
    CPML.find (Cell.annotate_var p) a

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

  let init (man : ('a, t) manager) ctx prog (flow : 'a flow) =
    ctx, set_domain_cur top man flow

  let mk_offset_var p =
    let v = {vname = (var_uniq_name p) ^ "_offset"; vuid = 0; vkind = V_orig; vtyp = T_int} in
    {v with vkind = V_cell {b = V v; o = Z.zero; t = v.vtyp}}

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

    | E_var {vkind = V_cell c} when is_c_array_type c.t ->
      debug "points to array cell";
      let pt = E_p_var (c.b, mk_z c.o range, under_array_type c.t) in
      oeval_singleton (Some pt, flow, [])

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
           | E_var {vkind = V_cell c} ->
             let pt = E_p_var (c.b, mk_z c.o (tag_range range "offset"), c.t) in
             oeval_singleton (Some pt, flow, [])

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

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      map_domain_cur (points_to_invalid p) man flow |>
      man.exec ctx (mk_remove_var (mk_offset_var p) range) |>
      return

    | S_assign({ekind = E_c_deref _ | E_c_arrow_access _} as lval, rval, mode) ->
      man.eval ctx lval flow |>
      eval_to_oexec (fun lval' flow ->
          let stmt' = {stmt with skind = S_assign(lval', rval, mode)} in
          man.exec ctx stmt' flow |>
          return
        )
        (man.exec ctx) man.flow


    | S_assign(p, q, k) when is_c_pointer_type p.etyp ->
      eval_p man ctx q flow |>
      oeval_to_oexec
        (fun pt flow ->
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
    | E_c_deref(p) ->
      eval_p man ctx p flow |>
      oeval_compose
        (fun pt flow ->
           match pt with
           | E_p_fun fundec ->
             oeval_singleton (Some ({exp with ekind = E_c_function fundec}), flow, [])
           | E_p_var (base, offset, t) ->
             debug "E_p_var(%a, %a, %a)" pp_base base pp_expr offset pp_typ t;
             let itv = man.ask ctx (Universal.Numeric.Query.QIntList offset) flow in
             begin
               match itv with
               | None -> assert false
               | Some itv ->
                 List.fold_left (fun acc o ->
                     let c = {Cell.b = base; o; t} in
                     let exp' = Cell.mk_gen_cell_var c range in
                     man.eval ctx exp' flow |>
                     eval_compose
                       (fun exp' flow ->
                          oeval_join acc (oeval_singleton (Some exp', flow, []))
                       )
                   ) None itv
             end

           | E_p_null ->
             let flow = man.flow.add (Alarms.TNullDeref exp.erange) (man.flow.get TCur flow) flow |>
                        man.flow.set TCur man.env.Framework.Lattice.bottom
             in
             oeval_singleton (None, flow, [])


           | E_p_invalid ->
             let flow = man.flow.add (Alarms.TInvalidDeref exp.erange) (man.flow.get TCur flow) flow |>
                        man.flow.set TCur man.env.Framework.Lattice.bottom
             in
             oeval_singleton (None, flow, [])
        )

    | E_c_arrow_access(p, i, f) ->
      eval_p man ctx p flow |>
      oeval_compose
        (fun pt flow ->
           match pt with
           | E_p_var (base, offset, t) ->
             let record = match remove_typedef t with T_c_record r -> r | _ -> assert false in
             let field = List.nth record.c_record_fields i in
             let itv = man.ask ctx (Universal.Numeric.Query.QIntList offset) flow in
             begin
               match itv with
               | None -> assert false
               | Some itv ->
                 List.fold_left (fun acc o ->
                     let o' = Z.add o (Z.of_int field.c_field_offset) in
                     let c = {Cell.b = base; o = o'; t = field.c_field_type} in
                     let exp' = Cell.mk_gen_cell_var c range in
                     man.eval ctx  exp' flow |>
                     eval_compose
                       (fun exp' flow ->
                          oeval_join acc (oeval_singleton (Some exp', flow, []))
                       )
                   ) None itv
             end
           | E_p_fun _ ->
             debug "arrow access to function pointers";
             assert false
           | E_p_null ->
             let flow = man.flow.add (Alarms.TNullDeref exp.erange) (man.flow.get TCur flow) flow |>
                        man.flow.set TCur man.env.Framework.Lattice.bottom
             in
             oeval_singleton (None, flow, [])

           | E_p_invalid ->
             let flow = man.flow.add (Alarms.TInvalidDeref exp.erange) (man.flow.get TCur flow) flow |>
                        man.flow.set TCur man.env.Framework.Lattice.bottom
             in
             oeval_singleton (None, flow, [])
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
  register_domain name (module Domain)
