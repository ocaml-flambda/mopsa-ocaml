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



type expr_kind +=
  | E_c_pointer of var (** base *) * expr (** offset *)

let () =
  (** Pretty-printer *)
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_pointer(v, e) ->
        Format.fprintf fmt "@(%a + %a)" pp_var v pp_expr e
      | _ -> default fmt expr
    );
  (** Visitors *)
  register_expr_visitor ( fun default exp ->
      match ekind exp with
      | E_c_pointer(v,e) ->
        {exprs = [e] ; stmts = []},
        (fun parts -> {exp with ekind = E_c_pointer(v,List.hd parts.exprs)})
      | _ -> default exp
    )



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

  let mk_offset_var p range =
    let v = {vname = (var_uniq_name p) ^ "_offset"; vuid = 0; vkind = V_orig; vtyp = T_int} in
    let v = {v with vkind = V_cell {v; o = 0; t = v.vtyp}} in
    mk_var v range

  let eval_p exp f man ctx flow =
    let range = erange exp in
    match ekind exp with
    (* | E_var p when is_c_pointer p.vtyp ->
     *   Eval.singleton (Some exp, flow, [])
     * 
     * | E_c_address_of(e) ->
     *   Eval.compose_eval e
     *     (fun e flow ->
     *        match ekind e with
     *        | E_var {vkind = V_cell c} ->
     *          let exp' = {exp with ekind = E_c_pointer (c.v, mk_int c.o (tag_range range "offset"))} in
     *          Eval.singleton (Some exp', flow, [])
     * 
     *        | E_var v when is_c_type v.vtyp ->
     *          let exp' = {exp with ekind = E_c_pointer (v, mk_zero (tag_range range "offset"))} in
     *          Eval.singleton (Some exp', flow, [])
     * 
     *        | _ -> assert false
     *     )
     *     (fun flow -> Eval.singleton (None, flow, []))
     *     man ctx flow
     * 
     * | E_binop(Universal.Ast.O_plus, p, e) when is_c_pointer p.etyp ->
     *   assert false
     * 
     * | E_binop(Universal.Ast.O_minus, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
     *   assert false *)

    | _ -> assert false

  let exec stmt man ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign(p, q, k) when is_c_pointer p.etyp ->
      eval_p q
        (fun v offset flow ->
           Eval.compose_exec p
             (fun p flow ->
                let p =
                  match p with
                  | {ekind = E_var p} -> p
                  | _ -> assert false
                in
                map_domain_cur (add_var p v) man flow |>
                man.exec (mk_assign (mk_offset_var p range) offset range) ctx |>
                Exec.return
             )
             (fun flow -> Exec.return flow)
        )
        man ctx flow
        
    | _ -> None
      
  let eval exp man ctx flow =
    match ekind exp with
    | E_var p when is_c_pointer p.vtyp ->
      Eval.singleton (Some exp, flow, [])
      
    | E_c_deref(p) ->
      assert false

    | E_binop(Universal.Ast.O_eq, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      assert false
        
    | E_binop(Universal.Ast.O_ne, p, q) when is_c_pointer p.etyp && is_c_pointer q.etyp ->
      assert false

    | _ -> None
      
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
