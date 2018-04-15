(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell abstraction of low-level C memory operations. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Global
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Framework.Lattice
open Framework.Utils
open Ast


let name = "c.memory.cell"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                              {2 Cells}                                  *)
(*==========================================================================*)

(** Memory cells. *)
type cell = {
  v: var; (** base variable *)
  o: Z.t; (** offset *)
  t: typ; (** type *)
}

let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%a,%a⟩"
    pp_var c.v
    Z.pp_print c.o
    pp_typ c.t

let compare_cell c c' =
  compare_composer [
    (fun () -> compare_var c.v c'.v);
    (fun () -> Z.compare c.o c'.o);
    (fun () -> compare (remove_typedef c.t) (remove_typedef c'.t)); (* TODO replace this compare by a real comparison function over C_AST.type_qual*)
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_cell of cell

type expr_kind +=
  | E_c_gen_cell_var of cell

type stmt_kind +=
  | S_c_remove_cell of cell

let mk_cell v o t = {v = v; o = o; t}

let mk_gen_cell_var c range =
  mk_expr (E_c_gen_cell_var c) ~etyp:c.t range

let mk_remove_cell c range =
  mk_stmt (S_c_remove_cell c) range

let mk_base_cell v =
  let c = mk_cell v Z.zero v.vtyp in
  {v with vkind = V_cell c}, c

let mk_cell_var v =
  match vkind v with
  | V_cell c -> v, c
  | _ -> mk_base_cell v

let annotate_var v = fst @@ mk_cell_var v

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Make(ValAbs : DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** Set of cells variables. *)
  module CS = Framework.Lattices.Top_set.Make(Framework.Utils.Var)

  (** Type of an abstract environment. *)
  type t = {
    cs : CS.t; (* set of cells *)
    a  : ValAbs.t (* abstract numeric environment *)
  }

  (** Pretty printer. *)
  let print fmt x =
    Format.fprintf fmt "cells: @[%a@]@\n@[%a@]"
      CS.print x.cs
      ValAbs.print x.a

  let mem_pred c = function {vkind = V_cell c'} -> compare_cell c c' = 0 | _ -> false

  let mem_cell c cs =
    CS.exists (mem_pred c) cs

  let find_cell c cs =
    CS.filter (mem_pred c) cs |>
    CS.choose

  let exist_and_find_cell f cs =
    let exception Found of var * cell in
    try
      let () = CS.iter (function ({vkind = V_cell c} as v) -> if f c then raise (Found (v, c)) else () | _ -> ()) cs in
      None
    with
    | Found (v, c) -> Some (v, c)

  (*==========================================================================*)
  (**                            {2 Managers}                                 *)
  (*==========================================================================*)

  (** [subman man] lifts a manager [man] defined on [t] to a sub-layer manager
      defined on the numeric abstraction [ValAbs.t]. *)
  let subman : ('a, t) manager -> ('a, ValAbs.t) manager =
    fun man ->
    {
      man with
      ax =
        {
          get = (fun x -> (man.ax.get x).a);
          set = (fun y x -> man.ax.set {(man.ax.get x) with a = y} x)
        }
    }

  (** Manager of the sub-domain limited to a local scope. Useful for unifying
      numeric invariants when applying point-wise lattice operations such as [join]. *)
  let rec local_subman =
    let env_manager = Framework.Domains.Global.mk_lattice_manager (module ValAbs : DOMAIN with type t = ValAbs.t) in
    {
      env = env_manager;
      flow = Framework.Flow.lift_lattice_manager env_manager;
      exec = (fun stmt ctx flow -> match ValAbs.exec stmt local_subman ctx flow with Some flow -> flow | None -> assert false);
      eval = (fun exp ctx flow -> match ValAbs.eval exp local_subman ctx flow with Some evl -> evl | None -> eval_singleton (Some exp, flow, []) );
      ask = (fun query ctx flow -> assert false);
      ax = {
        get = (fun env -> env);
        set = (fun env' env -> env');
      }
    }

  (** Execute a statement on [ValAbs] using the local scope manager. *)
  let valabs_trivial_exec (stmt : stmt) (a : ValAbs.t) : ValAbs.t =
    set_domain_cur a local_subman local_subman.flow.bottom |>
    local_subman.exec stmt Framework.Context.empty |>
    local_subman.flow.get TCur

  (*==========================================================================*)
  (**                          {2 Unification}                                *)
  (*==========================================================================*)

  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c : cell) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u.cs in
    match exist_and_find_cell (fun c' -> compare_cell c' c = 0) cs with
    | Some (v', c') ->
      Nexp (
        Some
          {ekind = E_var v';
           etyp = c.t;
           erange = mk_fresh_range ()
          }
      )
    | None ->
      begin
        match exist_and_find_cell (fun c' ->
            is_c_int_type c'.t &&
            sizeof_type c'.t = sizeof_type c.t &&
            compare_var c.v c'.v = 0 &&
            Z.equal c.o c'.o) cs with
        | Some (v', c') ->
          Nexp (Some (warp v' (int_rangeof c.t) range))
        | None ->
          begin
            match exist_and_find_cell (
                fun c' ->
                  let b = Z.sub c.o c'.o in
                  Z.lt b (sizeof_type c'.t) &&
                  is_c_int_type c'.t &&
                  compare (remove_typedef c.t) (T_c_integer(C_unsigned_char)) = 0
              ) cs with
            | Some (v', c') ->
              begin
                let b = Z.sub c.o c'.o in
                let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
                Nexp (Some (
                    mk_binop
                      (mk_binop
                         (mk_var v' range)
                         O_div
                         (mk_z base range)
                         range
                      )
                      O_mod
                      (mk_int 256 range)
                      range
                  ))
              end
            | None ->
              begin
                let exception NotPossible in
                try
                  if is_c_int_type c.t then
                    begin
                      let t' = T_c_integer(C_unsigned_char) in
                      let n = Z.to_int (sizeof_type (c.t)) in
                      let rec aux i l =
                        if i < n then
                          let tobein = {v = c.v ; o = Z.add c.o (Z.of_int i); t = t'} in
                          match exist_and_find_cell (fun c' ->
                              compare_cell c' tobein = 0
                            ) cs with
                          | Some (v', c') ->
                            aux (i+1) (v' :: l)
                          | None ->
                            raise NotPossible
                        else
                          List.rev l
                      in
                      let ll = aux 0 [] in
                      let _,e = List.fold_left (fun (time,res) x ->
                          let res' =
                            mk_binop
                              (mk_binop
                                 (mk_z time range)
                                 O_mult
                                 (mk_var x range)
                                 range
                              )
                              O_plus
                              res
                              range
                          in
                          let time' = Z.mul time (Z.of_int 256) in
                          time',res'
                        ) (Z.of_int 1,(mk_int 0 range)) ll
                      in
                      Nexp (Some e)
                    end
                  else
                    raise NotPossible
                with
                | NotPossible ->
                  begin
                    if is_c_scalar_type c.t then
                      let a,b = rangeof c.t in
                      Nexp (Some ( mk_z_interval a b range))
                    else if is_c_pointer_type c.t then
                      Pexp Invalid
                    else
                      Nexp None
                  end
              end
          end
      end

  let add_var_cell v c u range =
    let open Universal.Ast in
    if not (is_c_scalar_type c.t) then
      u
    else
    if is_c_pointer_type c.t then
      {cs = CS.add v u.cs;
       a = u.a;
      }
    else
      match phi c u range with
      | Nexp (Some e) ->
        debug "add_cell case 1: nexp = %a" pp_expr e;
        let s = mk_assume
            (mk_binop
               (mk_var v range)
               O_eq
               e
               range
            )
            range
        in
        {cs = CS.add v u.cs;
         a = valabs_trivial_exec s u.a
        }
      | Nexp None ->
        debug "add_cell case 2: nexp = none";
        {cs = CS.add v u.cs;
         a = u.a
        }
      | Pexp Invalid ->
        debug "add_cell case 3: pexp = invalid";
        assert false

  (** [add_cell c u] adds cell [c] to the abstraction [u] *)
  let add_var (v : var) (u : t) range : t * var =
    if CS.mem v u.cs then
      u, v
    else
      let v', c = mk_cell_var v in
      add_var_cell v' c u range, v'


  (** [unify u u'] finds non-common cells in [u] and [u'] and adds them. *)
  let unify (u : t) (u' : t) range : t * t =
    let unify_cells (u  : t) (u' : t) : t * t =
      let diff' = CS.fold (fun v acc ->
          if CS.mem v u'.cs then acc
          else CS.add v acc
        ) u.cs CS.empty
      in
      let diff = CS.fold (fun v acc ->
          if CS.mem v u.cs then acc
          else CS.add v acc
        ) u'.cs CS.empty
      in
      CS.fold (fun v acc ->
          fst @@ add_var v acc range
        ) diff u,
      CS.fold (fun v acc ->
          fst @@ add_var v acc range
        ) diff' u'
    in
    let u,u' = unify_cells u u' in
    let rebind_cells (u  : t) (u' : t) : t * t =
      let is_same_cell v v' =
        match vkind v, vkind v' with
        | V_cell c, V_cell c' -> compare_cell c c' = 0
        | _ -> assert false
      in
      let a' = CS.fold (fun v a ->
          let cs' = CS.filter (is_same_cell v) u'.cs in
          CS.fold (fun v' a ->
              valabs_trivial_exec (Universal.Ast.mk_rename v' v range) a
            ) cs' a
        ) u.cs u'.a
      in
      u, {u with a = a'}
    in
    rebind_cells u u'

  let extract_cell v =
    match vkind v with
    | V_cell c -> c
    | _ -> assert false

  let remove_overlapping_cells v c range man ctx flow =
    let u = get_domain_cur man flow in
    CS.fold (fun v' acc ->
        if compare_var v v' = 0 then
          acc
        else
          let c' = extract_cell v' in
          let cell_range c = (c.o, Z.add c.o (sizeof_type c.t)) in
          let check_overlap (a1, b1) (a2, b2) =
            Z.lt (Z.max a1 a2) (Z.min b1 b2)
          in
          if compare_var c.v c'.v = 0 && check_overlap (cell_range c) (cell_range c') then
            man.exec (Universal.Ast.mk_remove_var v' range) ctx acc
          else
            acc
      ) u.cs flow



  (*==========================================================================*)
  (**                      {2 Lattice operators}                              *)
  (*==========================================================================*)


  let top = {cs = CS.top; a = ValAbs.top}

  let bottom = {cs = CS.bottom; a = ValAbs.bottom}

  let join (u : t) (u' : t) : t =
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.join u.a u'.a}

  let meet (u : t) (u' : t) : t =
    if ValAbs.leq ValAbs.top u.a then
      u'
    else if ValAbs.leq ValAbs.top u'.a then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.meet u.a u'.a}

  let widening (ctx : Framework.Context.context) (u : t) (u' : t) : t =
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.widening ctx u.a u'.a}

  let leq (u : t) (u' : t) : bool =
    if ValAbs.leq u.a ValAbs.bottom then
      true
    else if ValAbs.leq u'.a ValAbs.bottom then
      false
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      ValAbs.leq u.a u'.a


  let is_top x = ValAbs.is_top x.a

  let is_bottom x = ValAbs.is_bottom x.a


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init prog (man : ('a, t) manager) ctx (flow : 'a flow) =
    let ctx, flow = ValAbs.init prog (subman man) ctx flow in
    let u = get_domain_cur man flow in
    ctx, set_domain_cur {u with cs = CS.empty} man flow

  let exec (stmt : stmt) (man : ('a, t) manager) (ctx : Framework.Context.context) (flow : 'a flow)
    : 'a flow option =
    match skind stmt with
    | Universal.Ast.S_rename_var(v, v') ->
      assert false

    | Universal.Ast.S_remove_var v when is_c_int_type v.vtyp ->
      let u = get_domain_cur man flow in
      let v' = annotate_var v in
      let u' = {u with cs = CS.remove v' u.cs} in
      let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
      let flow = set_domain_cur u' man flow in
      ValAbs.exec stmt' (subman man) ctx flow

    | Universal.Ast.S_assign(lval, rval, mode) when is_c_int_type lval.etyp ->
      man_eval_list [rval; lval] man ctx flow |>
      oeval_to_exec
        (fun el flow ->
           let rval, lval, v, c = match el with
             | [rval; ({ekind = E_var ({vkind = V_cell c} as v)} as lval)] -> rval, lval, v, c
             | _ -> assert false
           in
           let stmt' = {stmt with skind = Universal.Ast.S_assign(lval, rval, mode)} in
           match ValAbs.exec stmt' (subman man) ctx flow with
           | None -> None
           | Some flow ->
             remove_overlapping_cells v c stmt.srange man ctx flow |>
             return
        )
        man ctx

    | S_c_remove_cell(c) ->
      map_domain_cur (fun u -> {u with cs = CS.filter (fun v -> not (mem_pred c v)) u.cs}) man flow |>
      return

    | _ -> ValAbs.exec stmt (subman man) ctx flow

  let is_safe_cell_access c =
    Z.leq (Z.add c.o (sizeof_type c.t)) (sizeof_type c.v.vtyp)

  let eval exp man ctx flow =
    match ekind exp with
    | E_var {vkind = V_cell _ }  -> None

    | E_var v when is_c_scalar_type v.vtyp ->
      debug "evaluating a scalar variable %a" pp_var v;
      let u = get_domain_cur man flow in
      let u', v' = add_var v u exp.erange in
      debug "new variable %a in %a" pp_var v' print u';
      let flow = set_domain_cur u' man flow in
      re_eval_singleton (Some (mk_var v' exp.erange), flow, []) man ctx

    | E_c_gen_cell_var c ->
      if is_safe_cell_access c then
        let u = get_domain_cur man flow in
        let u', v' =
          try u, find_cell c u.cs
          with Not_found ->
            let tmp = mktmp ~vtyp:c.t ~vkind:(V_cell(c)) () in
            debug "generating tmp for cell %a" pp_cell c;
            add_var tmp u exp.erange
        in
        let flow = set_domain_cur u' man flow in
        re_eval_singleton (Some (mk_var v' exp.erange), flow, []) man ctx
      else
        let flow = man.flow.add (Alarms.TOutOfBound exp.erange) (man.flow.get TCur flow) flow |>
                   man.flow.set TCur man.env.bottom
        in
        oeval_singleton (None, flow, [])

    | _ -> ValAbs.eval exp (subman man) ctx flow

  let ask query man ctx flow =
    ValAbs.ask query (subman man) ctx flow

end

let setup () =
  register_functor name (module Make);
  register_var_compare (fun next v1 v2 ->
      match vkind v1, vkind v2 with
      | V_cell c1, V_cell c2 -> compare_cell c1 c2
      | _ -> next v1 v2
    );
  register_pp_var (fun next fmt v ->
      match vkind v with
      | V_cell c -> pp_cell fmt c
      | _ -> next fmt v
    );
  register_pp_expr (fun next fmt exp ->
      match ekind exp with
      | E_c_gen_cell_var c -> Format.fprintf fmt "gen cell %a" pp_cell c
      | _ -> next fmt exp
    );
  register_expr_visitor (fun next exp ->
      match ekind exp with
      | E_c_gen_cell_var _ -> leaf exp
      | _ -> next exp
    );
  register_pp_stmt (fun next fmt stmt ->
      match skind stmt with
      | S_c_remove_cell c -> Format.fprintf fmt "remove cell %a" pp_cell c
      | _ -> next fmt stmt
    );
  register_stmt_visitor (fun next stmt ->
      match skind stmt with
      | S_c_remove_cell _ -> leaf stmt
      | _ -> next stmt
    )
