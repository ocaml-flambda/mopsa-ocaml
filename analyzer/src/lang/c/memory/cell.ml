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
open Framework.Domains
open Framework.Flow
open Ast


let name = "c.memory.cell"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                              {2 Cells}                                  *)
(*==========================================================================*)

(** Memory cells. *)
type cell = {
  v: var; (** base variable *)
  o: int; (** offset *)
  t: typ; (** type *)
}

let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%d,%a⟩"
    pp_var c.v
    c.o
    pp_typ c.t

let compare_cell c c' =
  compare_composer [
    (fun () -> compare_var c.v c'.v);
    (fun () -> compare c.o c'.o);
    (fun () -> compare c.t c'.t); (* TODO replace this compare by a real comparison function over C_AST.type_qual*)
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_cell of cell

let () =
  register_var_compare (fun next v1 v2 ->
      match vkind v1, vkind v2 with
      | V_cell c1, V_cell c2 -> compare_cell c1 c2
      | _ -> next v1 v2
    );
  register_pp_var (fun next fmt v ->
      match vkind v with
      | V_cell c -> pp_cell fmt c
      | _ -> next fmt v
    )


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


  let mem_cell c cs =
    CS.exists (function {vkind = V_cell c'} -> compare_cell c c' = 0 | _ -> false) cs

  let exist_and_find_cell f cs =
    let exception Found of var * cell in
    try
      let () = CS.iter (function ({vkind = V_cell c} as v) -> if f c then raise (Found (v, c)) else () | _ -> ()) cs in
      None
    with
    | Found (v, c) -> Some (v, c)
      
  let mk_base_cell v =
    let c = {v = v; o = 0; t = v.vtyp} in
    {v with vkind = V_cell c}, c
  
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
    debug "trivial exec %a in@ %a" Framework.Pp.pp_stmt stmt ValAbs.print a;
    let a' =
      set_domain_cur a local_subman local_subman.flow.bottom |>
      local_subman.exec stmt Framework.Context.empty |>
      local_subman.flow.get TCur
    in
    debug "res = %a" ValAbs.print a';
    a'

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
            c.v = c'.v &&
            c.o = c'.o) cs with
        | Some (v', c') ->
          Nexp (Some (warp v' (rangeof_int c.t) range))
        | None ->
          begin
            match exist_and_find_cell (
                fun c' ->
                  let b = Z.of_int (c.o - c'.o) in
                  Z.lt b (sizeof_type c'.t) &&
                  is_c_int_type c'.t &&
                  c.t = T_c_integer(C_unsigned_char)
              ) cs with
            | Some (v', c') ->
              begin
                let b = c.o - c'.o in
                let base = (Z.pow (Z.of_int 2) (8 * b))  in
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
                          let tobein = {v = c.v ; o = c.o + i ; t = t'} in
                          match exist_and_find_cell (fun c' -> compare_cell c' tobein = 0) cs with
                          | Some (v', c') ->
                            aux (i+1) (v' :: l)
                          | None ->
                            raise NotPossible
                        else
                          List.rev l
                      in
                      let ll = aux 0 [] in
                      let _,e = List.fold_left (fun (exp,res) x ->
                          let time = Z.mul (Z.pow (Z.of_int 2) 8) exp in
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
                          time,res'
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
                      Nexp (Some ( mk_constant ~etyp:T_int (C_int_range(a,b)) range))
                    else if is_c_pointer c.t then
                      Pexp Invalid
                    else
                      Nexp None
                  end
              end
          end
      end

  let mk_cell_var v =
    match vkind v with
    | V_cell c -> v, c
    | _ -> mk_base_cell v

  let annotate_var v = fst @@ mk_cell_var v
      
  (** [add_cell c u] adds cell [c] to the abstraction [u] *)
  let add_var_cell (v : var) (u : t) range : t * var =
    let v', c = mk_cell_var v in
    if mem_cell c u.cs then
      u, v'
    else
      let open Universal.Ast in
      match phi c u range with
      | Nexp (Some e) ->
        let s = mk_assume
            (mk_binop
               (mk_var v' range)
               O_eq
               e
               range
            )
            range
        in
        {cs = CS.add v' u.cs;
         a = valabs_trivial_exec s u.a
        }, v'
      | Nexp None ->
        {cs = CS.add v' u.cs;
         a = u.a
        }, v'
      | Pexp Invalid ->
        {cs = CS.add v' u.cs;
         a = u.a
        }, v' (* TODO : this case needs work*)

  
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
          fst @@ add_var_cell v acc range
        ) diff u,
      CS.fold (fun v acc ->
          fst @@ add_var_cell v acc range
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


  (*==========================================================================*)
  (**                      {2 Lattice operators}                              *)
  (*==========================================================================*)


  let top = {cs = CS.top; a = ValAbs.top}

  let bottom = {cs = CS.top; a = ValAbs.bottom}

  let join (u : t) (u' : t) : t =
    debug "join:@\n u = @[%a@]@\n u' = @[%a@]" print u print u';
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
      {u with a = ValAbs.join u.a u'.a}

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


  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    let flow = ValAbs.init prog (subman man) flow in
    let u = get_domain_cur man flow in
    set_domain_cur {u with cs = CS.empty} man flow

  let exec (stmt : stmt) (man : ('a, t) manager) (ctx : Framework.Context.context) (flow : 'a flow)
    : 'a flow option =
    match skind stmt with
    | Universal.Ast.S_rename_var(v, v') ->
      assert false

    | Universal.Ast.S_remove_var v when is_c_type v.vtyp ->
      let u = get_domain_cur man flow in
      let v' = annotate_var v in
      let u' = {u with cs = CS.remove v' u.cs} in
      let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
      let flow = set_domain_cur u' man flow in
      ValAbs.exec stmt' (subman man) ctx flow

    | Universal.Ast.S_assign(({ekind = E_var v} as lval), e, assign_kind) ->
      let u = get_domain_cur man flow in
      let u', v' = add_var_cell v u stmt.srange in
      let stmt' = {stmt with skind = Universal.Ast.S_assign(mk_var v' lval.erange, e, assign_kind)} in
      let flow = set_domain_cur u' man flow in
      ValAbs.exec stmt' (subman man) ctx flow

    | _ -> ValAbs.exec stmt (subman man) ctx flow


  let eval exp man ctx flow =
    match ekind exp with
    | E_var {vkind = V_cell _ } -> Eval.singleton (Some exp, flow, [])

    | E_var v when is_c_type v.vtyp ->
      debug "evaluating non-annotated base variable %a" pp_var v;
      let u = get_domain_cur man flow in
      let u', v' = add_var_cell v u exp.erange in
      debug "new variable %a" pp_var v';
      let flow = set_domain_cur u' man flow in
      Eval.re_eval_singleton man ctx (Some (mk_var v' exp.erange), flow, [])

    | _ -> ValAbs.eval exp (subman man) ctx flow

      

  let ask request man ctx flow =
    ValAbs.ask request (subman man) ctx flow

end

let setup () =
  register_functor name (module Make)
