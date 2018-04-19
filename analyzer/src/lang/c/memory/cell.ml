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
open Framework.Domains.Fun
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Framework.Lattice
open Framework.Eval
open Framework.Exec
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
    (fun () -> compare_typ c.t c'.t);
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_cell of cell

type expr_kind +=
  | E_c_gen_cell_var of cell

let mk_gen_cell_var c range =
  mk_expr (E_c_gen_cell_var c) ~etyp:c.t range


let cell_to_var c =
  let open Framework.Ast in
  { vname = (let () = Format.fprintf Format.str_formatter "%a" pp_cell c in Format.flush_str_formatter ());
    vuid = 0;
    vtyp = c.t;
    vkind = V_cell c;
  }

let var_to_var v =
  match v.vkind with
  | V_cell _ -> v
  | _ -> { v with vkind = V_cell {v; o = Z.zero; t= v.vtyp} }

let cell_of_var v =
  match v.vkind with
  | V_cell c -> c
  | _ -> assert false

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Make(ValAbs : Framework.Domains.Stateful.DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** Set of cells variables. *)
  module CS = Framework.Lattices.Top_set.Make(struct
      type t = var
      let compare = compare_var
      let print fmt v =
        let c = cell_of_var v in
        pp_cell fmt c
    end)

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
    let env_manager = Framework.Domains.Stateful.(mk_lattice_manager (module ValAbs : DOMAIN with type t = ValAbs.t)) in
    {
      env = env_manager;
      flow = Framework.Flow.lift_lattice_manager env_manager;
      exec = (fun ctx stmt flow -> match ValAbs.exec local_subman ctx stmt flow with Some flow -> flow | None -> assert false);
      eval = (fun ctx exp flow -> match ValAbs.eval local_subman ctx exp flow with Some evl -> evl | None -> eval_singleton (Some exp, flow, []) );
      ask = (fun ctx query flow -> assert false);
      ax = {
        get = (fun env -> env);
        set = (fun env' env -> env');
      }
    }

  (** Execute a statement on [ValAbs] using the local scope manager. *)
  let valabs_trivial_exec (stmt : stmt) (a : ValAbs.t) : ValAbs.t =
    set_domain_cur a local_subman local_subman.flow.bottom |>
    local_subman.exec Framework.Context.empty stmt |>
    local_subman.flow.get TCur

  (*==========================================================================*)
  (**                          {2 Unification}                                *)
  (*==========================================================================*)

  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi v u] collects constraints over cell var [v] found in [u] *)
  let phi (v : var) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u.cs in
    let c = cell_of_var v in
    match exist_and_find_cell (fun c' -> compare_cell c c' = 0) cs  with
    | Some (v', _) ->
      Nexp (
        Some (mk_var v' range)
      )
    | None ->
      begin
        match exist_and_find_cell (fun c' ->
            is_c_int_type c'.t &&
            Z.equal (sizeof_type c'.t) (sizeof_type c.t) &&
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

  (** [add_var v u] adds a variable [v] to the abstraction [u] *)
  let add_var (v : var) (u : t) range : t  =
    if CS.mem v u.cs then u
    else
      if not (is_c_scalar_type v.vtyp) then u
      else if is_c_pointer_type v.vtyp then
        {
          cs = CS.add v u.cs;
          a = u.a;
        }
      else
        let open Universal.Ast in
        match phi v u range with
        | Nexp (Some e) ->
          let s = mk_assume
              (mk_binop
                 (mk_var v range)
                 O_eq
                 e
                 range
              )
              range
          in
          {
            cs = CS.add v u.cs;
            a = valabs_trivial_exec s u.a
          }
        | Nexp None ->
          {
            cs = CS.add v u.cs;
            a = u.a
          }
        | Pexp Invalid ->
          assert false


  (** [unify u u'] finds non-common cells in [u] and [u'] and adds them. *)
  let unify (u : t) (u' : t) range : t * t =
    debug "unify@ %a@ and@ %a" print u print u';
    let unify_cells (u  : t) (u' : t) : t * t =
      let t = Timing.start () in
      let diff' = CS.diff u.cs u'.cs in
      debug "diff' done in %.4fs" (Timing.stop t);
      let t = Timing.start () in
      let diff = CS.diff u'.cs u.cs in
      debug "diff done in %.4fs" (Timing.stop t);
      CS.fold (fun v acc ->
          add_var v acc range
        ) diff u,
      CS.fold (fun v acc ->
          add_var v acc range
        ) diff' u'
    in
    let t = Timing.start () in
    let u,u' = unify_cells u u' in
    debug "unification done in %.4fs" (Timing.stop t);
    u, u'

  let extract_cell v =
    match vkind v with
    | V_cell c -> c
    | _ -> assert false

  let remove_overlapping_cells v range man ctx flow =
    let c = cell_of_var v in
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
            man.exec ctx (Universal.Ast.mk_remove_var v' range) acc
          else
            acc
      ) u.cs flow



  (*==========================================================================*)
  (**                      {2 Lattice operators}                              *)
  (*==========================================================================*)


  let top = {cs = CS.top; a = ValAbs.top}

  let bottom = {cs = CS.bottom; a = ValAbs.bottom}

  let join (u : t) (u' : t) : t =
    if ValAbs.is_bottom u.a then u'
    else if ValAbs.is_bottom u'.a then u
    else
      let range = mk_fresh_range () in
      debug "join";
      let t = Timing.start () in
      let u,u' = unify u u' range in
      debug "unify done in %.4f" (Timing.stop t);
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


  let init (man : ('a, t) manager) ctx prog (flow : 'a flow) =
    let ctx, flow = ValAbs.init (subman man) ctx prog flow in
    let u = get_domain_cur man flow in
    ctx, set_domain_cur {u with cs = CS.empty} man flow

  let exec (man : ('a, t) manager) (ctx : Framework.Context.context) (stmt : stmt) (flow : 'a flow)
    : 'a flow option =
    match skind stmt with
    | Universal.Ast.S_rename_var(v, v') ->
      assert false

    | Universal.Ast.S_remove_var v when is_c_int_type v.vtyp ->
      let u = get_domain_cur man flow in
      let v' = var_to_var v in
      let u' = {u with cs = CS.remove v' u.cs} in
      let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
      let flow = set_domain_cur u' man flow in
      ValAbs.exec (subman man) ctx stmt' flow

    | Universal.Ast.S_assign(lval, rval, mode) when is_c_int_type lval.etyp ->
      eval_list [rval; lval] (man.eval ctx) flow |>
      eval_to_oexec
        (fun el flow ->
           let rval, lval, v = match el with
             | [rval; ({ekind = E_var v} as lval)] -> rval, lval, v
             | _ -> assert false
           in
           let stmt' = {stmt with skind = Universal.Ast.S_assign(lval, rval, mode)} in
           match ValAbs.exec (subman man) ctx stmt' flow with
           | None -> None
           | Some flow ->
             remove_overlapping_cells v stmt.srange man ctx flow |>
             return
        )
        (man.exec ctx) man.flow

    | _ -> ValAbs.exec (subman man) ctx stmt flow

  let is_safe_cell_access c =
    Z.leq (Z.add c.o (sizeof_type c.t)) (sizeof_type c.v.vtyp)

  let eval man ctx exp flow =
    match ekind exp with
    | E_var {vkind = V_cell _ }  -> None

    | E_var v when is_c_scalar_type v.vtyp ->
      debug "evaluating a scalar variable %a" pp_var v;
      let u = get_domain_cur man flow in
      let v = var_to_var v in
      let u' = add_var v u exp.erange in
      debug "new variable %a in %a" pp_var v print u';
      let flow = set_domain_cur u' man flow in
      re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow, [])

    | E_c_gen_cell_var c ->
      if is_safe_cell_access c then
        let u = get_domain_cur man flow in
        let v = match exist_and_find_cell (fun c' -> compare_cell c c' = 0) u.cs  with
          | Some (v', _) -> v'
          | None -> cell_to_var c
        in
        let u' =  add_var v u exp.erange in
        let flow = set_domain_cur u' man flow in
        re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow, [])
      else
        let flow = man.flow.add (Alarms.TOutOfBound exp.erange) (man.flow.get TCur flow) flow |>
                   man.flow.set TCur man.env.bottom
        in
        oeval_singleton (None, flow, [])

    | _ -> ValAbs.eval (subman man) ctx exp flow

  let ask man ctx query flow =
    ValAbs.ask (subman man) ctx query flow

end

let setup () =
  register_domain name (module Make);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_cell c1, V_cell c2 -> compare_cell c1 c2
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_cell c -> pp_cell fmt c
      | _ -> next fmt vk
    );
  register_pp_expr (fun next fmt exp ->
      match ekind exp with
      | E_c_gen_cell_var c -> Format.fprintf fmt "gen var %a" pp_cell c
      | _ -> next fmt exp
    );
  register_expr_visitor (fun next exp ->
      match ekind exp with
      | E_c_gen_cell_var _ -> leaf exp
      | _ -> next exp
    )
