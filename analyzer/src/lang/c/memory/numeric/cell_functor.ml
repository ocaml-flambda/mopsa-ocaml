(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell abstraction for C language *)
open Framework.Ast
open Framework.Domains.Global
open Framework.Manager
open Framework.Domains
open Framework.Flow
open Ast
open Cell
open Cell_ast


let name = "c.memory.numeric.cell_functor"
let debug fmt = Debug.debug ~channel:name fmt

let name_counters = Hashtbl.create 12

let fresh name =
  let i =
    try
      Hashtbl.find name_counters name
    with
    | Not_found -> 0
  in
  let () = Hashtbl.replace name_counters name (i+1) in
  Format.sprintf "%s_%d" name i

let fresh_var_t t name =
  let n = fresh name in
  {Universal.Ast.orgname = n ; Universal.Ast.unname = n ; vtyp = t}

let fresh_var name =
  let n = fresh name in
  {Universal.Ast.orgname = n ;
   Universal.Ast.unname = n ;
   vtyp = Framework.Ast.T_any
  }


let vargen_var () = fresh_var_t (Universal.Ast.T_int) "var"

module Make(ValAbs : DOMAIN) = struct
  type t =
    {
      cs : CS.t;    (* set of cells *)
      bd : CVE.t;   (* cells <-> variables *)
      a  : ValAbs.t (* abstract domain over variables *)
    }
  let print fmt x =
    Format.fprintf fmt "cs = @[%a@]@\nbd = @[%a@]@\n@[%a@]"
      CS.print x.cs
      CVE.print x.bd
      ValAbs.print x.a

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
  let valabs_trivial_exec (stmt : stmt) (a : ValAbs.t) : ValAbs.t =
    (* let module A = Analyzer.Make(ValAbs) in
     * let open Flow in
     * A.exec stmt Context.empty (Top.Nt (Flow.Map.singleton Flow.TCur a))
     * |> A.manager.flow.get TCur *)
    assert false

  let remove_vars (r : Typ.VS.t) (u : t) range =
    let nvars_to_remove =
      CVE.fold (fun (c,v) acc ->
          if Typ.VS.mem c.v r then
            Typ.VS.add v acc
          else
            acc
        ) u.bd Typ.VS.empty in
    let nvars_remove_block =
      nvars_to_remove
      |> Typ.VS.elements
      |> List.map (fun v -> mk_stmt (Universal.Ast.S_remove_var v) range)
    in
    let nvars_remove_stmt = Universal.Ast.mk_block nvars_remove_block range in
    {
        cs = CS.remove_vars r u.cs;
        bd = CVE.remove_vars r u.bd;
        a = valabs_trivial_exec nvars_remove_stmt u.a
      }

  let get_myself (env : 'a) (man : ('a,t) manager) =
    man.ax.get env

  let set_myself (u : t) (env : 'a) (man : ('a,t) manager) =
    man.ax.set u env

  let apply_renaming (r : Typ.VVM.t) (u : t) =
    {
      u with
      cs = CS.apply_renaming r u.cs;
      bd = CVE.apply_renaming r u.bd;
    }
  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi c u] collects constraints over [c] found in [u] *)
  let phi (c : cell) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u.cs and bd = u.bd in
    match CS.exist_and_find (fun c' -> compare_cell c' c = 0) cs with
    | Some c' ->
      Nexp (
        Some
          {ekind = Universal.Ast.E_var (CVE.find_l c' bd);
           etyp = c.t;
           erange = mk_fresh_range ()
          }
      )
    | None ->
       begin
         match CS.exist_and_find (fun c' ->
             is_inttype c'.t &&
             sizeof_type c'.t = sizeof_type c.t &&
             c.v = c'.v &&
             c.o = c'.o) cs with
         | Some c' ->
            Nexp (Some (warp (CVE.find_l c' bd) (rangeof_int c.t) range))
         | None ->
            begin
              match CS.exist_and_find (
                fun c' ->
                  let b = Z.of_int (c.o - c'.o) in
                  Z.lt b (sizeof_type c'.t) &&
                    is_inttype c'.t &&
                    c.t = T_c_integer(C_unsigned_char)
              ) cs with
              | Some c' ->
                 begin
                   let b = c.o - c'.o in
                   let x = CVE.find_l c' bd in
                   let base = (Z.pow (Z.of_int 2) (8 * b))  in
                   Nexp (Some (
                       mk_binop
                         (mk_binop
                            (mk_var x range)
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
                     if is_inttype c.t then
                       begin
                         let t' = T_c_integer(C_unsigned_char) in
                         let n = Z.to_int (sizeof_type (c.t)) in
                         let rec aux i l =
                           if i < n then
                             let tobein = {v = c.v ; o = c.o + i ; t = t'} in
                             if CS.mem tobein cs then
                               aux (i+1) ((CVE.find_l tobein bd) :: l)
                             else
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
                        if is_scalartype c.t then
                          let a,b = rangeof c.t in
                          Nexp (Some ( mk_constant ~etyp:T_int (C_int_range(a,b)) range))
                        else if is_pointer c.t then
                          Pexp Invalid
                        else
                          Nexp None
                      end
                 end
            end
       end

  (** [add_cell c u] adds the cell [c] to the abstraction [u] *)
  let add_cell (c : cell) (u : t) range : t =
    if CS.mem c u.cs then
      u
    else
      begin
        match phi c u range with
        | Nexp (Some e) ->
          begin
            let var_c = vargen_var () in
            let open Universal.Ast in
            let s = mk_assume
                (mk_binop
                   (mk_var var_c range)
                   O_eq
                   e
                   range
                )
                range
            in
            {cs = CS.add c u.cs;
             bd = CVE.add (c,var_c) u.bd;
             a = valabs_trivial_exec s u.a
            }
          end
        | Nexp None ->
          begin
            let var_c =  vargen_var () in
            {cs = CS.add c u.cs;
             bd = CVE.add (c,var_c) u.bd;
             a = u.a
            }
          end
        | Pexp Invalid ->
          begin
            let var_c = vargen_var () in
            {cs = CS.add c u.cs;
             bd = CVE.add (c,var_c) u.bd;
             a = u.a
            } (* TODO : this case needs work*)
          end
      end

  let unify (u : t) (u' : t) range : t * t =
    let unify_cells
        (u  : t)
        (u' : t)
      : t * t =
      let diff' = CS.fold (fun e acc ->
          if CS.mem e u'.cs then acc
          else CS.add e acc
        ) u.cs CS.empty
      in
      let diff = CS.fold (fun e acc ->
          if CS.mem e u.cs then acc
          else CS.add e acc
        ) u'.cs CS.empty
      in
      CS.fold (fun c acc ->
          add_cell c acc range
        ) diff u,
      CS.fold (fun c acc ->
          add_cell c acc range
        ) diff' u'
    in
    let u,u' = unify_cells u u' in
    let rebind_cells
        (u  : t)
        (u' : t)
      : t * t =
      let open Universal.Ast in
      let a' = CS.fold (fun c a ->
          let v' = CVE.find_l c u'.bd in
          let v  = CVE.find_l c u.bd  in
          valabs_trivial_exec (mk_rename v' v range) a
        ) u.cs u'.a
      in
      u, {u with a = a'}
    in
    rebind_cells u u'

  let top = {cs = CS.empty ; bd = CVE.empty ; a = ValAbs.top}
  let bottom = {cs = CS.empty ; bd = CVE.empty ; a = ValAbs.bottom}

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


  let is_top x = leq top x
  let is_bottom x = leq x bottom

  let get_my_current_abstraction (flow : 'a Framework.Flow.flow) (man : ('a, t) manager)
    : t =
    let module FF = Framework.Flow in
    man.flow.FF.get FF.TCur flow |>
    man.ax.get

  let set_my_current_abstraction (u : t) (flow : 'a Framework.Flow.flow) (man : ('a, t) manager)
    : 'a Framework.Flow.flow =
    let module FF = Framework.Flow in
    man.flow.FF.set FF.TCur (man.ax.set u (man.flow.FF.get FF.TCur flow)) flow


  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    let flow = ValAbs.init prog (subman man) flow in
    let u = get_domain_cur man flow in
    set_domain_cur {u with cs = CS.empty ; bd = CVE.empty} man flow


  let exec (stmt : stmt) (man : ('a, t) manager) (ctx : Framework.Context.context) (flow : 'a flow)
    : 'a flow option =
    let open Universal.Ast in
    let do_on_all_flows f flow =
      man.flow.map (fun (env : 'a) _ ->
          let flow = get_myself env man in
          let flow = f flow in
          set_myself flow env man
        ) flow
    in
    let cell_to_var u stmt =
      Framework.Visitor.fold_map_stmt
        (fun u expr -> match ekind expr with
            | E_c_cell c ->
               let u'' = add_cell c u stmt.srange in
               (u'', Universal.Ast.mk_var (CVE.find_l c u.bd) stmt.srange)
             | _ -> (u, expr)
          )
          (fun u stmt -> (u,stmt))
          u stmt
    in
    match skind stmt with
    | S_rename_var(v, v') ->
      do_on_all_flows (fun u -> apply_renaming (Typ.VVM.singleton v v') u) flow
      |> Exec.return
    | S_remove_var v      ->
      do_on_all_flows (fun u -> remove_vars (Typ.VS.singleton v) u stmt.srange) flow
      |> Exec.return
    | S_assign ({ ekind = E_c_cell c} , e') ->
      debug "assign";
      let u = get_my_current_abstraction flow man in
      let u', stmt = cell_to_var u stmt in
      set_my_current_abstraction {u' with a = valabs_trivial_exec stmt u'.a} flow man
      |> Exec.return
    | S_assume e ->
      begin
        Eval.compose_exec
          e
          (fun e flow ->
            let u = get_my_current_abstraction flow man in
            let u', stmt = cell_to_var u stmt in
            let u'' = {u' with a = valabs_trivial_exec stmt u'.a} in
            set_my_current_abstraction u'' flow man
            |> Exec.return
          )
          (fun flow -> Exec.return flow )
          man ctx flow
      end
    | _ -> Exec.fail

  let eval exp man ctx flow =
    let u = get_domain_cur man flow in
    let u', exp' = Framework.Visitor.fold_map_expr
          (fun u expr -> match ekind expr with
             | E_c_cell c ->
               let u'' = add_cell c u exp.erange in
               (u'', Universal.Ast.mk_var (CVE.find_l c u.bd) exp.erange)
             | _ -> (u, expr)
          )
          (fun u stmt -> (u,stmt))
          u exp
    in
    let flow = set_domain_cur u' man flow in
    ValAbs.eval exp' (subman man) ctx flow

  let ask : type b. b Framework.Query.query -> ('a, t) manager -> Framework.Context.context -> 'a Framework.Flow.flow -> b option
    = fun request man ctx flow ->
    match request with
      | Universal.Numeric.Query.QInterval exp ->
        let u = get_my_current_abstraction flow man in
        let u', exp =
          Framework.Visitor.fold_map_expr
            (fun u expr -> match ekind expr with
               | E_c_cell c ->
                 let u' = add_cell c u expr.erange in
                 (u', Universal.Ast.mk_var (CVE.find_l c u'.bd) expr.erange)
               | _ -> (u,expr)
            )
            (fun u stmt -> (u,stmt))
            u exp
        in
        ValAbs.ask (Universal.Numeric.Query.QInterval exp) (subman man) ctx (set_my_current_abstraction u' flow man)
      | _ -> None
  let unify _ u u' = (u,u')
end

let setup () =
  register_functor name (module Make)
