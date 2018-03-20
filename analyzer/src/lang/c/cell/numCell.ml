(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell abstraction for C language *)
module CAst = Ast
open Framework
open Framework.Ast
open Framework.Domains.Global
open Framework.Domains.Manager
open Framework.Domains
open CellAst


let vargen_var () = VarGen.fresh_var_t (Universal.Ast.T_int) "var"

let debug fmt = Debug.debug ~channel:"c.cell.abstract" fmt

module Make(ValAbs : DOMAIN) = struct
  type t =
    {
      cs : Typ.CS.t;    (* set of cells *)
      bd : Typ.CVE.t;   (* cells <-> variables *)
      a  : ValAbs.t (* abstract domain over variables *)
    }
  let print fmt x =
    Format.fprintf fmt "@[<v 1>{@,cs = %a,@,bd = %a,@,a = %a@]@,}"
      Typ.CS.print x.cs
      Typ.CVE.print x.bd
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
  let valabs_trivial_exec (stmt : Ast.stmt) (a : ValAbs.t) : ValAbs.t =
    let module A = Analyzer.Make(ValAbs) in
    let open Flow in
    A.exec stmt Context.empty (Top.Nt (Flow.Map.singleton Flow.TCur a))
    |> A.manager.flow.get TCur

  let remove_vars (r : Typ.VS.t) (u : t) =
    let nvars_to_remove =
      Typ.CVE.fold (fun (c,v) acc ->
          if Typ.VS.mem c.Typ.Cell.v r then
            Typ.VS.add v acc
          else
            acc
        ) u.bd Typ.VS.empty in
    let nvars_remove_stmt =
      nvars_to_remove
      |> Typ.VS.elements
      |> List.map (fun v -> mk_stmt (Universal.Ast.S_remove_var v))
      |> Universal.Ast.mk_block
    in
      {
        cs = Typ.CS.remove_vars r u.cs;
        bd = Typ.CVE.remove_vars r u.bd;
        a = valabs_trivial_exec nvars_remove_stmt u.a
      }

  let get_myself (env : 'a) (man : ('a,t) manager) =
    man.ax.get env

  let set_myself (u : t) (env : 'a) (man : ('a,t) manager) =
    man.ax.set u env

  let apply_renaming (r : Typ.VVM.t) (u : t) =
    {
      u with
      cs = Typ.CS.apply_renaming r u.cs;
      bd = Typ.CVE.apply_renaming r u.bd;
    }
  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi c u] collects constraints over [c] found in [u] *)
  let phi (c : Typ.Cell.t) (u : t)
    : phi_exp =
    let open Typ.Cell in
    let open Universal.Ast in
    let cs = u.cs and bd = u.bd in
    match Typ.CS.exist_and_find (fun c' -> Typ.Cell.compare c' c = 0) cs with
    | Some c' ->
      Nexp (
        Some
          {ekind = Universal.Ast.E_var (Typ.CVE.find_l c' bd);
           etyp = CAst.TClang c.t;
           erange = Ast.unknown_range
          }
      )
    | None ->
       begin
         match Typ.CS.exist_and_find (fun c' ->
             CTools.is_inttype c'.t &&
             CTools.sizeof c'.t = CTools.sizeof c.t &&
             c.v = c'.v &&
             c.o = c'.o) cs with
         | Some c' ->
            Nexp (Some (warp (Typ.CVE.find_l c' bd) (range_int c.t)))
         | None ->
            begin
              match Typ.CS.exist_and_find (
                fun c' ->
                  let b = Z.of_int (c.o - c'.o) in
                  Z.lt b (sizeof c'.t) &&
                    is_inttype c'.t &&
                    fst c.t = C_AST.(T_integer(UNSIGNED_CHAR))
              ) cs with
              | Some c' ->
                 begin
                   let b = c.o - c'.o in
                   let x = Typ.CVE.find_l c' bd in
                   let base = (Z.pow (Z.of_int 2) (8 * b))  in
                   Nexp (Some (
                       mk_int_binop O_mod
                         (mk_int_binop O_div
                            (mk_int_expr (E_var x))
                            (mk_z base)
                         )
                         (mk_int 256)
                     )
                     )
                 end
              | None ->
                 begin
                   let exception NotPossible in
                   try
                     if is_inttype c.t then
                       begin
                         let t' = C_AST.(T_integer(UNSIGNED_CHAR)) in
                         let n = Z.to_int (sizeof (c.t)) in
                         let rec aux i l =
                           if i < n then
                             let tobein = {v = c.v ; o = c.o + i ; t = (t',snd c.t)} in
                             if Typ.CS.mem tobein cs then
                               aux (i+1) ((Typ.CVE.find_l tobein bd) :: l)
                             else
                               raise NotPossible
                           else
                             List.rev l
                         in
                         let ll = aux 0 [] in
                         let _,e = List.fold_left (fun (exp,res) x ->
                           let time = Z.mul (Z.pow (Z.of_int 2) 8) exp in
                           let res' =
                             mk_int_binop O_plus
                               (mk_int_binop O_mult
                                  (mk_z time)
                                  (mk_int_expr (E_var x))
                               )
                               res
                           in
                           time,res'
                           ) (Z.of_int 1,(mk_int 0)) ll
                         in
                         Nexp (Some e)
                       end
                     else
                       raise NotPossible
                   with
                   | NotPossible ->
                      begin
                        if is_scalartype c.t then
                          let a,b = range c.t in
                          Nexp (Some ( mk_int_expr (E_constant (C_int_range(a,b)))))
                        else if is_pointer c.t then
                          Pexp Invalid
                        else
                          Nexp None
                      end
                 end
            end
       end

  (** [add_cell c u] adds the cell [c] to the abstraction [u] *)
  let add_cell (c : Typ.Cell.t) (u : t) : t =
    if Typ.CS.mem c u.cs then
      u
    else
      begin
        match phi c u with
        | Nexp (Some e) ->
          begin
            let var_c = vargen_var () in
            let open Universal.Ast in
            let s = mk_stmt (S_assume (
                CTools.mk_int_binop O_eq
                  (CTools.mk_int_expr (E_var var_c))
                  e))
            in
            {cs = Typ.CS.add c u.cs;
             bd = Typ.CVE.add (c,var_c) u.bd;
             a = valabs_trivial_exec s u.a
            }
          end
        | Nexp None ->
          begin
            let var_c =  vargen_var () in
            {cs = Typ.CS.add c u.cs;
             bd = Typ.CVE.add (c,var_c) u.bd;
             a = u.a
            }
          end
        | Pexp Invalid ->
          begin
            let var_c = vargen_var () in
            {cs = Typ.CS.add c u.cs;
             bd = Typ.CVE.add (c,var_c) u.bd;
             a = u.a
            } (* TODO : this case needs work*)
          end
      end

  let unify (u : t) (u' : t) : t * t =
    let unify_cells
        (u  : t)
        (u' : t)
      : t * t =
      let diff' = Typ.CS.fold (fun e acc ->
          if Typ.CS.mem e u'.cs then acc
          else Typ.CS.add e acc
        ) u.cs Typ.CS.empty
      in
      let diff = Typ.CS.fold (fun e acc ->
          if Typ.CS.mem e u.cs then acc
          else Typ.CS.add e acc
        ) u'.cs Typ.CS.empty
      in
      Typ.CS.fold (fun c acc ->
          add_cell c acc
        ) diff u,
      Typ.CS.fold (fun c acc ->
          add_cell c acc
        ) diff' u'
    in
    let u,u' = unify_cells u u' in
    let rebind_cells
        (u  : t)
        (u' : t)
      : t * t =
      let open Universal.Ast in
      let a' = Typ.CS.fold (fun c a ->
          let v' = Typ.CVE.find_l c u'.bd in
          let v  = Typ.CVE.find_l c u.bd  in
          valabs_trivial_exec (mk_rename v' v) a
        ) u.cs u'.a
      in
      u, {u with a = a'}
    in
    rebind_cells u u'

  let init (man : ('a, t) Manager.manager) (flow : 'a Flow.flow) =
    let myenv = man.flow.Flow.get Flow.TCur flow in
    man.flow.Flow.set
      Flow.TCur
      (man.ax.set {cs = Typ.CS.empty ; bd = Typ.CVE.empty ; a = ValAbs.top} myenv)
      flow

  let top = {cs = Typ.CS.empty ; bd = Typ.CVE.empty ; a = ValAbs.top}
  let bottom = {cs = Typ.CS.empty ; bd = Typ.CVE.empty ; a = ValAbs.bottom}

  let join (u : t) (u' : t) : t =
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let u,u' = unify u u' in
      {u with a = ValAbs.join u.a u'.a}

  let meet (u : t) (u' : t) : t =
    if ValAbs.leq ValAbs.top u.a then
      u'
    else if ValAbs.leq ValAbs.top u'.a then
      u
    else
      let u,u' = unify u u' in
      {u with a = ValAbs.join u.a u'.a}

  let widening (ctx : Context.context) (u : t) (u' : t) : t =
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let u,u' = unify u u' in
      {u with a = ValAbs.widening ctx u.a u'.a}

  let leq (u : t) (u' : t) : bool =
    if ValAbs.leq u.a ValAbs.bottom then
      true
    else if ValAbs.leq u'.a ValAbs.bottom then
      false
    else
      let u,u' = unify u u' in
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


  let exec (stmt : stmt) (man : ('a, t) manager) (ctx : Context.context) (flow : 'a Flow.flow)
    : 'a Flow.flow option =
    let open Universal.Ast in
    let do_on_all_flows f flow =
      man.flow.Flow.map (fun (env : 'a) _ ->
          let flow = get_myself env man in
          let flow = f flow in
          set_myself flow env man
        ) flow
    in
    let cell_to_var u stmt =
      Visitor.fold_map_stmt
        (fun u expr -> match ekind expr with
            | CellAst.Cell c ->
               let u'' = add_cell c u in
               (u'', Universal.Ast.mk_var (Typ.CVE.find_l c u.bd))
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
      do_on_all_flows (fun u -> remove_vars (Typ.VS.singleton v) u) flow
      |> Exec.return
    | S_assign ({ ekind = CellAst.Cell c} , e') ->
      let u = get_my_current_abstraction flow man in
      let u', stmt = cell_to_var u stmt in
      set_my_current_abstraction {u' with a = valabs_trivial_exec stmt u'.a} flow man
      |> Exec.return
    | S_assume e ->
      begin
        Eval.compose_exec (fun e flow ->
            let u = get_my_current_abstraction flow man in
            let u', stmt = cell_to_var u stmt in
            let u'' = {u' with a = valabs_trivial_exec stmt u'.a} in
            set_my_current_abstraction u'' flow man
            |> Exec.return
          ) (fun _ -> None )e man ctx flow
      end
    | _ -> Exec.fail

  let eval _ _ _ _ = None

  let ask : type b. b Framework.Query.query -> ('a, t) manager -> Context.context -> 'a Flow.flow -> b option
    = fun (request : b Framework.Query.query) (man : ('a, t) manager) (ctx : Context.context) (flow : 'a Flow.flow) ->
    match request with
      | Universal.Numeric.Query.QInterval exp ->
        let u = get_my_current_abstraction flow man in
        let u', exp =
          Visitor.fold_map_expr
            (fun u expr -> match ekind expr with
               | CellAst.Cell c ->
                 let u' = add_cell c u in
                 (u', Universal.Ast.mk_var (Typ.CVE.find_l c u'.bd))
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
  register_functor "c.cell.numcell" (module Make)
