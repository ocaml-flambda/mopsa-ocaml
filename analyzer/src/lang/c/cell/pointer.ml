(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural control flow abstraction *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Global
open Framework.Manager
open Framework.Ast
open Ast

let name = "C.cell.CPointer"

module CPointer =
struct


  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)


  (* include Typ.CPML *)
  open Typ.CPML
  type t = Typ.CPML.t
  let join = join

  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    let myenv = man.flow.get TCur flow in
    man.flow.set
      TCur
      (man.ax.set top myenv)
      flow

  let top = top
  let print = print
  let widening = widening
  let meet = meet
  let leq = leq
  let is_top = is_top
  let is_bottom = is_bottom
  let bottom = bottom

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt


  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let get_my_current_abstraction (flow : 'a flow) (man : ('a, t) manager)
    : t =
    let module FF = Framework.Flow in
    man.flow.FF.get FF.TCur flow |>
    man.ax.get

  let set_my_current_abstraction (u : t) (flow : 'a flow) (man : ('a, t) manager)
    : 'a flow =
    let module FF = Framework.Flow in
    man.flow.FF.set FF.TCur (man.ax.set u (man.flow.FF.get FF.TCur flow)) flow

  let exec stmt man ctx gabs : 'a flow option =
    match skind stmt with
    | Universal.Ast.S_assign(e,e') ->
      Eval.compose_exec_list
        [e; e']
        (fun l flow ->
          match l with
          | [e;e'] ->
            begin
              match ekind e, etyp e |> is_pointer with
              | CellAst.Cell _, false ->
                Some (man.exec {stmt with skind = Universal.Ast.S_assign(e,e')} ctx flow)
              | CellAst.Cell c, true  ->
                begin
                  match ekind e' with
                  | CellAst.Pointer(p, o) ->
                    let u = get_my_current_abstraction flow man in
                    let u = u
                            |> Typ.CPML.remove c
                            |> Typ.CPML.add c p
                    in
                    let new_flow = set_my_current_abstraction u flow man in
                    Some (man.exec {stmt with skind = Universal.Ast.S_assign(e,o)} ctx new_flow)
                  | _ ->
                    failwith "[C.cell.CPointer.exec] assignation of \
                              pointer type, can not rewrite right hand \
                              side in a pointer"
                end
              | _ ->
                Some (man.exec {stmt with skind = Universal.Ast.S_assign(e,e')} ctx flow)
            end
          | _ -> assert false
        ) (fun flow -> Exec.return flow)
        man ctx gabs
    | _ -> Exec.fail

  let rec fold_int f x0 (g,d) =
    if g > d then x0
    else fold_int f (f g x0) (g+1,d)

  let eval expr man ctx flow =
    let open Universal.Ast in
    let range = erange expr in
    match ekind expr with
    | E_c_address_of e' ->
      begin
        Eval.compose_eval
          e'
          (fun e' flow ->
            let open Typ.Cell in
            match ekind e' with
            | CellAst.Cell c ->
              (
                Some { expr with ekind = CellAst.Pointer(Typ.PSL.singleton (Typ.P.V c.v), mk_int c.o (tag_range range "offset"))},
                flow, []
              ) |>
              Eval.singleton
            | _ -> None
          )
          (fun flow -> Eval.singleton (None, flow, []))
          man ctx flow
      end
    | CellAst.Cell c when c.Typ.Cell.t |> is_pointer ->
      begin
        (
          let pp = get_my_current_abstraction flow man in
          let p = find c pp in
          (Some {expr with ekind = CellAst.Pointer(p, mk_expr (CellAst.Cell c) (erange expr))},flow,[])
          |> Eval.singleton
        )
      end
    | E_binop(op, g, d) when op = O_plus && g |> etyp |> is_pointer ->
      begin
        Eval.compose_eval
          g
          (fun g' flow ->
            match ekind g' with
            | CellAst.Pointer(b, o) ->
              (Some {expr with ekind = CellAst.Pointer(b, mk_binop o O_plus d range)}, flow, [])
              |> Eval.singleton
            | _ -> None
          )
          (fun flow -> Eval.singleton (None, flow, []))
          man ctx flow
      end
    | E_c_deref e' ->
      begin
        Eval.compose_eval
          e'
          (fun e' flow ->
            match ekind e' with
            | CellAst.Pointer(b, o) ->
              begin
                let s =
                  man.ask
                    (Universal.Numeric.Query.QInterval o)
                    ctx flow
                in
                match s with
                | None -> assert false (*TODO*)
                | Some int ->
                  if Universal.Numeric.Interval.is_bounded int then
                    let left, right = Universal.Numeric.Interval.get_bounds int in
                    Typ.PSL.fold (fun p acc ->
                        match p with
                        | Typ.P.V v ->
                          let open Typ.Cell in
                          fold_int (fun i acc ->
                              let new_cell = {v = v; o = i; t = expr |> etyp} in
                              let this_case = (Some {expr with ekind = CellAst.Cell new_cell}, flow, []) |> Eval.singleton in
                              Eval.join acc this_case
                            ) acc (left,right)
                        | Typ.P.Invalid -> assert false (*TODO*)
                        | Typ.P.Null    -> assert false (*TODO*)
                      ) b None
                  else
                    assert false
              end
            | _ -> assert false
          )
          (fun flow -> Eval.singleton (None, flow, []))
          man ctx flow
      end
    | _ -> None
  let ask _ _ _ _ = None
    let unify _ u u' = (u,u')
  end

let setup () =
  register_domain name (module CPointer)
