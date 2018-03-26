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
open Ast
open Typ

let name = "c.memory.cell.pointer"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                               {2 AST}                                   *)
(*==========================================================================*)

(** points-to elements *)
module P =
struct
  type t =
    | V of Universal.Ast.var (* points to a variable *)
    | Null                   (* Null pointer         *)
    | Invalid                (* Invalid pointer      *)
  let print fmt p = match p with
    | V v -> Format.fprintf fmt "%a"
               Format.pp_print_string Universal.Ast.(v.unname)
    | Null -> Format.fprintf fmt "Null"
    | Invalid -> Format.fprintf fmt "Invalid"
  let compare p p' =
    match p, p' with
    | V x    , V y     -> Universal.Ast.compare_var x y
    | Null   , Null    -> 0
    | Invalid, Invalid -> 0
    | _                -> 1
  let apply_renaming (r : VVM.t) (p : t) =
    match p with
    | V v -> V (apply_renaming_var r v)
    | _ -> p
end


(** points-to set abstraction *)
module PSL = struct
  include Framework.Lattices.Top_set.Make(P)
  let apply_renaming (r : VVM.t) =
    map (P.apply_renaming r)
end


type expr_kind +=
  | E_c_pointer of PSL.t * expr

let () =
  (** Pretty-printer *)
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_pointer(v, e) ->
        Format.fprintf fmt "(%a,%a)" PSL.print v Framework.Pp.pp_expr e
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


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module CPointer =
struct

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)


  (** (cell -> pointsto lattice) lattice *)
  module CPML = struct
    include Framework.Lattices.Partial_map.Make(CellValue)(PSL)
    let apply_renaming (r : VVM.t) =
      map_p (fun (k,v) -> CellValue.apply_renaming r k, PSL.apply_renaming r v)
    let remove_vars (s : VS.t) l =
      let filtering = mem_predicate s in
      filter (fun k _ -> filtering k.v) l
  end


  include CPML

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    let myenv = man.flow.get TCur flow in
    man.flow.set
      TCur
      (man.ax.set top myenv)
      flow

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
    | Universal.Ast.S_assign(e,e', kind) when is_pointer e.etyp ->
      Eval.compose_exec_list
        [e; e']
        (fun l flow ->
          match l with
          | [e;e'] ->
            begin
              match ekind e with
              | E_c_cell c ->
                begin
                  match ekind e' with
                  | E_c_pointer(p, o) ->
                    let u = get_my_current_abstraction flow man in
                    let u = u
                            |> CPML.remove c
                            |> CPML.add c p
                    in
                    let new_flow = set_my_current_abstraction u flow man in
                    Some (man.exec {stmt with skind = Universal.Ast.S_assign(e,o,kind)} ctx new_flow)
                  | _ ->
                    failwith "[C.cell.CPointer.exec] assignation of \
                              pointer type, can not rewrite right hand \
                              side in a pointer"
                end
              | _ ->
                assert false
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
            match ekind e' with
            | E_c_cell c ->
              (
                Some { expr with ekind = E_c_pointer(PSL.singleton (P.V c.v), mk_int c.o (tag_range range "offset"))},
                flow, []
              ) |>
              Eval.singleton
            | _ -> None
          )
          (fun flow -> Eval.singleton (None, flow, []))
          man ctx flow
      end
    | E_c_cell c when c.t |> is_pointer ->
      begin
        (
          let pp = get_my_current_abstraction flow man in
          let p = find c pp in
          (Some {expr with ekind = E_c_pointer(p, mk_expr (E_c_cell c) (erange expr))},flow,[])
          |> Eval.singleton
        )
      end
    | E_binop(op, g, d) when op = O_plus && g |> etyp |> is_pointer ->
      begin
        Eval.compose_eval
          g
          (fun g' flow ->
            match ekind g' with
            | E_c_pointer(b, o) ->
              (Some {expr with ekind = E_c_pointer(b, mk_binop o O_plus d range)}, flow, [])
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
            | E_c_pointer(b, o) ->
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
                    PSL.fold (fun p acc ->
                        match p with
                        | P.V v ->
                          fold_int (fun i acc ->
                              let new_cell = {v = v; o = i; t = expr |> etyp} in
                              let this_case = (Some {expr with ekind = E_c_cell new_cell}, flow, []) |> Eval.singleton in
                              Eval.join acc this_case
                            ) acc (left,right)
                        | P.Invalid -> assert false (*TODO*)
                        | P.Null    -> assert false (*TODO*)
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
