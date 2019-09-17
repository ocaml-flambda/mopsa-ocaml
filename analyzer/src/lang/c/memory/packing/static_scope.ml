(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Simple packing strategy based on static scoping of variables.

    The idea is simple: global variables are kept in one pack and
    local variables of each function are kept in separate packs.

    The packs may overlap to preserve some relations between local and
    global variables. Two pivot variables are considered:
      - return variables of functions,
      - formal parameters of functions.
*)

open Mopsa
open Sig.Domain.Simplified
open Universal.Numeric.Packing.Strategy
open Universal.Ast
open Ast
open Common.Base

module Strategy(Domain:DOMAIN) =
struct

  (** Name of the packing strategy *)
  let name = "static_c_scope"

  let debug fmt = Debug.debug ~channel:"packing.static_c_scope" fmt

  (** Packing key *)
  type key =
    | Globals (** Pack of global variables *)
    | Locals of string (** Pack of local variables of a function *)


  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1 f2
    | Globals, Locals _ -> 1
    | Locals _, Globals -> -1


  (** Pretty printer of packing keys *)
  let print fmt = function
    | Globals -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f


  (** Packing map binding keys to abstract elements *)
  module Map = MapExt.Make(struct type t = key let compare = compare end)


  (** Set of packing keys *)
  module Set = SetExt.Make(struct type t = key let compare = compare end)



  (** Inclusion test *)
  let subset m1 m2 =
    Map.for_all2zo
      (fun _ a1 -> false)
      (fun _ a2 -> true)
      (fun _ a1 a2 -> Domain.subset a1 a2)
      m1 m2


  (** Abstract union *)
  let join m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.join a1 a2)
      m1 m2


  (** Abstract intersection *)
  let meet m1 m2 =
    Map.merge (fun _ a1 a2 ->
        match a1, a2 with
        | None, _ | _, None -> None
        | Some aa1, Some aa2 -> Some (Domain.meet aa1 aa2)
      ) m1 m2


  (** Widening operator *)
  let widen ctx m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.widen ctx a1 a2)
      m1 m2


  (** Merging of divergent states *)
  let merge pre (m1,log1) (m2,log2) =
    Map.map2o
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun pack a1 a2 ->
         let prea = try Map.find pack pre with Not_found -> Domain.top in
         Domain.merge prea (a1,log1) (a2,log2)
      ) m1 m2


  (** Initial state *)
  let init prog = Map.singleton Globals (Domain.init prog)


  let is_return_var ctx v =
    try
      let ret, _ = Context.ufind Universal.Iterators.Interproc.Common.return_key ctx in
      compare_var ret v = 0
    with Not_found ->
      false
  
  (** Packs of a base memory block *)
  let packs_of_base ctx b =
    match b with
    | V { vkind = V_cvar {cvar_scope = Variable_global} }
    | V { vkind = V_cvar {cvar_scope = Variable_file_static _} } ->
      Set.empty

    | V { vkind = V_cvar {cvar_scope = Variable_local f} }
    | V { vkind = V_cvar {cvar_scope = Variable_func_static f} } ->
      Set.singleton (Locals f.c_func_unique_name)

    | V { vkind = V_cvar {cvar_scope = Variable_parameter f} } ->
      (* Parameters are part of the caller and the callee packs *)
      debug "parameter of %s" f.c_func_org_name; 
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs then Set.empty
      else
        let callee, cs' = Callstack.pop cs in
        if Callstack.is_empty cs' then Set.empty
        else
          let caller, _ = Callstack.pop cs' in
          Set.of_list [Locals caller.call_fun; Locals callee.call_fun]

    | V { vkind = Universal.Iterators.Interproc.Common.V_return call } ->
      (* Return variables are also part of the caller and the callee packs *)
      (* Note that the top of the callstack is not always the callee
         function, because the return variable is used after the function
         returns 
      *)
      debug "return of %a" pp_expr call; 
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs then Set.empty
      else
        let f1, cs' = Callstack.pop cs in
        let fname = match ekind call with
          | E_call ({ekind = E_function (User_defined f)},_) -> f.fun_name
          | Stubs.Ast.E_stub_call(f,_) -> f.stub_func_name
          | _ -> assert false
        in
        if Callstack.is_empty cs' then Set.singleton (Locals f1.call_fun)
        else if f1.call_fun <> fname then Set.singleton (Locals f1.call_fun)
        else
          let f2, _ = Callstack.pop cs' in
          Set.of_list [Locals f1.call_fun; Locals f2.call_fun]
    | V { vkind = V_tmp _ } ->
      (* Temporary variables are considered as locals *)
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs then Set.singleton Globals
      else
        let callee, _ = Callstack.pop cs in
        Set.singleton (Locals callee.call_fun)
    | _ ->
      debug "empty base pack";
      Set.empty


  let packs_of_base ctx b =
    let packs = packs_of_base ctx b in
    debug "packs(%a) = %a" pp_base b (Set.fprint SetExt.printer_default print) packs;
    packs


  (** Packing function returning packs of a variable *)
  let rec packs_of_var ctx prev v =
    debug "packs_of_var(%a)" pp_var v;
    match v.vkind with
    | V_cvar _ -> packs_of_base ctx (V v)
    | Lowlevel.Cells.Domain.V_c_cell {base} -> packs_of_base ctx base
    | Lowlevel.String_length.Domain.V_c_string_length (base,_) -> packs_of_base ctx base
    | Scalars.Pointers.Domain.Domain.V_c_ptr_offset vv -> packs_of_var ctx prev vv
    | _ ->
      match prev with
      | None -> debug "empty var pack"; Set.empty
      | Some e -> packs_of_expr ctx e

  and packs_of_expr ctx e =
    match e.ekind with
    | E_var (v,_) -> packs_of_var ctx e.eprev v
    | E_unop(_, ee) -> packs_of_expr ctx ee
    | E_binop(_, e1, e2) -> Set.union (packs_of_expr ctx e1) (packs_of_expr ctx e2)
    | _ -> Set.empty


  (** Check if an expression belongs to a pack *)
  let mem_pack ctx e pack =
    let packs = packs_of_expr ctx e in
    Set.mem pack packs


  (** Rewrite an expression by replacing variables not belonging to a pack by their over-approximation *)
  let resolve_missing_variables ctx pack e =
    Visitor.fold_map_expr
      (fun found ee ->
         match ekind ee with
         | E_var _ when mem_pack ctx ee pack ->
           Visitor.Keep (true, ee)
         | E_var _ ->
           (* FIXME: use intervals instead of top *)
           Visitor.Keep (found, mk_top ee.etyp ee.erange)
         | _ ->
           Visitor.VisitParts (found, ee)
      )
      (fun found s -> Visitor.VisitParts (found, s))
      false e


  (** Transfer function of assignments *)
  let exec_assign ctx v e range m =
    (* Find packs of the lval *)
    let packs = packs_of_expr ctx v in

    (* Initialize new packs *)
    let m = Set.fold (fun pack acc ->
        if Map.mem pack acc then acc else Map.add pack Domain.top acc
      ) packs m
    in
    debug "%a = %a;" pp_expr v pp_expr e;
    (* Execute pointwise *)
    Map.mapi (fun pack a ->
        if Set.mem pack packs then
          let _,e' = resolve_missing_variables ctx pack e in
          debug "   assigning %a in pack %a" pp_expr e' print pack;
          let stmt' = mk_assign v e' range in
          Domain.exec ctx stmt' a |> Option.none_to_exn
        else
          let () = debug "   skipping pack %a" print pack in
          a
      ) m

  (** Transfer function of tests *)
  let exec_assume ctx cond range m =
    let has_vars = Visitor.fold_expr
        (fun b e ->
           match ekind e with
           | E_var _ -> Keep true
           | _ -> if b then Keep true else VisitParts b
        )
        (fun b s -> if b then Keep true else VisitParts b)
        false cond
    in
    Map.mapi (fun pack a ->
        if not has_vars then
          Domain.exec ctx (mk_assume cond range) a |> Option.none_to_exn
        else
          let found, cond' = resolve_missing_variables ctx pack cond in
          if found then
            let stmt' = mk_assume cond' range in
            Domain.exec ctx stmt' a |> Option.none_to_exn
          else
            a
      ) m

  (** Transfer functions entry point *)
  let exec ctx stmt m =
    match skind stmt with
    | S_assign({ekind = E_var _} as v, e) ->
      exec_assign ctx v e stmt.srange m |>
      Option.return

    | S_assume(cond) ->
      exec_assume ctx cond stmt.srange m |>
      Option.return

    | _ ->
      try
        let m' = Map.map (fun a ->
            Domain.exec ctx stmt a |> Option.none_to_exn
          ) m
        in
        Some m'
      with Option.Found_None -> None

end

(** Registration *)
let () =
  Universal.Numeric.Packing.Functor.register_strategy "static_c_scope" (module Strategy)
