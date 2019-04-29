(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

(** An environment is a total map from variables to addresses. *)

open Mopsa
open Sig.Domain.Intermediate
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute


(*==========================================================================*)
(**                            {2 Addresses}                                *)
(*==========================================================================*)

module PyAddr =
struct
  type t =
    | Def of addr (* defined address *)
    | Undef_local (* initial undefined value of local variables *)
    | Undef_global (* initial undefined value of global variables *)

  (** Address comparator. *)
  let compare a1 a2 =
    match a1, a2 with
    | Def addr1, Def addr2 -> compare_addr addr1 addr2
    | _ -> compare a1 a2

  let print fmt =
    function
    | Def a -> Universal.Ast.pp_addr fmt a
    | Undef_local -> Format.fprintf fmt "unbound"
    | Undef_global -> Format.fprintf fmt "undef"
end

let mk_avar ?(vtyp = T_any) addr_uid =
  mkfresh (fun uid -> "$addr@" ^ (string_of_int addr_uid) ^ "_" ^ (string_of_int uid)) vtyp ()


module Domain =
struct

  module ASet = Framework.Lattices.Powerset.Make(PyAddr)
  module AMap = Framework.Lattices.Partial_map.Make
      (struct type t = var let compare = compare_var let print = pp_var end)
      (ASet)

  include AMap

  include Framework.Core.Id.GenDomainId(struct
      type typ = t
      let name = "python.types.addr_env"
    end)

  let interface = {
    iexec = { provides = [Zone.Z_py]; uses = [Zone.Z_py; Zone.Z_py_obj]; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let merge _ _ _ = assert false

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow =
    set_domain_env T_cur empty man flow

  let rec exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    (* | S_add ({ekind = E_var (v, mode)}) ->
     *   let cur = Flow.get_domain_cur man flow in
     *   if mem v cur then
     *     flow |> Post.return
     *   else
     *     let ncur = add v ASet.empty cur in
     *     let () = debug "cur was %a@\nncur is %a@\n" print cur print ncur in
     *     let flow =  Flow.set_domain_cur ncur man flow in
     *     let () = debug "flow is now %a@\n" (Flow.print man) flow in
     *     flow |> Post.return *)

    | S_assign({ekind = E_var (v, WEAK)}, {ekind = E_var (w, WEAK)}) ->
      let cur = get_domain_env T_cur man flow in
      if mem w cur then
        set_domain_env T_cur (add v (find w cur) cur) man flow |> Post.return |> Option.return
      else
        flow |> Post.return |> Option.return

    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var (v, mode)}, e) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
        post_eval man
          (fun e flow ->
            match ekind e with
              | E_py_undefined true ->
                assign_addr man v PyAddr.Undef_global mode flow |> Post.return

              | E_py_undefined false ->
                assign_addr man v PyAddr.Undef_local mode flow |> Post.return

              | E_py_object (addr, None) ->
                assign_addr man v (PyAddr.Def addr) mode flow |> Post.return

              | E_py_object (addr, Some expr) ->
                assign_addr man v (PyAddr.Def addr) mode flow |>
                man.exec ~zone:Zone.Z_py_obj (mk_assign (mk_addr addr range) expr range) |>  Post.return

              | _ -> debug "%a@\n" pp_expr e; assert false
          )
      |> Option.return

    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
      Eval.eval_list (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) [lval; rval] flow |>
      post_eval man (fun args flow ->
          let elval, erval = match args with [e1;e2] -> e1, e2 | _ -> assert false in
          man.exec ~zone:Zone.Z_py_obj (mk_assign (mk_py_attr elval attr range) erval range) flow |> Post.return
        )
      |> Option.return


    | S_remove ({ekind = E_var (v, _)} as var) ->
      let flow = map_domain_env T_cur (remove v) man flow in
      if String.length v.org_vname >= 4 && String.sub v.org_vname 0 4 = "$tmp" then
        Post.return flow |> Option.return
      else if String.length v.org_vname >= 3 && String.sub v.org_vname 0 3 = "$l*" then
        Post.return flow |> Option.return
      else
        (* if the variable maps to a list, we should remove the temporary variable associated, ONLY if it's not used by another list *)
        man.exec (mk_assign var (mk_expr (E_py_undefined true) range) range) flow |> Post.return |> Option.return
      (* let v' = mk_py_value_var v T_any in
       * man.exec (mk_remove_var v' range) flow |> Post.return *)
      (* Post.return flow *)

    | S_assume e ->
      debug "S_assume %a in flow@\n%a@\n" pp_expr e (Flow.print man.lattice) flow;
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
      post_eval man (fun expr flow ->
        match ekind expr with
        | E_constant (C_top T_bool)
        | E_constant (C_bool true)
          -> Post.return flow
        | E_py_object (a, _) when compare_addr a Typing.addr_true = 0 || compare_addr a Typing.addr_bool_top = 0
          -> Post.return flow
        | E_py_object (a, _) when compare_addr a Typing.addr_false = 0
          -> Post.return (set_domain_env T_cur bottom man flow)
        | E_constant (C_bool false) ->
          Post.return (set_domain_env T_cur bottom man flow)
        | _ ->
          Exceptions.panic_at range "todo addr_env/assume on %a@\n" pp_expr e
        )
      |> Option.return

    | S_rename ({ekind = E_addr a}, {ekind = E_addr a'}) ->
      let cur = get_domain_env T_cur man flow in
      let ncur = AMap.map (ASet.map (fun addr -> if addr = Def a then Def a' else addr)) cur in
      debug "ncur = %a@\n" print ncur;
      let flow = set_domain_env T_cur ncur man flow in
      let annot = Flow.get_ctx flow in
      let to_rename = Flow.fold (fun acc tk d ->
          match tk with
          | T_alarm {alarm_kind = Alarms.APyException ({ekind = E_py_object _}, _)} -> true
          | _ -> acc) false flow in
      let flow =
        if to_rename then
          Flow.fold (fun acc tk d ->
              match tk with
              | T_alarm ({alarm_kind = Alarms.APyException ({ekind = E_py_object (oa, oe)} as e, s)} as al) when compare_addr a oa = 0 ->
                Flow.add (T_alarm {al with alarm_kind = Alarms.APyException ({e with ekind = E_py_object (a', oe)}, s)}) d man.lattice acc
              | _ -> Flow.add tk d man.lattice acc) (Flow.bottom annot) flow
        else
          flow in
      begin match akind a with
        | A_py_instance _ -> man.exec ~zone:Zone.Z_py_obj stmt flow |> Post.return |> Option.return
        | _ -> flow |> Post.return |> Option.return
      end

    | _ -> None

  and assign_addr man v av mode flow =
    debug "assign_addr %a %a@\n" pp_var v PyAddr.print av;
    let cur = get_domain_env T_cur man flow in
    let aset = match mode with
      | STRONG -> ASet.singleton av
      | WEAK ->
        if mem v cur then
          ASet.add av (find v cur)
        else
          ASet.singleton av
    in
    set_domain_env T_cur (add v aset cur) man flow


  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) ->
      let cur = get_domain_env T_cur man flow in
      if AMap.mem v cur then
        let aset = AMap.find v cur in
        let evals, annot = ASet.fold (fun a (acc, annots) ->
            let flow = set_domain_env T_cur (AMap.add v (ASet.singleton a) cur) man flow in
            let flow = Flow.set_ctx annots flow in
            match a with
            | Undef_global when is_builtin_name v.org_vname ->
              (Eval.singleton (mk_py_object (find_builtin v.org_vname) range) flow :: acc, annots)

            | Undef_local when is_builtin_name v.org_vname ->
              (Eval.singleton (mk_py_object (find_builtin v.org_vname) range) flow :: acc, annots)

            | Undef_global ->
              let flow = man.exec (Utils.mk_builtin_raise "NameError" range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Undef_local ->
              let flow = man.exec (Utils.mk_builtin_raise "UnboundLocalError" range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Def addr ->
              let res = man.eval (mk_py_object (addr, Option.return @@ mk_addr addr range) range) flow in
              let annots = Eval.choose_ctx res in
              res :: acc, annots

          ) aset ([], Flow.get_ctx flow) in
        let evals = List.map (fun eval -> Eval.map_flow (fun flow -> Flow.set_ctx annot flow) eval) evals in
        evals |> Eval.join_list
        |> Option.return
      else if is_builtin_name v.org_vname then
        (* let () = debug "bla %s %s %d" v.org_vname v.uniq_vname v.vuid in *)
        (* man.eval (mk_py_object (find_builtin v.org_vname) range) flow |> Option.return *)
        let obj = find_builtin v.org_vname in
        Eval.singleton (mk_py_object obj range) flow |> Option.return
      else
        Eval.empty_singleton flow |> Option.return


    | _ -> None

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow

end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain);
