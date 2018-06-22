(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** An environment is a total map from variables to addresses. *)

open Framework.Essentials
open Framework.Domain
open Universal.Ast
open Ast
open Addr

let name = "python.memory.addr_env"


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
    | Def a -> Universal.Pp.pp_addr fmt a
    | Undef_local -> Format.fprintf fmt "unbound"
    | Undef_global -> Format.fprintf fmt "undef"
end



(*==========================================================================*)
(**                          {2 Environment}                                *)
(*==========================================================================*)

module Domain =
struct

  module ASet = Framework.Lattices.Powerset.Make(PyAddr)
  module AMap = Framework.Lattices.Total_map.Make(Var)(ASet)

  include AMap

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man ctx flow = Some (ctx, set_domain_cur top man flow)

  let import_exec = [Zone.Z_py_value]
  let export_exec = [Zone.Z_py]

  let py_to_obj = Zone.Z_py, Zone.Z_py_object
  let import_eval = []
  let export_eval = [py_to_obj]

  let rec exec zone stmt man ctx flow =
    let range = srange stmt in
    match skind stmt with
    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var v}, e, mode) ->
      bind_post py_to_obj e man ctx flow @@ fun e flow ->
      begin match ekind e with
        | E_py_undefined true ->
          assign_addr man ctx v PyAddr.Undef_global mode flow |>
          Post.of_flow |>
          return

        | E_py_undefined false ->
          assign_addr man ctx v PyAddr.Undef_local mode flow  |>
          Post.of_flow |>
          return

        | E_py_object(addr, ev) ->
          let flow' = assign_addr man ctx v (PyAddr.Def addr) mode flow in
          let stmt' = mk_assign (mk_var v range) ev ~mode range in
          man.exec ~zone:Zone.Z_py_value stmt' ctx flow' |>
          Post.of_flow |>
          return

        | _ -> assert false
      end

    | S_remove_var v ->
      let flow = map_domain_cur (remove v) man flow in
      man.exec ~zone:Zone.Z_py_value stmt ctx flow |>
      Post.of_flow |>
      return


    | _ -> None

  and assign_addr man ctx v av mode flow =
    let cur = get_domain_cur man flow in
    let aset = match mode with
      | STRONG | EXPAND -> ASet.singleton av
      | WEAK -> ASet.add av (find v cur)
    in
    set_domain_cur (add v aset cur) man flow


  let eval zpath exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ v | v ∈ Var ⟧ *)
    | E_var v ->
      let cur = get_domain_cur man flow in
      let aset = find v cur in
      ASet.fold (fun a acc ->
          let cur' = add v (ASet.singleton a) cur in
          let flow' = set_domain_cur cur' man flow in
          match a with
          | PyAddr.Undef_global when Addr.is_builtin_name v.vname ->
            Eval.singleton (Some (mk_py_object (Addr.find_builtin v.vname) range)) flow :: acc

          | PyAddr.Undef_local when Addr.is_builtin_name v.vname ->
            Eval.singleton (Some (mk_py_object (Addr.find_builtin v.vname) range)) flow :: acc

          | PyAddr.Undef_global ->
             let flow = man.exec (Utils.mk_builtin_raise "NameError" range) ctx flow' in
             Eval.singleton None flow :: acc

          | PyAddr.Undef_local ->
            let flow = man.exec (Utils.mk_builtin_raise "UnboundLocalError" range) ctx flow' in
            Eval.singleton None flow :: acc

          | PyAddr.Def addr ->
            let t = Addr.type_of_object (addr, mk_py_empty range) in
            let vv = {v with vtyp = t} in
            Eval.singleton (Some (mk_py_object (addr, mk_var vv range) range)) flow :: acc

        ) aset [] |>
      Eval.of_list

    | _ -> None

  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain);
  ()
