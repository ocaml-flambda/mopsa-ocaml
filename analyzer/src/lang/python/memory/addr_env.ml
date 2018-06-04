(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** An environment is a total map from variables to addresses. *)

open Framework.Domains.Stateful
open Framework.Flow
open Framework.Manager
open Framework.Query
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Framework.Visitor
open Framework.Utils
open Framework.Pp
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
(**                        {2 Value variables}                              *)
(*==========================================================================*)

type var_kind +=
  | V_py_value_var of var (** Value variable associated to an original
                             program variable *)

let mk_py_value_var v t =
  match vkind v with
  | V_orig -> {v with vkind = V_py_value_var v; vtyp = t}
  | _ -> assert false


(*==========================================================================*)
(**                          {2 Environment}                                *)
(*==========================================================================*)

module Domain =
struct

  module ASet = Framework.Lattices.Top_set.Make(PyAddr)
  module AMap = Framework.Lattices.Total_map.Make
      (struct type t = var let compare = compare_var let print = Framework.Pp.pp_var end)
      (ASet)

  include AMap

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init man ctx prog flow = ctx, set_domain_cur top man flow

  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var ({vkind = V_orig} as v)}, e, mode) ->
      man.eval ctx e flow |>
      eval_to_exec (fun e flow ->
          match ekind e with
          | E_py_undefined true -> assign_addr man ctx v PyAddr.Undef_global mode flow

          | E_py_undefined false -> assign_addr man ctx v PyAddr.Undef_local mode flow

          | E_addr a -> assign_addr man ctx v (PyAddr.Def a) mode flow

          | E_py_addr_value(a, ev) ->
            let flow = assign_addr man ctx v (PyAddr.Def a) mode flow in
            let t = Addr.to_atomic_type a in
            let v' = mk_py_value_var v t in
            man.exec ctx (mk_assign (mk_var v' range) ev ~mode range) flow

          | _ -> assert false
        ) (man.exec ctx) man.flow |>
      return

    | _ -> None

  and assign_addr man ctx v av mode flow =
    let cur = get_domain_cur man flow in
    let aset = match mode with
      | STRONG | EXPAND -> ASet.singleton av
      | WEAK -> ASet.add av (find v cur)
    in
    set_domain_cur (add v aset cur) man flow


  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ c | c ∈ Const ⟧ *)
    | E_constant c ->
      let addr = Addr.of_constant c range in
      oeval_singleton (Some (mk_py_addr_value addr exp range), flow, [])

    (* E⟦ v | v ∈ Var ⟧ *)
    | E_var ({vkind = V_orig} as v) ->
      let cur = get_domain_cur man flow in
      let aset = find v cur in
      ASet.fold (fun a acc ->
          let cur' = add v (ASet.singleton a) cur in
          let flow' = set_domain_cur cur' man flow in
          match a with
          | PyAddr.Undef_global ->
             let flow = man.exec ctx (Utils.mk_builtin_raise "NameError" range) flow' in
             oeval_singleton (None, flow, []) |>
             oeval_join acc

          | PyAddr.Undef_local ->
            let flow = man.exec ctx (Utils.mk_builtin_raise "UnboundLocalError" range) flow' in
            oeval_singleton (None, flow, []) |>
            oeval_join acc

          | PyAddr.Def addr when Addr.has_atomic_type addr ->
            let t = Addr.to_atomic_type addr in
            let vv = mk_py_value_var v t in
            oeval_singleton (Some (mk_py_addr_value addr (mk_var vv range) range), flow, []) |>
            oeval_join acc

          | PyAddr.Def addr ->
            oeval_singleton (Some (mk_addr addr range), flow, []) |>
            oeval_join acc

        ) aset None

    | _ -> None

  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_py_value_var v1, V_py_value_var v2 -> compare_var v1 v2
      | _ -> next vk1 vk2
    );
  register_pp_var (fun next fmt v ->
      match v.vkind with
      | V_py_value_var v ->
        Format.fprintf fmt "⟨%a⟩" Framework.Pp.pp_var v
      | _ -> next fmt v
    );
  ()
