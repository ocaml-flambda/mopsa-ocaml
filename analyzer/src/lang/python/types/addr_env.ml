(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** An environment is a total map from variables to addresses. *)

open Mopsa
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

(*==========================================================================*)
(**                          {2 Environment}                                *)
(*==========================================================================*)
(* type addr_kind +=
 *    | A_py_instance of py_object (\** class of the instance *\) * obj_param option (\** optional parameters *\)
 *
 * let () =
 *   Format.(
 *     register_addr {
 *       print = (fun default fmt a ->
 *           match a.addr_kind with
 *           | A_py_instance(obj, _) -> fprintf fmt "<inst of %a at $addr%@%d>" pp_addr (addr_of_object obj) a.addr_uid
 *           | _ -> default fmt a);
 *       compare =(fun default a1 a2 ->
 *           match a1.addr_kind, a2.addr_kind with
 *           | A_py_instance (obj1, l1), A_py_instance (obj2, l2) ->
 *             Compare.pair
 *               Ast.compare_py_object
 *               (Compare.option (fun op1 op2 ->
 *                    match op1, op2 with
 *                    | List p1, List p2
 *                    | Tuple p1, Tuple p2
 *                    | Dict p1, Dict p2
 *                    | Range p1, Range p2 -> Ast.compare_py_object p1 p2
 *                    | Generator g1, Generator g2 -> Exceptions.panic "todo"
 *                    | _ -> Pervasives.compare op1 op2)) (obj1, l1) (obj2, l2)
 *           | _ -> default a1 a2)
 *     }
 *   ) *)

let mk_avar ?(vtyp = T_any) addr_uid =
  mkfresh (fun uid -> "$addr@" ^ (string_of_int addr_uid) ^ "_" ^ (string_of_int uid)) vtyp ()


module Domain =
struct

  module ASet = Framework.Lattices.Powerset.Make(PyAddr)
  module AMap = Framework.Lattices.Partial_map.Make
      (struct type t = var let compare = compare_var let print = Framework.Ast.pp_var end)
      (ASet)

  include AMap

  type _ domain += D_python_types_addr_env : t domain

  let id = D_python_types_addr_env
  let name = "python.types.addr_env"
  let identify : type a. a domain -> (t, a) eq option = function
    | D_python_types_addr_env -> Some Eq
    | _  -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = { export = [Zone.Z_py]; import = [Zone.Z_py_types]; }
  let eval_interface = { export = [Zone.Z_py, Zone.Z_py_addr]; import = [Zone.Z_py, Zone.Z_py_addr]; }

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow =
    Flow.set_domain_cur empty man flow
    |> Flow.without_callbacks
    |> OptionExt.return

  let rec exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var (v, mode)}, e) ->
      man.eval e flow |>
        Post.bind man
          (fun e flow ->
            match ekind e with
              | E_py_undefined true ->
                assign_addr man v PyAddr.Undef_global mode flow |> Post.of_flow

              | E_py_undefined false ->
                assign_addr man v PyAddr.Undef_local mode flow |> Post.of_flow

              (* | E_addr a ->
               *   assign_addr man v (PyAddr.Def a) mode flow |> Post.of_flow *)

              | E_py_object (addr, expr) ->
                assign_addr man v (PyAddr.Def addr) mode flow |> Post.of_flow

              | _ -> debug "%a@\n" pp_expr e; assert false
          )
      |> OptionExt.return

    | S_remove { ekind = E_var (v, _) } ->
      let flow = Flow.map_domain_cur (remove v) man flow in
      (* let v' = mk_py_value_var v T_any in
       * man.exec (mk_remove_var v' range) flow |> Post.return *)
      Post.return flow

    | S_assume e ->
       man.eval e flow |>
         Post.bind man (fun expr flow ->
             match ekind expr with
             | E_constant (C_bool true) -> Post.of_flow flow
             | E_constant (C_bool false) -> Post.of_flow (Flow.set_domain_cur bottom man flow)
             | _ ->
                Exceptions.panic_at range "todo addr_env/assume")
       |> OptionExt.return

    | S_rename ({ekind = E_addr a}, {ekind = E_addr a'}) ->
      let cur = Flow.get_domain_cur man flow in
      let ncur = AMap.map (ASet.map (fun addr -> if addr = Def a then Def a' else addr)) cur in
      debug "ncur = %a@\n" print ncur;
      let flow = Flow.set_domain_cur ncur man flow in
      man.exec ~zone:Zone.Z_py_types stmt flow |> Post.return

    | _ -> None

  and assign_addr man v av mode flow =
    let cur = Flow.get_domain_cur man flow in
    let aset = match mode with
      | STRONG -> ASet.singleton av
      | WEAK -> ASet.add av (find v cur)
    in
    Flow.set_domain_cur (add v aset cur) man flow


  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) ->
      let cur = Flow.get_domain_cur man flow in
      let aset = AMap.find v cur in
      ASet.fold (fun a acc ->
          let flow = Flow.set_domain_cur (AMap.add v (ASet.singleton a) cur) man flow in
          match a with
          | Undef_global when is_builtin_name v.org_vname ->
            (man.eval (mk_py_object (find_builtin v.org_vname) range) flow) :: acc

          | Undef_local when is_builtin_name v.org_vname ->
            (man.eval (mk_py_object (find_builtin v.org_vname) range) flow) :: acc

          | Undef_global ->
            let flow = man.exec (Utils.mk_builtin_raise "NameError" range) flow in
            Eval.empty_singleton flow :: acc

          | Undef_local ->
            let flow = man.exec (Utils.mk_builtin_raise "UnboundLocalError" range) flow in
            Eval.empty_singleton flow :: acc

          | Def addr ->
            man.eval (mk_py_object (addr, exp) range) flow :: acc

        ) aset []
      |> Eval.join_list |> OptionExt.return

    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
