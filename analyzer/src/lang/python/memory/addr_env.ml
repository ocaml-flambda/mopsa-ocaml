(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** An environment is a total map from variables to addresses. *)

open Framework.Essentials
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
    | Def a -> Universal.Ast.pp_addr fmt a
    | Undef_local -> Format.fprintf fmt "unbound"
    | Undef_global -> Format.fprintf fmt "undef"
end

(*==========================================================================*)
(**                          {2 Environment}                                *)
(*==========================================================================*)
type addr_kind +=
   | A_py_instance of py_object (** class of the instance *) * obj_param option (** optional parameters *)

let () =
  Format.(
    let info = {print =
                  (fun default fmt a ->
                    match a.addr_kind with
                    | A_py_instance(obj, _) -> fprintf fmt "<inst of %a at $addr%@%d>" pp_addr (addr_of_object obj) a.addr_uid
                    | _ -> default fmt a);
                compare =
                  (fun default a1 a2 ->
                    match a1.addr_kind, a2.addr_kind with
                    | A_py_instance (obj1, l1), A_py_instance (obj2, l2) ->
                       Compare.pair
                         Ast.compare_py_object
                         (Compare.option (fun op1 op2 ->
                              match op1, op2 with
                              | List p1, List p2
                                | Tuple p1, Tuple p2
                                | Dict p1, Dict p2
                                | Range p1, Range p2 -> Ast.compare_py_object p1 p2
                              | Generator g1, Generator g2 -> Debug.fail "todo"
                              | _ -> Pervasives.compare op1 op2)) (obj1, l1) (obj2, l2)
                    | _ -> default a1 a2) } in
    register_addr info
  )

module Domain =
struct

  module ASet = Framework.Lattices.Powerset.Make(PyAddr)
  module AMap = Framework.Lattices.Total_map.Make
      (struct type t = var let compare = compare_var let print = Framework.Ast.pp_var end)
      (ASet)

  include AMap

  type _ domain += D_python_memory_addr_env : t domain

  let id = D_python_memory_addr_env
  let name = "python.memory.addr_env"
  let identify : type a. a domain -> (t, a) eq option = function
    | D_python_memory_addr_env -> Some Eq
    | _  -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Zone.Z_py]; import = [Universal.Zone.Z_u_string]}
  let eval_interface = {export = [any_zone, any_zone]; import = []}

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow = Some (Flow.set_domain_cur top man flow)

  let mk_avar addr_uid ?(vtyp = T_any) () =
    let vname = "$addr@" ^ (string_of_int addr_uid) in
    {vname; vuid = addr_uid (*FIXME*); vtyp}

  let rec exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var (v, mode)}, e) ->
      man.eval e flow |>
        Post.bind man
          (fun e flow ->
            match ekind e with
            | E_py_undefined true -> assign_addr man v PyAddr.Undef_global mode flow |> Post.of_flow

            | E_py_undefined false -> assign_addr man v PyAddr.Undef_local mode flow |> Post.of_flow

            | E_py_object((addr, ev) as obj) ->
               debug "addr is %a@\n" pp_addr addr;
               let flow = assign_addr man v (PyAddr.Def addr) mode flow in
               debug "cur is now %a@\n" print (Flow.get_domain_cur man flow);
               man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [e] range) flow |>
                 Post.bind man
                   (fun cls flow ->
                     let t = match kind_of_object (Addr.most_derive_builtin_base (object_of_expr cls)) with
                       | A_py_class (C_builtin "int", _) -> T_int
                       | A_py_class (C_builtin "float", _) -> (T_float F_DOUBLE) (* FIXME *)
                       | A_py_class (C_builtin "bool", _) -> T_bool
                       | A_py_class (C_builtin "complex", _) -> T_py_complex
                       | A_py_class (C_builtin "str", _) -> T_string
                       | A_py_class (C_builtin "NoneType", _) -> T_py_none
                       | A_py_class (C_builtin "NotImplementedType", _) -> T_py_not_implemented
                       | _ -> T_py_empty
                     in
                     let v' = mk_avar addr.addr_uid ~vtyp:t () in
                     man.exec ~zone:Universal.Zone.Z_u_string (mk_assign (mk_var v' range) ev range) flow |> Post.of_flow
                   )
            | _ -> debug "%a@\n" pp_expr e; assert false
          )
      |> OptionExt.return

    | S_remove_var v ->
      let flow = Flow.map_domain_cur (remove v) man flow in
      (* let v' = mk_py_value_var v T_any in
       * man.exec (mk_remove_var v' range) flow |> Post.return *)
      Post.return flow

    | _ -> None

  and assign_addr man v av mode flow =
    let cur = Flow.get_domain_cur man flow in
    let aset = match mode with
      | STRONG -> ASet.singleton av
      | WEAK -> ASet.add av (find v cur)
    in
    debug "av=%a@\n" PyAddr.print av;
    debug "aset=%a@\n" ASet.print aset;
    Flow.set_domain_cur (add v aset cur) man flow


  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ v | v ∈ Var ⟧ *)
    | E_var (v, mode) ->
      let cur = Flow.get_domain_cur man flow in
      let aset = find v cur in
      ASet.fold (fun a acc ->
          let cur' = add v (ASet.singleton a) cur in
          let flow' = Flow.set_domain_cur cur' man flow in
          match a with
          | PyAddr.Undef_global when Addr.is_builtin_name v.vname ->
             man.eval (mk_py_object (Addr.find_builtin v.vname) range) flow |> Eval.join acc

          | PyAddr.Undef_local when Addr.is_builtin_name v.vname ->
             man.eval (mk_py_object (Addr.find_builtin v.vname) range) flow |> Eval.join acc

          | PyAddr.Undef_global ->
             let flow = man.exec (Utils.mk_builtin_raise "NameError" range) flow' in
             Eval.empty_singleton flow |> Eval.join acc

          | PyAddr.Undef_local ->
             let flow = man.exec (Utils.mk_builtin_raise "UnboundLocalError" range) flow' in
             Eval.empty_singleton flow |> Eval.join acc

          | PyAddr.Def addr ->
             man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [mk_py_object (addr, mk_py_empty range) range] range) flow |>
               Eval.bind
                 (fun cls flow ->
                   let t = match kind_of_object (Addr.most_derive_builtin_base (object_of_expr cls)) with
                     | A_py_class (C_builtin "int", _) -> T_int
                     | A_py_class (C_builtin "float", _) -> (T_float F_DOUBLE) (* FIXME *)
                     | A_py_class (C_builtin "bool", _) -> T_bool
                     | A_py_class (C_builtin "complex", _) -> T_py_complex
                     | A_py_class (C_builtin "str", _) -> T_string
                     | A_py_class (C_builtin "NoneType", _) -> T_py_none
                     | A_py_class (C_builtin "NotImplementedType", _) -> T_py_not_implemented
                     | _ -> T_py_empty
                   in
                   let v' = mk_avar addr.addr_uid ~vtyp:t () in
                   Eval.singleton (mk_py_object (addr, mk_var v' range) range) flow |> Eval.join acc
                 )
            (*  let t = Addr.type_of_object (addr, mk_py_empty range) in
             * let vv = mk_py_value_var v t in
             * Eval.singleton (mk_py_object (addr, mk_var vv range) range) flow |> Eval.join acc *)
        ) aset (*FIXME?*) (Eval.empty_singleton flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
       man.eval arg flow |>
         Eval.bind (fun e_arg flow ->
             match ekind e_arg with
             | E_py_object ({addr_kind = A_py_instance (instobj, _)}, _) ->
                Eval.singleton (mk_py_object instobj range) flow
             | _ -> assert false)
       |> OptionExt.return
    | _ -> None

  let ask _ _ _ = None


end

let () =
  Framework.Domain.register_domain (module Domain);
