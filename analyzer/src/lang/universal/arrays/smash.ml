(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Smashing abstraction of arrays. *)

open Framework.Essentials
open Ast

(** Zoning definition *)
(** ***************** *)

type Framework.Zone.zone +=
  | Z_smashing

let () = Framework.Zone.(register_zone {
    subset = (fun next z1 z2 ->
        match z1, z2 with
        | Z_smashing, Zone.Z_u -> true
        | _ -> next z1 z2
      );
    print = (fun next fmt z ->
        match z with
        | Z_smashing -> Format.fprintf fmt "universal/smash"
        | _ -> next fmt z
      )
  })


(** Smashing cardinality *)
(** ******************** *)

module Cardinality =
struct
  type t =
    | Empty
    | Singleton
    | Multi

  let bottom = Empty
  let top = Multi
  let is_bottom a = (a = Empty)
  let subset a b =
    match a, b with
    | Empty, _ -> true
    | _, Multi -> true
    | Singleton, Singleton -> true
    | _ -> false
  let join _ a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | Multi, _ | _, Multi -> Multi
    | Singleton, Singleton -> Singleton
  let meet _ a b =
    match a, b with
    | Empty, _ | _, Empty -> Empty
    | Multi, x | x, Multi -> x
    | Singleton, Singleton -> Singleton
  let widen = join
  let print fmt =
    function
    | Empty -> Format.fprintf fmt "∅"
    | Singleton -> Format.fprintf fmt  "𝟙"
    | Multi -> Format.fprintf fmt "⊤"
end



(** Abstract domain definition *)
(** ************************** *)

module Domain : Framework.Domain.DOMAIN =
struct

  (** Lattice structure *)
  (** ***************** *)

  (** Map of arrays cardinality *)
  include Framework.Lattices.Partial_map.Make(Var)(Cardinality)

  let is_bottom _ = false
  let widen = join
  let print fmt a = Format.fprintf fmt "smash:@\n  @[%a@]@\n" print a


  (** Domain identification *)
  (** ********************* *)

  type _ domain += D_universal_arrays_smash : t domain
  let id = D_universal_arrays_smash
  let name = "universal.arrays.smash"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_arrays_smash -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Initialization *)
  (** ************** *)

  let init prog man flow =
    Some (
      let flow' = Flow.set_domain_env T_cur empty man flow in
      debug "init %a" (Flow.print man) flow';
      flow'
    )


  (** Transfer functions interface *)
  (** **************************** *)

  let exec_interface = {
    export = [Zone.Z_u];
    import = [];
  }

  let eval_interface = {
    export = [
      (Framework.Zone.top, Z_smashing);
      (Framework.Zone.top, Zone.Z_u);
      (Framework.Zone.top, Zone.Z_u_num)
    ];
    import = [
      (Framework.Zone.top, Z_smashing);
      (Framework.Zone.top, Zone.Z_u)
    ];
  }


  (** Post-conditions *)
  (** *************** *)

  let mk_smash_var a =
    let vname =
      let () = Format.fprintf Format.str_formatter "%a[⋆]" pp_var a in
      Format.flush_str_formatter ()
    in
    let vtyp = match a.vtyp with
      | T_array t' -> t'
      | _ -> assert false
    in
    {vname; vuid = a.vuid; vtyp}

  (** Assign an expression to the smash of an array *)
  let assign_array a e mode range man flow =
    (* Get cardinality of the array variable *)
    let m = Flow.get_domain_env T_cur man flow in
    let card = find a m in

    (* Compute the assignment mode depending on cardinality *)
    let mode', card' =
      match card with
      | Cardinality.Empty ->  mode, Cardinality.Singleton
      | Cardinality.Singleton | Cardinality.Multi -> WEAK, Cardinality.Multi
    in

    let m' = add a card' m in
    let flow' = Flow.set_domain_env T_cur m' man flow in

    let lval' = mk_smash_var a in
    let stmt' = mk_assign (mk_var lval' ~mode:mode' range) e range in
    man.exec stmt' flow' |>
    Post.of_flow |>
    Post.add_merger (mk_remove_var lval' range)


  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_subscript(a, i)}, e) ->
      Some (
        (* Evaluate the array expression *)
        man.eval a ~zone:(Framework.Zone.top, Z_smashing) flow |>
        Post.bind man @@ fun a flow ->

        (* Evaluate the index expression *)
        man.eval i flow |>
        Post.bind man @@ fun i flow ->

        (* Compute assignment *)
        match ekind a with
        | E_var(a', mode) -> assign_array a' e mode range man flow
        | _ -> assert false
      )

    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval_array a range man flow =
    let v = mk_smash_var a in
    (* Get cardinality of array a *)
    let card = Flow.get_domain_env T_cur man flow |>
               find a
    in
    match card with
    | Cardinality.Empty | Cardinality.Singleton ->
      Eval.singleton (mk_var v ~mode:STRONG (tag_range range "strong")) flow
    | Cardinality.Multi ->
      Eval.singleton (mk_var v ~mode:WEAK (tag_range range "weak")) flow

  let eval zone exp man flow =
    match ekind exp with
    | E_subscript(a, i) ->
      Some (
        (* Evaluate the array expression *)
        man.eval a ~zone:(Framework.Zone.top, Z_smashing) flow |>
        Eval.bind @@ fun a flow ->

        (* Evaluate the index expression *)
        man.eval i flow |>
        Eval.bind @@ fun i flow ->

        match ekind a with
        | E_var(a', _) -> eval_array a' (erange exp) man flow
        | _ -> assert false
      )

    | _ -> None


  (** Queries *)
  (** ******* *)

  let ask query man flow = None

end

let () =
  Framework.Domain.register_domain (module Domain)
