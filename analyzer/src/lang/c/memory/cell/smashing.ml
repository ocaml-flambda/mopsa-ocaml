(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Smashing-based abstraction of C memory cells. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Reduce_unify.Domain
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Framework.Lattice
open Framework.Eval
open Framework.Exec
open Ast
open Pointer

let name = "c.memory.cell.smashing"
let debug fmt = Debug.debug ~channel:name fmt


(** Smashed cell *)
type cell = {
  b : Pointer.base;
  t : typ;
}

let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%a⟩"
    pp_base c.b
    pp_typ c.t

let compare_cell c c' =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> compare_typ c.t c'.t);
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_smash_cell of cell



(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain(SubDomain: Framework.Domains.Stateful.DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** Set of cells variables. *)
  module CS = Framework.Lattices.Top_set.Make(struct
      type t = var
      let compare = compare_var
      let print fmt v = pp_var fmt v
    end)

  include CS

  let is_bottom x = false

  (** Pretty printer. *)
  let print fmt c =
    Format.fprintf fmt "smash cells: @[%a@]@\n"
      CS.print c

  let unify ctx a1 a2 = a1, a2

  (*==========================================================================*)
  (**                      {2 Sub-domain manager}                             *)
  (*==========================================================================*)

  let local_manager = mk_local_manager (module SubDomain : Framework.Domains.Stateful.DOMAIN with type t = SubDomain.t)

  let local_exec ctx stmt s =
    let flow = set_domain_cur s local_manager local_manager.flow.bottom in
    let flow' = local_manager.exec ctx stmt flow in
    get_domain_cur local_manager flow'


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init (man : ('a, t) manager) ctx prog (flow : 'a flow) =
    ctx, set_domain_cur empty man flow

  let exec (man : ('a, t) manager) subman (ctx : Framework.Context.context) (stmt : stmt) (flow : 'a flow)
    : 'a rflow option =
    match skind stmt with
    | _ -> None

  let eval man subman ctx exp flow =
    match ekind exp with

    | _ -> None

  let ask man subman ctx query flow = None

  let refine man subman ctx channel flow = None


end

let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_smash_cell c1, V_smash_cell c2 -> compare_cell c1 c2
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_smash_cell c -> pp_cell fmt c
      | _ -> next fmt vk
    )
