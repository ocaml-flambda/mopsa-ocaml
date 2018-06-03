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
open Framework.Utils
open Universal.Ast
open Ast
open Addr

let name = "python.memory.env"


(*==========================================================================*)
(**                            {2 Addresses}                                *)
(*==========================================================================*)

module PyAddr =
struct
  type t =
    | A of addr (* defined address *)
    | LU (* initial undefined value of local variables *)
    | GU (* initial undefined value of global variables *)

  (** Address comparator. *)
  let compare a1 a2 =
    match a1, a2 with
    | A addr1, A addr2 -> compare_addr addr1 addr2
    | _ -> compare a1 a2

  let print fmt =
    function
    | A a -> Universal.Pp.pp_addr fmt a
    | LU -> Format.fprintf fmt "unbound"
    | GU -> Format.fprintf fmt "undef"
end


(*==========================================================================*)
(**                          {2 Environment}                                *)
(*==========================================================================*)

module Domain =
struct

  module S = Framework.Lattices.Top_set.Make(PyAddr)
  module M = Framework.Lattices.Total_map.Make
      (struct type t = var let compare = compare_var let print = Framework.Pp.pp_var end)
      (S)

  include M

  let init man ctx prog flow = ctx, set_domain_cur top man flow
  let exec man ctx smt flow = None
  let eval _ _ _ _  = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain);
  ()
