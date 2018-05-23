(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Queries on heap abstract domain. *)

open Ast

type _ Framework.Query.query +=
  | QAllocatedAddresses : addr list Framework.Query.query
  (** Get the list of allocated objects *)

let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, addr list) eq option =
                   function
                   | QAllocatedAddresses -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (@);
      meet = (@);
    };

  )
