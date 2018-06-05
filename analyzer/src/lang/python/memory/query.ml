(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Queries on abstract values. *)

type _ Framework.Query.query +=
  | QBool : Framework.Ast.expr -> Value.B.t Framework.Query.query
  | QInt : Framework.Ast.expr -> Value.I.t Framework.Query.query

let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Value.B.t) eq option =
                   function
                   | QBool _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = Value.B.join;
      meet = Value.B.meet;
    };

    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Value.I.t) eq option =
                   function
                   | QInt _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = Value.I.join;
      meet = Value.I.meet;
    };

  )
