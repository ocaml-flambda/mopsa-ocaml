(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Queries on abstract values. *)

type _ Framework.Query.query +=
  | QInt : Framework.Ast.expr -> Value.I.t Framework.Query.query
  | QFloat : Framework.Ast.expr -> Value.F.t Framework.Query.query
  | QString : Framework.Ast.expr -> Value.S.t Framework.Query.query

let () =
  Framework.Query.(
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

    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Value.F.t) eq option =
                   function
                   | QFloat _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = Value.F.join;
      meet = Value.F.meet;
    };

    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Value.S.t) eq option =
                   function
                   | QString _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = Value.S.join;
      meet = Value.S.meet;
    };

  )
