(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type _ Framework.Query.query +=
  | QIntInterval : Framework.Ast.expr -> Values.Int.t Framework.Query.query
  | QIntStepInterval : Framework.Ast.expr -> (Values.Int.t (** interval *) * Z.t (** step *)) Framework.Query.query

let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Values.Int.t) eq option =
                   function
                   | QIntInterval _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = Values.Int.join;
      meet = Values.Int.meet;
    };

    register_reply_manager {
      domatch = (let check : type a. a query -> (a, (Values.Int.t * Z.t)) eq option =
                   function
                   | QIntStepInterval _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun (i1, s1) (i2, s2) -> Values.Int.join i1 i2, Z.gcd s1 s2);
      meet = (fun (i1, s1) (i2, s2) -> Values.Int.meet i1 i2, Z.lcm s1 s2);
    };
  )
