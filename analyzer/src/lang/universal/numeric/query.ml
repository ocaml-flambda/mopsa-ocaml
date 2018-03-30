(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type _ Framework.Query.query +=
  | QInterval : Framework.Ast.expr -> Interval.t Framework.Query.query

let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Interval.t) eq option =
                   function
                   | QInterval _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun itv1 itv2 ->
          Interval.join itv1 itv2
        );

      meet = (fun itv1 itv2 ->
          Interval.meet itv1 itv2
        );
    }
  )
