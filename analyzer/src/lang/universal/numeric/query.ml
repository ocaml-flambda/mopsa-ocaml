(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type _ Framework.Query.query +=
  | QIntInterval : Framework.Ast.expr -> Values.Int.t Framework.Query.query
  | QIntList : Framework.Ast.expr -> Z.t list Framework.Query.query

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
      domatch = (let check : type a. a query -> (a, Z.t list) eq option =
                   function
                   | QIntList _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun l1 l2 -> l1 @ l2 |> List.sort_uniq Z.compare);
      meet = (fun l1 l2 -> List.filter (fun i1 -> List.exists (Z.equal i1) l1)  l1);
    }

  )
