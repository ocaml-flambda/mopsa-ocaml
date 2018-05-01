open Framework.Query
open Framework.Ast
open Base
    
type _ query +=
  | QExtractVarBase : var -> (base * expr) Framework.Query.query

let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, base * expr) eq option =
                   function
                   | QExtractVarBase _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun _ _ -> assert false);
      meet = (fun _ _ -> assert false);
    };

  )
