
let name_counters = Hashtbl.create 12

let fresh name =
  let i =
    try
      Hashtbl.find name_counters name
    with
    | Not_found -> 0
  in
  let () = Hashtbl.replace name_counters name (i+1) in
  Format.sprintf "%s_%d" name i

let fresh_var_t t name =
  let n = fresh name in
  {Universal.Ast.orgname = n ; Universal.Ast.unname = n ; vtyp = t}

let fresh_var name =
  let n = fresh name in
  {Universal.Ast.orgname = n ;
   Universal.Ast.unname = n ;
   vtyp = Framework.Ast.T_any
  }
