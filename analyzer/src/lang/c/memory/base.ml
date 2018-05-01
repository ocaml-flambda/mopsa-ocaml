open Framework.Ast
open Universal.Ast
       
(** lv base *)
type base =
  | V of var
  | A of Universal.Ast.addr

let pp_base fmt = function
  | V v -> Framework.Pp.pp_var fmt v
  | A a -> Universal.Pp.pp_addr fmt a

let compare_base b b' = match b, b' with
  | V v, V v' -> compare_var v v'
  | A a, A a' -> Universal.Ast.compare_addr a a'
  | _ -> compare b b'
