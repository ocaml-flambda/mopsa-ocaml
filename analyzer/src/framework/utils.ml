open Ast
open Pp

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

module Total_var_map = Lattices.Total_map.Make(Var)
