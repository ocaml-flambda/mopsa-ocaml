(** types and modules needed for the cell abstract domain *)

open Framework.Ast
open Framework.Pp

(** variables *)
module V =
struct
  type t = Universal.Ast.var
  let compare = Universal.Ast.compare_var
  let print fmt v = Format.fprintf fmt "%s" (Universal.Ast.orgname v)
end

(** (var -> var) map *)
module VVM = MapP.Make(V)(V)

let apply_renaming_var (r : VVM.t) (v : V.t) =
  try
    VVM.find v r
  with | Not_found -> v

(** variable set *)
module VS =
struct
  include SetP.Make(V)
  let apply_renaming (r : VVM.t) (s : t) =
    map (apply_renaming_var r) s
end

let mem_predicate (s : VS.t) =
  fun (x : V.t) -> VS.mem x s

(** points-to elements *)
module P =
struct
  type t =
    | V of Universal.Ast.var (* points to a variable *)
    | Null                   (* Null pointer         *)
    | Invalid                (* Invalid pointer      *)
  let print fmt p = match p with
    | V v -> Format.fprintf fmt "%a"
               Format.pp_print_string Universal.Ast.(v.unname)
    | Null -> Format.fprintf fmt "Null"
    | Invalid -> Format.fprintf fmt "Invalid"
  let compare p p' =
    match p, p' with
    | V x    , V y     -> Universal.Ast.compare_var x y
    | Null   , Null    -> 0
    | Invalid, Invalid -> 0
    | _                -> 1
  let apply_renaming (r : VVM.t) (p : t) =
    match p with
    | V v -> V (apply_renaming_var r v)
    | _ -> p
end

