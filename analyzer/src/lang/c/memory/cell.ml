open Framework.Ast
open Framework.Pp
open Typ
    
type cell =
  {
    v : Universal.Ast.var ; (* Base variable *)
    o : int ;               (* Offset        *)
    t : typ     (* Type          *)
  }

let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%a,%a⟩"
    Format.pp_print_string (Universal.Ast.(c.v.unname))
    Format.pp_print_int c.o
    pp_typ c.t

let compare_cell c c' =
  Comp.triple_compare
    Universal.Ast.compare_var
    (-)
    compare (* TODO replace this compare by a real comparison function over C_AST.type_qual*)
    (c.v , c.o , c.t )
    (c'.v, c'.o, c'.t)
(* Cell *)
module CellValue =
struct
  type t = cell
  let print = pp_cell
  let compare = compare_cell
  let apply_renaming (r : VVM.t) (c : t) =
    {c with v = apply_renaming_var r c.v}
end


(** cell set *)
module CS = struct
  include SetP.Make(CellValue)
  let apply_renaming (r : VVM.t) =
    map (CellValue.apply_renaming r)

  (** [exist_and_find p cs] gives back an element of [cs] that satisfies [p]*)
  let exist_and_find (p : cell -> bool) (cs : t) : cell option =
      let exception Found of cell in
      try
        let () = iter (fun e -> if p e then raise (Found e) else ()) cs in
        None
      with
      | Found e -> Some e

  let remove_vars (s : VS.t) (cs : t) =
    let filtering = mem_predicate s in
    filter (fun c -> filtering c.v) cs
end

(** (var <-> cell) equiv *)
module CVE = struct
  include Equiv.Make(CellValue)(V)
  let apply_renaming (r : VVM.t) =
    map (fun (c,v) -> CellValue.apply_renaming r c,v)
  let remove_vars (s : VS.t) (e : t) =
    let filtering = mem_predicate s in
    {lr = LR.filter (fun k _ -> filtering k.v) e.lr ;
     rl = RL.filter (fun _ v -> filtering v.v) e.rl ;
    }
end
