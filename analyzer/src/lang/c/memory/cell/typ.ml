(** types and modules needed for the cell abstract domain *)

open Framework.Ast
open Framework.Pp
open Framework.Visitor

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


type cell =
  {
    v : Universal.Ast.var ; (* Base variable *)
    o : int ;               (* Offset        *)
    t : typ                 (* Type          *)
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

(* Cell as a value*)
module CellValue =
struct
  type t = cell
  let print = pp_cell
  let compare = compare_cell
  let apply_renaming (r : VVM.t) (c : t) =
    {c with v = apply_renaming_var r c.v}
end


(** Set of cells *)
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



type expr_kind +=
  | E_c_cell of cell


let () =
  (** Pretty-printer *)
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_cell c -> pp_cell fmt c
      | _ -> default fmt expr
    );
  (** Visitors *)
  register_expr_visitor ( fun default exp ->
      match ekind exp with
      | E_c_cell c -> leaf exp
      | _ -> default exp
    )



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

