open Framework.Ast
open Framework.Pp
open Framework.Visitor
open Cell
open Typ

(** points-to set abstraction *)
module PSL = struct
  include Framework.Lattices.Top_set.Make(P)
  let apply_renaming (r : VVM.t) =
    map (P.apply_renaming r)
end


type expr_kind +=
  | E_c_cell of cell
  | E_c_pointer of PSL.t * expr


let () =
  (** Pretty-printer *)
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_cell c -> pp_cell fmt c
      | E_c_pointer(v, e) ->
        Format.fprintf fmt "(%a,%a)" PSL.print v Framework.Pp.pp_expr e
      | _ -> default fmt expr
    );
  (** Visitors *)
  register_expr_visitor ( fun default exp ->
      match ekind exp with
      | E_c_cell c -> leaf exp
      | E_c_pointer(v,e) ->
        {exprs = [e] ; stmts = []},
        (fun parts -> {exp with ekind = E_c_pointer(v,List.hd parts.exprs)})
      | _ -> default exp
    )
