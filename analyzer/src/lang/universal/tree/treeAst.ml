open Mopsa
open Framework.Visitor

type expr_kind +=
  (** Set of tree *)
  | E_tree_set of Value.VString.t

let () =
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_tree_set v1, E_tree_set v2 -> (*TODO: à quoi sert le compare??*)
        Pervasives.compare v1 v2
      | _ -> next e1 e2
    );
  register_expr_pp (fun next fmt e ->
      match ekind e with
      | E_tree_set v ->
        Format.fprintf fmt "E_tree_set(%a)" Value.VString.print v
      | _ -> next fmt e
    );
  register_expr_visitor (fun default exp ->
      match ekind exp with
      | E_tree_set _ -> leaf exp
      | _ -> default exp
    )
