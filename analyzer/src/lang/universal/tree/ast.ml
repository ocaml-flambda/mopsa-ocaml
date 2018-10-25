open Framework.Essentials

type expr_kind +=
  (** Set of tree *)
  | E_tree_set of TreeAbs.V.t

let () =
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_tree_set v1, E_tree_set v2 -> (*TODO: à quoi sert le compare??*)
        Pervasives.compare v1 v2
      | _ -> next e1 e2
    );
  register_pp_expr (fun next fmt e ->
      match ekind e with
      | E_tree_set v ->
        TreeAbs.V.print fmt v
      | _ -> next fmt e
    )
