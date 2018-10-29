module Make(A : SIG.COMPARABLE)(B : SIG.COMPARABLE) = struct
  type mapper =
    | L of B.t
    | N of (B.t * mapper) list

  let print_mapper v=
    let rec aux path = fun fmt m ->
      match m with
      | L x ->
        if path = [] then
          Format.fprintf fmt "%a -> %a @,"
            A.print v
            B.print x
        else
          Format.fprintf fmt "%a(%a) -> %a @,"
            A.print v
            (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") B.print) (List.rev path)
            B.print x
      | N l ->
        Format.fprintf fmt "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
             (fun fmt (x, m) -> (aux (x::path) fmt m))
          ) l
          (* List.iter (fun (s, m) -> aux m (s::path)) l *)
    in
    aux []

  module AM =
    Map.Make(A)

  type t =
    mapper AM.t
  let print fmt (u: t) =
    let l = (AM.bindings u) in
    if List.length l > 1 then
      Format.fprintf fmt "@[<v>@[<v 2>{@,%a@]@,}@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
           (fun fmt (z,t) -> Format.fprintf fmt "%a" (print_mapper z) t)
        ) l
    else
      Format.fprintf fmt "@[{%a}@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
           (fun fmt (z,t) -> Format.fprintf fmt "%a" (print_mapper z) t)
        ) l

  let empty =
    AM.empty

  let filter_symbol (s: A.t) (u: t): t =
    try
      let x = AM.find s u in
      AM.singleton s x
    with
    | Not_found -> AM.empty

  let add_transition (s, ql, q) (u: t): t =
    let map =
      try
        AM.find s u
      with
      | Not_found ->
        N []
    in
    let rec aux m ql = match ql with
      | p :: q ->
        begin
          match m with
          | L _ -> failwith "trying to insert too long work in mapper"
          | N l ->
            N (ToolBox.assoc_set B.compare p (function
                | None   -> aux (N []) q
                | Some x -> aux x q
              ) l
              )
        end
      | []    ->
        L q
    in
    u
    |> AM.remove s
    |> AM.add s (aux map ql)

  let find_transition (s: A.t) (ql : B.t list) (u: t) =
    let m = AM.find s u in
    let rec aux m ql = match ql with
      | p::q ->
        begin
          match m with
          | L _ -> raise Not_found
          | N l ->
            aux (ToolBox.assoc_find_exc B.compare p l) q
        end
      | [] ->
        begin
          match m with
          | L a -> a
          | _ -> raise Not_found
        end
    in
    aux m ql

  let fold f m acc =
    AM.fold (fun s map acc ->
        let rec aux acc map path = match map with
          | L q -> f (s, List.rev path, q) acc
          | N l -> List.fold_left (fun acc (qpart, x) ->
              aux acc x (qpart::path)
            ) acc l
        in
        aux acc map []
      ) m acc
end
