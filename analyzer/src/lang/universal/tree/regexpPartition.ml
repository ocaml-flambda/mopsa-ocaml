type t =
  (RegExp.t * string) list

let print_pair =
  ToolBox.print_pair
    (fun fmt x -> RegExp.pp_print_u fmt (RegExp.regexp_of_automata x))
    Format.pp_print_string

let print =
  ToolBox.print_list_inline
    (fun fmt (x, y) -> Format.fprintf fmt "(%a <~> %a)"
        RegExp.pp_print_u (RegExp.regexp_of_automata x)
        (* RegExp.print x *)
        Format.pp_print_string y)

let print_left =
  ToolBox.print_list_inline
    (fun fmt (x, (_: string)) -> Format.fprintf fmt "%a"
        RegExp.pp_print_u (RegExp.regexp_of_automata x)
    )

let s_of_a (a: RegExp.t) (u: t)=
  let exception Found of string in
  try
    List.iter (fun (a', s) -> if RegExp.default_eq a a' then raise (Found s)) u;
    raise Not_found
  with
  | Found s -> s

let apply_renaming (r : (string * string) list) (u: t) : t=
  List.map (fun (ue, un) ->
      try
        (ue, List.assoc un r)
      with
      | Not_found -> (ue, un)
    ) u

let rec replace_named n (v, n') l =
  List.map (fun ((_, na) as t)-> if na = n then (v, n') else t)

let rec replace (u, n) l = match l with
  | (u', n')::q when n = n' -> (u, n):: q
  | p :: q -> p :: (replace (u, n) q)
  | [] -> failwith "replace could not find what to replace"

let rec find_by_name n l =
  List.find (fun (x, n') -> n = n') l

let merge u l' l =
  u :: (List.filter (fun (ue', un') -> not (List.exists (fun (ue, un) -> un = un') l')) l)

let fold f x0 acc =
  List.fold_left (fun acc x -> f x acc) acc x0
