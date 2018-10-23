let debug fmt = ToolBox.debug "enumerator" fmt

type 'a state = 'a list

type 'a origin_type =
  | All of 'a state
  | Dif of 'a state list

type 'a nonempty =
  {
    origin  : 'a origin_type;
    current : 'a list state
  }

type 'a t =
  | E
  | NE of 'a nonempty

let init_id l n =
  if n = 0 then E
  else
    let rec aux i rep = if i = n then rep else aux (i+1) (l::rep) in
    NE
      {
        origin  = All l;
        current = aux 0 []
    }

let init (i : 'a list list) =
  if List.length i = 0 then
    E
  else
    NE
      {
        origin = Dif i;
        current = i;
      }

let reset (x: 'a t): 'a t = match x with
  | E -> E
  | NE x ->
    match x.origin with
    | All l -> init_id l (List.length x.current)
    | Dif l -> init l

let read_top_list c =
  if List.length c = 0 then
    None
  else
    begin
      try
        Some (List.map List.hd c)
      with
      | Failure _ -> None
    end

let next (l : 'a t) =
  let exception End in
  let rec aux o c = match o, c with
    | (p::r::r')::q, _ -> (r::r')::q
    | (p::[])::q, Dif (p'::q') -> p'::(aux q (Dif q'))
    | (p::[])::q, All l -> l :: (aux q c)
    | [], _ -> raise End
    | _ -> assert false
  in
  match l with
  | E -> Some ([], NE {origin = All []; current = []})
  | NE l ->
    begin
      match read_top_list l.current with
      | None -> None
      | Some x ->
        try
          Some (x, NE {l with current = aux l.current l.origin})
        with
        | End -> Some(x, NE {origin = All []; current = []})
    end

let fold (f : 'a state -> 'b -> 'b) (i : 'a list state) (acc : 'b) : 'b =
  let it = init i in
  let rec aux (i : 'a t) (acc : 'b) = match next i with
    | Some(s, i') -> aux i' (f s acc)
    | None -> acc
  in
  aux it acc

let fold_id (f : 'a state -> 'b -> 'b) (i : 'a list) (n : int) (acc : 'b) : 'b =
  let it = init_id i n in
  let rec aux (i : 'a t) (acc : 'b) = match next i with
    | Some(s, i') -> aux i' (f s acc)
    | None -> acc
  in
  aux it acc
