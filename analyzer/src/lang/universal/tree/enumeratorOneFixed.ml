(**/**)
let debug fmt = ToolBox.debug "enumeratorOneFixed" fmt
(**/**)

type 'a t =
  {
    length     : int;
    mustbe     : 'a;
    cur_pos    : int;
    enumerator : 'a Enumerator.t
  }

let init e l n =
  if n <= 0 then failwith "EnumeratorOneFixed.init should be of length at least 1"
  else
    {
      length = n;
      mustbe = e;
      cur_pos = 0;
      enumerator = Enumerator.init_id l (n-1)
    }

let insert_nth e l n =
  let rec aux l n acc =
    if n = 0 then List.rev_append acc (e::l)
    else
      match l with
      | p::q -> aux q (n-1) (p::acc)
      | []   -> failwith "insert_nth too big"
  in
  let rep = aux l n [] in
  rep

let rec next x =
  match Enumerator.next x.enumerator with
  | None when x.cur_pos = x.length -1 -> None
  | None ->
    next ({ x with
            cur_pos = x.cur_pos +1;
            enumerator = Enumerator.reset x.enumerator
          }
         )
  | Some(e, enum) ->
    Some (insert_nth x.mustbe e x.cur_pos,
          {x with enumerator = enum})

let fold (f: 'a list -> 'b -> 'b) (i : 'a list) (e: 'a) (n: int) (acc: 'b) : 'b=
  let beg = init e i n in
  let rec aux (i : 'a t) acc =
    match next i with
    | None -> acc
    | Some(s, i') -> aux i' (f s acc)
  in
  aux beg acc
