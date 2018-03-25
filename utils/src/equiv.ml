(** Relation with printing builder *)

module Make(L : Sig.VALUE)(R : Sig.VALUE) =
struct
  exception Already_Paired
  module LR = Map.Make(L)
  module RL = Map.Make(R)
  type t =
    {
      lr : R.t LR.t ;
      rl : L.t RL.t ;
    }
  let print fmt (e : t) =
    Format.fprintf fmt "@[{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (z,t) -> Format.fprintf fmt "%a ↔ %a" L.print z R.print t)
      ) (LR.bindings e.lr)
  let compare (e : t) (e' : t) : int =
    try
      let r = LR.fold (fun k v acc ->
          try
            let v' = LR.find k acc in
            if R.compare v' v = 0 then
              LR.remove k acc
            else raise Exit
          with
          | Not_found -> raise Exit
        ) e.lr e'.lr
      in
      if LR.cardinal r = 0 then
        0
      else -1
    with
    | Exit -> 1

  let empty =
    {lr = LR.empty ; rl = RL.empty}

  let fold (f : L.t * R.t -> 'a -> 'a) (e : t) (a : 'a) =
    LR.fold (fun k v acc -> f (k,v) acc) e.lr a

  let remove_l (l : L.t) (e : t) =
    try
      let b = LR.find l e.lr in
      {lr = LR.remove l e.lr ; rl = RL.remove b e.rl}
    with
    | Not_found -> e

  let remove_r (r : R.t) (e : t) =
    try
      let l = RL.find r e.rl in
      {lr = LR.remove l e.lr ; rl = RL.remove r e.rl}
    with
    | Not_found -> e

  let add ((l,r) : L.t * R.t) (e : t) =
    try
      let r' = LR.find l e.lr in
      if R.compare r r' = 0 then
        e
      else
        raise Already_Paired
    with
    | Not_found ->
      {lr = LR.add l r e.lr ; rl = RL.add r l e.rl}

  let mem ((l,r) : L.t * R.t) (e : t) =
    try
      let r' = LR.find l e.lr in
      R.compare r r' = 0
    with
    | Not_found -> false

  let find_l l e =
    LR.find l e.lr

  let find_r l e =
    RL.find l e.rl

  let find_l_opt (l : L.t) (e : t) : R.t option =
    try
      Some (LR.find l e.lr)
    with
    | Not_found -> None

  let find_r_opt (r : R.t) (e : t) : L.t option =
    try
      Some (RL.find r e.rl)
    with
    | Not_found -> None

  let map (f : (L.t * R.t) -> (L.t * R.t)) (e : t) : t =
    fold (fun p acc -> add (f p) acc) e empty
end
