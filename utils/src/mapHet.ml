(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open MapHetSig

(** Maps with heterogeneous contents *)
module Make(Key: KEY) =
struct

  type 'a w = {
    eq : 'b. 'b Key.t -> ('a, 'b) eq option;
  }

  type v = V : 'a w * 'a -> v

  type t = v list

  type xw = XWitness : 'a w -> xw

  let witness_pool : xw list ref = ref []

  let register witness = witness_pool := (XWitness witness) :: !witness_pool

  let find_witness k () =
    let rec aux : type a. a Key.t -> xw list -> a w =
      fun k l ->
        match l with
        | [] -> raise Not_found
        | hd :: tl ->
          let XWitness w = hd in
          match w.eq k with
          | Some Eq -> w
          | None -> aux k tl
    in
    aux k !witness_pool


  let rec add : type a. a Key.t -> a -> t -> t =
    fun k v -> function
      | [] -> [ V(find_witness k (), v)]
      | hd :: tl ->
        let V (w, _) = hd in
        match w.eq k with
        | Some Eq -> (V (w, v)) :: tl
        | None -> hd :: (add k v tl)

  let rec find : type a. a Key.t -> t -> a =
    fun k -> function
      | [] -> raise Not_found
      | hd :: tl ->
        let V (w, v) = hd in
        match w.eq k with
        | Some Eq -> v
        | None -> find k tl

end
