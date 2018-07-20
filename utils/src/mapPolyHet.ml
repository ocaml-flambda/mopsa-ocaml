(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Maps with polymorphic heterogeneous contents.

    [module M = Make(Key)] creates a module for encoding mappings from
    [('a, 'b) Key.t] to ['b], where ['b] is a polymorphic in ['a] and
    can change from one mapping to another.

    For a key [('a, 'a t) K], an equality witness should be defined as
    follows:
    {[
      M.register {
        eq = (let check : type b y. (b, y) Key.t -> ('a, b, 'a t, y) eq option =
                function
                | K -> some Eq
                | _ -> None
              in check);
      }
   ]}
*)

(** Witness of type equality *)
type (_, _, _, _) eq = Eq : ('a, 'a, 'b, 'b) eq

(** Signature of keys *)
module type KEY =
sig
  type ('a, 'b) t
end

module Make(Key: KEY) =
struct

  type ('a, 'b) w = {
    eq : 'c 'd. ('c, 'd) Key.t -> ('a, 'c, 'b, 'd) eq option;
  }

  type 'a v = V : ('a, 'b) w * 'b -> 'a v

  type 'a t = 'a v list

  type xw = XWitness : ('a, 'b) w -> xw

  let witness_pool : xw list ref = ref []

  let register witness = witness_pool := (XWitness witness) :: !witness_pool

  let find_witness k () =
    let rec aux : type a b. (a, b) Key.t -> xw list -> (a, b) w =
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

  let empty : 'a t = []

  let rec add : type b. ('a, b) Key.t -> b -> 'a t -> 'a t =
    fun k v -> function
      | [] -> [ V(find_witness k (), v)]
      | hd :: tl ->
        let V (w, _) = hd in
        match w.eq k with
        | Some Eq -> (V (w, v)) :: tl
        | None -> hd :: (add k v tl)

  let rec find : type b. ('a, b) Key.t -> 'a t -> b =
    fun k -> function
      | [] -> raise Not_found
      | hd :: tl ->
        let V (w, v) = hd in
        match w.eq k with
        | Some Eq -> v
        | None -> find k tl

end
