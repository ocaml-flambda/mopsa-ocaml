(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Polymorphic `old` notations. *)

open Mopsa

type 'a with_old =
  | Cur of 'a (** current value *)
  | Old of 'a (** old value *)

let old_extract (a: 'a with_old) : 'a =
  match a with
  | Cur aa -> aa
  | Old aa -> aa

let old_lift (f: 'a -> 'b) (a: 'a with_old) : 'b with_old =
  match a with
  | Cur aa -> Cur (f aa)
  | Old aa -> Old (f aa)

let old_apply (f: 'a -> 'b) (a: 'a with_old) : 'b =
  f (old_extract a)

let old_apply2 (f: 'a -> 'b -> 'c) (a: 'a with_old) (b: 'b with_old) : 'c =
  f (old_extract a) (old_extract b)

let old_bind (f: 'a -> 'b with_old) (a: 'a with_old) : 'b with_old =
  old_apply f a

let old_print pp fmt a =
  match a with
  | Cur aa -> pp fmt aa
  | Old aa -> Format.fprintf fmt "old(%a)" pp aa

module Lift
    (M : sig
       type t
       val compare : t -> t -> int
       val print : Format.formatter -> t -> unit
     end) =
struct
  type t = M.t with_old

  let compare = old_apply2 M.compare

  let print = old_print M.print
end
