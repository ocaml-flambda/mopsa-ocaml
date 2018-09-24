(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Utility functions for ['a option] type. *)

let option_lift1 (f:'a -> 'b) (a:'a option) : 'b option =
  match a with None -> None | Some x -> Some (f x)

let option_lift2 (f:'a  -> 'b -> 'c) (a:'a option) (b:'b option) : 'c option =
  match a,b with None,_ | _,None -> None | Some x, Some y -> Some (f x y)

let option_absorb1 (f:'a -> 'b option) (a:'a option) : 'b option =
  match a with None -> None | Some x -> f x

let option_absorb2
    (f:'a  -> 'b -> 'c option)
    (a:'a option)
    (b:'b option) : 'c option
  =
  match a,b with None,_ | _,None -> None | Some x, Some y -> f x y

let option_neutral2 (f:'a -> 'a -> 'a) (a:'a option) (b:'a option) : 'a option =
  match a,b with None,_ -> b | _,None -> a | Some x, Some y -> Some (f x y)

let option_apply (f:'b->'a) (a:unit ->'a) (b:'b option) : 'a =
  match b with None -> a () | Some x -> f x

let option_apply2
    (f1:'b -> 'a)
    (f2:'c -> 'a)
    (f12:'b ->'c -> 'a)
    (a:unit -> 'a)
    (b1:'b option)
    (b2:'c option) : 'a
  =
  match b1, b2 with
  | None, None -> a ()
  | Some x1, None -> f1 x1
  | None, Some x2 -> f2 x2
  | Some x1, Some x2 -> f12 x1 x2


let option_merge2 a b =
  match a,b with Some x, Some y -> Some (x,y) | _ -> None

let option_equal (f:'a->'b->bool) (a:'a option) (b:'b option) : bool =
  match a,b with None, None -> true | Some x, Some y -> f x y | _ -> false

let option_included (f:'a->'b->bool) (a:'a option) (b:'b option) : bool =
  match a,b with None, _ -> true | Some x, Some y -> f x y | _ -> false

let option_dfl1 (dfl:'b) (f:'a->'b) (a:'a option) : 'b =
  match a with None -> dfl | Some x -> f x

let option_dfl2
    (dfl:'c)
    (f:'a->'b->'c)
    (a:'a option)
    (b:'b option) : 'c
  =
  match a,b with None,_ | _,None -> dfl | Some x, Some y -> f x y

let option_compare (cmp: 'a -> 'a -> int) (a: 'a option) (b: 'a option) : int =
  match a, b with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> cmp x y

exception Found_None

let raise_none () = raise Found_None
let catch_none (dfl:'b) (f:'a -> 'b) (a:'a) : 'b =
  try f a with Found_None -> dfl
let none_to_exn (a:'a option) : 'a =
  match a with None -> raise Found_None | Some x -> x
let exn_to_none (f:'a ->'b)  (x:'a) : 'b option =
  try Some (f x) with Found_None -> None

let print pp fmt x =
  match x with
  | None -> Format.fprintf fmt "None"
  | Some a -> pp fmt a

let return x = Some x

let bind f x =
  match x with
  | None -> None
  | Some x -> f x

let lift (f:'a -> 'b) (a:'a option) : 'b option =
  match a with None -> None | Some x -> Some (f x)
