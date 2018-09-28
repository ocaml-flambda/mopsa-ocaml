(**
  Bot - Lift operations to a bottom element.


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


(** {2 Types} *)

type 'a with_bot = BOT | Nb of 'a
(** Adds a bottom element to a type. *)


(** {2 Operator lifting} *)


let bot_lift1 (f:'a -> 'b) (a:'a with_bot) : 'b with_bot =
  match a with BOT -> BOT | Nb x -> Nb (f x)

let bot_lift2 (f:'a  -> 'b -> 'c) (a:'a with_bot) (b:'b with_bot) : 'c with_bot =
  match a,b with BOT,_ | _,BOT -> BOT | Nb x, Nb y -> Nb (f x y)

let bot_absorb1 (f:'a -> 'b with_bot) (a:'a with_bot) : 'b with_bot =
  match a with BOT -> BOT | Nb x -> f x

let bot_absorb2 (f:'a  -> 'b -> 'c with_bot) (a:'a with_bot) (b:'b with_bot) : 'c with_bot =
  match a,b with BOT,_ | _,BOT -> BOT | Nb x, Nb y -> f x y

let bot_neutral2 (f:'a -> 'a -> 'a) (a:'a with_bot) (b:'a with_bot) : 'a with_bot =
  match a,b with BOT,_ -> b | _,BOT -> a | Nb x, Nb y -> Nb (f x y)

let bot_apply (f:'a->'b->'a) (a:'a) (b:'b with_bot) : 'a =
  match b with BOT -> a | Nb x -> f a x

let bot_apply2 (a1:'a) (a2:'a) (f:'b ->'b -> 'a) (b1:'b with_bot) (b2:'b with_bot) : 'a =
  match b1, b2 with
  | BOT, BOT -> a1
  | BOT, _ | _, BOT -> a2
  | Nb x1, Nb x2 -> f x1 x2

let bot_merge2 a b =
  match a,b with Nb x, Nb y -> Nb (x,y) | _ -> BOT

let bot_equal (f:'a->'b->bool) (a:'a with_bot) (b:'b with_bot) : bool =
  match a,b with BOT, BOT -> true | Nb x, Nb y -> f x y | _ -> false

let bot_included (f:'a->'b->bool) (a:'a with_bot) (b:'b with_bot) : bool =
  match a,b with BOT, _ -> true | Nb x, Nb y -> f x y | _ -> false

let bot_compare (cmp:'a -> 'a -> int) (a:'a with_bot) (b:'a with_bot) : int =
  match a,b with BOT,BOT -> 0 | BOT,_ -> -1 | _,BOT -> 1 | Nb x, Nb y -> cmp x y

let bot_dfl1 (dfl:'b) (f:'a->'c) (a:'a with_bot) : 'c =
  match a with BOT -> dfl | Nb x -> f x

let bot_dfl2 (dfl:'c) (f:'a->'b->'c) (a:'a with_bot) (b:'b with_bot) : 'c =
  match a,b with BOT,_ | _,BOT -> dfl | Nb x, Nb y -> f x y

let list_remove_bot (a:'a with_bot list) : 'a list =
  List.fold_left
    (fun acc x -> match x with Nb y -> y::acc | BOT -> acc)
    [] (List.rev a)


(** {2 Exceptions} *)


exception Found_BOT

let raise_bot () = raise Found_BOT
let catch_bot (dfl:'b) (f:'a -> 'b) (a:'a) : 'b = try f a with Found_BOT -> dfl
let bot_to_exn (a:'a with_bot) : 'a = match a with BOT -> raise Found_BOT | Nb x -> x
let exn_to_bot (f:'a ->'b)  (x:'a) : 'b with_bot = try Nb (f x) with Found_BOT -> BOT
let nobot msg (a:'a with_bot) : 'a = match a with BOT -> failwith (msg^": unexpected ⊥")  | Nb x -> x


(** {2 Printing} *)

let bot_string = "⊥"

let bot_to_string f x = match x with BOT -> bot_string | Nb x -> f x
let bot_print f ch x = match x with BOT -> output_string ch bot_string | Nb x -> f ch x
let bot_fprint f ch x = match x with BOT -> Format.pp_print_string ch bot_string | Nb x -> f ch x
let bot_bprint f ch x = match x with BOT -> Buffer.add_string ch bot_string | Nb x -> f ch x
