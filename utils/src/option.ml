(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Utility functions for ['a option] type. *)

let absorb (f:'a -> 'b option) (a:'a option) : 'b option =
  match a with None -> None | Some x -> f x

let absorb2
    (f:'a  -> 'b -> 'c option)
    (a:'a option)
    (b:'b option) : 'c option
  =
  match a,b with None,_ | _,None -> None | Some x, Some y -> f x y

let neutral2 (f:'a -> 'a -> 'a) (a:'a option) (b:'a option) : 'a option =
  match a,b with None,_ -> b | _,None -> a | Some x, Some y -> Some (f x y)

let merge
    (f1:'b -> 'a)
    (f2:'c -> 'a)
    (f12:'b ->'c -> 'a)
    (none:'a)
    (b1:'b option)
    (b2:'c option) : 'a
  =
  match b1, b2 with
  | None, None -> none
  | Some x1, None -> f1 x1
  | None, Some x2 -> f2 x2
  | Some x1, Some x2 -> f12 x1 x2


let equal (f:'a->'b->bool) (a:'a option) (b:'b option) : bool =
  match a,b with None, None -> true | Some x, Some y -> f x y | _ -> false

let included (f:'a->'b->bool) (a:'a option) (b:'b option) : bool =
  match a,b with None, _ -> true | Some x, Some y -> f x y | _ -> false

let apply (f:'b->'a) (none:'a) (b:'b option) : 'a =
  match b with None -> none | Some x -> f x

let apply2
    (none:'c)
    (f:'a->'b->'c)
    (a:'a option)
    (b:'b option) : 'c
  =
  match a,b with None,_ | _,None -> none | Some x, Some y -> f x y

let default (none:'b) (a:'a option) : 'b =
  apply (fun x -> x) none a

let compare (cmp: 'a -> 'a -> int) (a: 'a option) (b: 'a option) : int =
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

let lift2 (f:'a  -> 'b -> 'c) (a:'a option) (b:'b option) : 'c option =
  match a,b with None,_ | _,None -> None | Some x, Some y -> Some (f x y)

let lift_list (l: 'a option list) : 'a list option =
  let rec aux =
    function
    | [] -> Some []
    | None :: _ -> None
    | Some x :: tl ->
      aux tl |>
      lift (fun tl -> x :: tl)
  in
  aux l
