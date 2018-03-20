(**
  IntCong - Integer congruences.

  We rely on Zarith for arithmetic operations.


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


open Bot

       
(** {2 Types} *)

             
type t = Z.t (** stride *) * Z.t (** offset *)
(**
  The type of non-empty congruence sets (a,b) represents aℤ + b.
  a must be positive, and 0 ≤ b < a when a > 0.
  0ℤ+b represents the singleton {b}.
  1ℤ+0 represents the whole set of integers.
  aℤ+b represents the set { ak + b | k ∊ ℕ }.
 *)    

type t_with_bot = t with_bot
(** The type of possibly empty congruence sets. *)

let is_valid ((a,b):t) : bool =
  a = Z.zero || (a > Z.zero && Z.zero <= b && b < a)



(** {2 Arithmetic utilities} *)
  

let gcd (a:Z.t) (b:Z.t) : Z.t =
  if a = Z.zero then b
  else if b = Z.zero then a
  else Z.gcd a b
(** Greatest common divisor of |a| and |b|. 0 is neutral. *)

let gcd3 (a:Z.t) (b:Z.t) (c:Z.t) : Z.t =
  gcd (gcd a b) c
  
let gcd_ext (a:Z.t) (b:Z.t) : Z.t * Z.t * Z.t * Z.t =
  let gcd, u, v = Z.gcdext a b in
  gcd, Z.div (Z.mul a b) gcd, u, v  
(** Returns the gcd, lcm and cofactors u, v such that ua+vb=gcd. 
    Undefined if a or  is 0. 
*)
  
let divides (a:Z.t) (b:Z.t) : bool =
  if a = Z.zero then b = Z.zero
  else Z.rem b a = Z.zero
(** Wheter b is a multiple of a. Always true if b = 0. *)

let rem_zero (a:Z.t) (b:Z.t) : Z.t =
  if b = Z.zero then a else Z.erem a b
(** As Z.erem, but rem_zero a 0 = a. *)


  
(** {2 Constructors} *)


let of_z (a:Z.t) (b:Z.t) : t =
  if a = Z.zero then a, b
  else
    let a = Z.abs a in
    a, Z.erem b a
(** Returns  aℤ + b. *)

let of_int (a:int) (b:int) : t =
  of_z (Z.of_int a) (Z.of_int b)
    
let of_int64 (a:int64) (b:int64) : t =
  of_z (Z.of_int64 a) (Z.of_int64 b)


let cst (b:Z.t) : t = Z.zero, b
(** Returns  0ℤ + b *)

let cst_int (b:int) : t = cst (Z.of_int b)

let cst_int64 (b:int64) : t = cst (Z.of_int64 b)
                    
let zero : t = cst_int 0
(** 0ℤ+0 *)

let one  : t = cst_int 1
(** 0ℤ+1 *)
             
let mone : t = cst_int (-1)
(** 0ℤ - 1 *)
             
let minf_inf : t = of_int 1 0
(** 1ℤ + 0 *)


                 
(** {2 Predicates} *)



let equal (a:t) (b:t) : bool =
  a = b
(** Equality. (=) also works. *)

let equal_bot : t_with_bot -> t_with_bot -> bool =
  bot_equal equal

let included ((a,b):t) ((c,d):t) : bool =
  divides c a && rem_zero b c = d
(** Set ordering. *)

let included_bot : t_with_bot -> t_with_bot -> bool =
  bot_included included  

let intersect ((a,b):t) ((a',b'):t) : bool =
  rem_zero (Z.sub b b') (gcd a a') = Z.zero
(** Whether the intersection is non-empty. *)
     
let intersect_bot : t_with_bot -> t_with_bot -> bool =
  bot_dfl2 false intersect

let contains (x:Z.t) ((a,b):t) : bool =
  rem_zero x a = b
(** Whether the set contains some value x. *)

let total_order ((a,b):t) ((a',b'):t) : int =
  if a = a' then Z.compare b b' else Z.compare a a'
(**
  A total ordering (lexical ordering). 
  Can be used as compare for sets, maps, etc. 
*)

let contains_zero ((a,b):t) : bool =
  b = Z.zero
(** Whether the congruence contains zero. *)
   
let contains_one ((a,b):t) : bool =
  a = Z.one || b = Z.one
(** Whether the congruence contains one. *)
   
let contains_nonzero (ab:t) : bool =
  ab <> zero
(** Whether the congruence contains a non-zero value. *)

let is_zero (ab:t) : bool = ab = zero
let is_positive ((a,b):t) : bool = a = Z.zero && b >= Z.zero
let is_negative ((a,b):t) : bool = a = Z.zero && b <= Z.zero
let is_positive_strict ((a,b):t) : bool = a = Z.zero && b > Z.zero
let is_negative_strict ((a,b):t) : bool = a = Z.zero && b < Z.zero
let is_nonzero ((a,b):t) : bool = b <> Z.zero
(** Sign. *)

let is_singleton ((a,b):t) : bool =
  a = Z.zero
(** Whether the congruence contains a single element. *)

let is_bounded (ab:t) : bool =
  is_singleton ab
(** Whether the congruence contains a finite number of elements. *)
  
let is_in_range ((a,b):t) (lo:Z.t) (up:Z.t) =
  a = Z.zero && b >= lo && b <= up
(** Whether the congruence is included in the range [lo,up]. *)

  

(** {2 Printing} *)


let to_string ((a,b):t) : string = (Z.to_string a)^"ℤ+"^(Z.to_string b)
let print ch (x:t) = output_string ch (to_string x)
let fprint ch (x:t) = Format.pp_print_string ch (to_string x)
let bprint ch (x:t) = Buffer.add_string ch (to_string x)

let to_string_bot = bot_to_string to_string
let print_bot = bot_print print
let fprint_bot = bot_fprint fprint
let bprint_bot = bot_bprint bprint


               
(** {2 Set operations} *)


let join ((a,b):t) ((a',b'):t) : t =
  of_z (gcd3 a a' (Z.sub b b')) b'
(** Abstract union. *)
    
let join_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_neutral2 join a b

let join_list (l:t list) : t_with_bot =
  List.fold_left (fun a b -> join_bot a (Nb b)) BOT l

let meet ((a,b):t) ((a',b'):t) : t_with_bot =
  match a = Z.zero, a' = Z.zero with
  | true, true   -> if b = b' then Nb (a,b) else BOT
  | true, false  -> if contains b (a',b') then Nb (a,b) else BOT
  | false, true  -> if contains b' (a,b) then Nb (a',b') else BOT
  | false, false ->
     let gcd, lcm, u, _ = gcd_ext a a' in
     if Z.rem (Z.sub b b') gcd = Z.zero
     then Nb (of_z lcm (Z.sub b (Z.div (Z.mul a (Z.mul u (Z.sub b b'))) gcd)))
     else BOT
(** Abstract intersection. *)
     
let meet_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_absorb2 meet a b
          
let meet_list (l:t list) : t_with_bot =
  List.fold_left (fun a b -> meet_bot a (Nb b)) (Nb minf_inf) l

let meet_range ((a,b):t) (lo:Z.t) (up:Z.t) : t_with_bot =
  if a = Z.zero && b < lo || b > up then BOT
  else Nb (a,b)
(** Abstract intersection with [lo,up]. *)
  
let positive (a:t) : t_with_bot =
  if is_negative_strict a then BOT else Nb a

let negative (a:t) : t_with_bot =
  if is_positive_strict a then BOT else Nb a
(** Positive and negative part. *)

let meet_zero (a:t) : t_with_bot =
  meet a zero
(** Intersects with {0}. *)

let meet_nonzero (a:t) : t_with_bot =
  if equal a zero then BOT else Nb a
(** Keeps only non-zero elements. *)


  
(** {2 Forward operations} *)

  
let neg ((a,b):t) : t =
  of_z a (Z.neg b)
(** Negation. *)
  
let abs ((a,b):t) : t =
  if a = Z.zero then a, Z.abs b
  else if b = Z.zero then a, b
  else join (a,b) (a,Z.sub a b)
(** Absolute value. *)
  
let succ ((a,b):t) =
  of_z a (Z.succ b)
(** Adding 1. *)
  
let pred ((a,b):t) =
  of_z a (Z.pred b)
(** Subtracting 1. *)

  
let add ((a,b):t) ((a',b'):t) : t =
  of_z (gcd a a') (Z.add b b')
(** Addition. *)

let sub ((a,b):t) ((a',b'):t) : t =
  of_z (gcd a a') (Z.sub b b')
(** Subtraction. *)

let mul ((a,b):t) ((a',b'):t) : t =
  of_z (gcd3 (Z.mul a a') (Z.mul a b') (Z.mul a' b)) (Z.mul b b')
(** Multiplication. *)

let div ((a,b):t) ((a',b'):t) : t_with_bot =
  if a' = Z.zero then
    if b' = Z.zero then BOT (* aℤ+b / 0 *)
    else if divides b' a && divides b' b
    then Nb (of_z (Z.div a b') (Z.div b b')) (* aℤ+b / b' where b' divides a and b *)
    else Nb minf_inf (* aℤ+b / b' for other cases *)
  else Nb minf_inf (* general case *)
(** Division (with truncation). *)
  
let rem ((a,b):t) ((a',b'):t) : t_with_bot =
  if a' = Z.zero then
    if b' = Z.zero then BOT (* aℤ+b mod 0 *)
    else Nb (of_z (gcd a b') (Z.rem b b')) (* aℤ+b mod b' *)
  else Nb (of_z (gcd3 a a' b') b) (* general case *)
(** Remainder. Uses the C semantics for remainder (%). *)

let wrap ((a,b):t) (lo:Z.t) (up:Z.t) : t =
  let w = Z.succ (Z.sub up lo) in 
  of_z (gcd a w) (Z.add lo (Z.erem (Z.sub b lo) w))
(** Put back inside [lo,up] by modular arithmetics. *)
  
let shift_left ((a,b):t) ((a',b'):t) : t_with_bot =
  try
    if a' = Z.zero then
      if b' < Z.zero then BOT (* aℤ+b << negative constant *)
      else
        let bb' = Z.to_int b' in
        Nb (of_z (Z.shift_left a bb') (Z.shift_left b bb')) (* aℤ+b << b' *)
    else Nb minf_inf
  with Z.Overflow -> Nb minf_inf
(** Bitshift left: multiplication by a power of 2. *)

let shift_right ((a,b):t) ((a',b'):t) : t_with_bot =
  try
    if a' = Z.zero then
      if b' < Z.zero then BOT (* aℤ+b >> negative constant *)
      else 
        let bb' = Z.to_int b' in
        let z = Z.shift_left Z.one bb' in
        if a = Z.zero then Nb (cst (Z.shift_right b bb')) (* b >> b' *)
        else if divides z a && divides z b then
          Nb (of_z (Z.shift_right a bb') (Z.shift_right b bb')) (* aℤ+b >> b' exact *)
        else Nb minf_inf
    else Nb minf_inf
  with Z.Overflow -> Nb minf_inf
(** Bitshift right: division by a power of 2 rounding towards -∞. *)

let shift_right_trunc ((a,b):t) ((a',b'):t) : t_with_bot =
  try
    if a' = Z.zero then
      if b' < Z.zero then BOT (* aℤ+b >> negative constant *)
      else 
        let bb' = Z.to_int b' in
        let z = Z.shift_left Z.one bb' in
        if a = Z.zero then Nb (cst (Z.shift_right_trunc b bb')) (* b >> b' *)
        else if divides z a && divides z b then
          Nb (of_z (Z.shift_right a bb') (Z.shift_right b bb')) (* aℤ+b >> b' exact *)
        else Nb minf_inf
    else Nb minf_inf
  with Z.Overflow -> Nb minf_inf
(** Unsigned bitshift right: division by a power of 2 with truncation. *)
  
  
(** {2 Filters} *)



(** {2 Backward operations} *)


let _ = ()
      
