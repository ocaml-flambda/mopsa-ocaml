(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(**
  IntCong - Integer congruences.

  We rely on Zarith for arithmetic operations.
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


module I = ItvUtils.IntItv
module B = ItvUtils.IntBound


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
    Undefined if a or b is 0. 
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
(** Returns aℤ + b. *)

let of_int (a:int) (b:int) : t =
  of_z (Z.of_int a) (Z.of_int b)
    
let of_int64 (a:int64) (b:int64) : t =
  of_z (Z.of_int64 a) (Z.of_int64 b)

let cst (b:Z.t) : t = Z.zero, b
(** Returns 0ℤ + b *)

let cst_int (b:int) : t = cst (Z.of_int b)

let cst_int64 (b:int64) : t = cst (Z.of_int64 b)
                            
let zero : t = cst_int 0
(** 0ℤ+0 *)

let one  : t = cst_int 1
(** 0ℤ+1 *)
             
let mone : t = cst_int (-1)
(** 0ℤ-1 *)
             
let minf_inf : t = of_int 1 0
(** 1ℤ+0 *)

let of_range (lo:Z.t) (hi:Z.t) : t =
  if lo = hi then cst lo else minf_inf

let of_range_bot (lo:Z.t) (hi:Z.t) : t with_bot =  
  if lo = hi then Nb (cst lo)
  else if lo > hi then BOT
  else Nb minf_inf

let of_bound (lo:B.t) (hi:B.t) : t =
  match lo,hi with
  | B.Finite l, B.Finite h -> of_range l h
  | _ -> minf_inf

let of_bound_bot (lo:B.t) (hi:B.t) : t with_bot =
  match lo,hi with
  | B.Finite l, B.Finite h -> of_range_bot l h
  | _ -> Nb minf_inf

(** Congruence overapproximating an interval. *)
  

  
                 
(** {2 Predicates} *)


let equal (a:t) (b:t) : bool =
  a = b
(** Equality. = also works. *)

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

let compare ((a,b):t) ((a',b'):t) : int =
  if a = a' then Z.compare b b' else Z.compare a a'
(**
  A total ordering (lexical ordering) returning -1, 0, or 1.
  Can be used as compare for sets, maps, etc. 
 *)

let compare_bot (x:t with_bot) (y:t with_bot) : int =
  Bot.bot_compare compare x y
(** Total ordering on possibly empty congruences. *)

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

let is_minf_inf ((a,b):t) : bool =
  a = Z.one
(** The congruence represents [-∞,+∞]. *)
                                 
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


let to_string ((a,b):t) : string =
  if a = Z.zero then Z.to_string b else
    let prefix = (if a = Z.one then "" else Z.to_string a) in
    match Z.sign b with
    | 1 -> prefix^"ℤ+"^(Z.to_string b)
    | -1 -> prefix^"ℤ"^(Z.to_string b)
    | _ -> prefix^"ℤ"
  
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
  if a = Z.zero then of_z Z.zero (Z.add lo (Z.erem (Z.sub b lo) w)) (* singleton *)
  else of_z (gcd a w) (Z.add lo (Z.erem (Z.sub b lo) w)) (* non-singleton *)
(** Put back inside [lo,up] by modular arithmetics. *)

let to_bool (can_be_zero:bool) (can_be_one:bool) : t =
  match can_be_zero, can_be_one with
  | true, false -> zero
  | false, true -> one
  | true, true -> minf_inf
  | _ -> failwith "unreachable case encountered in IntCong.to_bool"
(* helper function for operators returning a boolean that can be zero and/or one *)
            
let log_cast (ab:t) : t =
  to_bool (contains_zero ab) (contains_nonzero ab)
(** Conversion from integer to boolean in [0,1]: maps 0 to 0 (false) and non-zero to 1 (true). 
    [0;1] is over-approximated as ℤ. 
*)
                  
let log_not (ab:t) : t =
  to_bool (contains_nonzero ab) (contains_zero ab)
(** Logical negation.
    Logical operation use the C semantics: they accept 0 and non-0 respectively as false and true, but they always return 0 and 1 respectively for false and true. 
    [0;1] is over-approximated as ℤ. 
*)

let log_and (ab:t) (ab':t) : t =
  to_bool (contains_zero ab || contains_zero ab') (contains_nonzero ab && contains_nonzero ab')
(** Logical and. *)
    
let log_or (ab:t) (ab':t) : t =
  to_bool (contains_zero ab && contains_zero ab') (contains_nonzero ab || contains_nonzero ab')
(** Logical or. *)

let log_xor (ab:t) (ab':t) : t =
  let f,f' = contains_zero ab, contains_zero ab'
  and t,t' = contains_nonzero ab, contains_nonzero ab' in
  to_bool ((f && f') || (t && t')) ((f && t') || (t && f'))
(** Logical exclusive or. *)


let log_eq (ab:t) (ab':t) : t =
  to_bool (not (equal ab ab' && is_singleton ab)) (intersect ab ab')

let log_neq (ab:t) (ab':t) : t =
  to_bool (intersect ab ab') (not (equal ab ab' && is_singleton ab)) 

let log_sgl op ((a,b):t) ((a',b'):t) : t =
  if a <> Z.zero || a' <> Z.zero then minf_inf
  else if op b b' then one else zero
(* utility function, only handles the case of singletons *)
  
let log_leq = log_sgl (<=)
let log_geq = log_sgl (>=)
let log_lt  = log_sgl (<)
let log_gt  = log_sgl (>)


(** C comparison tests. Returns an interval included in [0,1] (a boolean) *)

  
let is_log_eq (ab:t) (ab':t) : bool =
  intersect ab ab'

let is_log_neq (ab:t) (ab':t) : bool =
  not (equal ab ab' && is_singleton ab)

let is_log_sgl op ((a,b):t) ((a',b'):t) : bool =
  a <> Z.zero || a' <> Z.zero || op b b'
(* utility function, only handles the case of singletons *)
  
let is_log_leq = is_log_sgl (<=)
let is_log_geq = is_log_sgl (>=)
let is_log_lt  = is_log_sgl (<)
let is_log_gt  = is_log_sgl (>)


(** C comparison tests. Returns a boolean if the test may succeed *)

  
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

                   
let bit_not (ab:t) : t =
  pred (neg ab)
(** Bitwise negation: ~x = -x-1 *)

(* TODO: other bitwise operations? *)


  
(** {2 Filters} *)

  
(** Given two interval aruments, return the arguments assuming that the predicate holds. 
 *)

let filter_eq (ab:t) (ab':t) : (t*t) with_bot =
  match meet ab ab' with BOT -> BOT | Nb x -> Nb (x,x)

let filter_sgl op ((a,b) as ab:t) ((a',b') as ab':t) : (t*t) with_bot =
  if a = Z.zero && a' = Z.zero && not (op b b') then BOT else Nb (ab,ab')
(* utility function: we only handle the simple case of singletons *)

let filter_neq = filter_sgl (<>)
let filter_leq = filter_sgl (<=)
let filter_geq = filter_sgl (>=)
let filter_lt  = filter_sgl (<)
let filter_gt  = filter_sgl (>)

               

(** {2 Backward operations} *)

               
(** Given one or two interval argument(s) and a result interval, return the
    argument(s) assuming the result in the operation is in the given result.
 *)

let bwd_neg (a:t) (r:t) : t_with_bot =
  meet a (neg r)

let bwd_abs (a:t) (r:t) : t_with_bot =
  meet a (join r (neg r))

let bwd_succ (a:t) (r:t) : t_with_bot =
  meet a (pred r)

let bwd_pred (a:t) (r:t) : t_with_bot =
  meet a (succ r)
            
let bwd_add (a:t) (b:t) (r:t) : (t*t) with_bot =
  (* r = a + b ⇒ a = r - b ∧ b = r - a *)
  bot_merge2 (meet a (sub r b)) (meet b (sub r a))

let bwd_sub (a:t) (b:t) (r:t) : (t*t) with_bot =
  (* r = a - b ⇒ a = b + r ∧ b = a - r *)
  bot_merge2 (meet a (add b r)) (meet b (sub a r))

let bwd_mul (a:t) (b:t) (r:t) : (t*t) with_bot =
  (* r = a * b ⇒ ((a = r / b) ∨ (b = r = 0)) ∧ ((b = r / a) ∨ (a = r = 0)) *)
  let aa = if contains_zero b && contains_zero r then Nb a else div r b
  and bb = if contains_zero a && contains_zero r then Nb b else div r a in
  bot_merge2 aa bb

let bwd_bit_not (a:t) (r:t) : t_with_bot =
  meet a (bit_not r)            

let bwd_join (a:t) (b:t) (r:t) : (t*t) with_bot =
  bot_merge2 (meet a r) (meet b r)
(** Backward join: both arguments and intersected with the result. *)

let bwd_rem (a:t) ((b1,b2) as b:t) ((r1,r2):t) =
  if b1 = Z.zero && r1 = Z.zero then
    (* r = a % b => a = bℤ + r *)
    bot_merge2 (meet a (b2,r2)) (Nb b)
  else
    Nb (a,b)

let bwd_div (a:t) (b:t) (r:t) = Nb (a,b)
let bwd_wrap (a:t) range (r:t) : t_with_bot = Nb a
let bwd_shift_left (a:t) (b:t) (r:t) = Nb (a,b)
let bwd_shift_right (a:t) (b:t) (r:t) = Nb (a,b)
let bwd_shift_right_trunc (a:t) (b:t) (r:t) = Nb (a,b)
(* TODO: more precise backward for those, and also bit-wise operations *)
                                                              

(** {2 Reduction} *)

                                            
let meet_inter ((a,b):t) ((l,h):I.t) : (t * I.t) with_bot =
  (* smallest element in aℤ+b greater or equal to l *)
  let l' = match l with
    | B.Finite f -> B.Finite (Z.add f (rem_zero (Z.sub b f) a))
    | _ -> l
  (* greatest element in aℤ+b smaller or equal to h *)
  and h' = match h with
    | B.Finite f -> B.Finite (Z.sub f (rem_zero (Z.sub f b) a))
    | _ -> h
  in
  match l',h' with
  | B.Finite ll, B.Finite hh ->
     if ll > hh then BOT
     else if ll = hh then Nb (cst ll, (l',h'))
     else Nb ((a,b),(l',h'))
  | _ -> Nb ((a,b),(l',h'))
(** Intersects a congruence with an interval, and returns the set represented
    both as a congruence and as an interval.
    Useful to implement reductions.
 *)

                                            
