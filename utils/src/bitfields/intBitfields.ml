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
  Bitfields - Sequences of bits that can be set, cleared, or unknown

  We rely on Zarith for arithmetic operations.
 *)

open Bot


(** {2 Types} *)

             
type t = Z.t (** set *) * Z.t (** cleared *)
(**
   Bitfields: bit sequences with bit values in a 3-valued logic: set, 
   cleared, or unknown.
   Alternatively, this can be seen representing a set of arbtirary      
   precision integers through a Cartesian (non-relational) abstraction: 
   each bit is abstracted independently.

   The first component has a 1 for each bit that can be set.
   The second component has a 1 for each bit that can be cleared.
   A bit can be both in the set and the clear state, indicating a bit that
   may be set of cleared (set & cleared gives the bits at top).
   At least one of set or cleared must be true for each bit
   (set | cleared = -1), i.e., an element of [t] is not empty.
   set and cleared can be negative, indicating bit sequences starting with
   infinitely many 1s in 2's complement representation.
 *)    

       
type t_with_bot = t with_bot
(** The type of possibly empty bitfields. *)

  
module I = ItvUtils.IntItv
module B = ItvUtils.IntBound
module C = CongUtils.IntCong


let is_valid ((set,clr):t) : bool =
  Z.logor set clr = Z.minus_one
(** Every bit in a bitfield must be set, cleared, or both. *)  

  
  
(** {2 Constructors} *)


let of_z (set:Z.t) (clr:Z.t) : t =
  if not (is_valid (set,clr)) then
    invalid_arg (Printf.sprintf "Bitfields.of_z (set:%s,clear:%s)" (Z.to_string set) (Z.to_string clr));
  (set,clr)

let of_z_bot (set:Z.t) (clr:Z.t) : t_with_bot =
  if Z.logor set clr <> Z.minus_one then BOT
  else Nb (set,clr)

let cst (c:Z.t) : t =
  (* all bits at 1 in c must be set, and all bits at 0 must be cleared *)
  c, Z.lognot c
(** Constant. *)

let cst_int (c:int) : t =
  cst (Z.of_int c)

let cst_int64 (c:int64) : t =
  cst (Z.of_int64 c)
  
let zero : t =
  cst Z.zero
(** 0 *)

let one : t =
  cst Z.one
(** 1 *)

let mone : t =
  cst Z.minus_one
(** -1 *)

let zero_one : t =
  (* bit 0 can be in set or cleared, other bits must be cleared *)
  Z.one, Z.minus_one
(** [0,1] *)

let minf_inf : t =
  (* all bits can be set and cleared *)
  Z.minus_one, Z.minus_one
(** All integers. Indistiguishable from [0,+∞]. *)
                                    
let unsigned (sz:int) : t =
  (* bits 0 to sz can be set or cleared, other bits must be cleared *)
  Z.pred (Z.shift_left Z.one sz), Z.minus_one
(** Bitfields of unsigned integers with the specified bitsize. *)
                            
let unsigned8 : t = unsigned 8
let unsigned16 : t = unsigned 16
let unsigned32 : t = unsigned 32
let unsigned64 : t = unsigned 64


let of_range_bot (lo:Z.t) (hi:Z.t) : t with_bot =
  if lo > hi then
    (* empty set *)
    BOT
  else if lo < Z.zero && hi >= Z.zero then
    (* contains -1 and 0 -> every bit can be 0 and can be 1 *)
    Nb minf_inf
  else
    (* mask is all 1 after the highest bit != in lo and hi *)
    let to_mask = Z.numbits (Z.logxor lo hi) in
    let mask = Z.pred (Z.shift_left Z.one to_mask) in
    (* the bits in the mask can be set or cleared, 
       other bits are fixed to lo (or equivalently to hi) *)
    Nb (Z.logor lo mask, Z.logor (Z.lognot lo) mask)
(** Bitfield enclosing the range [lo,hi]. *)
    
let of_bound_bot (lo:B.t) (hi:B.t) : t with_bot =
  match lo,hi with
  | B.Finite l, B.Finite h -> of_range_bot l h
  | _ -> Nb minf_inf
(** Bitfield enclosing the range [lo,hi]. *)

let of_range (lo:Z.t) (hi:Z.t) : t =
  match of_range_bot lo hi with
  | Nb x -> x
  | BOT -> invalid_arg (Printf.sprintf "IntBitfields.of_range [%s,%s]" (Z.to_string lo) (Z.to_string hi))
(** Bitfield enclosing the range [lo,hi]. 
    Fails with [invalid_arg] if the range is empty.
 *)

let of_bound (lo:B.t) (hi:B.t) : t =
  match of_bound_bot lo hi with
  | Nb x -> x
  | BOT -> invalid_arg (Printf.sprintf "IntBitfields.of_bound [%s,%s]" (B.to_string lo) (B.to_string hi))
(** Bitfield enclosing the range [lo,hi]. 
    Fails with [invalid_arg] if the range is empty.
 *)

    

(** {2 Predicates} *)

                  
let equal ((set1,clr1):t) ((set2,clr2):t) : bool =
  set1 = set2 && clr1 = clr2
(** Equality. = also works. *)

let equal_bot : t_with_bot -> t_with_bot -> bool =
  bot_equal equal
  
let included ((set1,clr1):t) ((set2,clr2):t) : bool =
  (* set1 (resp. clr1) has less bits at 1 than set2 (resp. clr2) *)
  Z.logor set1 set2 = set2 && Z.logor clr1 clr2 = clr2
(** Set ordering. *)

let included_bot : t_with_bot -> t_with_bot -> bool =
  bot_included included  

let intersect ((set1,clr1):t) ((set2,clr2):t) : bool =
  (* for each bit, it must be set in both arguments, or cleared in both *)
  Z.logor (Z.logand set1 set2) (Z.logand clr1 clr2) = Z.minus_one
(** Whether the intersection is non-empty. *)
     
let intersect_bot : t_with_bot -> t_with_bot -> bool =
  bot_dfl2 false intersect

let contains (x:Z.t) ((set,clr):t) : bool =
  (* each bit at 1 in x must be in set, and each bit at 0 must be in clr *)
  (x = Z.logand x set) && (x = Z.logor x (Z.lognot clr))
  
let compare ((set1,clr1):t) ((set2,clr2):t) : int =
  if set1 = set2 then Z.compare clr1 clr2 else Z.compare set1 set2
(**
  A total ordering on bitfields, returning -1, 0, or 1.
  Can be used as compare for sets, maps, etc. 
 *)

let compare_bot (x:t with_bot) (y:t with_bot) : int =
  Bot.bot_compare compare x y
(** Total ordering on possibly empty bitfields. *)

let contains_zero ((set,clr):t) : bool =
  (* all bits must be in clr *)
  clr = Z.minus_one
(** Whether the bifield contains zero. *)

let contains_one (x:t) : bool =
  contains Z.one x
(** Whether the bifield contains one. *)

let contains_nonzero ((set,clr):t) : bool =
  (* at least one bit must be in set *)
  set <> Z.zero
(** Whether the bifield contains a non-zero value. *)

let is_zero (x:t) : bool =
  x = zero

let is_positive ((set,clr):t) : bool =
  (* there are only finitely many bits that cn be 1 *)
  set >= Z.zero

let is_positive_strict (a:t) : bool =
  is_positive a && not (contains_zero a)

let is_negative_strict ((set,clr):t) : bool =
  (* there are only finitely many bits that can be 0 *)
  clr >= Z.zero

let is_negative (a:t) : bool =
  (* any bitfield allowing 0 and a negative number also allows a strictly 
     positive number; the only way to be negative or nul is to either 
     contain only strictly negative numbers, or to contain only 0
   *)
  is_negative_strict a || is_zero a

let is_nonzero ((set,clr):t) : bool =
  (* there is a position with a bit that cannot be 0 *)
  clr <> Z.minus_one
(** Contains only non-zero elements. *)

let is_minf_inf ((a,b):t) : bool =
  a = Z.minus_one && b = Z.minus_one
(** The bitfield represents [-∞,+∞]. *)
                                 
let is_singleton ((set,clr):t) : bool =
  (* every bit can be either set of cleared, but not both *)
  set = Z.lognot clr
(** Whether the bitfield contains a single element. *)

let is_bounded ((set,clr):t) : bool =
  (* the set of bits that can be both set and cleared is finite *)
  Z.logand set clr >= Z.zero
(** Whether the bitfield contains a finite number of elements. *)

let lower_bound ((set,clr):t) : B.t =
  if set < Z.zero && clr < Z.zero then
    B.MINF (* can be arbitrarily negative *)
  else
    B.Finite (Z.lognot clr) (* clear all the bits that can be cleared *)
(** Get the lower bound (possibly MINF). *)

let upper_bound ((set,clr):t) : B.t =
  if set < Z.zero && clr < Z.zero then
    B.PINF (* can be arbitrarily positive *)
  else
    B.Finite set (* set all the bits that can be set *)
(** Get the upper bound (possibly PINF). *)

  

(** {2 Printing} *)


let to_string ((set,clr):t) : string =
  let b = Buffer.create 10 in
  let both = Z.logand set clr in
  (match set < Z.zero, clr < Z.zero with
   | true, false -> Buffer.add_string b "…1"
   | true, true  -> Buffer.add_string b "…⊤"
   | _ -> ()
  );
  for i = Z.numbits both-1 downto 0 do
    match Z.testbit set i, Z.testbit clr i with
    | true, false -> Buffer.add_string b "1"
    | false, true -> Buffer.add_string b "0"
    | true,true   -> Buffer.add_string b "⊤"
    | _ -> invalid_arg (Printf.sprintf "IntBitfields.to_string (set:%s,clear:%s)" (Z.to_string set) (Z.to_string clr));
  done;
  Buffer.contents b

let print ch (x:t) = output_string ch (to_string x)
let fprint ch (x:t) = Format.pp_print_string ch (to_string x)
let bprint ch (x:t) = Buffer.add_string ch (to_string x)

let to_string_bot = bot_to_string to_string
let print_bot = bot_print print
let fprint_bot = bot_fprint fprint
let bprint_bot = bot_bprint bprint

  
  
(** {2 Enumeration} *)

               
let size ((set,clr):t) : int =
  let both = Z.logand set clr in
  if both < Z.zero then 
    invalid_arg (Printf.sprintf "Bitfields.size: unbounded set %s" (to_string (set,clr)));
  Z.popcount both
(** Number of elements. Raises an invalid argument if it is unbounded. *)

  
let to_list ((set,clr):t) : Z.t list =
  let both = Z.logand set clr in
  if both < Z.zero then 
    invalid_arg (Printf.sprintf "Bitfields.size: unbounded set %s" (to_string (set,clr)));
  let rec doit acc v i =
    if i < 0 then v::acc else
      (* bit i can be 0 *)
      let acc =
        if Z.testbit clr i then doit acc v (i-1)
        else acc
      in
      (* bit i can be 1 *)
      if Z.testbit set i then doit acc (Z.logor v (Z.shift_left Z.one i)) (i-1)
      else acc
  in
  let org = Z.lognot clr in
  doit [] org (Z.numbits both - 1)
(** List of elements, in increasing order. 
    Raises an invalid argument if it is unbounded. 
 *)

       
       
(** {2 Set operations} *)

               
let join ((set,clr):t) ((set',clr'):t) : t =
  Z.logor set set', Z.logor clr clr'
(** Abstract union. *)

let join_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_neutral2 join a b

let join_list (l:t list) : t_with_bot =
  List.fold_left (fun a b -> join_bot a (Nb b)) BOT l

let meet ((set,clr):t) ((set',clr'):t) : t_with_bot =
  of_z_bot (Z.logand set set') (Z.logand clr clr')
(** Abstract intersection. *)
     
let meet_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_absorb2 meet a b
          
let meet_list (l:t list) : t_with_bot =
  List.fold_left (fun a b -> meet_bot a (Nb b)) (Nb minf_inf) l

  
  
(** {2 Forward operations} *)


let to_bool (can_be_zero:bool) (can_be_one:bool) : t =
  match can_be_zero, can_be_one with
  | true, false -> zero
  | false, true -> one
  | true, true -> zero_one
  | _ -> failwith "unreachable case encountered in IntBitfields.to_bool"
(* helper function for operators returning a boolean that can be zero and/or one *)
            
let log_cast (a:t) : t =
  to_bool (contains_zero a) (contains_nonzero a)
(** Conversion from integer to boolean in [0,1]: maps 0 to 0 (false) and non-zero to 1 (true). *)
                  
let log_not (a:t) : t =
  to_bool (contains_nonzero a) (contains_zero a)
(** Logical negation.
    Logical operation use the C semantics: they accept 0 and non-0 respectively as false and true, but they always return 0 and 1 respectively for false and true.
*)

let log_and (a:t) (b:t) : t =
  to_bool (contains_zero a || contains_zero b) (contains_nonzero a && contains_nonzero b)
(** Logical and. *)
    
let log_or (a:t) (b:t) : t =
  to_bool (contains_zero a && contains_zero b) (contains_nonzero a || contains_nonzero b)
(** Logical or. *)

let log_xor (a:t) (b:t) : t =
  let f,f' = contains_zero a, contains_zero b
  and t,t' = contains_nonzero a, contains_nonzero b in
  to_bool ((f && f') || (t && t')) ((f && t') || (t && f'))
(** Logical exclusive or. *)

let log_eq (a:t) (b:t) : t =
  to_bool (not (equal a b && is_singleton a)) (intersect a b)

let log_neq (a:t) (b:t) : t =
  to_bool (intersect a b) (not (equal a b && is_singleton a))

let log_leq (a:t) (b:t) : t =
  to_bool (B.gt  (upper_bound a) (lower_bound b))
          (B.leq (lower_bound a) (upper_bound b))

let log_geq (a:t) (b:t) : t =
  to_bool (B.lt  (lower_bound a) (upper_bound b))
          (B.geq (upper_bound a) (lower_bound b))

let log_lt (a:t) (b:t) : t =
  to_bool (B.geq (upper_bound a) (lower_bound b))
          (B.lt  (lower_bound a) (upper_bound b))

let log_gt (a:t) (b:t) : t =
  to_bool (B.leq (lower_bound a) (upper_bound b))
          (B.gt  (upper_bound a) (lower_bound b))

(** C comparison tests. Returns an interval included in [0,1] (a boolean) *)

  
let is_log_eq (a:t) (b:t) : bool =
  intersect a b

let is_log_neq (a:t) (b:t) : bool =
  not (equal a b && is_singleton a)

let is_log_leq (a:t) (b:t) : bool =
  B.leq (lower_bound a) (upper_bound b)

let is_log_geq (a:t) (b:t) : bool =
  B.geq (upper_bound a) (lower_bound b)

let is_log_lt (a:t) (b:t) : bool =
  B.lt (lower_bound a) (upper_bound b)

let is_log_gt (a:t) (b:t) : bool =
  B.gt (upper_bound a) (lower_bound b)

(** C comparison tests. Returns a boolean if the test may succeed *)


  
let shift_left ((set,clr):t) ((set',clr'):t) : t =
  if is_singleton (set',clr') && set' >= Z.zero then
    (* exact when the right argument is a positive singleton, top else *)
    try
      let l = Z.to_int set' in
      Z.shift_left set l, Z.shift_left clr l
    with Z.Overflow -> minf_inf
  else
    minf_inf
(** Bitshift left: multiplication by a power of 2. *)

let shift_right ((set,clr):t) ((set',clr'):t) : t =
  if is_singleton (set',clr') && set' >= Z.zero then
    (* exact when the right argument is a positive singleton, top else *)
    try
      let l = Z.to_int set' in
      Z.shift_right set l, Z.shift_right clr l
    with Z.Overflow -> minf_inf
  else
    minf_inf
(** Bitshift right: division by a power of 2 rounding towards -∞. *)

let shift_right_trunc ((set,clr):t) ((set',clr'):t) : t =
  if is_singleton (set',clr') && set' >= Z.zero then
    (* exact when the right argument is a positive singleton, top else *)
    try
      let l = Z.to_int set' in
      Z.shift_right_trunc set l, Z.shift_right_trunc clr l
    with Z.Overflow -> minf_inf
  else
    minf_inf
(** Unsigned bitshift right: division by a power of 2 with truncation. *)

let bit_not ((set,clr):t) : t =
  clr, set
(** Bitwise negation. *)

let bit_or ((set,clr):t) ((set',clr'):t) : t =
  Z.logor set set', Z.logand clr clr'
(** Bitwise or. *)

let bit_and ((set,clr):t) ((set',clr'):t) : t =
  Z.logand set set', Z.logor clr clr'
(** Bitwise and. *)

let bit_xor ((set,clr):t) ((set',clr'):t) : t =
  Z.logor (Z.logand set clr') (Z.logand set' clr),
  Z.logor (Z.logand clr clr') (Z.logand set set')
(** Bitwise exclusive or. *)


                    
(** {2 Filters} *)


(** Given two interval aruments, return the arguments assuming that the predicate holds.
 *)
           
let filter_eq (a:t) (b:t) : (t*t) with_bot =
  match meet a b with BOT -> BOT | Nb x -> Nb (x,x)

let filter_sgl op ((set,clr) as a:t) ((set',clr') as a':t) : (t*t) with_bot =
  if is_singleton (set,clr) && is_singleton (set',clr') && not (op set set')
  then BOT else Nb (a,a')
(* utility function: we only handle the simple case of singletons *)

let filter_neq = filter_sgl (<>)
let filter_leq = filter_sgl (<=)
let filter_geq = filter_sgl (>=)
let filter_lt  = filter_sgl (<)
let filter_gt  = filter_sgl (>)
  

(** {2 Reduction} *)

  
let of_interval ((lo,hi):I.t) : t =
  of_bound lo hi

let to_interval ((set,clr):t) : I.t =
  lower_bound (set,clr), upper_bound (set,clr) 

let meet_inter (b:t) (i:I.t) : (t * I.t) with_bot =
  bot_merge2 (meet b (of_interval i)) (I.meet i (to_interval b))
(** Intersects a bitfield with an interval, and returns the set represented
    both as a bitfield and as an interval.
    Useful to implement reductions.
 *)

                                            
                              
