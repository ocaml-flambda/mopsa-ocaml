(**
  FloatItv - Floating-point interval arithmetics with rounding.  

  We rely on C code to provide functions with correct rounding
  (rounding direction and rounding precision).


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


open Bot
module F = Float
       

(** {2 Types} *)

             
type t =
  { mutable lo: float;  (** lower bound *)
    mutable up: float;  (** upper bound *)
  }
(**
  The type of non-empty intervals: a lower bound and an upper bound. 
  The bounds can be -∞ and +∞.
  In particular, we can have [-∞,-∞] and [+∞,+∞] (useful to model sets
  of floating-point numbers).
  We must have lo ≤ up.
  The bounds shall not be NaN.
 *)    

type t_with_bot = t with_bot
(** The type of possibly empty intervals. *)

                    
let is_valid (a:t) : bool =
  a.lo <= a.up && not (F.is_nan a.lo || F.is_nan a.up)

      

                    
(** {2 Constructors} *)

                    
let mk (lo:float) (up:float) : t = { lo; up; }


let zero : t = mk 0. 0.
let one : t = mk 1. 1.
let two : t = mk 2. 2.
let mone : t = mk (-1.) (-1.)
let zero_one : t  = mk 0. 1.
let mone_zero : t = mk (-1.) 0.
let mone_one : t  = mk (-1.) 1.
let mhalf_half : t = mk (-0.5) 0.5
let zero_inf : t  = mk 0. infinity
let minf_zero : t = mk neg_infinity 0.
let minf_inf : t  = mk neg_infinity infinity
(** Useful intervals *)

                  
let of_float (lo:float) (up:float) : t =
  let lo = if F.is_nan lo then neg_infinity else lo
  and up = if F.is_nan up then infinity else up in
  if lo > up then invalid_arg (Printf.sprintf "FloatItv.of_float: invalid bound [%g,%g]" lo up)
  else { lo; up; }
(** Constructs a non-empty interval. We must have lo ≤ up, or an exception is raised. 
    NaN bounds are transformed into infinities.
 *)
    
let of_float_bot (lo:float) (up:float) : t_with_bot =
  let lo = if F.is_nan lo then neg_infinity else lo
  and up = if F.is_nan up then infinity else up in
  if lo <= up then Nb { lo; up; }
  else BOT
(** Constructs a possibly empty interval (no rounding).
    NaN bounds are transformed into infinities.    
 *)

let hull (a:float) (b:float) : t =
  if F.is_nan a || F.is_nan b then minf_inf
  else mk (min a b) (max a b)
(** Constructs the smallest interval containing a and b. *)
                                                           
let cst (c:float) : t =
  if F.is_nan c then minf_inf else mk c c
(** Singleton interval. *)


                  
(** {2 Predicates} *)

                  
let equal (a:t) (b:t) : bool =
  a = b

let equal_bot : t_with_bot -> t_with_bot -> bool =
  bot_equal equal  

let included (a:t) (b:t) : bool =
  a.lo >= b.lo && a.up <= b.up
(** Set ordering. = also works to compare for equality. *)

let included_bot : t_with_bot -> t_with_bot -> bool =
  bot_included included  

let intersect (a:t) (b:t) : bool =
  a.lo <= b.up && b.lo <= a.up
(** Whether the intervals have an non-empty intersection. *)  

let intersect_bot : t_with_bot -> t_with_bot -> bool =
  bot_dfl2 false intersect
  

let contains (x:float) (a:t) =
  a.lo <= x && a.up >= x
(** Whether the interval contains a certain value. *)
                                              
let total_order (a:t) (b:t) : int =
  if a.lo = b.lo then
    compare a.up b.up
  else
    compare a.lo b.lo
(**
  A total ordering of intervals: lexical ordering. 
  Can be used as compare for sets, maps, etc. 
  (This hypothesis that bounds cannot be NaN is important to make the order total.
*)
                                             
let contains_zero (a:t) : bool =
  a.lo <= 0. && a.up >= 0.

let contains_nonzero (a:t) : bool =
  a.lo <> 0. || a.up <> 0.
                               
let is_zero (a:t) : bool = equal a zero
let is_positive (a:t) : bool = a.lo >= 0.
let is_negative (a:t) : bool = a.up <= 0.
let is_positive_strict (a:t) : bool = a.lo > 0.
let is_negative_strict (a:t) : bool = a.up < 0.
let is_nonzero (a:t) : bool = a.lo > 0. || a.up < 0.
(** Interval sign. *)

let is_singleton (a:t) : bool =
  a.lo = a.up
(** Whether the interval contains a single element. *)

let is_bounded (a:t) : bool =
  a.lo > neg_infinity && a.up < infinity
(** Whether the interval has finite bounds. *)

let is_in_range (a:t) (lo:float) (up:float) =
  included a { lo; up; }
(** Whether the interval is included in the range [lo,up]. *)

                            
                                                   
                                                  
(** {2 Printing} *)

type print_format = F.print_format
let dfl_fmt = F.dfl_fmt
  
let to_string (fmt:print_format) (x:t) : string =
  "["^(F.to_string fmt x.lo)^","^(F.to_string fmt x.up)^"]"
                       
let print fmt ch (x:t) = output_string ch (to_string fmt x)
let fprint fmt ch (x:t) = Format.pp_print_string ch (to_string fmt x)
let bprint fmt ch (x:t) = Buffer.add_string ch (to_string fmt x)

let to_string_bot fmt = bot_to_string (to_string fmt)
let print_bot fmt = bot_print (print fmt)
let fprint_bot fmt = bot_fprint (fprint fmt)
let bprint_bot fmt = bot_bprint (bprint fmt)


(** {2 Operations without rounding} *)

                                        
let neg (t:t) : t =
  of_float (-.t.up) (-.t.lo)
(** Negation. *)
              
let abs (t:t) : t =
  if t.lo <= 0. then
    if t.up <= 0. then neg t
    else of_float  0. (max (-.t.lo) t.up)
  else t
(** Absolute value. *)

let fmod (x:t) (y:t) : t_with_bot =
  (* x % y = x % |y| *)
  let y = abs y in
  if y = zero then BOT else (* case [a,b] % {0} ⟹ ⊥ *)
    if x.lo > -. y.lo && x.up < y.lo then
      (* case x ⊆ [-min y, min y] ⟹ identity *)
      Nb x
    else if y.lo = y.up && F.is_finite y.lo && F.is_finite x.lo && F.is_finite x.up &&
              F.Double.round_int_zero (F.Double.div_zero x.lo y.lo) =
              F.Double.round_int_zero (F.Double.div_zero x.up y.lo) then
      (* case x % {z} and x ⊆ [zk,z(k+1)] *)
      Nb (of_float (F.fmod x.lo y.lo) (F.fmod x.up y.lo))
    else if x.lo >= 0. then
      (* case x % y positive *)
      Nb (of_float 0. y.up)
    else if x.up <= 0. then
      (* case x % y negative *)
      Nb (of_float (-.y.up) 0.)
    else
      (* general case *)
      Nb (of_float (-.y.up) y.up)
(** Remainder (fmod). *)

  
let join (a:t) (b:t) : t =
  of_float (min a.lo b.lo) (max a.up b.up)
(** Join of non-empty intervals. *)
    
let join_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_neutral2 join a b
(** Join of possibly empty intervals. *)

let join_list (l:t list) : t_with_bot  =
  List.fold_left (fun a b -> join_bot a (Nb b)) BOT l
(** Join of a list of (non-empty) intervals. *)

let meet (a:t) (b:t) : t_with_bot =
  of_float_bot (max a.lo b.lo) (min a.up b.up)
(** Intersection of non-emtpty intervals (possibly empty) *)

let meet_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_absorb2 meet a b
(** Intersection of possibly empty intervals. *)
          
let meet_list (l:t list) : t_with_bot =
  List.fold_left (fun a b -> meet_bot a (Nb b)) (Nb minf_inf) l
(** Meet of a list of (non-empty) intervals. *)

let positive (a:t) : t_with_bot =
  meet a zero_inf
       
let negative (a:t) : t_with_bot =
  meet a minf_zero
(** Positive and negative part. *)
  
let meet_zero (a:t) : t_with_bot =
  meet a zero
(** Intersects with {0}. *)

let widen (a:t) (a':t) : t =
  of_float
    (if F.lt a'.lo a.lo then neg_infinity else a.lo)
    (if F.gt a'.up a.up then infinity else a.up)
(** Basic widening: put unstable bounds to infinity. *)

let widen_bot (a:t_with_bot) (b:t_with_bot) : t_with_bot =
  bot_neutral2 widen a b


  
let bwd_default_unary (a:t) (r:t) : t_with_bot =
  Nb a
(** Fallback for backward unary operators *)
  
let bwd_default_binary (a:t) (b:t) (r:t) : (t*t) with_bot =
  Nb (a,b)
(** Fallback for backward binary operators *)

let bwd_neg (a:t) (r:t) : t_with_bot =
  meet a (neg r)
(** Backward negation. *)

let bwd_abs (a:t) (r:t) : t_with_bot =
  join_bot (meet a r) (meet a (neg r))  
(** Backward absolute value. *)
  
let bwd_fmod (x:t) (y:t) (r:t) : (t*t) with_bot =
  let yy = abs y in
  if x.lo > -. yy.lo && x.up < yy.lo then
    (* case x ⊆ [-min |y|, min |y|] ⟹ fmod is the identity on x *)
    bot_merge2 (meet x r) (Nb y)
  else
    (* default: no refinement *)
    Nb (x,y)
(** Backward remainder (fmod). *)
  
                              
(** {2 Rounding-dependent functions} *)

                      
(**
  Interval operations support six rounding modes.
  The first four correspond to rounding both bounds in the same direction:
  to nearest, upwards, downards, or to zero.
  To this, we add outer rounding (lower bound downwards and upper bound 
  upwards) and inner rounding (lower bound upwards and upper bound
  downwards).

  Rounding can be performed for single-precision or double-precision.

  Outer interval arithmetic can model soundly real arithmetic. In this
  case, infinities model unbounded intervals.

  Directed roundings and outer arithmetics can model soundly float
  arithmetic. In this case, infinite bounds signal the precence of
  infinite float values.
  Directed roundings can produce [-∞,-∞] or [+∞,+∞] (denoting a set
  of floats reduced to an infinite float).

  Inner rounding can return empty intervals. Divisions and square roots
  can return empty intervals for any rounding mode.
                 
  We do not track NaN.
  NaN bounds, as in [-∞,-∞] + [+∞,+∞], are transformed into infinities.
 *)


(* binding to (internal) C functions *)

(* The C functions are designed to avoid allocation. We pass them the struct
   that will be set to the result.
   We do not export them directly, but we wrap in some OCaml code to give
   them a functional flavor.
 *)

external add_dbl_itv_near:  t -> t -> t -> unit = "ml_add_dbl_itv_near"
external add_dbl_itv_up:    t -> t -> t -> unit = "ml_add_dbl_itv_up"
external add_dbl_itv_down:  t -> t -> t -> unit = "ml_add_dbl_itv_down"
external add_dbl_itv_zero:  t -> t -> t -> unit = "ml_add_dbl_itv_zero"
external add_dbl_itv_outer: t -> t -> t -> unit = "ml_add_dbl_itv_outer"
external add_dbl_itv_inner: t -> t -> t -> unit = "ml_add_dbl_itv_inner"
external add_sgl_itv_near:  t -> t -> t -> unit = "ml_add_sgl_itv_near"
external add_sgl_itv_up:    t -> t -> t -> unit = "ml_add_sgl_itv_up"
external add_sgl_itv_down:  t -> t -> t -> unit = "ml_add_sgl_itv_down"
external add_sgl_itv_zero:  t -> t -> t -> unit = "ml_add_sgl_itv_zero"
external add_sgl_itv_outer: t -> t -> t -> unit = "ml_add_sgl_itv_outer"
external add_sgl_itv_inner: t -> t -> t -> unit = "ml_add_sgl_itv_inner"

external sub_dbl_itv_near:  t -> t -> t -> unit = "ml_sub_dbl_itv_near"
external sub_dbl_itv_up:    t -> t -> t -> unit = "ml_sub_dbl_itv_up"
external sub_dbl_itv_down:  t -> t -> t -> unit = "ml_sub_dbl_itv_down"
external sub_dbl_itv_zero:  t -> t -> t -> unit = "ml_sub_dbl_itv_zero"
external sub_dbl_itv_outer: t -> t -> t -> unit = "ml_sub_dbl_itv_outer"
external sub_dbl_itv_inner: t -> t -> t -> unit = "ml_sub_dbl_itv_inner"
external sub_sgl_itv_near:  t -> t -> t -> unit = "ml_sub_sgl_itv_near"
external sub_sgl_itv_up:    t -> t -> t -> unit = "ml_sub_sgl_itv_up"
external sub_sgl_itv_down:  t -> t -> t -> unit = "ml_sub_sgl_itv_down"
external sub_sgl_itv_zero:  t -> t -> t -> unit = "ml_sub_sgl_itv_zero"
external sub_sgl_itv_outer: t -> t -> t -> unit = "ml_sub_sgl_itv_outer"
external sub_sgl_itv_inner: t -> t -> t -> unit = "ml_sub_sgl_itv_inner"

external mul_dbl_itv_near:  t -> t -> t -> unit = "ml_mul_dbl_itv_near"
external mul_dbl_itv_up:    t -> t -> t -> unit = "ml_mul_dbl_itv_up"
external mul_dbl_itv_down:  t -> t -> t -> unit = "ml_mul_dbl_itv_down"
external mul_dbl_itv_zero:  t -> t -> t -> unit = "ml_mul_dbl_itv_zero"
external mul_dbl_itv_outer: t -> t -> t -> unit = "ml_mul_dbl_itv_outer"
external mul_dbl_itv_inner: t -> t -> t -> unit = "ml_mul_dbl_itv_inner"
external mul_sgl_itv_near:  t -> t -> t -> unit = "ml_mul_sgl_itv_near"
external mul_sgl_itv_up:    t -> t -> t -> unit = "ml_mul_sgl_itv_up"
external mul_sgl_itv_down:  t -> t -> t -> unit = "ml_mul_sgl_itv_down"
external mul_sgl_itv_zero:  t -> t -> t -> unit = "ml_mul_sgl_itv_zero"
external mul_sgl_itv_outer: t -> t -> t -> unit = "ml_mul_sgl_itv_outer"
external mul_sgl_itv_inner: t -> t -> t -> unit = "ml_mul_sgl_itv_inner"

external divpos_dbl_itv_near:  t -> t -> t -> unit = "ml_divpos_dbl_itv_near"
external divpos_dbl_itv_up:    t -> t -> t -> unit = "ml_divpos_dbl_itv_up"
external divpos_dbl_itv_down:  t -> t -> t -> unit = "ml_divpos_dbl_itv_down"
external divpos_dbl_itv_zero:  t -> t -> t -> unit = "ml_divpos_dbl_itv_zero"
external divpos_dbl_itv_outer: t -> t -> t -> unit = "ml_divpos_dbl_itv_outer"
external divpos_dbl_itv_inner: t -> t -> t -> unit = "ml_divpos_dbl_itv_inner"
external divpos_sgl_itv_near:  t -> t -> t -> unit = "ml_divpos_sgl_itv_near"
external divpos_sgl_itv_up:    t -> t -> t -> unit = "ml_divpos_sgl_itv_up"
external divpos_sgl_itv_down:  t -> t -> t -> unit = "ml_divpos_sgl_itv_down"
external divpos_sgl_itv_zero:  t -> t -> t -> unit = "ml_divpos_sgl_itv_zero"
external divpos_sgl_itv_outer: t -> t -> t -> unit = "ml_divpos_sgl_itv_outer"
external divpos_sgl_itv_inner: t -> t -> t -> unit = "ml_divpos_sgl_itv_inner"

                                                    
(* internal utilities *)

let mkop () = { lo=0.; up=0.; }
let wrap_op1 f = fun a -> let r = mkop () in f a r; r
let wrap_op2 f = fun a b -> let r = mkop () in f a b r; r
(* wrapper helpers from imperative C to functional OCaml *)

let wrap_op1_bot f a =
  let r = wrap_op1 f a in
  if r.lo <= r.up then Nb r else BOT
  
let wrap_op2_bot f a b =
  let r = wrap_op2 f a b in
  if r.lo <= r.up then Nb r else BOT
  
let wrap_sqrt f g a =
  if a.up < 0. then
    BOT
  else
    of_float_bot (f (max 0. a.lo)) (g a.up)
(* wrapper for sqrt *)

let wrap_div_unmerged d a b =
  let l1 = if b.up > 0. then [d a { b with lo = if F.sign b.lo > 0 then b.lo else 0.; }] else []
  and l2 = if b.lo < 0. then [d a { b with up = if F.sign b.up < 0 then b.up else -.0.; }] else []
  in
  l1@l2
(* wrapper for division; 
   we split into a positive divisor interval and a negative divisor interval;
   we have to be extra careful of the sign of zeros when splitting (to get the correct infinity);
   returns a list of intervals to keep precision;
 *)

let wrap_div_unmerged_bot d a b =
  list_remove_bot (wrap_div_unmerged d a b)
  
let wrap_div d a b =
  join_list (wrap_div_unmerged d a b)
(* wrapper for division; returns a single (possibly empty) interval *)

let wrap_div_bot d a b =
  join_list (wrap_div_unmerged_bot d a b)
  
                                                                         
module Double = struct

  module FF = F.Double

  (** {2 Arithmetic} *)
                
  let add_near  : t -> t -> t = wrap_op2 add_dbl_itv_near
  let add_up    : t -> t -> t = wrap_op2 add_dbl_itv_up
  let add_down  : t -> t -> t = wrap_op2 add_dbl_itv_down
  let add_zero  : t -> t -> t = wrap_op2 add_dbl_itv_zero
  let add_outer : t -> t -> t = wrap_op2 add_dbl_itv_outer
  let add_inner : t -> t -> t_with_bot = wrap_op2_bot add_dbl_itv_inner
  (** Addition. *)

  let sub_near  : t -> t -> t = wrap_op2 sub_dbl_itv_near
  let sub_up    : t -> t -> t = wrap_op2 sub_dbl_itv_up
  let sub_down  : t -> t -> t = wrap_op2 sub_dbl_itv_down
  let sub_zero  : t -> t -> t = wrap_op2 sub_dbl_itv_zero
  let sub_outer : t -> t -> t = wrap_op2 sub_dbl_itv_outer
  let sub_inner : t -> t -> t_with_bot = wrap_op2_bot sub_dbl_itv_inner
  (** Subtraction. *)

  let mul_near  : t -> t -> t = wrap_op2 mul_dbl_itv_near
  let mul_up    : t -> t -> t = wrap_op2 mul_dbl_itv_up
  let mul_down  : t -> t -> t = wrap_op2 mul_dbl_itv_down
  let mul_zero  : t -> t -> t = wrap_op2 mul_dbl_itv_zero
  let mul_outer : t -> t -> t = wrap_op2 mul_dbl_itv_outer
  let mul_inner : t -> t -> t_with_bot = wrap_op2_bot mul_dbl_itv_inner
  (** Multiplication. *)

  let divpos_near  : t -> t -> t = wrap_op2 divpos_dbl_itv_near
  let divpos_up    : t -> t -> t = wrap_op2 divpos_dbl_itv_up
  let divpos_down  : t -> t -> t = wrap_op2 divpos_dbl_itv_down
  let divpos_zero  : t -> t -> t = wrap_op2 divpos_dbl_itv_zero
  let divpos_outer : t -> t -> t = wrap_op2 divpos_dbl_itv_outer
  let divpos_inner : t -> t -> t_with_bot = wrap_op2_bot divpos_dbl_itv_inner
  (* Division by a divisor of constant sign. *)

  let div_unmerged_near  : t -> t -> t list = wrap_div_unmerged divpos_near
  let div_unmerged_up    : t -> t -> t list = wrap_div_unmerged divpos_up
  let div_unmerged_down  : t -> t -> t list = wrap_div_unmerged divpos_down
  let div_unmerged_zero  : t -> t -> t list = wrap_div_unmerged divpos_zero
  let div_unmerged_outer : t -> t -> t list = wrap_div_unmerged divpos_outer
  let div_unmerged_inner : t -> t -> t list = wrap_div_unmerged_bot divpos_inner
  (** Division. Returns a list of 0, 1, or 2 intervals to remain precise. *)

  let div_near  : t -> t -> t_with_bot = wrap_div divpos_near
  let div_up    : t -> t -> t_with_bot = wrap_div divpos_up
  let div_down  : t -> t -> t_with_bot = wrap_div divpos_down
  let div_zero  : t -> t -> t_with_bot = wrap_div divpos_zero
  let div_outer : t -> t -> t_with_bot = wrap_div divpos_outer
  let div_inner : t -> t -> t_with_bot = wrap_div_bot divpos_inner
  (** Division. Returns a single interval. *)

  let square_near  (a:t) : t = let aa = abs a in mul_near  aa aa
  let square_up    (a:t) : t = let aa = abs a in mul_up    aa aa
  let square_down  (a:t) : t = let aa = abs a in mul_down  aa aa
  let square_zero  (a:t) : t = let aa = abs a in mul_zero  aa aa
  let square_outer (a:t) : t = let aa = abs a in mul_outer aa aa
  let square_inner (a:t) : t_with_bot = let aa = abs a in mul_inner aa aa
  (** Square. *)
                                              
  let sqrt_near  : t -> t_with_bot = wrap_sqrt FF.sqrt_near FF.sqrt_near
  let sqrt_up    : t -> t_with_bot = wrap_sqrt FF.sqrt_up   FF.sqrt_up
  let sqrt_down  : t -> t_with_bot = wrap_sqrt FF.sqrt_down FF.sqrt_down
  let sqrt_zero  : t -> t_with_bot = wrap_sqrt FF.sqrt_zero FF.sqrt_zero
  let sqrt_outer : t -> t_with_bot = wrap_sqrt FF.sqrt_down FF.sqrt_up
  let sqrt_inner : t -> t_with_bot = wrap_sqrt FF.sqrt_up   FF.sqrt_down
  (** Square root. Returns the square root of the positive part, possibly ⊥. *)

  let round_int_near  (a:t) : t = of_float (FF.round_int_near a.lo) (FF.round_int_near a.up)
  let round_int_up    (a:t) : t = of_float (FF.round_int_up   a.lo) (FF.round_int_up   a.up)
  let round_int_down  (a:t) : t = of_float (FF.round_int_down a.lo) (FF.round_int_down a.up)
  let round_int_zero  (a:t) : t = of_float (FF.round_int_zero a.lo) (FF.round_int_zero a.up)
  let round_int_outer (a:t) : t = of_float (FF.round_int_down a.lo) (FF.round_int_up a.up)
  let round_int_inner (a:t) : t_with_bot = of_float_bot (FF.round_int_up a.lo)   (FF.round_int_down a.up)
  (** Round to integer. *)
 
  let unround_int_near (a:t) : t = add_outer a mhalf_half
  let unround_int_up   (a:t) : t = sub_outer a zero_one
  let unround_int_down (a:t) : t = add_outer a zero_one
  let unround_int_zero (a:t) : t =
    of_float (if a.lo <= 0. then FF.sub_down a.lo 1. else a.lo)
             (if a.up >= 0. then FF.add_up   a.up 1. else a.up)
  let unround_int_any (a:t) : t = sub_outer a mone_one
  (** Values that, after rounding to integer in the specified direction, may be in the argument interval. 
      Useful for backward operators.
   *)

  let unround_near (a:t) : t = of_float (FF.pred a.lo) (FF.succ a.up)
  let unround_up   (a:t) : t = of_float (FF.pred a.lo) (a.up)
  let unround_down (a:t) : t = of_float (a.lo) (FF.succ a.up)
  let unround_zero (a:t) : t = 
    of_float (if a.lo <= 0. then FF.pred a.lo else a.lo)
             (if a.up >= 0. then FF.succ a.up else a.up)
  let unround_any (a:t) : t = unround_near a
  (** Values that, after rounding to float, may be in the argument interval.
      Useful for backward operators.
   *)

  let of_int_near  (lo:int) (up:int) : t = of_float (FF.of_int_near lo) (FF.of_int_near up)
  let of_int_up    (lo:int) (up:int) : t = of_float (FF.of_int_up   lo) (FF.of_int_up   up)
  let of_int_down  (lo:int) (up:int) : t = of_float (FF.of_int_down lo) (FF.of_int_down up)
  let of_int_zero  (lo:int) (up:int) : t = of_float (FF.of_int_zero lo) (FF.of_int_zero up)
  let of_int_outer (lo:int) (up:int) : t = of_float (FF.of_int_down lo) (FF.of_int_up up)
  let of_int_inner (lo:int) (up:int) : t_with_bot = of_float_bot (FF.of_int_up lo)   (FF.of_int_down up)
  (** Conversion from int. *)
 
  let of_int64_near  (lo:int64) (up:int64) : t = of_float (FF.of_int64_near lo) (FF.of_int64_near up)
  let of_int64_up    (lo:int64) (up:int64) : t = of_float (FF.of_int64_up   lo) (FF.of_int64_up   up)
  let of_int64_down  (lo:int64) (up:int64) : t = of_float (FF.of_int64_down lo) (FF.of_int64_down up)
  let of_int64_zero  (lo:int64) (up:int64) : t = of_float (FF.of_int64_zero lo) (FF.of_int64_zero up)
  let of_int64_outer (lo:int64) (up:int64) : t = of_float (FF.of_int64_down lo) (FF.of_int64_up up)
  let of_int64_inner (lo:int64) (up:int64) : t_with_bot = of_float_bot (FF.of_int64_up lo)   (FF.of_int64_down up)
  (** Conversion from int64. *)

  let of_z_near  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_near lo) (FF.of_z_near up)
  let of_z_up    (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_up   lo) (FF.of_z_up   up)
  let of_z_down  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_down lo) (FF.of_z_down up)
  let of_z_zero  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_zero lo) (FF.of_z_zero up)
  let of_z_outer (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_down lo) (FF.of_z_up up)
  let of_z_inner (lo:Z.t) (up:Z.t) : t_with_bot = of_float_bot (FF.of_z_up lo)   (FF.of_z_down up)
  (** Conversion from Z.t. *)

                                                
                                
  (** {2 Filters} *)

                                                
  (** Given two interval aruments, return the arguments assuming that the predicate holds.
   *)
                                                
                                                
  let filter_leq (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot a.lo (min a.up b.up)) (of_float_bot (max a.lo b.lo) b.up)

  let filter_geq (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot (max a.lo b.lo) a.up) (of_float_bot b.lo (min a.up b.up))

  let filter_lt (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot a.lo (min a.up (FF.pred b.up))) (of_float_bot (max (FF.succ a.lo) b.lo) b.up)

  let filter_gt (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot (max a.lo (FF.succ b.lo)) a.up) (of_float_bot b.lo (min (FF.pred a.up) b.up))

  let filter_eq (a:t) (b:t) : (t*t) with_bot =
    match meet a b with BOT -> BOT | Nb x -> Nb (x,x)

  let filter_neq (a:t) (b:t) : (t*t) with_bot =
    match a.lo = a.up, b.lo = b.up with
    | true, true  when a.lo = b.lo -> BOT
    | true, false when a.lo = b.lo -> bot_merge2 (Nb a) (of_float_bot (FF.succ b.lo) b.up)
    | true, false when a.up = b.up -> bot_merge2 (Nb a) (of_float_bot b.lo (FF.pred b.up))
    | false, true when a.lo = b.lo -> bot_merge2 (of_float_bot (FF.succ a.lo) a.up) (Nb b)
    | false, true when a.up = b.up -> bot_merge2 (of_float_bot a.lo (FF.pred a.up)) (Nb b)
    | _ -> Nb (a,b)


         
  (** {2 Backward operations} *)

         
  (** Given one or two interval argument(s) and a result interval, return the
      argument(s) assuming the result in the operation is in the given result.
   *)


  let bwd_add (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a + b) ⇒ a = unround(r) - b ∧ b = unround(r) - a *)
    bot_merge2 (meet a (sub_outer r b)) (meet b (sub_outer r a))

  let bwd_add_near  a b r = bwd_add a b (unround_near r)
  let bwd_add_up    a b r = bwd_add a b (unround_up   r)
  let bwd_add_down  a b r = bwd_add a b (unround_down r)
  let bwd_add_zero  a b r = bwd_add a b (unround_zero r)
  let bwd_add_outer a b r = bwd_add a b (unround_any r)
  let bwd_add_noround a b r = bwd_add a b r
  (** Backward addition. *)

                         
  let bwd_sub (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a - b) ⇒ a = b + unround(r) ∧ b = a - unround(r) *)
    bot_merge2 (meet a (add_outer b r)) (meet b (sub_outer a r))

  let bwd_sub_near  a b r = bwd_sub a b (unround_near r)
  let bwd_sub_up    a b r = bwd_sub a b (unround_up   r)
  let bwd_sub_down  a b r = bwd_sub a b (unround_down r)
  let bwd_sub_zero  a b r = bwd_sub a b (unround_zero r)
  let bwd_sub_outer a b r = bwd_sub a b (unround_any r)
  let bwd_sub_noround a b r = bwd_sub a b r
  (** Backward subtraction. *)

                         
  let bwd_mul (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a * b) ⇒ ((a = unround(r) / b) ∨ (b = r = 0) ∨ (b unbounded)) ∧ 
                          ((b = unround(r) / a) ∨ (a = r = 0) ∨ (a unbounded)) *)
    let aa =
      if not (is_bounded b) || (contains_zero b && contains_zero r) then Nb a
      else meet_bot (Nb a) (div_outer r b)
    and bb =
      if not (is_bounded a) || (contains_zero a && contains_zero r) then Nb b
      else meet_bot (Nb b) (div_outer r a)
    in
    bot_merge2 aa bb

  let bwd_mul_near  a b r = bwd_mul a b (unround_near r)
  let bwd_mul_up    a b r = bwd_mul a b (unround_up   r)
  let bwd_mul_down  a b r = bwd_mul a b (unround_down r)
  let bwd_mul_zero  a b r = bwd_mul a b (unround_zero r)
  let bwd_mul_outer a b r = bwd_mul a b (unround_any r)
  let bwd_mul_noround a b r = bwd_mul a b r
  (** Backward multiplication. *)

                         
  let bwd_div (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a / b) ⇒ ((a = b * unround(r)) ∧ (b = a / unround(r)) ∨ (a = r = 0)) ∨ unbounded) *)
    if not (is_bounded a && is_bounded b && is_bounded r) then Nb (a,b)
    else
      let aa = meet a (mul_outer b r)
      and bb =
        if (contains_zero a && contains_zero r) then Nb b
        else meet_bot (Nb b) (div_outer a r)
      in
      bot_merge2 aa bb

  let bwd_div_near  a b r = bwd_div a b (unround_near r)
  let bwd_div_up    a b r = bwd_div a b (unround_up   r)
  let bwd_div_down  a b r = bwd_div a b (unround_down r)
  let bwd_div_zero  a b r = bwd_div a b (unround_zero r)
  let bwd_div_outer a b r = bwd_div a b (unround_any r)
  let bwd_div_noround a b r = bwd_div a b r
  (** Backward division. *)


  let bwd_round_int_near  a r = meet a (unround_int_near r)
  let bwd_round_int_up    a r = meet a (unround_int_up   r)
  let bwd_round_int_down  a r = meet a (unround_int_down r)
  let bwd_round_int_zero  a r = meet a (unround_int_zero r)
  let bwd_round_int_outer a r = meet a (unround_int_any r)
  let bwd_round_int_noround a r = meet a r
  (** Backward rounding to int. *)


  let bwd_square (a:t) (r:t) : t_with_bot =
    let rr = sqrt_outer r in
    join_bot (meet_bot (Nb a) rr) (meet_bot (Nb a) (bot_lift1 neg rr))
    
  let bwd_square_near  a r = bwd_square a (unround_near r)
  let bwd_square_up    a r = bwd_square a (unround_up   r)
  let bwd_square_down  a r = bwd_square a (unround_down r)
  let bwd_square_zero  a r = bwd_square a (unround_zero r)
  let bwd_square_outer a r = bwd_square a (unround_any r)
  let bwd_square_noround a r = bwd_square a r
  (** Backward square. *)

                             
  let bwd_sqrt (a:t) (r:t) : t_with_bot =
    meet a (square_outer r)
    
  let bwd_sqrt_near  a r = bwd_sqrt a (unround_near r)
  let bwd_sqrt_up    a r = bwd_sqrt a (unround_up   r)
  let bwd_sqrt_down  a r = bwd_sqrt a (unround_down r)
  let bwd_sqrt_zero  a r = bwd_sqrt a (unround_zero r)
  let bwd_sqrt_outer a r = bwd_sqrt a (unround_any r)
  let bwd_sqrt_noround a r = bwd_sqrt a r
  (** Backward square root. *)

                           
  let meet_nonzero (a:t) : t_with_bot =
    let lo = if a.lo = 0. then FF.min_denormal else a.lo
    and up = if a.up = 0. then -. FF.min_denormal else a.up
    in
    of_float_bot lo up
  (** Keeps only non-zero elements. *)
              
end
(** Intervals with rounding to double. *)
                  
                  
module Single = struct

  module FF = F.Single

  (** {2 Arithmetic} *)
                
  let add_near  : t -> t -> t = wrap_op2 add_sgl_itv_near
  let add_up    : t -> t -> t = wrap_op2 add_sgl_itv_up
  let add_down  : t -> t -> t = wrap_op2 add_sgl_itv_down
  let add_zero  : t -> t -> t = wrap_op2 add_sgl_itv_zero
  let add_outer : t -> t -> t = wrap_op2 add_sgl_itv_outer
  let add_inner : t -> t -> t_with_bot = wrap_op2_bot add_sgl_itv_inner
  (** Addition. *)

  let sub_near  : t -> t -> t = wrap_op2 sub_sgl_itv_near
  let sub_up    : t -> t -> t = wrap_op2 sub_sgl_itv_up
  let sub_down  : t -> t -> t = wrap_op2 sub_sgl_itv_down
  let sub_zero  : t -> t -> t = wrap_op2 sub_sgl_itv_zero
  let sub_outer : t -> t -> t = wrap_op2 sub_sgl_itv_outer
  let sub_inner : t -> t -> t_with_bot = wrap_op2_bot sub_sgl_itv_inner
  (** Subtraction. *)

  let mul_near  : t -> t -> t = wrap_op2 mul_sgl_itv_near
  let mul_up    : t -> t -> t = wrap_op2 mul_sgl_itv_up
  let mul_down  : t -> t -> t = wrap_op2 mul_sgl_itv_down
  let mul_zero  : t -> t -> t = wrap_op2 mul_sgl_itv_zero
  let mul_outer : t -> t -> t = wrap_op2 mul_sgl_itv_outer
  let mul_inner : t -> t -> t_with_bot = wrap_op2_bot mul_sgl_itv_inner
  (** Multiplication. *)

  let divpos_near  : t -> t -> t = wrap_op2 divpos_sgl_itv_near
  let divpos_up    : t -> t -> t = wrap_op2 divpos_sgl_itv_up
  let divpos_down  : t -> t -> t = wrap_op2 divpos_sgl_itv_down
  let divpos_zero  : t -> t -> t = wrap_op2 divpos_sgl_itv_zero
  let divpos_outer : t -> t -> t = wrap_op2 divpos_sgl_itv_outer
  let divpos_inner : t -> t -> t_with_bot = wrap_op2_bot divpos_sgl_itv_inner
  (* Division by a divisor of constant sign. *)

  let div_unmerged_near  : t -> t -> t list = wrap_div_unmerged divpos_near
  let div_unmerged_up    : t -> t -> t list = wrap_div_unmerged divpos_up
  let div_unmerged_down  : t -> t -> t list = wrap_div_unmerged divpos_down
  let div_unmerged_zero  : t -> t -> t list = wrap_div_unmerged divpos_zero
  let div_unmerged_outer : t -> t -> t list = wrap_div_unmerged divpos_outer
  let div_unmerged_inner : t -> t -> t list = wrap_div_unmerged_bot divpos_inner
  (** Division. Returns a list of 0, 1, or 2 intervals to remain precise. *)

  let div_near  : t -> t -> t_with_bot = wrap_div divpos_near
  let div_up    : t -> t -> t_with_bot = wrap_div divpos_up
  let div_down  : t -> t -> t_with_bot = wrap_div divpos_down
  let div_zero  : t -> t -> t_with_bot = wrap_div divpos_zero
  let div_outer : t -> t -> t_with_bot = wrap_div divpos_outer
  let div_inner : t -> t -> t_with_bot = wrap_div_bot divpos_inner
  (** Division. Returns a single interval. *)

  let square_near  (a:t) : t = let aa = abs a in mul_near  aa aa
  let square_up    (a:t) : t = let aa = abs a in mul_up    aa aa
  let square_down  (a:t) : t = let aa = abs a in mul_down  aa aa
  let square_zero  (a:t) : t = let aa = abs a in mul_zero  aa aa
  let square_outer (a:t) : t = let aa = abs a in mul_outer aa aa
  let square_inner (a:t) : t_with_bot = let aa = abs a in mul_inner aa aa
  (** Square. *)
                                              
  let sqrt_near  : t -> t_with_bot = wrap_sqrt FF.sqrt_near FF.sqrt_near
  let sqrt_up    : t -> t_with_bot = wrap_sqrt FF.sqrt_up   FF.sqrt_up
  let sqrt_down  : t -> t_with_bot = wrap_sqrt FF.sqrt_down FF.sqrt_down
  let sqrt_zero  : t -> t_with_bot = wrap_sqrt FF.sqrt_zero FF.sqrt_zero
  let sqrt_outer : t -> t_with_bot = wrap_sqrt FF.sqrt_down FF.sqrt_up
  let sqrt_inner : t -> t_with_bot = wrap_sqrt FF.sqrt_up   FF.sqrt_down
  (** Square root. Returns the square root of the positive part, possibly ⊥. *)

  let round_int_near  (a:t) : t = of_float (FF.round_int_near a.lo) (FF.round_int_near a.up)
  let round_int_up    (a:t) : t = of_float (FF.round_int_up   a.lo) (FF.round_int_up   a.up)
  let round_int_down  (a:t) : t = of_float (FF.round_int_down a.lo) (FF.round_int_down a.up)
  let round_int_zero  (a:t) : t = of_float (FF.round_int_zero a.lo) (FF.round_int_zero a.up)
  let round_int_outer (a:t) : t = of_float (FF.round_int_down a.lo) (FF.round_int_up a.up)
  let round_int_inner (a:t) : t_with_bot = of_float_bot (FF.round_int_up a.lo)   (FF.round_int_down a.up)
  (** Round to integer. *)
 
  let unround_int_near (a:t) : t = add_outer a mhalf_half
  let unround_int_up   (a:t) : t = sub_outer a zero_one
  let unround_int_down (a:t) : t = add_outer a zero_one
  let unround_int_zero (a:t) : t =
    of_float (if a.lo <= 0. then FF.sub_down a.lo 1. else a.lo)
             (if a.up >= 0. then FF.add_up   a.up 1. else a.up)
  let unround_int_any (a:t) : t = add_outer a mone_one
  (** Values that, after rounding to integer in the specified direction, may be in the argument interval. 
      Useful for backward operators.
   *)

  let unround_near (a:t) : t = of_float (FF.pred a.lo) (FF.succ a.up)
  let unround_up   (a:t) : t = of_float (FF.pred a.lo) (a.up)
  let unround_down (a:t) : t = of_float (a.lo) (FF.succ a.up)
  let unround_zero (a:t) : t = 
    of_float (if a.lo <= 0. then FF.pred a.lo else a.lo)
             (if a.up >= 0. then FF.succ a.up else a.up)
  let unround_any (a:t) : t = unround_near a
  (** Values that, after rounding to float, may be in the argument interval.
      Useful for backward operators.
   *)

  let of_int_near  (lo:int) (up:int) : t = of_float (FF.of_int_near lo) (FF.of_int_near up)
  let of_int_up    (lo:int) (up:int) : t = of_float (FF.of_int_up   lo) (FF.of_int_up   up)
  let of_int_down  (lo:int) (up:int) : t = of_float (FF.of_int_down lo) (FF.of_int_down up)
  let of_int_zero  (lo:int) (up:int) : t = of_float (FF.of_int_zero lo) (FF.of_int_zero up)
  let of_int_outer (lo:int) (up:int) : t = of_float (FF.of_int_down lo) (FF.of_int_up up)
  let of_int_inner (lo:int) (up:int) : t_with_bot = of_float_bot (FF.of_int_up lo)   (FF.of_int_down up)
  (** Conversion from int. *)
 
  let of_int64_near  (lo:int64) (up:int64) : t = of_float (FF.of_int64_near lo) (FF.of_int64_near up)
  let of_int64_up    (lo:int64) (up:int64) : t = of_float (FF.of_int64_up   lo) (FF.of_int64_up   up)
  let of_int64_down  (lo:int64) (up:int64) : t = of_float (FF.of_int64_down lo) (FF.of_int64_down up)
  let of_int64_zero  (lo:int64) (up:int64) : t = of_float (FF.of_int64_zero lo) (FF.of_int64_zero up)
  let of_int64_outer (lo:int64) (up:int64) : t = of_float (FF.of_int64_down lo) (FF.of_int64_up up)
  let of_int64_inner (lo:int64) (up:int64) : t_with_bot = of_float_bot (FF.of_int64_up lo)   (FF.of_int64_down up)
  (** Conversion from int64. *)

  let of_double_near  (lo:float) (up:float) : t = of_float (FF.of_double_near lo) (FF.of_double_near up)
  let of_double_up    (lo:float) (up:float) : t = of_float (FF.of_double_up   lo) (FF.of_double_up   up)
  let of_double_down  (lo:float) (up:float) : t = of_float (FF.of_double_down lo) (FF.of_double_down up)
  let of_double_zero  (lo:float) (up:float) : t = of_float (FF.of_double_zero lo) (FF.of_double_zero up)
  let of_double_outer (lo:float) (up:float) : t = of_float (FF.of_double_down lo) (FF.of_double_up up)
  let of_double_inner (lo:float) (up:float) : t_with_bot = of_float_bot (FF.of_double_up lo)   (FF.of_double_down up)
  (** Conversion from double. *)
 
  let of_z_near  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_near lo) (FF.of_z_near up)
  let of_z_up    (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_up   lo) (FF.of_z_up   up)
  let of_z_down  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_down lo) (FF.of_z_down up)
  let of_z_zero  (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_zero lo) (FF.of_z_zero up)
  let of_z_outer (lo:Z.t) (up:Z.t) : t = of_float (FF.of_z_down lo) (FF.of_z_up up)
  let of_z_inner (lo:Z.t) (up:Z.t) : t_with_bot = of_float_bot (FF.of_z_up lo)   (FF.of_z_down up)
  (** Conversion from Z.t. *)
 

                                                 
  (** {2 Filters} *)


  (** Given two interval aruments, return the arguments assuming that the predicate holds.
   *)

                                                
  let filter_leq (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot a.lo (min a.up b.up)) (of_float_bot (max a.lo b.lo) b.up)

  let filter_geq (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot (max a.lo b.lo) a.up) (of_float_bot b.lo (min a.up b.up))

  let filter_lt (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot a.lo (min a.up (FF.pred b.up))) (of_float_bot (max (FF.succ a.lo) b.lo) b.up)

  let filter_gt (a:t) (b:t) : (t*t) with_bot =
    bot_merge2 (of_float_bot (max a.lo (FF.succ b.lo)) a.up) (of_float_bot b.lo (min (FF.pred a.up) b.up))

  let filter_eq (a:t) (b:t) : (t*t) with_bot =
    match meet a b with BOT -> BOT | Nb x -> Nb (x,x)

  let filter_neq (a:t) (b:t) : (t*t) with_bot =
    match a.lo = a.up, b.lo = b.up with
    | true, true  when a.lo = b.lo -> BOT
    | true, false when a.lo = b.lo -> bot_merge2 (Nb a) (of_float_bot (FF.succ b.lo) b.up)
    | true, false when a.up = b.up -> bot_merge2 (Nb a) (of_float_bot b.lo (FF.pred b.up))
    | false, true when a.lo = b.lo -> bot_merge2 (of_float_bot (FF.succ a.lo) a.up) (Nb b)
    | false, true when a.up = b.up -> bot_merge2 (of_float_bot a.lo (FF.pred a.up)) (Nb b)
    | _ -> Nb (a,b)

              
         
  (** {2 Backward operations} *)

         
  (** Given one or two interval argument(s) and a result interval, return the
      argument(s) assuming the result in the operation is in the given result.
   *)


  let bwd_add (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a + b) ⇒ a = unround(r) - b ∧ b = unround(r) - a *)
    bot_merge2 (meet a (sub_outer r b)) (meet b (sub_outer r a))

  let bwd_add_near  a b r = bwd_add a b (unround_near r)
  let bwd_add_up    a b r = bwd_add a b (unround_up   r)
  let bwd_add_down  a b r = bwd_add a b (unround_down r)
  let bwd_add_zero  a b r = bwd_add a b (unround_zero r)
  let bwd_add_outer a b r = bwd_add a b (unround_any r)
  let bwd_add_noround a b r = bwd_add a b r
  (** Backward addition. *)

                         
  let bwd_sub (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a - b) ⇒ a = b + unround(r) ∧ b = a - unround(r) *)
    bot_merge2 (meet a (add_outer b r)) (meet b (sub_outer a r))

  let bwd_sub_near  a b r = bwd_sub a b (unround_near r)
  let bwd_sub_up    a b r = bwd_sub a b (unround_up   r)
  let bwd_sub_down  a b r = bwd_sub a b (unround_down r)
  let bwd_sub_zero  a b r = bwd_sub a b (unround_zero r)
  let bwd_sub_outer a b r = bwd_sub a b (unround_any r)
  let bwd_sub_noround a b r = bwd_sub a b r
  (** Backward subtraction. *)

                         
  let bwd_mul (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a * b) ⇒ ((a = unround(r) / b) ∨ (b = r = 0) ∨ (b unbounded)) ∧ 
                          ((b = unround(r) / a) ∨ (a = r = 0) ∨ (a unbounded)) *)
    let aa =
      if not (is_bounded b) || (contains_zero b && contains_zero r) then Nb a
      else meet_bot (Nb a) (div_outer r b)
    and bb =
      if not (is_bounded a) || (contains_zero a && contains_zero r) then Nb b
      else meet_bot (Nb b) (div_outer r a)
    in
    bot_merge2 aa bb

  let bwd_mul_near  a b r = bwd_mul a b (unround_near r)
  let bwd_mul_up    a b r = bwd_mul a b (unround_up   r)
  let bwd_mul_down  a b r = bwd_mul a b (unround_down r)
  let bwd_mul_zero  a b r = bwd_mul a b (unround_zero r)
  let bwd_mul_outer a b r = bwd_mul a b (unround_any r)
  let bwd_mul_noround a b r = bwd_mul a b r
  (** Backward multiplication. *)

  let bwd_div (a:t) (b:t) (r:t) : (t*t) with_bot =
    (* r = round(a / b) ⇒ ((a = b * unround(r)) ∧ (b = a / unround(r)) ∨ (a = r = 0)) ∨ unbounded) *)
    if not (is_bounded a && is_bounded b && is_bounded r) then Nb (a,b)
    else
      let aa = meet a (mul_outer b r)
      and bb =
        if (contains_zero a && contains_zero r) then Nb b
        else meet_bot (Nb b) (div_outer a r)
      in
      bot_merge2 aa bb

  let bwd_div_near  a b r = bwd_div a b (unround_near r)
  let bwd_div_up    a b r = bwd_div a b (unround_up   r)
  let bwd_div_down  a b r = bwd_div a b (unround_down r)
  let bwd_div_zero  a b r = bwd_div a b (unround_zero r)
  let bwd_div_outer a b r = bwd_div a b (unround_any r)
  let bwd_div_noround a b r = bwd_div a b r
  (** Backward division. *)
         
  let bwd_round_int_near  a r = meet a (unround_int_near r)
  let bwd_round_int_up    a r = meet a (unround_int_up   r)
  let bwd_round_int_down  a r = meet a (unround_int_down r)
  let bwd_round_int_zero  a r = meet a (unround_int_zero r)
  let bwd_round_int_outer a r = meet a (unround_int_any r)
  let bwd_round_int_noround a r = meet a r
  (** Backward rounding to int. *)

                              
  let bwd_square (a:t) (r:t) : t_with_bot =
    let rr = sqrt_outer r in
    join_bot (meet_bot (Nb a) rr) (meet_bot (Nb a) (bot_lift1 neg rr))
    
  let bwd_square_near  a r = bwd_square a (unround_near r)
  let bwd_square_up    a r = bwd_square a (unround_up   r)
  let bwd_square_down  a r = bwd_square a (unround_down r)
  let bwd_square_zero  a r = bwd_square a (unround_zero r)
  let bwd_square_outer a r = bwd_square a (unround_any r)
  let bwd_square_noround a r = bwd_square a r
  (** Backward square. *)

                             
  let bwd_sqrt (a:t) (r:t) : t_with_bot =
    meet a (square_outer r)
    
  let bwd_sqrt_near  a r = bwd_sqrt a (unround_near r)
  let bwd_sqrt_up    a r = bwd_sqrt a (unround_up   r)
  let bwd_sqrt_down  a r = bwd_sqrt a (unround_down r)
  let bwd_sqrt_zero  a r = bwd_sqrt a (unround_zero r)
  let bwd_sqrt_outer a r = bwd_sqrt a (unround_any r)
  let bwd_sqrt_noround a r = bwd_sqrt a r
  (** Backward square root. *)

                           
  let meet_nonzero (a:t) : t_with_bot =
    let lo = if a.lo = 0. then FF.min_denormal else a.lo
    and up = if a.up = 0. then -. FF.min_denormal else a.up
    in
    of_float_bot lo up
  (** Keeps only non-zero elements. *)

end
(** Intervals with rounding to float. *)


let to_bool (can_be_zero:bool) (can_be_one:bool) : t =
  match can_be_zero, can_be_one with
  | true, false -> zero
  | false, true -> one
  | true, true -> zero_one
  | _ -> failwith "unreachable case encountered in IntItv.to_bool"
(* helper function for operators returning a boolean that can be zero and/or one *)

                
let log_not (a: t) : t =
  to_bool (contains_nonzero a) (contains_zero a)

let add (a1:t) (a2:t) : t = Double.add_near a1 a2

let sub (a1:t) (a2:t) : t = Double.sub_near a1 a2
    
let mul (a1:t) (a2:t) : t = Double.mul_near a1 a2
    
let div (a1:t) (a2:t) : t_with_bot = Double.div_near a1 a2

let round (a:t) : t = Double.round_int_near a

let sqrt (a:t) : t_with_bot = Double.sqrt_near a

let pow (a:t) (b:t) : t = assert false

let log_or a1 a2 = assert false

let log_and a1 a2 = assert false

let is_log_eq (ab:t) (ab':t) : bool = intersect ab ab'
let is_log_leq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : bool = F.leq a b'
let is_log_geq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : bool = F.geq b a'
let is_log_lt ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : bool = F.lt a b'
let is_log_gt ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : bool = F.gt b a'
let is_log_neq (ab:t) (ab':t) : bool = not (equal ab ab' && is_singleton ab)
(** C comparison tests. Returns a boolean if the test may succeed *)


let meet_nonzero {lo=a;up=b} =
  match F.is_zero a, F.is_zero b with
  | true,  true  -> BOT
  | true,  false -> Nb {lo=F.Double.succ a;up=b}
  | false, true  -> Nb {lo=a; up=F.Double.pred b}
  | false, false -> Nb {lo=a;up=b}
(** Keeps only non-zero elements. *)


let meet_zero (a:t) : t_with_bot =
  meet a zero
(** Intersects with {0}. *)


let bwd_add a1 a2 r = Double.bwd_add a1 a2 r

let bwd_sub a1 a2 r = Double.bwd_sub a1 a2 r

let bwd_mul a1 a2 r = Double.bwd_mul a1 a2 r

let bwd_div a1 a2 r = Double.bwd_div a1 a2 r

let bwd_rem a1 a2 r = assert false


let of_bound_bot (a:F.t) (b:F.t) : t_with_bot =
  if F.gt a b || a = infinity || b = neg_infinity then BOT
  else Nb {lo=a; up= b}


(** Given two interval aruments, return the arguments assuming that the predicate holds.
 *)
           
           
let filter_leq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  bot_merge2 (of_bound_bot a (F.min b b')) (of_bound_bot (F.max a a') b')
         
let filter_geq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  bot_merge2 (of_bound_bot (F.max a a') b) (of_bound_bot a' (F.min b b'))
         
let filter_lt ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  bot_merge2 (of_bound_bot a (F.min b (F.Double.pred b'))) (of_bound_bot (F.max (F.Double.succ a) a') b')
         
let filter_gt ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  bot_merge2 (of_bound_bot (F.max a (F.Double.succ a')) b) (of_bound_bot a' (F.min (F.Double.pred b) b'))
         
let filter_eq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  match meet {lo=a;up=b} {lo=a';up=b'} with BOT -> BOT | Nb x -> Nb (x,x)
                                              
let filter_neq ({lo=a;up=b}:t) ({lo=a';up=b'}:t) : (t*t) with_bot =
  match F.equal a b, F.equal  a' b' with
  | true, true  when F.equal a a' -> BOT
  | true, false when F.equal a a' -> bot_merge2 (Nb {lo=a;up=b}) (of_bound_bot (F.Double.succ a') b')
  | true, false when F.equal b b' -> bot_merge2 (Nb {lo=a;up=b}) (of_bound_bot a' (F.Double.pred b'))
  | false, true when F.equal a a' -> bot_merge2 (of_bound_bot (F.Double.succ a) b) (Nb {lo=a';up=b'})
  | false, true when F.equal b b' -> bot_merge2 (of_bound_bot a (F.Double.pred b)) (Nb {lo=a';up=b'})
  | _ -> Nb ({lo=a;up=b},{lo=a';up=b'})
