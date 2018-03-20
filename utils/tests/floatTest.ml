(**
  FloatTest - Unit tests for the Float library.

  We test the Float module against MPFR.
  The gmp OCaml library must be available.

  Important notes:

  - A recent version of mlgmpidl is required ; older versions incorrectly 
  handle rounding modes other than to nearest in MPFR.

  - The test issues errors. This is expected because MPFR does not
  actually simulate IEEE single and double precision : MPFR floats
  have a wider exponent range and does not support denormals.
  We try to compensate by converting the MPFR result into a single or
  double precision hardware number, to clamp the exponent to the
  correct range. However, for rounding to nearest, this can cause
  double rounding issues.
  To sum up, when the result has a very low or very high exponent
  (especially multiplications and divisions), the hardware and the 
  MPFR results may differ when rounding to nearest.


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


open Intervals.Float

let nb_samples = 250


   
(* Float / MPFR operator correspondance *)
(* ************************************ *)
               
   
type round = NEAR | ZERO | UP | DOWN
type prec = SINGLE | DOUBLE
type op1 = SQRT | ROUND_INT
type op2 = ADD | SUB | MUL | MULZ | DIV | DIVZ | MOD

let round_name = function NEAR -> "near" | ZERO -> "zero" | UP -> "up" | DOWN -> "down"
let prec_name = function SINGLE -> "single" | DOUBLE -> "double"
let op1_name = function SQRT -> "sqrt" | ROUND_INT -> "round_int"
let op2_name = function ADD -> "add" | SUB -> "sub" | MUL -> "mul" | MULZ -> "mulz" | DIV -> "div" | DIVZ -> "divz" | MOD -> "mod"


let float_op1 prec op round : float -> float =
  match prec,round,op with
  | DOUBLE, NEAR, SQRT -> Double.sqrt_near
  | DOUBLE, ZERO, SQRT -> Double.sqrt_zero
  | DOUBLE, UP,   SQRT -> Double.sqrt_up
  | DOUBLE, DOWN, SQRT -> Double.sqrt_down
  | SINGLE, NEAR, SQRT -> Single.sqrt_near
  | SINGLE, ZERO, SQRT -> Single.sqrt_zero
  | SINGLE, UP,   SQRT -> Single.sqrt_up
  | SINGLE, DOWN, SQRT -> Single.sqrt_down

  | DOUBLE, NEAR, ROUND_INT -> Double.round_int_near
  | DOUBLE, ZERO, ROUND_INT -> Double.round_int_zero
  | DOUBLE, UP,   ROUND_INT -> Double.round_int_up
  | DOUBLE, DOWN, ROUND_INT -> Double.round_int_down
  | SINGLE, NEAR, ROUND_INT -> Single.round_int_near
  | SINGLE, ZERO, ROUND_INT -> Single.round_int_zero
  | SINGLE, UP,   ROUND_INT -> Single.round_int_up
  | SINGLE, DOWN, ROUND_INT -> Single.round_int_down


let float_op2 prec op round : float -> float -> float =
  match prec,round,op with
  | DOUBLE, NEAR, ADD -> Double.add_near
  | DOUBLE, ZERO, ADD -> Double.add_zero
  | DOUBLE, UP,   ADD -> Double.add_up
  | DOUBLE, DOWN, ADD -> Double.add_down
  | SINGLE, NEAR, ADD -> Single.add_near
  | SINGLE, ZERO, ADD -> Single.add_zero
  | SINGLE, UP,   ADD -> Single.add_up
  | SINGLE, DOWN, ADD -> Single.add_down

  | DOUBLE, NEAR, SUB -> Double.sub_near
  | DOUBLE, ZERO, SUB -> Double.sub_zero
  | DOUBLE, UP,   SUB -> Double.sub_up
  | DOUBLE, DOWN, SUB -> Double.sub_down
  | SINGLE, NEAR, SUB -> Single.sub_near
  | SINGLE, ZERO, SUB -> Single.sub_zero
  | SINGLE, UP,   SUB -> Single.sub_up
  | SINGLE, DOWN, SUB -> Single.sub_down

  | DOUBLE, NEAR, MUL -> Double.mul_near
  | DOUBLE, ZERO, MUL -> Double.mul_zero
  | DOUBLE, UP,   MUL -> Double.mul_up
  | DOUBLE, DOWN, MUL -> Double.mul_down
  | SINGLE, NEAR, MUL -> Single.mul_near
  | SINGLE, ZERO, MUL -> Single.mul_zero
  | SINGLE, UP,   MUL -> Single.mul_up
  | SINGLE, DOWN, MUL -> Single.mul_down

  | DOUBLE, NEAR, MULZ -> Double.mulz_near
  | DOUBLE, ZERO, MULZ -> Double.mulz_zero
  | DOUBLE, UP,   MULZ -> Double.mulz_up
  | DOUBLE, DOWN, MULZ -> Double.mulz_down
  | SINGLE, NEAR, MULZ -> Single.mulz_near
  | SINGLE, ZERO, MULZ -> Single.mulz_zero
  | SINGLE, UP,   MULZ -> Single.mulz_up
  | SINGLE, DOWN, MULZ -> Single.mulz_down

  | DOUBLE, NEAR, DIV -> Double.div_near
  | DOUBLE, ZERO, DIV -> Double.div_zero
  | DOUBLE, UP,   DIV -> Double.div_up
  | DOUBLE, DOWN, DIV -> Double.div_down
  | SINGLE, NEAR, DIV -> Single.div_near
  | SINGLE, ZERO, DIV -> Single.div_zero
  | SINGLE, UP,   DIV -> Single.div_up
  | SINGLE, DOWN, DIV -> Single.div_down

  | DOUBLE, NEAR, DIVZ -> Double.divz_near
  | DOUBLE, ZERO, DIVZ -> Double.divz_zero
  | DOUBLE, UP,   DIVZ -> Double.divz_up
  | DOUBLE, DOWN, DIVZ -> Double.divz_down
  | SINGLE, NEAR, DIVZ -> Single.divz_near
  | SINGLE, ZERO, DIVZ -> Single.divz_zero
  | SINGLE, UP,   DIVZ -> Single.divz_up
  | SINGLE, DOWN, DIVZ -> Single.divz_down

  | DOUBLE, NEAR, MOD -> Double.mod_near
  | DOUBLE, ZERO, MOD -> Double.mod_zero
  | DOUBLE, UP,   MOD -> Double.mod_up
  | DOUBLE, DOWN, MOD -> Double.mod_down
  | SINGLE, NEAR, MOD -> Single.mod_near
  | SINGLE, ZERO, MOD -> Single.mod_zero
  | SINGLE, UP,   MOD -> Single.mod_up
  | SINGLE, DOWN, MOD -> Single.mod_down


let mpfr_round prec round v =
  match prec,round with
  | DOUBLE,_ -> v
  | SINGLE,NEAR -> Single.of_double_near v
  | SINGLE,UP   -> Single.of_double_up   v
  | SINGLE,DOWN -> Single.of_double_down v
  | SINGLE,ZERO -> Single.of_double_zero v
(* MPFR does not simulate IEEE float and double as it has a large exponent range;
   we try to fix this here;
   unfortunately, this may cause double-rounding errors in near rounding mode...
 *)

                 
let mk_mpfr prec round (v:t) : Mpfr.t =
  let r = Mpfr.init2 (match prec with SINGLE -> 24 | DOUBLE -> 53) in
  ignore (Mpfr.set_d r v round);
  r
                       
let mpfr_op1 prec op round (a:float) : float =
  let d = match round with ZERO -> Mpfr.Zero | NEAR -> Mpfr.Near | UP -> Mpfr.Up | DOWN -> Mpfr.Down in
  let a = mk_mpfr prec d a in
  let r = mk_mpfr prec d 0. in
  (match op with
   | SQRT -> ignore (Mpfr.sqrt r a d)
   | ROUND_INT  -> ignore (Mpfr.rint r a d)
  );
  mpfr_round prec round (Mpfr.get_d r d)
  
let mpfr_op2 prec op round (a:float) (b:float) : float =
  let d = match round with ZERO -> Mpfr.Zero | NEAR -> Mpfr.Near | UP -> Mpfr.Up | DOWN -> Mpfr.Down in
  let a = mk_mpfr prec d a in
  let b = mk_mpfr prec d b in
  let r = mk_mpfr prec d 0. in
  (match op with
   | ADD  -> ignore (Mpfr.add r a b d)
   | SUB  -> ignore (Mpfr.sub r a b d)
   | MUL  -> ignore (Mpfr.mul r a b d)
   | MULZ -> ignore (if Mpfr.sgn a = 0 || Mpfr.sgn b = 0 then Mpfr.set_si r 0 d else Mpfr.mul r a b d)
   | DIV  -> ignore (Mpfr.div r a b d)
   | DIVZ -> ignore (if Mpfr.sgn a = 0 || Mpfr.inf_p b then Mpfr.set_si r 0 d else Mpfr.div r a b d)
   | MOD  -> ignore (Mpfr.fmod r a b d)
  );
  mpfr_round prec round (Mpfr.get_d r d)
  

  
(* random non-NaN floats *)
(* ********************* *)
  

let rec mk_single () =
  let i1 = Int32.shift_left (Int32.of_int (Random.bits ())) 30 in
  let i2 = Int32.logxor i1 (Int32.of_int (Random.bits ())) in
  let f = Single.of_bits i2 in
  if is_nan f then mk_single () else f
  
let rec mk_double () =
  let i1 = Int64.shift_left (Int64.of_int (Random.bits ())) 60 in
  let i2 = Int64.logxor i1 (Int64.shift_left (Int64.of_int (Random.bits ())) 30) in
  let i3 = Int64.logxor i2 (Int64.of_int (Random.bits ())) in
  let f = Double.of_bits i3 in
  if is_nan f then mk_double () else f
  
let mk i prec =
  match i,prec with
  (* special values *)
  | 0,_ -> 0.
  | 1,_ -> (-.0.)
  | 2,_ -> infinity
  | 3,_ -> neg_infinity
  | 4,SINGLE -> Single.min_denormal
  | 5,SINGLE -> -.Single.min_denormal
  | 6,SINGLE -> Single.min_normal
  | 7,SINGLE -> -.Single.min_normal
  | 8,SINGLE -> Single.max_normal
  | 9,SINGLE -> -.Single.max_normal
  | 10,SINGLE -> Single.max_exact
  | 11,SINGLE -> -.Single.max_exact
  | 4,DOUBLE -> Double.min_denormal
  | 5,DOUBLE -> -.Double.min_denormal
  | 6,DOUBLE -> Double.min_normal
  | 7,DOUBLE -> -.Double.min_normal
  | 8,DOUBLE -> Double.max_normal
  | 9,DOUBLE -> -.Double.max_normal
  | 10,DOUBLE -> Double.max_exact
  | 11,DOUBLE -> -.Double.max_exact
  (* random values *)
  | _,SINGLE -> mk_single ()
  | _,DOUBLE -> mk_double ()

              
              
(* tests *)
(* ***** *)

              
let to_string f  =
  let p = Double.rep_of_bits (Double.to_bits f) in
  Printf.sprintf "%c%se%i" (if p.sign then '-' else '+') (Z.to_string p.fraction) p.exponent

  
let test_op1 () =
  List.iter
    (fun prec ->
      List.iter
        (fun round ->
          List.iter
            (fun op ->
              Printf.printf "testing %s %s %s\n%!" (prec_name prec) (round_name round) (op1_name op);
              for i = 1 to nb_samples do
                let f1 = mk i prec in
                let r1 = float_op1 prec op round f1 in
                let r2 = mpfr_op1 prec op round f1 in
                if not (equal_nan r1 r2) then Printf.printf "ERROR %g = %s -> %g = %s (Float) / %g = %s (MPFR)\n%!" f1 (to_string f1) r1 (to_string r1) r2 (to_string r2)
              done
            )          
            [SQRT;ROUND_INT]
        )
        [NEAR;ZERO;UP;DOWN]
    )
    [SINGLE;DOUBLE]
  
let test_op2 () =
  List.iter
    (fun prec ->
      List.iter
        (fun round ->
          List.iter
            (fun op ->
              Printf.printf "testing %s %s %s\n%!" (prec_name prec) (round_name round) (op2_name op);
              for i = 1 to nb_samples do
                for j = 1 to nb_samples do
                  let f1 = mk i prec in
                  let f2 = mk j prec in
                  let r1 = float_op2 prec op round f1 f2 in
                  let r2 = mpfr_op2 prec op round f1 f2 in
                  if not (equal_nan r1 r2) then Printf.printf "ERROR %g = %s, %g = %s -> %g = %s (Float) / %g = %s (MPFR)\n%!" f1 (to_string f1) f2 (to_string f2) r1 (to_string r1) r2 (to_string r2)
                done
              done
            )          
            [ADD;SUB;MUL;MULZ;DIV;DIVZ;MOD]
        )
        [NEAR;ZERO;UP;DOWN]
    )
    [SINGLE;DOUBLE]
  
  

(* entry point *)
(* *********** *)
  
  
let test () =
  Printf.printf "NOTES:\n- a recent OCaml gmp wrapper library (mlgmpidl) is required as an important bug concerning rounding modes has been fixed\n- currently, errors are expected for multiplication and division when the result has a very low or very high exponent, as MPFR does not obey the IEEE float and double exponent ranges\n\n";
  test_op1 ();
  test_op2 ();
  ()

let _ = test ()
