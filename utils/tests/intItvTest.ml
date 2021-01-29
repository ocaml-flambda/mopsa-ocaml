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

(**
  IntItvTest - Unit tests for integer intervals.
 *)

open Mopsa_utils
open Bot
open ItvUtils.IntItv


let max_exhaustive = 50
(* intervals smaller than this size are tested exhaustively; larger intervals are sampled *)


   
let one_two = of_int 1 2
let zero_two = of_int 0 2
let zero_ten = of_int 0 19
let mtwo_two = of_int (-2) 2
let mten_ten = of_int (-10)10
let one_inf = B.Finite Z.one, B.PINF
let mone_inf = B.Finite Z.minus_one, B.PINF
let two_inf = B.Finite (Z.of_int 2), B.PINF
let mtwo_inf = B.Finite (Z.of_int (-2)), B.PINF
let ten_inf = B.Finite (Z.of_int 10), B.PINF
let mten_inf = B.Finite (Z.of_int (-10)), B.PINF
let minf_one = B.MINF, B.Finite Z.one
let minf_mone = B.MINF, B.Finite Z.minus_one
let minf_two = B.MINF, B.Finite (Z.of_int 2)
let minf_mtwo = B.MINF, B.Finite (Z.of_int (-2))
let minf_ten = B.MINF, B.Finite (Z.of_int 10)
let minf_mten = B.MINF, B.Finite (Z.of_int (-10))
let ht = of_int 100 1000
let mht = of_int (-1000) (-100)
  
let interesting_intervals =
  [zero;one;mone;
   zero_one;mone_zero;mone_one;
   one_two;zero_two;zero_ten;mtwo_two;mten_ten;
   zero_inf;minf_zero;minf_inf;
   one_inf;mone_inf;two_inf;mtwo_inf;ten_inf;mten_inf;
   minf_one;minf_mone;minf_two;minf_mtwo;minf_ten;minf_mten;
   unsigned8;unsigned16;unsigned32;
   signed8;signed16;signed32;
   ht;mht;
  ]
(* some intersting intervals to test *)

  

(* concrete / abstract comparators *)
(* ******************************* *)

              
let unop (aop:t-> t) (cop:Z.t->Z.t) opname a1 c1 =
  let a2 = aop a1 in
  try
    let c2 = cop c1 in
    if not (contains c2 a2) then
      Printf.printf
        "error: %s %a = %a; %s %s = %s\n"
        opname print a1 print a2
        opname (Z.to_string c1) (Z.to_string c2);
  with _ -> () (* no concrete value *)
(* test contructor for unary operators outputing an interval *)
    
let binop (aop:t->t->t) (cop:Z.t->Z.t->Z.t) opname a1 a2 c1 c2 =
  let a3 = aop a1 a2 in
  try
    let c3 = cop c1 c2 in
    if not (contains c3 a3) then
      Printf.printf
        "error: %a %s %a = %a; %s %s %s = %s\n"
        print a1 opname print a2 print a3
        (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
  with _ -> () (* no concrete value *)
(* test contructor for binary operators outputing an interval *)
    
let binop_bot (aop:t->t->t with_bot) (cop:Z.t->Z.t->Z.t) opname a1 a2 c1 c2 =
  match aop a1 a2 with
  | Nb a3 ->
     (try
        let c3 = cop c1 c2 in
        if not (contains c3 a3) then
          Printf.printf
            "error: %a %s %a = %a; %s %s %s = %s\n"
            print a1 opname print a2 print a3
            (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
      with _ -> () (* no concrete value *)
     )
  | BOT ->
     (try
        let c3 = cop c1 c2 in
        (* error: we have a concrete value but no abstract one *)
          Printf.printf
            "error: %a %s %a = ⊥; %s %s %s = %s\n"
            print a1 opname print a2
            (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
      with _ -> () (* no concrete value *)
     )
(* test contructor for binary operators outputing an interval or bottom *)

let binop_unmerged (aop:t->t->t list) (cop:Z.t->Z.t->Z.t) opname a1 a2 c1 c2 =
  let l = aop a1 a2 in
  try
     let c3 = cop c1 c2 in
     if not (List.exists (contains c3) l) then
       Printf.printf
         "error: %a %s %a = %a; %s %s %s = %s\n"
         print a1 opname print a2 (ListExt.print ListExt.printer_list print) l
         (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
   with _ -> () (* no concrete value *)
(* test contructor for binary operators outputing an interval list *)

let filter (aop:t->t->(t*t) with_bot) (cop:Z.t->Z.t->bool) opname a1 a2 c1 c2 =
  match aop a1 a2 with
  | BOT ->
     (try
        if cop c1 c2 then
          Printf.printf
            "error: %a %s %a = ⊥; %s %s %s = true\n"
            print a1 opname print a2
            (Z.to_string c1) opname (Z.to_string c2)
      with _ -> ()
     )
  | Nb (aa1,aa2) ->
     (try
        if cop c1 c2 && (not (contains c1 aa1) || not (contains c2 aa2)) then
          Printf.printf
            "error: %a %s %a = %a,%a; %s %s %s = true\n"
            print a1 opname print a2 print aa1 print aa2
            (Z.to_string c1) opname (Z.to_string c2)
      with _ -> ()
     )
(* test a filter *)


let bwd_unop (aop:t->t->t with_bot) (cop:Z.t->Z.t) opname a1 r c1 =
  match aop a1 r with
  | BOT ->
     (try
        let c2 = cop c1 in
        if contains c2 r then
          Printf.printf
            "error: %s %a = %a -> ⊥; %s %s = %s\n"
            opname print a1 print r
            opname (Z.to_string c1) (Z.to_string c2)
      with _ -> ()
     )
  | Nb aa1 ->
     (try
        let c2 = cop c1 in
        if contains c2 r && not (contains c1 aa1) then
          Printf.printf
            "error: %s %a = %a -> %a; %s %s = %s\n"
            opname print a1 print r print aa1 
            opname (Z.to_string c1) (Z.to_string c2)
      with _ -> ()
     )
(* test contructor for backward unary operator *)
    
let bwd_binop (aop:t->t->t->(t*t) with_bot) (cop:Z.t->Z.t->Z.t) opname a1 a2 r c1 c2 =
  match aop a1 a2 r with
  | BOT ->
     (try
        let c3 = cop c1 c2 in
        if contains c3 r then
          Printf.printf
            "error: %a %s %a = %a -> ⊥; %s %s %s = %s\n"
            print a1 opname print a2 print r
            (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3)
      with _ -> ()
     )
  | Nb (aa1,aa2) ->
     (try
        let c3 = cop c1 c2 in
        if contains c3 r && (not (contains c1 aa1) || not (contains c2 aa2)) then
          Printf.printf
            "error: %a %s %a = %a -> %a,%a; %s %s %s = %s\n"
            print a1 opname print a2 print r print aa1 print aa2
            (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3)
      with _ -> ()
     )
(* test contructor for backward binary operator *)
    
  


(* random sampling *)
(* *************** *)

    
let sample ((lo,hi):t) =
  match lo,hi with
  | B.MINF, B.PINF -> Z.of_int (Random.bits() - Random.bits())
  | B.Finite l, B.PINF -> Z.add l (Z.of_int (Random.bits()))
  | B.MINF, B.Finite h -> Z.sub h (Z.of_int (Random.bits()))
  | B.Finite l, B.Finite h -> Z.add l (Z.rem (Z.of_int (Random.bits())) (Z.succ (Z.sub h l)))
  | _ -> failwith "IntItvTest.sample: invalid interval"
(* get one sample from the interval *)

let sample_nb (a:t) nb =
  let rec doit r acc =
    if r <= 0 then acc
    else doit (r-1) ((sample a)::acc)
  in
  let r = doit nb [] in
  let r = match fst a with B.Finite x -> if is_singleton a then x::r else x::(Z.succ x):: r | _ -> r in
  let r = match snd a with B.Finite x -> if is_singleton a then x::r else x::(Z.pred x):: r | _ -> r in
  r  
(* get nb samples from the interval + bounds if bounded *)
  
let generate_list (a:t) =
  if is_bounded a && size a <= Z.of_int max_exhaustive then to_list a
  else sample_nb a max_exhaustive
(* get values in the interval, eithe rsampled or exhaustive depending on the size *)
         


(* operator tests *)
(* ************** *)

  
let test_unop opname test =
  Printf.printf "testing %s\n%!" opname;
  List.iter (fun a -> List.iter (fun c -> test opname a c) (generate_list a)) interesting_intervals

let test_binop opname test =
  Printf.printf "testing %s\n%!" opname;
  (* test all for the interval pair a1,a2 *)
  let tst_all a1 a2 =
    let l1, l2 = generate_list a1, generate_list a2 in
    List.iter (fun c1 -> List.iter (fun c2 -> test opname a1 a2 c1 c2) l2) l1
  in
  (* test all pairs of intervals from l *)
  let tst_all_pair l =
    List.iter (fun a1 -> List.iter (fun a2 -> tst_all a1 a2) l) l
  in  
  tst_all_pair interesting_intervals
(* all binaries, except shift *)
  
let test_shift opname test =
  Printf.printf "testing %s\n%!" opname;
  (* test all for the interval pair a1,a2 *)
  let tst_all a1 ((_,h) as a2) =
    if B.is_positive h && B.leq h (B.of_int 256) then
      let l1, l2 = generate_list a1, generate_list a2 in
      List.iter (fun c1 -> List.iter (fun c2 -> test opname a1 a2 c1 c2) l2) l1
  in
  (* test all pairs of intervals from l *)
  let tst_all_pair l =
    List.iter (fun a1 -> List.iter (fun a2 -> tst_all a1 a2) l) l
  in  
  tst_all_pair interesting_intervals
(* for shifts, avoid negative or large second argument *)

let test_bwd_unop opname test =
  Printf.printf "testing %s\n%!" opname;
  let tst_all a1 r =
    let l1 = generate_list a1 in
    List.iter (fun c1 -> test opname a1 r c1) l1
  in
  let tst_all_pair l =
    List.iter (fun a1 -> List.iter (fun a2 -> tst_all a1 a2) l) l
  in  
  tst_all_pair interesting_intervals
  
let test_bwd_binop opname test =
  Printf.printf "testing %s\n%!" opname;
  let tst_all a1 a2 r =
    let l1, l2 = generate_list a1, generate_list a2 in
    List.iter (fun c1 -> List.iter (fun c2 -> test opname a1 a2 r c1 c2) l2) l1
  in
  let tst_all l =
    List.iter (fun a1 -> List.iter (fun a2 -> List.iter (fun r -> tst_all a1 a2 r) l) l) l
  in  
  tst_all interesting_intervals
  
let test_bwd_shift opname test =
  Printf.printf "testing %s\n%!" opname;
  let tst_all a1 ((_,h) as a2) r =
    if B.is_positive h && B.leq h (B.of_int 256) then
      let l1, l2 = generate_list a1, generate_list a2 in
      List.iter (fun c1 -> List.iter (fun c2 -> test opname a1 a2 r c1 c2) l2) l1
  in
  let tst_all l =
    List.iter (fun a1 -> List.iter (fun a2 -> List.iter (fun r -> tst_all a1 a2 r) l) l) l
  in  
  tst_all interesting_intervals
  

  
(* test *)
(* **** *)


(* TODO: set-theoretic, wrap *)

let test () =
  
  test_unop "-" (unop neg Z.neg);
  test_unop "abs" (unop abs Z.abs);  
  test_unop "succ" (unop succ Z.succ);  
  test_unop "pred" (unop pred Z.pred); 
  test_binop "+" (binop add Z.add);
  test_binop "-" (binop sub Z.sub);
  test_binop "*" (binop mul Z.mul);
  test_binop "/" (binop_bot div Z.div);
  test_binop "/↓" (binop_bot ediv Z.ediv);
  test_binop "unmerged /" (binop_unmerged div_unmerged Z.div);
  test_binop "unmerged /↓" (binop_unmerged ediv_unmerged Z.ediv);
  test_binop "%" (binop_bot rem Z.rem);
  test_binop "%↓" (binop_bot erem Z.erem);
  test_shift "<<" (binop_bot shift_left (fun a b -> Z.shift_left a (Z.to_int b)));
  test_shift ">>" (binop_bot shift_right (fun a b -> Z.shift_right a (Z.to_int b)));
  test_shift ">>>" (binop_bot shift_right_trunc (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))));
  test_unop "~" (unop bit_not Z.lognot);
  test_binop "&" (binop bit_and Z.logand);
  test_binop "|" (binop bit_or Z.logor);
  test_binop "^" (binop bit_xor Z.logxor);
  test_unop "bool" (unop log_cast (fun a -> if a = Z.zero then Z.zero else Z.one));
  test_unop "!" (unop log_not (fun a -> if a <> Z.zero then Z.zero else Z.one));
  test_binop "||" (binop log_or (fun a b -> if a <> Z.zero || b <> Z.zero then Z.one else Z.zero));
  test_binop "&&" (binop log_and (fun a b -> if a <> Z.zero && b <> Z.zero then Z.one else Z.zero));
  test_binop "^^" (binop log_xor (fun a b -> if (a <> Z.zero) <> (b <> Z.zero) then Z.one else Z.zero));
  test_binop "log ==" (binop log_eq (fun a b -> if a = b then Z.one else Z.zero));
  test_binop "log !=" (binop log_neq (fun a b -> if a <> b then Z.one else Z.zero));
  test_binop "log <" (binop log_lt (fun a b -> if a < b then Z.one else Z.zero));
  test_binop "log >" (binop log_gt (fun a b -> if a > b then Z.one else Z.zero));
  test_binop "log <=" (binop log_leq (fun a b -> if a <= b then Z.one else Z.zero));
  test_binop "log >=" (binop log_geq (fun a b -> if a >= b then Z.one else Z.zero));
  test_binop "filter =" (filter filter_eq (=));
  test_binop "filter ≠" (filter filter_neq (<>));
  test_binop "filter <" (filter filter_lt (<));
  test_binop "filter >" (filter filter_gt (>));
  test_binop "filter ≤" (filter filter_leq (<=));
  test_binop "filter ≥" (filter filter_geq (>=));
  test_bwd_unop "bwd neg" (bwd_unop bwd_neg Z.neg);
  test_bwd_unop "bwd abs" (bwd_unop bwd_abs Z.abs);
  test_bwd_unop "bwd succ" (bwd_unop bwd_succ Z.succ);
  test_bwd_unop "bwd pred" (bwd_unop bwd_pred Z.pred);
  test_bwd_binop "bwd +" (bwd_binop bwd_add Z.add);
  test_bwd_binop "bwd -" (bwd_binop bwd_sub Z.sub);
  test_bwd_binop "bwd *" (bwd_binop bwd_mul Z.mul);
  test_bwd_binop "bwd /" (bwd_binop bwd_div Z.div);
  test_bwd_binop "bwd /↓" (bwd_binop bwd_ediv Z.ediv);
  test_bwd_binop "bwd %" (bwd_binop bwd_rem Z.rem);
  test_bwd_binop "bwd %↓" (bwd_binop bwd_erem Z.erem);
  test_bwd_unop "bwd ~" (bwd_unop bwd_bit_not Z.lognot);
  test_bwd_binop "bwd ^" (bwd_binop bwd_bit_xor Z.logxor);
  test_bwd_binop "bwd |" (bwd_binop bwd_bit_or Z.logor);
  test_bwd_binop "bwd &" (bwd_binop bwd_bit_and Z.logand);
  test_bwd_shift "<<" (bwd_binop bwd_shift_left (fun a b -> Z.shift_left a (Z.to_int b)));
  test_bwd_shift ">>" (bwd_binop bwd_shift_right (fun a b -> Z.shift_right a (Z.to_int b)));
  test_bwd_shift ">>>" (bwd_binop bwd_shift_right_trunc (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))));
  ()
  
let _ = test ()
      
