(**
  CongItvTest - Unit tests for integer congruence domain.

  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


open Bot
open Congruences.IntCong


let mk_big () =
  Z.add (Z.shift_left (Z.of_int (Random.bits ())) 60)
        (Z.add (Z.shift_left (Z.of_int (Random.bits ())) 30)
               (Z.of_int (Random.bits ())))
   
let congs =
  [zero; one; mone; minf_inf;
   cst_int 2; cst_int 18; cst_int (-25); cst (mk_big()); cst (Z.neg (mk_big ()));
   of_int 2 0; of_int 2 1;
   of_int 3 0; of_int 3 1; of_int 3 2;
   of_int 4 0; of_int 4 1; of_int 4 2; of_int 4 3;
   of_int 5 0; of_int 5 1; of_int 5 2;
   of_int 6 0; of_int 6 1; of_int 6 3; of_int 6 4;
   of_int 12 0; of_int 12 1; of_int 12 3; of_int 12 4; of_int 12 6;
   of_int 13 0; of_int 13 7;
   of_int 25 0; of_int 28 12; of_int 29993 23498;
   of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big());
   of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big());
   of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big()); of_z (mk_big()) (mk_big());
  ]


let sample_cong ((a,b):t) : Z.t list =
  if a = Z.zero then [b]
  else [b; Z.add b a; Z.sub b a;
        Z.add b (Z.mul a (mk_big())); Z.sub b (Z.mul a (mk_big()));
        Z.add b (Z.mul a (mk_big())); Z.sub b (Z.mul a (mk_big()));
        Z.add b (Z.mul a (mk_big())); Z.sub b (Z.mul a (mk_big()));
       ]
  
  
let test_bin_val opname cop a1 a2 a3 c1 c2 =
  try
    let c3 = cop c1 c2 in
    if not (contains c3 a3) then
      Printf.printf
        "error: %a %s %a = %a; %s %s %s = %s\n"
        print a1 opname print a2 print a3
        (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
  with _ -> ()

          
let test_bin_val_bot opname cop a1 a2 a3 c1 c2 =
  match a3 with
  | Nb aa3 ->
     (try
       let c3 = cop c1 c2 in
       if not (contains c3 aa3) then
         Printf.printf
           "error: %a %s %a = %a; %s %s %s = %s\n"
           print a1 opname print a2 print aa3
           (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
     with _ -> ())
  | BOT ->
     (try
        let c3 = cop c1 c2 in
        Printf.printf
          "error: %a %s %a = ⊥; %s %s %s = %s\n"
          print a1 opname print a2
          (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c3);
      with _ -> ())

          
let test_bin opname test aop cop =
  Printf.printf "testing %s\n%!" opname;
  List.iter
    (fun a1 ->
      List.iter
        (fun a2 ->
          let a3 = aop a1 a2 in
          List.iter
            (fun c1 ->
              List.iter
                (fun c2 ->
                  test opname cop a1 a2 a3 c1 c2
                ) (sample_cong a2)
            ) (sample_cong a1)
        ) congs
    ) congs



let test_un_val opname cop a1 a3 c1 =
  try
    let c3 = cop c1 in
    if not (contains c3 a3) then
      Printf.printf
        "error: %s %a = %a; %s %s = %s\n"
        opname print a1 print a3
        opname (Z.to_string c1) (Z.to_string c3);
  with _ -> ()

let test_un opname test aop cop =
  Printf.printf "testing %s\n%!" opname;
  List.iter
    (fun a1 ->
      let a3 = aop a1 in
      List.iter
        (fun c1 ->
          test opname cop a1 a3 c1
        ) (sample_cong a1)
    ) congs



let test_bwd_un opname test aop cop =
  Printf.printf "testing %s\n%!" opname;
  List.iter
    (fun a1 ->
      List.iter
        (fun r ->
          let a1' = aop a1 r in
          List.iter
            (fun c1 ->
              test opname cop a1 r a1' c1
            ) (sample_cong a1)
        ) congs
    ) congs

let test_bwd_un_val opname cop a1 r a1' c1 =
  let c = cop c1 in
  if contains c r then
    match a1' with
    | BOT -> 
        Printf.printf
          "error: %s %a = %a -> ⊥; %s %s = %s\n"
          opname print a1 print r
          opname (Z.to_string c1) (Z.to_string c)
    | Nb aa ->
       if not (contains c1 aa) then
         Printf.printf
           "error: %s %a = %a -> %a; %s %s = %s\n"
           opname print a1 print r print aa
           opname (Z.to_string c1) (Z.to_string c)


let test_filter opname test aop cop =
  Printf.printf "testing %s\n%!" opname;
  List.iter
    (fun a1 ->
      List.iter
        (fun a2 ->
          let a' = aop a1 a2 in
          List.iter
            (fun c1 ->
              List.iter
                (fun c2 ->
                  test opname cop a1 a2 a' c1 c2
                ) (sample_cong a2)
            ) (sample_cong a1)
        ) congs
    ) congs

let test_filter_val opname cop a1 a2 a' c1 c2 =
  if cop c1 c2 then
    match a' with
    | BOT -> 
        Printf.printf
          "error: %a %s %a -> ⊥; %s %s %s\n"
          print a1 opname print a2
          (Z.to_string c1) opname (Z.to_string c2)
    | Nb (a1',a2') ->
       if not (contains c1 a1' && contains c2 a2') then
        Printf.printf
          "error: %a %s %a -> %a,%a; %s %s %s\n"
          print a1 opname print a2 print a1' print a2'
          (Z.to_string c1) opname (Z.to_string c2)

let test_bwd_bin opname test aop cop =
  Printf.printf "testing %s\n%!" opname;
  List.iter
    (fun a1 ->
      List.iter
        (fun a2 ->
          List.iter
            (fun r ->
              let a' = aop a1 a2 r in
              List.iter
                (fun c1 ->
                  List.iter
                    (fun c2 ->
                      test opname cop a1 a2 r a' c1 c2
                    ) (sample_cong a2)
                ) (sample_cong a1)
            ) congs
        ) congs
    ) congs

let test_bwd_bin_val opname cop a1 a2 r a' c1 c2 =
  let c = cop c1 c2 in
  if contains c r then
    match a' with
    | BOT -> 
        Printf.printf
          "error: %a %s %a = %a -> ⊥; %s %s %s = %s\n"
          print a1 opname print a2 print r
          (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c)
    | Nb (a1',a2') ->
       if not (contains c1 a1' && contains c2 a2') then
        Printf.printf
          "error: %a %s %a = %a -> %a,%a; %s %s %s = %s\n"
          print a1 opname print a2 print r print a1' print a2'
          (Z.to_string c1) opname (Z.to_string c2) (Z.to_string c)

       
let wrap2 lo up a =
  wrap a (Z.of_int lo) (Z.of_int up)
  
let wrap_scalar lo up a =
  let lo,up = Z.of_int lo, Z.of_int up in
  Z.add lo (Z.erem (Z.sub a lo) (Z.succ (Z.sub up lo)))

let test () =
  test_un "neg" test_un_val neg Z.neg;
  test_un "abs" test_un_val abs Z.abs;
  test_un "succ" test_un_val succ Z.succ;
  test_un "pred" test_un_val pred Z.pred;
  test_un "wrap [0,255]" test_un_val (wrap2 0 255) (wrap_scalar 0 255);
  test_un "wrap [-128,127]" test_un_val (wrap2 (-128) 127) (wrap_scalar (-128) 127);
  test_bin "join left" test_bin_val join (fun a b -> a);
  test_bin "join right" test_bin_val join (fun a b -> b);
  test_bin "meet" test_bin_val_bot meet (fun a b -> if a <> b then failwith "meet"; a);
  test_bin "+" test_bin_val add Z.add;
  test_bin "-" test_bin_val sub Z.sub;
  test_bin "*" test_bin_val mul Z.mul;
  test_bin "/" test_bin_val_bot div Z.div;
  test_bin "%" test_bin_val_bot rem Z.rem;
  test_un "bool" test_un_val log_cast (fun a -> if a = Z.zero then Z.zero else Z.one);
  test_un "!" test_un_val log_not (fun a -> if a <> Z.zero then Z.zero else Z.one);
  test_bin "||" test_bin_val log_or (fun a b -> if a <> Z.zero || b <> Z.zero then Z.one else Z.zero);
  test_bin "&&" test_bin_val log_and (fun a b -> if a <> Z.zero && b <> Z.zero then Z.one else Z.zero);
  test_bin "^^" test_bin_val log_xor (fun a b -> if (a <> Z.zero) <> (b <> Z.zero) then Z.one else Z.zero);
  test_bin "log ==" test_bin_val log_eq (fun a b -> if a = b then Z.one else Z.zero);
  test_bin "log !=" test_bin_val log_neq (fun a b -> if a <> b then Z.one else Z.zero);
  test_bin "log <" test_bin_val log_lt (fun a b -> if a < b then Z.one else Z.zero);
  test_bin "log >" test_bin_val log_gt (fun a b -> if a > b then Z.one else Z.zero);
  test_bin "log <=" test_bin_val log_leq (fun a b -> if a <= b then Z.one else Z.zero);
  test_bin "log >=" test_bin_val log_geq (fun a b -> if a >= b then Z.one else Z.zero);
  test_bin "<<" test_bin_val_bot shift_left (fun a b -> Z.shift_left a (Z.to_int b));
  test_bin ">>" test_bin_val_bot shift_right (fun a b -> Z.shift_right a (Z.to_int b));
  test_bin ">>>" test_bin_val_bot shift_right_trunc (fun a b -> Z.shift_right_trunc a (Z.to_int b));
  test_un "~" test_un_val bit_not Z.lognot;
  test_filter "filter =" test_filter_val filter_eq (=);
  test_filter "filter !=" test_filter_val filter_neq (<>);
  test_filter "filter <=" test_filter_val filter_leq (<=);
  test_filter "filter >=" test_filter_val filter_geq (>=);
  test_filter "filter <" test_filter_val filter_lt (<);
  test_filter "filter >" test_filter_val filter_gt (>);
  test_bwd_un "bwd neg" test_bwd_un_val bwd_neg Z.neg;
  test_bwd_un "bwd abs" test_bwd_un_val bwd_abs Z.abs;
  test_bwd_un "bwd succ" test_bwd_un_val bwd_succ Z.succ;
  test_bwd_un "bwd pred" test_bwd_un_val bwd_pred Z.pred;
  test_bwd_bin "bwd +" test_bwd_bin_val bwd_add Z.add;
  test_bwd_bin "bwd -" test_bwd_bin_val bwd_sub Z.sub;
  test_bwd_bin "bwd *" test_bwd_bin_val bwd_mul Z.mul;
  test_bwd_un "bwd ~" test_bwd_un_val bwd_bit_not Z.lognot;
  ()


let _ = test ()
      
