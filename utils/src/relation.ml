(**
  Relation - Relations (or multimaps) between ordered sets.

  Copyright (C) 2018 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


open RelationSig

module Make(Dom: OrderedType)(CoDom: OrderedType) = struct

  module CoDomSet = SetExt.Make(CoDom)
  module DomMap = MapExt.Make(Dom)
    
  type t = CoDomSet.t DomMap.t
  (** A relation is a map from the domain to the power-set of the codomain.
      The image f(x) of an element is never the empty set.
   *)

  type dom = Dom.t
  type codom = CoDom.t
  type codom_set = CoDomSet.t
  type binding = dom * codom
            
  let empty =
    DomMap.empty
            
  let image x r =
    try DomMap.find x r with Not_found -> CoDomSet.empty

  let set_image x ys r =
    if CoDomSet.is_empty ys then DomMap.remove x r
    else DomMap.add x ys r

  let is_image_empty x r =
    not (DomMap.mem x r)

  let is_empty r =
    DomMap.is_empty r


  let singleton x y =
    DomMap.singleton x (CoDomSet.singleton y)
    
  let add x y r =
    DomMap.add x (CoDomSet.add y (image x r)) r

  let add_set x ys r =
    set_image x (CoDomSet.union ys (image x r)) r

  let remove x y r =
    set_image x (CoDomSet.remove y (image x r)) r
    
  let remove_set x ys r =
    set_image x (CoDomSet.diff (image x r) ys) r

  let remove_image x r =
    DomMap.remove x r

  let mem x y r =
    CoDomSet.mem y (image x r)

    
  let of_list l =
    List.fold_left (fun r (x,y) -> add x y r) empty l

  let min_binding r =
    let x,ys = DomMap.min_binding r in
    x, CoDomSet.min_elt ys
    
  let max_binding r =
    let x,ys = DomMap.max_binding r in
    x, CoDomSet.max_elt ys

  let choose r =
    let x,ys = DomMap.choose r in
    x, CoDomSet.choose ys
       
  let cardinal r =
    DomMap.fold (fun _ i r -> r + CoDomSet.cardinal i) r 0


    
  let iter f r =
    DomMap.iter (fun x i -> CoDomSet.iter (fun y -> f x y) i) r

  let fold f r acc =
    DomMap.fold
      (fun x i acc -> CoDomSet.fold (fun y acc -> f x y acc) i acc)
      r acc

  let bindings r =
    List.rev (fold (fun x y l -> (x,y)::l) r [])

  let map f r =
    fold
      (fun x y acc -> let x',y' = f x y in add x' y' acc)
      r empty
    
  let domain_map f r =
    DomMap.fold
      (fun x i r -> add_set (f x) i r)
      r empty

  let codomain_map f r =
    DomMap.map (CoDomSet.map f) r

  let for_all f r =
    DomMap.for_all (fun x i -> CoDomSet.for_all (fun y -> f x y) i) r
    
  let exists f r =
    DomMap.exists (fun x i -> CoDomSet.exists (fun y -> f x y) i) r

  let filter f r =
    DomMap.fold
      (fun x i r -> set_image x (CoDomSet.filter (fun y -> f x y) i) r) r r


  (* binary operations *)


      
  let compare r1 r2 =
    DomMap.compare CoDomSet.compare r1 r2
    
  let equal r1 r2 =
    DomMap.equal CoDomSet.equal r1 r2

  let subset r1 r2 =
    DomMap.for_all2zo
      (fun _ _ -> false)
      (fun _ _ -> true)
      (fun _ -> CoDomSet.subset)
      r1 r2

  let union r1 r2 =
    DomMap.map2zo
      (fun _ ys -> ys)
      (fun _ ys -> ys)
      (fun _ -> CoDomSet.union)
      r1 r2

  let inter r1 r2 =
    (* start from r1
       - remove x's image if x is only in r1
       - nothing if x is only in r2 (as it is not in r1)
       - update x's image if both in r1 and r2
     *)
    DomMap.fold2zo
      (fun x _ r -> remove_image x r)
      (fun _ _ r -> r)
      (fun x ys1 ys2 r -> set_image x (CoDomSet.inter ys1 ys2) r)
      r1 r2 r1

  let diff r1 r2 =
    (* start from r1
       - nothing if x is only in r1
       - nothing if x is only in r2 (as it is not in r1)
       - update x's image if both in r1 and r2
     *)
    DomMap.fold2o
      (fun _ _ r -> r)
      (fun _ _ r -> r)
      (fun x ys1 ys2 r -> set_image x (CoDomSet.diff ys1 ys2) r)
      r1 r2 r1

    
  let iter2 f1 f2 f r1 r2 =
    DomMap.iter2o
    (fun x -> CoDomSet.iter (f1 x))
    (fun x -> CoDomSet.iter (f2 x))
    (fun x -> CoDomSet.iter2 (f1 x) (f2 x) (f x))
    r1 r2
    
  let iter2_diff f1 f2 r1 r2 =
    DomMap.iter2o
    (fun x -> CoDomSet.iter (f1 x))
    (fun x -> CoDomSet.iter (f2 x))
    (fun x -> CoDomSet.iter2_diff (f1 x) (f2 x))
    r1 r2


  let fold2 f1 f2 f r1 r2 acc =
    DomMap.fold2o
    (fun x -> CoDomSet.fold (f1 x))
    (fun x -> CoDomSet.fold (f2 x))
    (fun x -> CoDomSet.fold2 (f1 x) (f2 x) (f x))
    r1 r2 acc
    
  let fold2_diff f1 f2 r1 r2 =
    DomMap.fold2zo
    (fun x -> CoDomSet.fold (f1 x))
    (fun x -> CoDomSet.fold (f2 x))
    (fun x -> CoDomSet.fold2_diff (f1 x) (f2 x))
    r1 r2


  let for_all2 f1 f2 f r1 r2 =
    DomMap.for_all2o
    (fun x -> CoDomSet.for_all (f1 x))
    (fun x -> CoDomSet.for_all (f2 x))
    (fun x -> CoDomSet.for_all2 (f1 x) (f2 x) (f x))
    r1 r2
    
  let for_all2_diff f1 f2 r1 r2 =
    DomMap.for_all2o
    (fun x -> CoDomSet.for_all (f1 x))
    (fun x -> CoDomSet.for_all (f2 x))
    (fun x -> CoDomSet.for_all2_diff (f1 x) (f2 x))
    r1 r2


  let exists2 f1 f2 f r1 r2 =
    DomMap.exists2o
    (fun x -> CoDomSet.exists (f1 x))
    (fun x -> CoDomSet.exists (f2 x))
    (fun x -> CoDomSet.exists2 (f1 x) (f2 x) (f x))
    r1 r2
    
  let exists2_diff f1 f2 r1 r2 =
    DomMap.exists2o
    (fun x -> CoDomSet.exists (f1 x))
    (fun x -> CoDomSet.exists (f2 x))
    (fun x -> CoDomSet.exists2_diff (f1 x) (f2 x))
    r1 r2



  (* domain operations *)
    
  let iter_domain f r =
    DomMap.iter f r

  let fold_domain f r acc =
    DomMap.fold f r acc

  let map_domain f r =
    DomMap.fold
      (fun x i r -> set_image x (f x i) r)
      r DomMap.empty

  let for_all_domain f r =
    DomMap.for_all f r

  let exists_domain f r =
    DomMap.exists f r

  let filter_domain f r =
    DomMap.filter f r

  let min_domain r =
    fst (DomMap.min_binding r)

  let max_domain r =
    fst (DomMap.max_binding r)

  let choose_domain r =
    fst (DomMap.choose r)

  let cardinal_domain r =
    DomMap.cardinal r

  let elements_domain r =
    List.rev (DomMap.fold (fun x _ l -> x::l) r [])
    

  (* printing *)

    
  type relation_printer = {
      print_empty: string;
      print_begin: string;
      print_open: string;
      print_comma: string;
      print_close: string;
      print_sep: string;
      print_end: string;
    }
                   
  let printer_default = {
      print_empty="{}";
      print_begin="{";
      print_open="(";
      print_comma=",";
      print_close=")";
      print_sep=";";
      print_end="}";
    }
                      
  let print_gen o printer dom codom ch s =
    if is_empty s then o ch printer.print_empty else (
      let first = ref true in
      o ch printer.print_begin;
      iter
        (fun x y ->
          if !first then first := false else o ch printer.print_sep;
          o ch printer.print_open;
          dom ch x;
          o ch printer.print_comma;
          codom ch y;
          o ch printer.print_close;
        ) s;
      o ch printer.print_end
    )
  (* internal printing helper *)
    
  let print printer dom codom ch l = print_gen output_string printer dom codom ch l
  let bprint printer dom codom ch l = print_gen Buffer.add_string printer dom codom ch l
  let fprint printer dom codom ch l = print_gen Format.pp_print_string printer dom codom ch l
                              
  let to_string printer dom codom l =
    let b = Buffer.create 10 in
    print_gen (fun () s -> Buffer.add_string b s) printer
              (fun () k -> dom k)
              (fun () k -> codom k)
              () l;
    Buffer.contents b

end
                                                  
                                                   
