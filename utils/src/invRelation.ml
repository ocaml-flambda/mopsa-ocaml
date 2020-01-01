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
  InvRelation - Relations with access to inverse images.
 *)

open InvRelationSig

module Make(Dom: OrderedType)(CoDom: OrderedType) = struct

  module DomSet = SetExt.Make(Dom)
  module DomMap = MapExt.Make(Dom)
  module CoDomSet = SetExt.Make(CoDom)
  module CoDomMap = MapExt.Make(CoDom)
    
  type t = {
      img: CoDomSet.t DomMap.t;
      inv: DomSet.t CoDomMap.t
    }
  (** We maintain both the image of each domain element and the
      inverse image of each codomain element.
   *)

  type dom = Dom.t
  type dom_set = DomSet.t
  type codom = CoDom.t
  type codom_set = CoDomSet.t
  type binding = dom * codom
            
  let empty =
    { img = DomMap.empty;
      inv = CoDomMap.empty;
    }

  let image x r =
    try DomMap.find x r.img with Not_found -> CoDomSet.empty

  let inverse y r =
    try CoDomMap.find y r.inv with Not_found -> DomSet.empty

  let is_image_empty x r =
    not (DomMap.mem x r.img)
    
  let is_inverse_empty y r =
    not (CoDomMap.mem y r.inv)

  let is_empty r =
    DomMap.is_empty r.img


  let singleton x y =
    { img = DomMap.singleton x (CoDomSet.singleton y);
      inv = CoDomMap.singleton y (DomSet.singleton x);
    }


  (* internal function, to update fields *)
  let mk r img inv =
    if r.img == img && r.inv == inv then r
    else { img; inv; }
    
  (* internal function: does not update inv *)
  let set_img x ys r =
    if CoDomSet.is_empty ys then mk r (DomMap.remove x r.img) r.inv
    else mk r (DomMap.add x ys r.img) r.inv

  (* internal function: does not update img *)
  let set_inv y xs r =
    if DomSet.is_empty xs then mk r r.img (CoDomMap.remove y r.inv)
    else mk r r.img (CoDomMap.add y xs r.inv)

    
  let set_image x ys r =
    (* update inv *)
    CoDomSet.fold2_diff
      (fun y r -> set_inv y (DomSet.remove x (inverse y r)) r)
      (fun y r -> set_inv y (DomSet.add x (inverse y r)) r)
      (image x r) ys
      (* update img *)
      (set_img x ys r)  

  let set_inverse y xs r =
    (* update img *)
    DomSet.fold2_diff
      (fun x r -> set_img x (CoDomSet.remove y (image x r)) r)
      (fun x r -> set_img x (CoDomSet.add y (image x r)) r)
      (inverse y r) xs
      (* update inv *)
      (set_inv y xs r)  
    
    
  let add x y r =
    mk
      r
      (DomMap.add x (CoDomSet.add y (image x r)) r.img)
      (CoDomMap.add y (DomSet.add x (inverse y r)) r.inv)

  let remove x y r =
    set_img x (CoDomSet.remove y (image x r)) r
    |> set_inv y (DomSet.remove x (inverse y r))

  let add_image_set x ys r =
    set_image x (CoDomSet.union (image x r) ys) r

  let add_inverse_set y xs r =
    set_inverse y (DomSet.union (inverse y r) xs) r
    
  let remove_image_set x ys r =
    set_image x (CoDomSet.diff (image x r) ys) r

  let remove_inverse_set y xs r =
    set_inverse y (DomSet.diff (inverse y r) xs) r
    
  let remove_image x r =
    set_image x CoDomSet.empty r
    
  let remove_inverse y r =
    set_inverse y DomSet.empty r

  let mem x y r =
    CoDomSet.mem y (image x r)

  let mem_domain x r =
    DomMap.mem x r.img

        
  let of_list l =
    List.fold_left (fun r (x,y) -> add x y r) empty l

  let min_binding r =
    let x,ys = DomMap.min_binding r.img in
    x, CoDomSet.min_elt ys
    
  let max_binding r =
    let x,ys = DomMap.max_binding r.img in
    x, CoDomSet.max_elt ys

  let choose r =
    let x,ys = DomMap.choose r.img in
    x, CoDomSet.choose ys
       
  let cardinal r =
    DomMap.fold (fun _ i r -> r + CoDomSet.cardinal i) r.img 0


    
  let iter f r =
    DomMap.iter (fun x i -> CoDomSet.iter (fun y -> f x y) i) r.img

  let fold f r acc =
    DomMap.fold
      (fun x i acc -> CoDomSet.fold (fun y acc -> f x y acc) i acc)
      r.img acc

  let bindings r =
    List.rev (fold (fun x y l -> (x,y)::l) r [])

   let map f r =
    fold
      (fun x y acc -> let x',y' = f x y in add x' y' acc)
      r empty
    
  let domain_map f r =
    DomMap.fold
      (fun x ys r -> add_image_set (f x) ys r)
      r.img empty

  let codomain_map f r =
    CoDomMap.fold
      (fun y xs r -> add_inverse_set (f y) xs r)
      r.inv empty

  let for_all f r =
    DomMap.for_all (fun x i -> CoDomSet.for_all (fun y -> f x y) i) r.img
    
  let exists f r =
    DomMap.exists (fun x i -> CoDomSet.exists (fun y -> f x y) i) r.img

  let filter f r =
    DomMap.fold
      (fun x ys r ->
        CoDomSet.fold
          (fun y r -> if f x y then r else remove x y r)
          ys r
      ) r.img r

    
  (* binary operations *)
    
  let compare r1 r2 =
    DomMap.compare CoDomSet.compare r1.img r2.img
    
  let equal r1 r2 =
    DomMap.equal CoDomSet.equal r1.img r2.img

  let subset r1 r2 =
    DomMap.for_all2zo
      (fun _ _ -> false)
      (fun _ _ -> true)
      (fun _ s1 s2 -> CoDomSet.subset s1 s2)
      r1.img r2.img

  let union r1 r2 =
    (* apply union separately to the img relation and to the inv relation *)
    mk
      r1
      (DomMap.map2zo
         (fun _ ys -> ys) (fun _ ys -> ys) (fun _ -> CoDomSet.union)
         r1.img r2.img
      )
      (CoDomMap.map2zo
         (fun _ xs -> xs) (fun _ xs -> xs) (fun _ -> DomSet.union)
         r1.inv r2.inv
      )

  let inter r1 r2 =
    (* apply intersection separately to the img relation and to the inv relation *)
    r1
    |> DomMap.fold2zo
         (fun x _ r -> mk r (DomMap.remove x r.img) r.inv)
         (fun _ _ r -> r)
         (fun x ys1 ys2 r -> set_img x (CoDomSet.inter ys1 ys2) r)
         r1.img r2.img
    |> CoDomMap.fold2zo
         (fun y _ r -> mk r r.img (CoDomMap.remove y r.inv))
         (fun _ _ r -> r)
         (fun y xs1 xs2 r -> set_inv y (DomSet.inter xs1 xs2) r)
         r1.inv r2.inv


  let diff r1 r2 =
    (* apply difference separately to the img relation and to the inv relation *)
    r1
    |> DomMap.fold2zo
         (fun _ _ r -> r)
         (fun _ _ r -> r)
         (fun x ys1 ys2 r -> set_img x (CoDomSet.diff ys1 ys2) r)
         r1.img r2.img
    |> CoDomMap.fold2zo
         (fun _ _ r -> r)
         (fun _ _ r -> r)
         (fun y xs1 xs2 r -> set_inv y (DomSet.diff xs1 xs2) r)
         r1.inv r2.inv

    
  let iter2 f1 f2 f r1 r2 =
    DomMap.iter2o
    (fun x -> CoDomSet.iter (f1 x))
    (fun x -> CoDomSet.iter (f2 x))
    (fun x -> CoDomSet.iter2 (f1 x) (f2 x) (f x))
    r1.img r2.img
    
  let iter2_diff f1 f2 r1 r2 =
    DomMap.iter2o
    (fun x -> CoDomSet.iter (f1 x))
    (fun x -> CoDomSet.iter (f2 x))
    (fun x -> CoDomSet.iter2_diff (f1 x) (f2 x))
    r1.img r2.img


  let fold2 f1 f2 f r1 r2 acc =
    DomMap.fold2o
    (fun x -> CoDomSet.fold (f1 x))
    (fun x -> CoDomSet.fold (f2 x))
    (fun x -> CoDomSet.fold2 (f1 x) (f2 x) (f x))
    r1.img r2.img acc
    
  let fold2_diff f1 f2 r1 r2 =
    DomMap.fold2zo
    (fun x -> CoDomSet.fold (f1 x))
    (fun x -> CoDomSet.fold (f2 x))
    (fun x -> CoDomSet.fold2_diff (f1 x) (f2 x))
    r1.img r2.img


  let for_all2 f1 f2 f r1 r2 =
    DomMap.for_all2o
    (fun x -> CoDomSet.for_all (f1 x))
    (fun x -> CoDomSet.for_all (f2 x))
    (fun x -> CoDomSet.for_all2 (f1 x) (f2 x) (f x))
    r1.img r2.img
    
  let for_all2_diff f1 f2 r1 r2 =
    DomMap.for_all2o
    (fun x -> CoDomSet.for_all (f1 x))
    (fun x -> CoDomSet.for_all (f2 x))
    (fun x -> CoDomSet.for_all2_diff (f1 x) (f2 x))
    r1.img r2.img


  let exists2 f1 f2 f r1 r2 =
    DomMap.exists2o
    (fun x -> CoDomSet.exists (f1 x))
    (fun x -> CoDomSet.exists (f2 x))
    (fun x -> CoDomSet.exists2 (f1 x) (f2 x) (f x))
    r1.img r2.img
    
  let exists2_diff f1 f2 r1 r2 =
    DomMap.exists2o
    (fun x -> CoDomSet.exists (f1 x))
    (fun x -> CoDomSet.exists (f2 x))
    (fun x -> CoDomSet.exists2_diff (f1 x) (f2 x))
    r1.img r2.img
    
 
  (* domain operations *)
    
  let iter_domain f r =
    DomMap.iter f r.img

  let fold_domain f r acc =
    DomMap.fold f r.img acc

  let map_domain f r =
    DomMap.fold
      (fun x i r -> set_image x (f x i) r)
      r.img empty

  let map2_domain f r1 r2 =
    DomMap.fold2
      (fun x ys1 ys2 r -> set_image x (f x ys1 ys2) r)
      r1.img r2.img r1

  let map2o_domain f1 f2 f r1 r2 =
    DomMap.fold2o
      (fun x ys r -> set_image x (f1 x ys) r)
      (fun x ys r -> set_image x (f2 x ys) r)
      (fun x ys1 ys2 r -> set_image x (f x ys1 ys2) r)
      r1.img r2.img r1

  let map2zo_domain f1 f2 f r1 r2 =
    DomMap.fold2zo
      (fun x ys r -> set_image x (f1 x ys) r)
      (fun x ys r -> set_image x (f2 x ys) r)
      (fun x ys1 ys2 r -> set_image x (f x ys1 ys2) r)
      r1.img r2.img r1


  let for_all_domain f r =
    DomMap.for_all f r.img

  let exists_domain f r =
    DomMap.exists f r.img

  let filter_domain f r =
    DomMap.fold
      (fun x ys r -> if f x ys then r else remove_image x r)
      r.img r

  let min_domain r =
    fst (DomMap.min_binding r.img)

  let max_domain r =
    fst (DomMap.max_binding r.img)

  let choose_domain r =
    fst (DomMap.choose r.img)

  let cardinal_domain r =
    DomMap.cardinal r.img

  let elements_domain r =
    List.rev (DomMap.fold (fun x _ l -> x::l) r.img [])
    

  (* codomain operations *)
    
  let iter_codomain f r =
    CoDomMap.iter f r.inv

  let fold_codomain f r acc =
    CoDomMap.fold f r.inv acc

  let map_codomain f r =
    CoDomMap.fold
      (fun y i r -> set_inverse y (f y i) r)
      r.inv empty

  let for_all_codomain f r =
    CoDomMap.for_all f r.inv

  let exists_codomain f r =
    CoDomMap.exists f r.inv

  let filter_codomain f r =
    CoDomMap.fold
      (fun y xs r -> if f y xs then r else remove_inverse y r)
      r.inv r

  let min_codomain r =
    fst (CoDomMap.min_binding r.inv)

  let max_codomain r =
    fst (CoDomMap.max_binding r.inv)

  let choose_codomain r =
    fst (CoDomMap.choose r.inv)

  let cardinal_codomain r =
    CoDomMap.cardinal r.inv

  let elements_codomain r =
    List.rev (CoDomMap.fold (fun y _ l -> y::l) r.inv [])



    
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
