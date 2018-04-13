(*  
  This file is derived from the map.ml file from the OCaml distribution.
  Changes are marked with the [MOPSA] symbol.

  Original copyright follows.
*)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* [MOPSA] module signatures moved to a separate file *)
open MapExtSig

  
module Make(Ord: OrderedType) = (struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let singleton x d = Node(Empty, x, d, Empty, 1)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Mapext.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Mapext.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Mapext.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Mapext.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data m = match m with
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            if d == data then m else Node(l, x, data, r, h)
          else if c < 0 then
            let ll = add x data l in
            if l == ll then m else bal ll v d r
          else
            let rr = add x data r in
            if r == rr then m else bal l v d rr

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node(l, x, d, Empty, _) -> (x, d)
      | Node(l, x, d, r, _) -> max_binding r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Mapext.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x m = match m with
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            let ll = remove x l in if l == ll then m else bal ll v d r
          else
            let rr = remove x r in if r == rr then m else bal l v d rr

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node(l', v, d', r', h)

    let rec mapi f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node(l', v, d', r', h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    (* [MOPSA] changed to call p in the key order *)
    let rec for_all p = function
        Empty -> true
      | Node(l, v, d, r, _) -> for_all p l && p v d && for_all p r

    (* [MOPSA] changed to call p in the key order *)
    let rec exists p = function
        Empty -> false
      | Node(l, v, d, r, _) -> exists p l || p v d || exists p r

    (* [MOPSA] changed to call p in the key order *)
    let filter p s =
      fold (fun k d a -> if p k d then add k d a else a) s Empty

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty -> accu
        | Node(l, v, d, r, _) ->
            part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
      part (Empty, Empty) s

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add v d r
      | (_, Empty) -> add v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, h2)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    (* We replace the original equal by one based on iter2zo.
       This assumes that cmp x x returns 0.
     *)
    (*
    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)
     *)

    (* We replace the original equal by one based on iter2zo.
       This assumes that cmp x x returns true.
     *)
    (*
    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)
        *)
       
    let rec cardinal = function
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding



    (* [MOPSA] additions *)
    (* ***************** *)


    let of_list l =
      List.fold_left (fun acc (k,x) -> add k x acc) empty l


    (* internal function *)
    (* similar to split, but returns unbalanced trees *)
    let rec cut k = function
      Empty -> Empty,None,Empty
    | Node (l1,k1,d1,r1,h1) ->
        let c = Ord.compare k k1 in
        if c < 0 then 
          let l2,d2,r2 = cut k l1 in (l2,d2,Node (r2,k1,d1,r1,h1))
        else if c > 0 then
          let l2,d2,r2 = cut k r1 in (Node (l1,k1,d1,l2,h1),d2,r2)
        else (l1,Some d1,r1)


    (* binary operations that fail on maps with different keys *)

    (* functions are called in increasing key order *)

    let rec map2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then Empty else invalid_arg "Mapext.map2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 ->
              Node (map2 f l1 l2, k, f k d1 d2, map2 f r1 r2, h1)
          | _, None, _ -> invalid_arg "Mapext.map2"
            
    let rec iter2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then () else invalid_arg "Mapext.iter2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> iter2 f l1 l2; f k d1 d2; iter2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.iter2"
            
    let rec fold2 f m1 m2 acc =
      match m1 with
      | Empty -> if m2 = Empty then acc else invalid_arg "Mapext.fold2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              fold2 f r1 r2 (f k d1 d2 (fold2 f l1 l2 acc))
          | _, None, _ -> invalid_arg "Mapext.fold2"
            
    let rec for_all2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then true else invalid_arg "Mapext.for_all2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              for_all2 f l1 l2 && f k d1 d2 && for_all2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.for_all2"
            
    let rec exists2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then false else invalid_arg "Mapext.exists2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              exists2 f l1 l2 || f k d1 d2 || exists2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.exists2"
            

    (* as above, but ignore physically equal subtrees
       - for map, assumes: f k d d = d
       - for iter, assumes: f k d d has no effect
       - for fold, assumes: k f d d acc = acc
       - for for_all, assumes: f k d d = true
       - for exists, assumes: f k d d = false
     *)

    let rec map2z f m1 m2 =
      if m1 == m2 then m1 else
      match m1 with
      | Empty -> if m2 = Empty then Empty else invalid_arg "Mapext.map2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 ->
              let d = if d1 == d2 then d1 else f k d1 d2 in
              Node (map2z f l1 l2, k, d, map2z f r1 r2, h1)
          | _, None, _ -> invalid_arg "Mapext.map2z"
            
    let rec iter2z f m1 m2 =
      if m1 == m2 then () else
      match m1 with
      | Empty -> if m2 = Empty then () else invalid_arg "Mapext.iter2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              iter2z f l1 l2; (if d1 != d2 then f k d1 d2); iter2z f r1 r2
          | _, None, _ -> invalid_arg "Mapext.iter2z"
            
    let rec fold2z f m1 m2 acc =
      if m1 == m2 then acc else
      match m1 with
      | Empty -> if m2 = Empty then acc else invalid_arg "Mapext.fold2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              let acc = fold2z f l1 l2 acc in
              let acc = if d1 == d2 then acc else f k d1 d2 acc in
              fold2z f r1 r2 acc
          | _, None, _ -> invalid_arg "Mapext.fold2z"
            
    let rec for_all2z f m1 m2 =
      (m1 == m2) ||
      (match m1 with
      | Empty -> if m2 = Empty then true else invalid_arg "Mapext.for_all2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              (for_all2z f l1 l2) && 
              (d1 == d2 || f k d1 d2) && 
              (for_all2z f r1 r2)
          | _, None, _ -> invalid_arg "Mapext.for_all2z"
      )     
            
    let rec exists2z f m1 m2 =
      (m1 != m2) &&
      (match m1 with
      | Empty -> if m2 = Empty then false else invalid_arg "Mapext.exists2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              (exists2z f l1 l2) ||
              (d1 != d2 && f k d1 d2) ||
              (exists2z f r1 r2)
          | _, None, _ -> invalid_arg "Mapext.exists2z"
      )     


    (* as above, but allow maps with different keys *)

    let rec map2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> mapi f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let l = map2o f1 f2 f l1 l2 in
          let d = match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2 in
          let r = map2o f1 f2 f r1 r2 in
          join l k d r
            
    let rec iter2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> iter f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          iter2o f1 f2 f l1 l2;
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2);
          iter2o f1 f2 f r1 r2
            
    let rec fold2o f1 f2 f m1 m2 acc =
      match m1 with
      | Empty -> fold f2 m2 acc
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let acc = fold2o f1 f2 f l1 l2 acc in
          let acc = match d2 with 
          | None -> f1 k d1 acc | Some d2 -> f k d1 d2 acc
          in
          fold2o f1 f2 f r1 r2 acc
            
    let rec for_all2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> for_all f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (for_all2o f1 f2 f l1 l2) &&
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2) &&
          (for_all2o f1 f2 f r1 r2)

    let rec exists2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> exists f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (exists2o f1 f2 f l1 l2) ||
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2) ||
          (exists2o f1 f2 f r1 r2)


    (* all together now *)

    let rec map2zo f1 f2 f m1 m2 =
      if m1 == m2 then m1 else
      match m1 with
      | Empty -> mapi f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let l = map2zo f1 f2 f l1 l2 in
          let d = match d2 with 
          | None -> f1 k d1 
          | Some d2 -> if d1 == d2 then d1 else f k d1 d2 
          in
          let r = map2zo f1 f2 f r1 r2 in
          join l k d r
            
    let rec iter2zo f1 f2 f m1 m2 =
      if m1 == m2 then () else
      match m1 with
      | Empty -> iter f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          iter2zo f1 f2 f l1 l2;
          (match d2 with 
          | None -> f1 k d1 
          | Some d2 -> if d1 != d2 then f k d1 d2);
          iter2zo f1 f2 f r1 r2
            
    let rec fold2zo f1 f2 f m1 m2 acc =
      if m1 == m2 then acc else
      match m1 with
      | Empty -> fold f2 m2 acc
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let acc = fold2zo f1 f2 f l1 l2 acc in
          let acc = match d2 with 
          | None -> f1 k d1 acc 
          | Some d2 -> if d1 == d2 then acc else f k d1 d2 acc
          in
          fold2zo f1 f2 f r1 r2 acc
            
    let rec for_all2zo f1 f2 f m1 m2 =
      (m1 == m2) ||
      (match m1 with
      | Empty -> for_all f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (for_all2zo f1 f2 f l1 l2) &&
          (match d2 with None -> f1 k d1 | Some d2 -> d1 == d2 || f k d1 d2) &&
          (for_all2zo f1 f2 f r1 r2)
      )

    let rec exists2zo f1 f2 f m1 m2 =
      (m1 != m2) &&
      (match m1 with
      | Empty -> exists f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (exists2zo f1 f2 f l1 l2) ||
          (match d2 with None -> f1 k d1 | Some d2 -> d1 != d2 && f k d1 d2) ||
          (exists2zo f1 f2 f r1 r2)
      )
    
    let equal cmp m1 m2 =
      try
        iter2zo
          (fun _ _ -> raise Exit)
          (fun _ _ -> raise Exit)
          (fun _ x y -> if not (cmp x y) then raise Exit)
          m1 m2;
        true
      with Exit -> false
      
    let compare cmp m1 m2 =
      let r = ref 0 in
      try
        iter2zo
          (fun _ _ -> r :=  1; raise Exit)
          (fun _ _ -> r := -1; raise Exit)
          (fun _ x y -> r := cmp x y; if !r <> 0 then raise Exit)
          m1 m2;
        !r
      with Exit -> !r
      
      

    (* iterators limited to keys between two bounds *)

    let rec map_slice f m lo hi =
      match m with
      | Empty -> Empty
      | Node (l,k,d,r,h) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          let l = if c1 > 0 then map_slice f l lo k else l in
          let d = if c1 >= 0 && c2 <= 0 then f k d else d in
          let r = if c2 < 0 then map_slice f r k hi else r in
          Node (l,k,d,r,h)

    let rec iter_slice f m lo hi =
      match m with
      | Empty -> ()
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          if c1 > 0 then iter_slice f l lo k;
          if c1 >= 0 && c2 <= 0 then f k d;
          if c2 < 0 then iter_slice f r k hi

    let rec fold_slice f m lo hi acc =
      match m with
      | Empty -> acc
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          let acc = if c1 > 0 then fold_slice f l lo k acc else acc in
          let acc = if c1 >= 0 && c2 <= 0 then f k d acc else acc in
          if c2 < 0 then fold_slice f r k hi acc else acc

    let rec for_all_slice f m lo hi =
      match m with
      | Empty -> true
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          (c1 <= 0 || for_all_slice f l lo k) &&
          (c1 < 0 || c2 > 0 || f k d) &&
          (c2 >= 0 || for_all_slice f r k hi)

    let rec exists_slice f m lo hi =
      match m with
      | Empty -> false
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          (c1 > 0 && exists_slice f l lo k) ||
          (c1 >= 0 && c2 <= 0 && f k d) ||
          (c2 < 0 && exists_slice f r k hi)


    (* key set comparison *)

    let rec key_equal m1 m2 =
      (m1 == m2) || 
      (match m1 with
      | Empty -> m2 = Empty
      | Node (l1, k, _, r1, _) ->
          match cut k m2 with
          | _, None, _ -> false
          | l2, Some _, r2 -> key_equal l1 l2 && key_equal r1 r2
      )

    let rec key_subset m1 m2 =
      (m1 == m2) || 
      (match m1 with
      | Empty -> true
      | Node (l1, k, _, r1, _) ->
          match cut k m2 with
          | _, None, _ -> false
          | l2, Some _, r2 -> key_subset l1 l2 && key_subset r1 r2
      )


    (* navigation *)

    let find_greater_equal k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c = 0 then kk, d else
          if c > 0 then aux r found else
          aux l (Some (kk, d))
      in
      aux m None 

    let find_greater k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c >= 0 then aux r found else
          aux l (Some (kk, d))
      in
      aux m None 

    let find_less_equal k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c = 0 then kk, d else
          if c < 0 then aux l found else
          aux r (Some (kk, d))
      in
      aux m None 

    let find_less k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c <= 0 then aux l found else
          aux r (Some (kk, d))
      in
      aux m None 


    (* printing *)

    type map_printer = {
        print_empty: string;
        print_begin: string;
        print_arrow: string;
        print_sep: string;
        print_end: string;
      }
                          
    let printer_default = {
        print_empty="{}";
        print_begin="{";
        print_arrow=":";
        print_sep=";";
        print_end="}";
      }
                        
    let print_gen o printer key elem ch s =
      if s = Empty then o ch printer.print_empty else (
        let first = ref true in
        o ch printer.print_begin;
        iter
          (fun k e ->
            if !first then first := false else o ch printer.print_sep;
            key ch k;
            o ch printer.print_arrow;
            elem ch e
          ) s;
        o ch printer.print_end
      )
    (* internal printing helper *)
           
    let print printer key elem ch l = print_gen output_string printer key elem ch l
    let bprint printer key elem ch l = print_gen Buffer.add_string printer key elem ch l
    let fprint printer key elem ch l = print_gen Format.pp_print_string printer key elem ch l
                                             
    let to_string printer key elem l =
      let b = Buffer.create 10 in
      print_gen (fun () s -> Buffer.add_string b s) printer
                (fun () k -> key k) (fun () e ->  elem e) () l;
      Buffer.contents b
                      
          
end: S with type key = Ord.t)
