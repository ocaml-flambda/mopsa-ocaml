(*
  This file is derived from the set.ml file from the OCaml distribution.
  Changes are marked with the [MOPSA] symbol.

  Modifications are Copyright (C) 2017-2019 The MOPSA Project.

  Original copyright follows.
*)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Sets with polymorphic values *)

(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)
type 'a set = Empty | Node of {l:'a set; v:'a; r:'a set; h:int}

type 'a compare = 'a -> 'a -> int

type 'a t = {
  set:     'a set;
  compare: 'a compare;
}

type set_printer = {
  print_empty: string; (** Special text for empty sets *)
  print_begin: string; (** Text before the first element *)
  print_sep: string;   (** Text between two elements *)
  print_end: string;   (** Text after the last element *)
}
let height = function
    Empty -> 0
  | Node {h} -> h

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node {h} -> h in
  let hr = match r with Empty -> 0 | Node {h} -> h in
  Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node {h} -> h in
  let hr = match r with Empty -> 0 | Node {h} -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node{l=ll; v=lv; r=lr} ->
      if height ll >= height lr then
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> invalid_arg "Set.bal"
        | Node{l=lrl; v=lrv; r=lrr}->
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node{l=rl; v=rv; r=rr} ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> invalid_arg "Set.bal"
        | Node{l=rll; v=rlv; r=rlr} ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

(* Insertion of one element *)

let rec add compare x = function
    Empty -> Node{l=Empty; v=x; r=Empty; h=1}
  | Node{l; v; r} as t ->
    let c = compare x v in
    if c = 0 then t else
    if c < 0 then
      let ll = add compare x l in
      if l == ll then t else bal ll v r
    else
      let rr = add compare x r in
      if r == rr then t else bal l v rr

let singleton x = Node{l=Empty; v=x; r=Empty; h=1}

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element x = function
  | Empty -> singleton x
  | Node {l; v; r} ->
    bal (add_min_element x l) v r

let rec add_max_element x = function
  | Empty -> singleton x
  | Node {l; v; r} ->
    bal l v (add_max_element x r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node{l=Empty; v} -> v
  | Node{l} -> min_elt l

let rec min_elt_opt = function
    Empty -> None
  | Node{l=Empty; v} -> Some v
  | Node{l} -> min_elt_opt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node{v; r=Empty} -> v
  | Node{r} -> max_elt r

let rec max_elt_opt = function
    Empty -> None
  | Node{v; r=Empty} -> Some v
  | Node{r} -> max_elt_opt r

(* Remove the smallest element of the given set *)

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node{l=Empty; r} -> r
  | Node{l; v; r} -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

(* Splitting.  split x s returns a triple (l, present, r) where
   - l is the set of elements of s that are < x
   - r is the set of elements of s that are > x
   - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split compare x = function
    Empty ->
    (Empty, false, Empty)
  | Node{l; v; r} ->
    let c = compare x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split compare x l in (ll, pres, join rl v r)
    else
      let (lr, pres, rr) = split compare x r in (join l v lr, pres, rr)

(* Implementation of the set operations *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec mem compare x = function
    Empty -> false
  | Node{l; v; r} ->
    let c = compare x v in
    c = 0 || mem compare x (if c < 0 then l else r)

let rec remove compare x = function
    Empty -> Empty
  | (Node{l; v; r} as t) ->
    let c = compare x v in
    if c = 0 then merge l r
    else
    if c < 0 then
      let ll = remove compare x l in
      if l == ll then t
      else bal ll v r
    else
      let rr = remove compare x r in
      if r == rr then t
      else bal l v rr

let rec union compare s1 s2 =
  if s1 == s2 then s1 (* [MOPSA] *) else
    match (s1, s2) with
      (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
      if h1 >= h2 then
        if h2 = 1 then add compare v2 s1 else begin
          let (l2, _, r2) = split compare v1 s2 in
          join (union compare l1 l2) v1 (union compare r1 r2)
        end
      else
      if h1 = 1 then add compare v1 s2 else begin
        let (l1, _, r1) = split compare v2 s1 in
        join (union compare l1 l2) v2 (union compare r1 r2)
      end

let rec inter compare s1 s2 =
  if s1 == s2 then s1 (* [MOPSA] *) else
    match (s1, s2) with
      (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Node{l=l1; v=v1; r=r1}, t2) ->
      match split compare v1 t2 with
        (l2, false, r2) ->
        concat (inter compare l1 l2) (inter compare r1 r2)
      | (l2, true, r2) ->
        join (inter compare l1 l2) v1 (inter compare r1 r2)

let rec diff compare s1 s2 =
  if s1 == s2 then Empty (* [MOPSA] *) else
    match (s1, s2) with
      (Empty, _) -> Empty
    | (t1, Empty) -> t1
    | (Node{l=l1; v=v1; r=r1}, t2) ->
      match split compare v1 t2 with
        (l2, false, r2) ->
        join (diff compare l1 l2) v1 (diff compare r1 r2)
      | (l2, true, r2) ->
        concat (diff compare l1 l2) (diff compare r1 r2)

type 'a enumeration = End | More of 'a * 'a set * 'a enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node{l; v; r} -> cons_enum l (More(v, r, e))

let rec iter f = function
    Empty -> ()
  | Node{l; v; r} -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Empty -> accu
  | Node{l; v; r} -> fold f r (f v (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node{l; v; r} -> p v && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node{l; v; r} -> p v || exists p l || exists p r

let rec filter p = function
    Empty -> Empty
  | (Node{l; v; r}) as t ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pv = p v in
    let r' = filter p r in
    if pv then
      if l==l' && r==r' then t else join l' v r'
    else concat l' r'

let rec partition p = function
    Empty -> (Empty, Empty)
  | Node{l; v; r} ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition p l in
    let pv = p v in
    let (rt, rf) = partition p r in
    if pv
    then (join lt v rt, concat lf rf)
    else (concat lt rt, join lf v rf)

let rec cardinal = function
    Empty -> 0
  | Node{l; r} -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node{l; v; r} -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let choose_opt = min_elt_opt

let rec find compare x = function
    Empty -> raise Not_found
  | Node{l; v; r} ->
    let c = compare x v in
    if c = 0 then v
    else find compare x (if c < 0 then l else r)

let rec find_first_aux v0 f = function
    Empty ->
    v0
  | Node{l; v; r} ->
    if f v then
      find_first_aux v f l
    else
      find_first_aux v0 f r

let rec find_first f = function
    Empty ->
    raise Not_found
  | Node{l; v; r} ->
    if f v then
      find_first_aux v f l
    else
      find_first f r

let rec find_first_opt_aux v0 f = function
    Empty ->
    Some v0
  | Node{l; v; r} ->
    if f v then
      find_first_opt_aux v f l
    else
      find_first_opt_aux v0 f r

let rec find_first_opt f = function
    Empty ->
    None
  | Node{l; v; r} ->
    if f v then
      find_first_opt_aux v f l
    else
      find_first_opt f r

let rec find_last_aux v0 f = function
    Empty ->
    v0
  | Node{l; v; r} ->
    if f v then
      find_last_aux v f r
    else
      find_last_aux v0 f l

let rec find_last f = function
    Empty ->
    raise Not_found
  | Node{l; v; r} ->
    if f v then
      find_last_aux v f r
    else
      find_last f l

let rec find_last_opt_aux v0 f = function
    Empty ->
    Some v0
  | Node{l; v; r} ->
    if f v then
      find_last_opt_aux v f r
    else
      find_last_opt_aux v0 f l

let rec find_last_opt f = function
    Empty ->
    None
  | Node{l; v; r} ->
    if f v then
      find_last_opt_aux v f r
    else
      find_last_opt f l

let rec find_opt compare x = function
    Empty -> None
  | Node{l; v; r} ->
    let c = compare x v in
    if c = 0 then Some v
    else find_opt compare x (if c < 0 then l else r)

let try_join compare l v r =
  (* [join l v r] can only be called when (elements of l < v <
     elements of r); use [try_join l v r] when this property may
     not hold, but you hope it does hold in the common case *)
  if (l = Empty || compare (max_elt l) v < 0)
  && (r = Empty || compare v (min_elt r) < 0)
  then join l v r
  else union compare l (add compare v r)

let rec map compare f = function
  | Empty -> Empty
  | Node{l; v; r} as t ->
    (* enforce left-to-right evaluation order *)
    let l' = map compare f l in
    let v' = f v in
    let r' = map compare f r in
    if l == l' && v == v' && r == r' then t
    else try_join compare l' v' r'

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
    | 2, x0 :: x1 :: l ->
      Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
    | 3, x0 :: x1 :: x2 :: l ->
      Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
           r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_list compare l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add compare x1 (singleton x0)
  | [x0; x1; x2] -> add compare x2 (add compare x1 (singleton x0))
  | [x0; x1; x2; x3] -> add compare x3 (add compare x2 (add compare x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add compare x4 (add compare x3 (add compare x2 (add compare x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare l)



(* [MOPSA] additions *)
(* ***************** *)


(* internal function *)
(* similar to split, but returns unbalanced trees *)
let rec cut compare k = function
    Empty -> Empty,false,Empty
  | Node {l=l1; v=k1; r=r1; h=h1; } ->
    let c = compare k k1 in
    if c < 0 then
      let l2,d2,r2 = cut compare k l1 in (l2,d2,Node {l=r2; v=k1; r=r1; h=h1})
    else if c > 0 then
      let l2,d2,r2 = cut compare k r1 in (Node {l=l1; v=k1; r=l2; h=h1;},d2,r2)
    else (l1,true,r1)


(* binary operations *)

let rec iter2 compare f1 f2 f s1 s2 =
  match s1 with
  | Empty -> iter f2 s2
  | Node { l=l1; r=r1; v=k; } ->
    let l2,t,r2 = cut compare k s2 in
    iter2 compare f1 f2 f l1 l2;
    if t then f k else f1 k;
    iter2 compare f1 f2 f r1 r2

let rec fold2 compare f1 f2 f s1 s2 acc =
  match s1 with
  | Empty -> fold f2 s2 acc
  | Node { l=l1; r=r1; v=k; } ->
    let l2,t,r2 = cut compare k s2 in
    let acc = fold2 compare f1 f2 f l1 l2 acc in
    let acc = if t then f k acc else f1 k acc in
    fold2 compare f1 f2 f r1 r2 acc

let rec for_all2 compare f1 f2 f s1 s2 =
  match s1 with
  | Empty -> for_all f2 s2
  | Node { l=l1; r=r1; v=k; } ->
    let l2,t,r2 = cut compare k s2 in
    (for_all2 compare f1 f2 f l1 l2) &&
    (if t then f k else f1 k) &&
    (for_all2 compare f1 f2 f r1 r2)

let rec exists2 compare f1 f2 f s1 s2 =
  match s1 with
  | Empty -> exists f2 s2
  | Node { l=l1; r=r1; v=k; } ->
    let l2,t,r2 = cut compare k s2 in
    (exists2 compare f1 f2 f l1 l2) ||
    (if t then f k else f1 k) ||
    (exists2 compare f1 f2 f r1 r2)


(* the _diff functions ignore elements present in both
   sets; they can thus skip physically equal subtrees,
   which improves efficiency when the two sets are similar
*)

let rec iter2_diff compare f1 f2 s1 s2 =
  if s1 == s2 then ()
  else
    match s1 with
    | Empty -> iter f2 s2
    | Node { l=l1; r=r1; v=k; } ->
      let l2,t,r2 = cut compare k s2 in
      iter2_diff compare f1 f2 l1 l2;
      if not t then f1 k;
      iter2_diff compare f1 f2 r1 r2

let rec fold2_diff compare f1 f2 s1 s2 acc =
  if s1 == s2 then acc
  else
    match s1 with
    | Empty -> fold f2 s2 acc
    | Node { l=l1; r=r1; v=k; } ->
      let l2,t,r2 = cut compare k s2 in
      let acc = fold2_diff compare f1 f2 l1 l2 acc in
      let acc = if t then acc else f1 k acc in
      fold2_diff compare f1 f2 r1 r2 acc

let rec for_all2_diff compare f1 f2 s1 s2 =
  if s1 == s2 then true
  else
    match s1 with
    | Empty -> for_all f2 s2
    | Node { l=l1; r=r1; v=k; } ->
      let l2,t,r2 = cut compare k s2 in
      (for_all2_diff compare f1 f2 l1 l2) &&
      (t || f1 k) &&
      (for_all2_diff compare f1 f2 r1 r2)

let rec exists2_diff compare f1 f2 s1 s2 =
  if s1 == s2 then true
  else
    match s1 with
    | Empty -> exists f2 s2
    | Node { l=l1; r=r1; v=k; } ->
      let l2,t,r2 = cut compare k s2 in
      (exists2_diff compare f1 f2 l1 l2) ||
      (f1 k) ||
      (exists2_diff compare f1 f2 r1 r2)

let diff_list compare s1 s2 =
  fold2_diff compare (fun x l -> x::l) (fun _ l -> l) s1 s2 []

let sym_diff_list compare s1 s2 =
  fold2_diff compare
    (fun x (l1,l2) -> x::l1, l2)
    (fun x (l1,l2) -> l1, x::l2)
    s1 s2 ([],[])

let add_sym_diff compare s2 (a,r) =
  List.fold_left
    (fun s x -> add compare x s)
    (List.fold_left (fun s x -> remove compare x s) s2 r)
    a


(* these versions are limited to elements between two bounds *)

let rec iter_slice compare f m lo hi =
  match m with
  | Empty -> ()
  | Node {l;v;r} ->
    let c1, c2 = compare v lo, compare v hi in
    if c1 > 0 then iter_slice compare f l lo hi;
    if c1 >= 0 && c2 <= 0 then f v;
    if c2 < 0 then iter_slice compare f r lo hi

let rec fold_slice compare f m lo hi acc =
  match m with
  | Empty -> acc
  | Node {l;v;r} ->
    let c1, c2 = compare v lo, compare v hi in
    let acc = if c1 > 0 then fold_slice compare f l lo hi acc else acc in
    let acc = if c1 >= 0 && c2 <= 0 then f v acc else acc in
    if c2 < 0 then fold_slice compare f r lo hi acc else acc

let rec for_all_slice compare f m lo hi =
  match m with
  | Empty -> true
  | Node {l;v;r} ->
    let c1, c2 = compare v lo, compare v hi in
    (c1 <= 0 || for_all_slice compare f l lo hi) &&
    (c1 < 0 || c2 > 0 || f v) &&
    (c2 >= 0 || for_all_slice compare f r lo hi)

let rec exists_slice compare f m lo hi =
  match m with
  | Empty -> false
  | Node {l;v;r} ->
    let c1, c2 = compare v lo, compare v hi in
    (c1 > 0 && exists_slice compare f l lo hi) ||
    (c1 >= 0 && c2 <= 0 && f v) ||
    (c2 < 0 && exists_slice compare f r lo hi)


(* new versions, optimised with _diff functions *)

let equal compare s1 s2 =
  for_all2_diff compare (fun _ -> false) (fun _ -> false) s1 s2

let subset compare s1 s2 =
  for_all2_diff compare (fun _ -> false) (fun _ -> true) s1 s2

let compare cmp s1 s2 =
  let r = ref 0 in
  try
    iter2_diff cmp
      (fun _ -> r :=  1; raise Exit)
      (fun _ -> r := -1; raise Exit)
      s1 s2;
    !r
  with Exit -> !r


(* printing *)

let print_gen o printer key ch s =
  if s = Empty then o ch printer.print_empty else (
    let first = ref true in
    o ch printer.print_begin;
    iter
      (fun k ->
         if !first then first := false else o ch printer.print_sep;
         key ch k
      ) s;
    o ch printer.print_end
  )
(* internal printing helper *)

let print printer key ch l =
  print_gen output_string printer key ch l

let bprint printer key ch l =
  print_gen Buffer.add_string printer key ch l

let fprint printer key ch l =
  print_gen
    (fun fmt s -> Format.fprintf fmt "%s@," s)
    printer key ch l

let to_string printer key l =
  let b = Buffer.create 10 in
  print_gen (fun () s -> Buffer.add_string b s) printer
    (fun () k ->  Buffer.add_string b (key k)) () l;
  Buffer.contents b




let printer_default = {
  print_empty="{}";
  print_begin="{";
  print_sep=",";
  print_end="}";
}
(** [MOPSA] Print as set: {elem1,...,elemn} *)


let empty compare = {set = empty; compare}
let is_empty s = is_empty s.set
let mem x s = mem s.compare x s.set
let add x s =
  let set = add s.compare x s.set in
  if set == s.set then s else {s with set}
let singleton compare x = {set = singleton x; compare}
let remove x s =
  let set = remove s.compare x s.set in
  if set == s.set then s else {s with set}
let union s1 s2 = {s1 with set = union s1.compare s1.set s2.set}
let inter s1 s2 = {s1 with set = inter s1.compare s1.set s2.set}
let diff s1 s2 = {s1 with set = diff s1.compare s1.set s2.set}
let compare s1 s2 = compare s1.compare s1.set s2.set
let equal s1 s2 = equal s1.compare s1.set s2.set
let subset s1 s2 = subset s1.compare s1.set s2.set
let iter f s = iter f s.set
let map f s = {s with set = map s.compare f s.set}
let fold f s x = fold f s.set x
let for_all f s = for_all f s.set
let exists f s = exists f s.set
let filter f s = {s with set = filter f s.set}
let partition f s =
  let set1,set2 = partition f s.set in
  {s with set=set1}, {s with set=set1}
let cardinal s = cardinal s.set
let elements s = elements s.set
let min_elt s = min_elt s.set
let min_elt_opt s = min_elt_opt s.set
let max_elt s = max_elt s.set
let max_elt_opt s = max_elt_opt s.set
let choose s = choose s.set
let choose_opt s = choose_opt s.set
let split x s =
  let set1,b,set2 = split s.compare x s.set in
  {s with set=set1},b,{s with set=set2}
let find x s = find s.compare x s.set
let find_opt x s = find_opt s.compare x s.set
let find_first f s = find_first f s.set
let find_first_opt f s = find_first_opt f s.set
let find_last f s = find_last f s.set
let find_last_opt f s = find_last_opt f s.set
let of_list compare l = {set=of_list compare l; compare}
