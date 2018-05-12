(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
    An alternate abstraction of pointers, based on access paths in struct / 
    union / arrays.
 *)


open Bot
open Top
open Framework.Flow
open Framework.Domains
open Framework.Domains.Stateful
open Framework.Manager
open Framework.Exceptions
open Framework.Ast
open Framework.Visitor
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Utils
open Framework.Query
open Universal.Ast
open Universal.Pp
open Ast
open Base
open Pointer
   

let name = "c.memory.access_path"
let debug fmt = Debug.debug ~channel:name fmt

let smash_threshold = ref (Z.of_int 15)
(** Smash arrays with length greater than or equal to this. *)

let print_type_in_ap = ref true
                    
              
(*==========================================================================*)
(**                       {2 Concrete types}                                *)
(*==========================================================================*)


type slice = (Z.t * Z.t) with_top
(** Type of array index ranges, useful for smashed arrays. 
    TOP is used for unknown bounds.
 *)

let zero_slice = Nt (Z.zero,Z.zero)
(** 0, as a slice. *)

let join_slice : slice -> slice -> slice =
  top_lift2 (fun (a,b) (c,d) -> min a c, max b d)
(** Join for slices. *)

let widen_slice : slice -> slice -> slice =
  top_absorb2
    (fun (a,b) (c,d) ->
      if a > c || b < d then TOP
      else Nt (a,b)
    )
(** Widening for slices. *)

let meet_slice : slice -> slice -> slice =
  top_neutral2
    (fun (a,b) (c,d) ->
      let x,y = max a c, min b d in
      if x > y then raise Found_BOT;
      x,y
    )
(** Meet for slices. Can raise Found_BOT. *)

let add_slice : slice -> slice -> slice =
    top_lift2 (fun (a,b) (c,d) -> Z.add a c, Z.add b d)
(** Addition for slices. *)

let sub_slice : slice -> slice -> slice =
    top_lift2 (fun (a,b) (c,d) -> Z.sub a d, Z.sub b c)
(** Subtraction for slices. *)

let leq_slice : slice -> slice -> bool =
  top_included (fun (a,b) (c,d) -> a >= c && b <= d)
(** Slice inclusion. *)
  
let contains_slice (v:Z.t) : slice -> bool =
  top_dfl1 true (fun (l,h) -> l <= v && v <= h)
(** Wheter a value is contained in a slice. *)
                                          
let pp_slice : Format.formatter -> slice -> unit =
  top_fprint
    (fun fmt (a,b) -> Format.fprintf fmt "%a-%a" Z.pp_print a Z.pp_print b)

  
type sel =
  | E_s_array_index of typ * Z.t
  | E_s_array_slice of typ * slice
  | E_s_field of c_record_type * int
(** A selector is an element in an access path:
    - an array access, with element type and element (constant index or range)
    - or a field access, with type
 *)

type ap = base * sel list * typ
(** An access path is a base, followed by a list of selectors.
    typ is the type of the object at the end of the path.
 *)
        
type apexpr =
  | E_ap_ap of ap        (** Specific access path from base *)
  | E_ap_base of base    (** Unknown location within base *)
  | E_ap_fun of c_fundec (** Function pointer *)
  | E_ap_null            (** NULL *)
  | E_ap_invalid         (** Invalid *)
(** Resolved pointer expression. *)
                     
let pp_sel fmt : sel -> unit = function
  | E_s_array_index (_,i) ->
     Format.fprintf fmt "[%a]" Z.pp_print i
  | E_s_array_slice (_, Nt (i,j)) ->
     Format.fprintf fmt "[%a,%a]" Z.pp_print i Z.pp_print j
  | E_s_array_slice (_, TOP) ->
     Format.fprintf fmt "[*]"
  | E_s_field (f,i) ->
     Format.fprintf fmt ".%s" (List.nth f.c_record_fields i).c_field_name

let pp_sel_list fmt : sel list -> unit =
  List.iter (pp_sel fmt)
    
let pp_ap fmt ((b,s,t):ap) : unit =
  if !print_type_in_ap
  then Format.fprintf fmt "%a%a(%a)" pp_base b pp_sel_list s pp_typ t
  else  Format.fprintf fmt "%a%a" pp_base b pp_sel_list s

let string_of_ap (ap:ap) : string =
  pp_ap Format.str_formatter ap;
  Format.flush_str_formatter ()

let pp_apexpr fmt : apexpr -> unit = function
  | E_ap_ap ap -> pp_ap fmt ap
  | E_ap_base b ->  Format.fprintf fmt "%a.?" pp_base b
  | E_ap_null -> Format.pp_print_string fmt "NULL"
  | E_ap_invalid -> Format.pp_print_string fmt "Invalid"
  | E_ap_fun f -> Format.fprintf fmt "<fp %a>" pp_var f.c_func_var

                
(** Printers. *)


let compare_sel (s1:sel) (s2:sel) : int =
  match s1,s2 with
  | E_s_array_index (t1,i1), E_s_array_index (t2,i2) ->
     compare_composer [
         (fun () -> compare i1 i2);
         (fun () -> compare_typ t1 t2);
       ]
  | E_s_array_slice (t1,s1), E_s_array_slice (t2,s2) ->
     compare_composer [
         (fun () -> compare s1 s2);
         (fun () -> compare_typ t1 t2);
       ]
  | E_s_field (r1,i1), E_s_field (r2,i2) ->
     compare_composer [
         (fun () -> compare i1 i2);
         (fun () -> compare_typ (T_c_record r1) (T_c_record r2));
       ]
  | _ -> compare s1 s2
       
let compare_ap ((b1,s1,t1):ap) ((b2,s2,t2):ap) : int =
  compare_composer [
      (fun () -> compare_base b1 b2);
      (fun () -> compare_list compare_sel s1 s2);
      (fun () -> compare_typ t1 t2)
    ]

(** Comparators. *)


let sel_is_weak : sel list -> bool =
  List.exists
    (function
     | E_s_array_index _ -> false                          
     | E_s_array_slice (_, Nt (l,h)) -> l < h
     | E_s_array_slice (_, TOP) -> true
     | E_s_field _ -> false
    )
(** Whether the selector sequence has at least one smashed array. *)
  
                
(*==========================================================================*)
(**                       {2 Selector sequences}                            *)
(*==========================================================================*)

              
module Sel = struct

  (**
     Trees representing sets of sequences of selectors
     (i.e., access paths without a base).
     Each node is either a set of struct/union field selectors,
     or a set of array selectors, with associated subtrees.
     Each node is typed.
   *)


  (*============*)
  (** {3 Types} *)
  (*============*)
  
  
  module FieldMap = MapExt.Make(
    struct
      type t = c_record_field
      let compare a b = compare a.c_field_index b.c_field_index
    end)
  (** Maps with struct/union fields as keys *)

  type 'sel record_sel = 'sel FieldMap.t
  (** Record selector: each field can yield a set of access paths 
      The map must be non-empty.
   *)

  module ZMap = MapExt.ZMap
  (** Array index map (must be non-empty). *)
                       
  type 'sel array_sel =
    | A_extended of 'sel ZMap.t
    | A_smashed of slice * 'sel
  (** Array selector: 
      - either a set of access paths per index (extended)
      - or a global a single set of access paths (smashed)
   *)

  type t =
    | S_struct of
        c_record_type  (** record type *)
        * t record_sel (** selectors *)
    | S_array of
        typ              (** array element type *)
        * c_array_length (** array length *)
        * t array_sel    (** selectors *)
    | S_end of typ (** path terminator (= array leaf) *)
  (**
     Node of a selector tree.
     We keep type information along the path, including at its end.
     The type of a tree reduced to a S_end can be void, to denote an
     undetermined type.
   *)



  (*================*)
  (** {3 Utilities} *)
  (*================*)


  let equal_typ t1 t2 =
    compare_typ t1 t2 = 0

  let equal_record r1 r2 =
    equal_typ (T_c_record r1) (T_c_record r2)

  let equal_or_void_typ t1 t2 =
    t1 = T_c_void || t2 = T_c_void || equal_typ t1 t2

  let best_typ t1 t2 =
    if t2 = T_c_void then t1 else t2
  (* We use void type to denoted an access path with no type information yet.
     We replace void with non-void when information becomes available
     (e.g., at the first field or array selector).
   *)
                         


  (*===============*)
  (** {3 Printing} *)
  (*===============*)

    
  let printer =
    MapExtSig.
    { print_empty = bot_string;
      print_begin = "(";
      print_arrow = "";
      print_sep   = "∨";
      print_end   = ")";
    }

  let rec print fmt (a:t) =
    match a with
    | S_struct (_,s) ->
       if FieldMap.cardinal s = 1 then
         let f,aa = FieldMap.min_binding s  in
         Format.fprintf fmt ".%s%a" f.c_field_name print aa
       else
         Format.fprintf
           fmt ".%a"
           (FieldMap.fprint     
              printer
              (fun fmt f -> Format.fprintf fmt "%s" f.c_field_name ) print
           )
           s
      
    | S_array (_,_, A_extended a) ->
       if ZMap.cardinal a = 1 then
         let idx, aa = ZMap.min_binding a in
         Format.fprintf fmt "[%a]%a" Z.pp_print idx print aa
       else
         ZMap.fprint
           printer
           (fun fmt idx -> Format.fprintf fmt "[%a]" Z.pp_print idx) print
           fmt a
      
    | S_array (_,_, A_smashed (TOP,a)) ->
         Format.fprintf fmt "[*]%a" print a

    | S_array (_,_, A_smashed (Nt(l,h),a)) ->
         Format.fprintf fmt "[%a-%a]%a" Z.pp_print l Z.pp_print h print a

    | S_end t ->
       if !print_type_in_ap then Format.fprintf fmt "(%a)" pp_typ t

             
    
  (*=================*)
  (** {3 Operations} *)
  (*=================*)

                         
  let rec join_widen (widen:bool) (a:t) (b:t) : t =
    if a==b then a else
      match a,b with
      | S_struct (t1,s1), S_struct (t2,s2) when equal_record t1 t2 ->
         (* field-wise join *)
         let s =
           FieldMap.map2zo
             (fun _ x -> x)
             (fun _ x -> x)
             (fun _ x y -> join_widen widen x y)
             s1 s2
         in
         S_struct (t1, s)
         
      | S_array (t1,l1,s1), S_array (t2,l2,s2) when equal_typ t1 t2 ->
         let s = match s1, s2 with
           | A_extended a1, A_extended a2 ->
              (* index-wise join *)
              let a =
                ZMap.map2zo
                  (fun _ x -> x)
                  (fun _ x -> x)
                  (fun _ x y -> join_widen widen x y)
                  a1 a2
              in
              if widen && ZMap.cardinal a > ZMap.cardinal a1
              then
                (* widening: if the set of indices grows, then smash *)
                let s,a = smash_array widen (A_extended a) in
                A_smashed (s,a)
              else
                A_extended a

           | _ ->
              (* if a least one array is smashed, the result is smashed *)
              let ss1,aa1 = smash_array widen s1 in
              let ss2,aa2 = smash_array widen s2 in
              let ss = (if widen then widen_slice else join_slice) ss1 ss2
              and aa = join_widen widen aa2 aa2 in
              A_smashed (ss, aa)

         in
         S_array (t1,l1,s)
         
          
      | S_end t1, S_end t2 when equal_or_void_typ t1 t2 ->
         (* normal end *)
         S_end (best_typ t1 t2)
        
      | _ ->
         (* incompatible types *)
         raise Found_TOP           
  (** Join and widening. 
      If [widen=true], arrays with increasing set of indices are smashed.
      May raise Found_TOP (type incompatibility). 
   *)

  and smash_array (widen:bool) : t array_sel -> slice * t = function
    | A_smashed (s,v) -> s, v
    | A_extended a ->
       (* join all indices *)
       let v = 
         ZMap.fold
           (fun _ m acc -> join_widen widen m acc)
           a (snd (ZMap.min_binding a))
       and l = fst (ZMap.min_binding a)
       and h = fst (ZMap.max_binding a)
       in
       Nt (l,h), v
  (** Ensure that an array selector is smashed (not recursively).
      May raise Found_TOP (type incompatibility). 
   *)

      
  let join = join_widen false
           

  let rec meet (a:t) (b:t) : t =
    if a==b then a else
      match a,b with
      | S_struct (t1,s1), S_struct (t2,s2) when equal_record t1 t2 ->
         (* field-wise meet *)
         let s =
           (* start from s1 and modify until it becomes the meet of s1 and s2 *)
           FieldMap.fold2zo
             (fun k x r -> FieldMap.remove k r) (* only in s1 -> remove from s1 *)
             (fun k x r -> r) (* only in s2, no need to remove from s1 *)
             (fun k x y r ->
               (* meet of sub-trees, remove if that meet is mot*)
               try FieldMap.add k (meet x y) r
               with Found_BOT -> FieldMap.remove k r
             )
             s1 s2 s1
         in
         (* we shall never have nodes with empty subtree maps -> propagate bot *)
         if FieldMap.is_empty s then raise Found_BOT;
         S_struct (t1,s)
         
      | S_array (t1,l1,s1), S_array (t2,l2,s2) when equal_typ t1 t2 ->
         let s = match s1, s2 with
           | A_extended a1, A_extended a2 ->
              (* index-wise meet, similar to the field-wise meet *)
              let a =
                ZMap.fold2zo
                  (fun k x r -> ZMap.remove k r)
                  (fun k x r -> r)
                  (fun k x y r ->
                    try ZMap.add k (meet x y) r
                    with Found_BOT -> ZMap.remove k r
                  )
                  a1 a2 a1
              in
              if ZMap.is_empty a then raise Found_BOT;
              A_extended a

           | A_extended a, A_smashed (s2,a2)
           | A_smashed (s2,a2), A_extended a ->
              (* meet each index of the extended array with the smashed one 
                 and return an extended array
               *)
              let aa =
                ZMap.fold
                  (fun k a1 r ->
                    try
                      if contains_slice k s2
                      then ZMap.add k (meet a1 a2) r
                      else r
                    with Found_BOT -> r
                  )
                  a ZMap.empty
              in
              if ZMap.is_empty aa then raise Found_BOT;
              A_extended aa
              
           | A_smashed (s1,a1), A_smashed (s2,a2) ->
              A_smashed (meet_slice s1 s2, meet a1 a2)

         in
         S_array (t1,l1,s)
          
      | S_end t1, S_end t2 when equal_or_void_typ t1 t2 ->
         (* normal end *)
         S_end (best_typ t1 t2)

      | _ ->
         (* incompatible types, return the first argument by default *)
         a
  (** Meet. 
      May raise Found_BOT. 
   *)

        
  let rec leq (a:t) (b:t) : bool =
    (a==b) ||
      match a,b with
      | S_struct (t1,s1), S_struct (t2,s2) when equal_record t1 t2 ->
         (* point-wise inclusion *)
         FieldMap.for_all2zo
           (fun _ x -> false) (* s1 non-empty, s2 empty -> not included *)
           (fun _ x -> true)  (* s1 empty, s2 non-empty -> included *)
           (fun _ x y -> leq x y)
           s1 s2
        
      | S_array (t1,l1,s1), S_array (t2,l2,s2) when equal_typ t1 t2 ->
         (match s1, s2 with
          | A_extended a1, A_extended a2 ->
             (* point-wise inclusion, similar to the field case *)
             ZMap.for_all2zo
               (fun _ x -> false)
               (fun _ x -> true)
               (fun _ x y -> leq x y)
               a1 a2
            
          | A_extended a, A_smashed (s2,a2) ->
             (* evey index in a1 is included in a *)
             ZMap.for_all
               (fun k a1 -> contains_slice k s2 && leq a1 a2)
               a

          | A_smashed _, A_extended _ ->
             (* could be refined by checking that the A_extended contains
                all possible indices in slice
              *)
             false

          | A_smashed (s1,a1), A_smashed (s2,a2) ->
             leq_slice s1 s2 && leq a1 a2
         )
          
      | S_end t1, S_end t2 when equal_or_void_typ t1 t2 ->
         (* normal end *)
         true

      | _ ->
         (* incompatible types *)
         false
  (** Inclusion. *)



  let append_field (rt:c_record_type) (idx:int) (a:t) : t =
    let rec doit = function
      | S_struct (t,s) ->
         S_struct (t, FieldMap.map doit s)

      | S_array (t,l, A_extended a) ->
         S_array (t, l, A_extended (ZMap.map doit a))

      | S_array (t,l, A_smashed (s,a)) ->
         S_array (t, l, A_smashed (s,doit a))

      | S_end t ->
         (* test type compatibility at end of path *)
         if t <> T_c_void && not (equal_typ t (T_c_record rt)) then raise Found_TOP;
         (* add a field selection and update the type at the end of the path *)
         let f = List.nth rt.c_record_fields idx in
         S_struct (rt, FieldMap.singleton f (S_end f.c_field_type))
    in
    doit a
  (** Try to add a struct/union field selection at the end of each path.
      Raise Found_TOP in case of the type at the end of some path does not
      match the field selection.

      TODO: path equivalence for unions
   *)

  let append_array (elem:typ) (idx:slice) (a:t) : t =
    let rec doit = function
      | S_struct (t,s) ->
         S_struct (t, FieldMap.map doit s)
        
      | S_array (t,l, A_extended a) ->
         S_array (t, l, A_extended (ZMap.map doit a))

      | S_array (t,l, A_smashed (s,a)) ->
         S_array (t, l, A_smashed (s,doit a))
        
      | S_end t ->
         let r = match idx with
           | TOP -> A_smashed (TOP, S_end elem)
           | Nt (l,h) when l=h -> A_extended (ZMap.singleton l (S_end elem))
           | Nt (l,h) -> A_smashed (Nt (l,h),S_end elem)
         in
         match remove_typedef t with
         | T_c_array (et,l) when equal_typ et elem ->
            S_array (elem, l, r)
         | T_c_void ->
            S_array (elem, C_array_no_length, r)
         | _ ->
            (* incompatible type at end of path *)
            raise Found_TOP
    in
    doit a
  (** Try to add an array element selection at the end of each path.
      [elem] is the type of array elements.
      Raise Found_TOP in case of the type at the end of some path is not 
      an array of [elem].
      [idx] is the concrete index or smash slice.
      We don't perform array bound-check.
   *)

  let ptr_add (elem:typ) (sub:bool) (v:slice) (a:t) : t =
    debug "ptr_add %a / %a %s %a"
          print a pp_typ elem (if sub then "-" else "+") pp_slice v;
    let rec doit = function
      | S_struct (t,s) ->
         S_struct (t, FieldMap.map doit s)
        
      | S_end _ ->
         (* we should stop at an array selector before the end of a path *)
         raise Found_TOP

      | S_array (t,l, A_extended a) when equal_typ t elem || t=T_c_void ->
         (* we found the array selector, reconstruct the map *)
         (match v with
          | Nt (i,j) when i=j ->
             (* result is a singleton index *)
             let aa = 
               ZMap.fold
                 (fun vv a acc ->
                   match a with
                   | S_end _ ->
                      (* OK, replace the selector *)
                      let vv = (if sub then Z.sub else Z.add) vv i in
                      ZMap.add vv a acc
                   | _ ->
                      (* error, we should be at the end of the path *)
                      raise Found_TOP
                 )
                 a ZMap.empty
             in
             S_array (elem,l, A_extended aa)
          | _ ->
             (* result is a smashed slice *)
             let s,a = smash_array false (A_extended a) in
             let ss = (if sub then sub_slice else add_slice) s v in
             S_array (elem,l, A_smashed (ss, a))
         )
      | S_array (t,l, A_extended a) ->
         (* continue traversing the paths *)
         S_array (t, l, A_extended (ZMap.map doit a))

      | S_array (t,l, A_smashed (s,S_end e)) when equal_typ t elem || t=T_c_void ->
         (* update slice *)
         let ss = (if sub then sub_slice else add_slice) s v in
         S_array (t,l, A_smashed (ss,S_end e)) 

      | S_array (t,l, A_smashed (s,a)) ->
         (* continue traversing the paths *)
         S_array (t, l, A_smashed (s, doit a))
    in
    let a =
      if a = S_end T_c_void then
        (* special case for pointer to a malloced block *)
        S_array (elem, C_array_no_length, A_smashed (TOP, S_end elem))
      else a
    in
    doit a 
  (** Pointer arithmetic on array of type [elem]: 
      adds or subtracts [v] to the array selector at the end of each path.
      Adding a non-singleton value results in a smash array selector.
      Raise Found_TOP if some path does not end with an array element 
      selector to an array of type [elem].
      We don't perform any array bound-check.
   *)



  (*===================*)
  (** {3 Constructors} *)
  (*===================*)


  let base (t:typ) : t =
    S_end t
  (** Selector list reduced to a path terminator.*)
    
  let of_sel (t:typ) (s:sel list) : t =
    List.fold_left
      (fun ap s ->
        match s with
        | E_s_array_index (t,i) -> append_array t (Nt (i,i)) ap
        | E_s_array_slice (t,s) -> append_array t s ap
        | E_s_field (r,i) -> append_field r i ap
      )
      (base t) s
  (** Concrete access path type to pointer. *)

  let enumerate (a:t) : (sel list * typ) list =
    let rec doit all cur = function
      | S_end t ->
         (List.rev cur, t)::all (* add a new path *)
      | S_struct (rc,s) ->
         FieldMap.fold
           (fun t r all -> doit all ((E_s_field (rc,t.c_field_index))::cur) r)
           s all
      | S_array (t,_,A_extended a) ->
         ZMap.fold
           (fun i r all -> doit all ((E_s_array_index (t,i))::cur) r)
           a all
      | S_array (t,_,A_smashed (Nt (i,j),a)) when i=j ->
         doit all ((E_s_array_index (t,i))::cur) a
      | S_array (t,_,A_smashed (s,a)) ->
         doit all ((E_s_array_slice (t,s))::cur) a
    in
    doit [] [] a
  (** Unfold the tree into a set of selector sequences. *)


  let out_of_bound (l:c_array_length) (s:sel) : bool * bool =
    let may,must = match l,s with
      | C_array_length_cst len, E_s_array_index (_,i) ->
         let x = i < Z.zero || i >= len in
         x, x
      | C_array_length_cst len, E_s_array_slice (_,Nt (l,h)) ->
         l < Z.zero || h >= len, (* may fail *)
         h < Z.zero || l >= len  (* must fail *)
      | _ ->
         true, false
    in
    if may then debug "XXX out of bound %a %a %B XXX" pp_typ (T_c_array (T_c_void,l)) pp_sel s must;
    may, must
  (** Utility function to test whether an array access can be out of bound. 
      Returns both a "may fail" and a "must fail" value.
   *)




  let add_sel (aa:t) (ss:sel list) : t * sel list list * bool =
    debug "add_sel %a, %a" print aa pp_sel_list ss;
    let rec doit a s err = match a,s with
      | _, [] ->
         a, [s], err
         
      | S_struct (rc,s),
        (E_s_field (rc',i) as sel::tail)
           when equal_record rc rc' ->
         (* add in existing record selector node *)
         let f = List.nth rc'.c_record_fields i in
         let ss =
           try FieldMap.find f s
           with Not_found -> S_end f.c_field_type
         in
         let ss, tails, err = doit ss tail err in
         S_struct (rc, FieldMap.add f ss s),
         List.map (fun l -> sel::l) tails,
         err

      | S_array (t,l,A_smashed (TOP,s)),
        (E_s_array_index (t',_) | E_s_array_slice (t',_) as sel)::tail
           when equal_typ t t' ->
         (* add in existing smashed array node *)
         let s, tails, err = doit s tail err in
         let may,must = out_of_bound l sel in
         S_array (t,l,A_smashed (TOP,s)),
         (if must then [] else List.map (fun l -> (E_s_array_slice (t',TOP))::l) tails),
         err || may

      | S_array (t,l,A_extended s),
        (E_s_array_index (t',i) as sel::tail)
           when equal_typ t t' ->
         (* add in existing extended array node *)
         let ss =
           try ZMap.find i s
           with Not_found -> S_end t
         in
         let ss, tails, err = doit ss tail err in
         let may,must = out_of_bound l sel in
         S_array (t,l,A_extended (ZMap.add i ss s)),
         (if must then [] else List.map (fun l -> sel::l) tails),
         err || may

      | S_array (t,l,A_extended s),
        (E_s_array_slice (t',s') as sel)::tail ->
         let may,must = out_of_bound l sel in
         let len = match l with
           | C_array_length_cst l -> l
           | _ -> invalid_arg "non-constant array length in add_sel"
         in
         (* call the extended array case for every index in slice *)
         let lo,hi = match s' with
           | TOP -> Z.zero, len
           | Nt (a,b) -> max Z.zero a, min b (Z.pred len)
         in
         let rec doit2 a i acc err =
           if i >= hi then a,acc,err
           else
             let a,r,err = doit a (E_s_array_index (t',i)::tail) err in
             doit2 a (Z.succ i) (List.rev_append r acc) err
         in
         if must then a,[],true
         else doit2 a lo [] (err || may)

      | S_end t, (E_s_field (rc',i)::tail) ->
         (* at end of tree, add a field selector *)
         (match remove_typedef t with
          | T_c_record rc when equal_record rc rc' ->
             doit (S_struct (rc,FieldMap.empty)) s err
          | _ ->
             debug "add_sel %a, %a failed #1 at %a, %a"
                   print aa pp_sel_list ss print a pp_sel_list s;
             raise Not_found
         )

      | S_end t,
        ((E_s_array_index (t',_) | E_s_array_slice (t',_)) as sel::tail) ->
         (* at end of tree, add an array selector *)
         (match remove_typedef t with
          | T_c_array (t'', (C_array_length_cst len as l))
               when equal_or_void_typ t' t'' && len < !smash_threshold ->
             (* append non-smashed array selectors *)
             let r = A_extended ZMap.empty in
             doit (S_array (t', l, r)) s err

          | T_c_array (t'', l) when equal_or_void_typ t' t'' ->
             (* append a smashed array selector *)
             let s, tails, err = doit (S_end t') tail err in
             let may,must = out_of_bound l sel in
             S_array (t',l,A_smashed (TOP,s)),
             (if must then [] else List.map (fun l -> (E_s_array_slice (t',TOP))::l) tails),
             err || may

          | T_c_void ->
             (* append an array selector to a malloced block viewed as an array *)
             let s, tails, err = doit (S_end t') tail err in
             S_array (t',C_array_no_length,A_smashed (TOP,s)),
             List.map (fun l -> (E_s_array_slice (t',TOP))::l) tails,
             true

          | _ ->
             debug "add_sel %a, %a failed #2 at %a/%a, %a/%a"
                   print aa pp_sel_list ss print a pp_typ t pp_sel_list s pp_typ t';
             raise Not_found
         )

      | _ ->
         debug "add_sel %a, %a failed #3 at %a, %a"
               print aa pp_sel_list ss print a pp_sel_list s;
         raise Not_found
         
    in
    doit aa ss false
  (** Add an access path [ss] to a set of selector sequence [aa]. 
      Returns a boolean to denote out-of-bound array access.
      Raises Not_found if the path cannot be added due to type 
      incompatibilities.
      [ss] must originate from a memory block, and so, we assume
      that smashed arrays have TOP as slice (not a concrete interval).
      By contrast, [aa] comes from an pointer abstract value, and can
      have arbitrary slices.
      The path actually added may differ from the specified path 
      to ensure that array smashing matches the selector sequence set
      and the slice is always TOP.
      As a conseqence, the path(s) actually added is also returned, and
      its smashed slices are always TOP.

      NOTE: this function takes care of choosing whether to add smashed 
      or non-smashed array selectors.
   *)

    
  let remove_sel (aa:t) (ss:sel list) : t with_bot =
    let rec doit a s = match a,s with
      | _, [] ->
         BOT
         
      | S_struct (rc,s), (E_s_field (rc',i)::tail)
           when equal_record rc rc' ->
         let f = List.nth rc'.c_record_fields i in
         if FieldMap.mem f s then
           match doit (FieldMap.find f s) tail with
           | Nb ss ->
              Nb (S_struct (rc, FieldMap.add f ss s))
           | BOT ->
              let ss = FieldMap.remove f s in
              if FieldMap.is_empty ss then BOT
              else Nb (S_struct (rc, ss))
         else Nb a

      | S_array (t,l,A_smashed (TOP,s)), (E_s_array_slice (t',TOP))::tail
           when equal_typ t t' ->
         doit s tail  |>
         bot_lift1 (fun x -> S_array (t,l,A_smashed (TOP,s)))
         
      | S_array (t,l,A_extended s), (E_s_array_index (t',i)::tail)
           when equal_typ t t' ->
         if ZMap.mem i s then
           match doit (ZMap.find i s) tail with
           | Nb ss ->
              Nb (S_array (t,l,A_extended (ZMap.add i ss s)))
           | BOT ->
              let ss = ZMap.remove i s in
              if ZMap.is_empty ss then BOT
              else Nb (S_array (t,l,A_extended ss))
         else Nb a

      | _ ->
         debug "remove_sel %a, %a failed #3 at %a, %a"
               print aa pp_sel_list ss print a pp_sel_list s;
         raise Not_found
         
    in
    doit aa ss
  (** Remove an access path [ss] from a set of selector sequences [aa].
      Raises Not_found if the path cannot be removed due to type 
      or structure incompatibilities.

      NOTE: [ss] and [aa] represent memory blocks and cells, so,
      smashed array paths must are assumed to have TOP as slice,
      not a concrete interval.
  *)

    
  let contains_sel (a:t) (s:sel list) : bool =
    let rec doit a s = match a,s with
      | _, [] -> true
         
      | S_struct (rc,st), (E_s_field (rc',i)::tail)
           when equal_record rc rc' ->
         let f = List.nth rc'.c_record_fields i in
         FieldMap.mem f st && doit (FieldMap.find f st) tail

      | S_array (t,l,A_smashed (s,r)), (E_s_array_slice (t',s'))::tail
           when equal_typ t t' && s=s' ->
         doit r tail

      | S_array (t,l,A_extended s), (E_s_array_index (t',i)::tail)
           when equal_typ t t' ->
         ZMap.mem i s && doit (ZMap.find i s) tail

      | _ -> false
         
    in
    doit a s
  (** Whether a set of selector sequences contains a selector sequence. *)


end


           
          
(*==========================================================================*)
(**                          {2 Access paths}                               *)
(*==========================================================================*)


module AP = struct

  (**
     A set of access paths is represented as a set of bases with
     associated sets of selector sequences.
     We add a TOP element to represent access patterns that do not
     match the variable type.
   *)

  
  (*============*)
  (** {3 Types} *)
  (*============*)


  module B = struct
    type t = base
    let compare = compare_base
    let print = pp_base
  end
  (** A base (variable or address). *)
  
  module T = MapExt.Make(B)

  type t = Sel.t with_top T.t
  (** One access tree per base. *)


         
  (*===============*)
  (** {3 Printing} *)
  (*===============*)


  let print : Format.formatter -> t -> unit =
    T.fprint Sel.printer B.print (top_fprint Sel.print)
    

    
  (*=================*)
  (** {3 Operations} *)
  (*=================*)
        

  let join_widen (widen:bool) (a:t) (b:t) : t =
    (* point-wise, for each base *)
    T.map2zo
      (fun _ x -> x)
      (fun _ x -> x)
      (fun _ x y -> top_absorb2 (fun a b -> retop (Sel.join_widen widen a) b) x y)
      a b
  (** Join and widening. *)

  let join = join_widen false


  let meet (a:t) (b:t) : t =
    (* point-wise, for each base *)
    T.fold2zo
      (fun k x r -> T.remove k r) (* k in a=r but not b -> remove from a *)
      (fun k x r -> r) (* k in b but not a=r, so, remove from r *)
      (fun k x y r ->
        try T.add k (top_neutral2 Sel.meet x y) r
        with Found_BOT -> T.remove k r
      )
      a b a
  (** Meet. *)


  let leq (a:t) (b:t) : bool =
    (* point-wise inclusion *)
    T.for_all2zo
      (fun _ _ -> false) (* only in a, prevents inclusion *)
      (fun _ _ -> true)  (* only in b, OK *)
      (fun _ x y -> top_included Sel.leq x y)
      a b

  let empty = T.empty

  let is_empty = T.is_empty

  let append_field (rt:c_record_type) (idx:int) (a:t) : t =
    T.map (top_absorb1 (retop (Sel.append_field rt idx))) a
  (** Add a struct/union field selection at the end of each path. *)

  let append_array (elem:typ) (idx:slice) (a:t) : t =
    T.map (top_absorb1 (retop (Sel.append_array elem idx))) a
  (** Add an array element selection at the end of each path. *)

  let ptr_add (elem:typ) (sub:bool) (v:slice) (a:t) : t =
    T.map (top_absorb1 (retop (Sel.ptr_add elem sub v))) a
  (** Pointer arithmetic on array of type [elem]: adds [v] to the
      array selector at the end of each path.
   *)

    
  (*===================*)
  (** {3 Constructors} *)
  (*===================*)


  let type_of_base : base -> typ = function
    | V v -> v.vtyp
    | A _ -> T_c_void (* no type yet *)
           
           
  let base (b:base) : t =
    T.singleton b (Nt (Sel.base (type_of_base b)))
  (** An access path corresponding to a base. *)

  let of_sel (b:base) (s:sel list) : t =
    T.singleton b (Nt (Sel.of_sel (type_of_base b) s))
  (** Concrete access path type to pointer. *)
    

  let enumerate (a:t) : ap list * base list =
    T.fold
      (fun b x (acc_ap,acc_top) ->
        match x with
        | TOP -> acc_ap, b::acc_top
        | Nt e ->
           List.fold_left
             (fun acc (s,t) -> (b,s,t)::acc)
             acc_ap (Sel.enumerate e),
           acc_top
      )
      a ([],[])
  (** Returns both a list of precise access paths, and a list of
      unknown accesses within bases.
   *)

    
end


          
(*==========================================================================*)
(**                       {2 Pointer lattice}                               *)
(*==========================================================================*)


module Ptr = struct

  (**
     Pointers can be access paths (pointers to valid data), function pointers
     or special pointers (invalid, NULL).
   *)


  
  (*============*)
  (** {3 Types} *)
  (*============*)

  
  module Fun = struct
    type t = c_fundec
    let compare a b = compare a.c_func_var b.c_func_var
    let print fmt f = Format.fprintf fmt "%a" pp_var f.c_func_var
  end

  module FunSet = SetExt.Make(Fun)
  
  
  type ptr =
    { invalid: bool; (** can be invalid *)
      null: bool;    (** can be NULL *)
      data: AP.t;    (** pointers to actual data *)
      fn: FunSet.t;  (** function pointers *)
    }

  type t = ptr Top.with_top


  (*===============*)         
  (** {3 Printing} *)
  (*===============*)


  let ptr_print (fmt:Format.formatter) (a:ptr) : unit =
    Format.fprintf fmt
      "%a%s%s,%a"
      AP.print a.data
      (if a.invalid then ",invalid" else "")
      (if a.null then ",NULL" else "")
      (FunSet.fprint SetExt.printer_default (fun fmt a -> pp_var fmt a.c_func_var))
      a.fn
    
  let print : Format.formatter -> t -> unit =
    top_fprint ptr_print    

    
         
  (*=========================*)
  (** {3 Lattice operations} *)
  (*=========================*)

    
  let empty : ptr = 
    { invalid = false;
      null    = false;
      data    = AP.empty;
      fn      = FunSet.empty;
    }
    
  let bottom : t =
    Nt empty

  let top : t =
    Top.TOP
                   
  let is_bottom : t -> bool =
    Top.top_dfl1
      false
      (fun a -> not a.invalid  && not a.null && AP.is_empty a.data && FunSet.is_empty a.fn)
                  
  let is_top (a:t) : bool =
    a = Top.TOP
    
  let leq : t -> t -> bool =
    Top.top_included
      (fun a b ->
        (b.invalid || not a.invalid) &&
        (b.null || not a.null) &&
        (AP.leq a.data b.data) &&
        (FunSet.subset a.fn b.fn)
      )

  let join_widen (widen:bool)  =
    Top.top_absorb2
      (fun a b ->
        Top.Nt
          { invalid = a.invalid || b.invalid;
            null    = a.null || b.null;
            data    = AP.join_widen widen a.data b.data;
            fn      = FunSet.union a.fn b.fn;
          }
      )
  (** Utility factoring join and widening. *)

  let join (a:t) (b:t) : t =
    let r = join_widen false a b in
    debug "%a U %a = %a@." print a print b print r;
    r

  let widening (_:Framework.Context.context) : t -> t -> t =
    join_widen true
                  
  let meet : t -> t -> t =
    Top.top_neutral2
      (fun a b ->
        { invalid = a.invalid && b.invalid;
          null    = a.null && b.null;
          data    = AP.meet a.data b.data;
          fn      = FunSet.inter a.fn b.fn;
        }
      )

  let lift f : t -> t =
    top_lift1
      (fun a ->
        { a with
          data = f a.data;
          invalid = a.invalid || a.null || not (FunSet.is_empty a.fn);
        }
      )
  (** Utility to invalid NULL and function pointers after unary operators.
      Also take care of the TOP case.
   *)
    
  let append_field (rt:c_record_type) (idx:int) : t -> t =
    lift (AP.append_field rt idx)
  (** Add a struct/union field selection at the end of each path. *)

  let append_array (elem:typ) (idx:slice) : t -> t =
    lift (AP.append_array elem idx)
  (** Add an array element selection at the end of each path. *)

  let ptr_add (elem:typ) (sub:bool) (v:slice) : t -> t =
    lift (AP.ptr_add elem sub v)
  (** Pointer arithmetic on array of type [elem]: adds [v] to the
      array selector at the end of each path.
   *)
    

  (*==========================*)
  (** {3 Simple constructors} *)
  (*==========================*)

    
  let base (b:base) : t =
    Nt { empty with data = AP.base b; }
    
  let fn (f:c_fundec) : t =
    Nt { empty with fn = FunSet.singleton f; }

  let null : t =
    Nt { empty with null = true; }

  let invalid : t =
    Nt { empty with invalid = true; }

    
  let of_ap ((b,s,_):ap) : t =
    Nt { empty with data = AP.of_sel b s; }
  (** Concrete access path type to pointer. *)

  let enumerate : t -> apexpr list with_top =
    top_lift1
      (fun a ->
        let ap,top = AP.enumerate a.data in
        (if a.invalid then [E_ap_invalid] else [])
        @(if a.null then [E_ap_null] else [])
        @(List.map (fun x -> E_ap_fun x) (FunSet.elements a.fn))
        @(List.map (fun x -> E_ap_base x) top)
        @(List.map (fun x -> E_ap_ap x) ap)
      )
  (** Enumerate all pointer values. *)

    
end

           
           
module PtrLattice = (Ptr:Framework.Lattice.LATTICE)
(* Check here that Ptr obeys the lattice structure. *)


type expr_kind +=
  | E_c_access_path of Ptr.t

type _ query +=
  | QExtractVarAccessPath : var -> Ptr.t Framework.Query.query
(** Extract access path of a given variable *)
      
let () =
  Framework.Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, Ptr.t) eq option =
                   function
                   | QExtractVarAccessPath _ -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun _ _ -> assert false);
      meet = (fun _ _ -> assert false);
    };

  )

       
          
(*==========================================================================*)
(**                           {2 Environment domain}                        *)
(*==========================================================================*)

           
           
module Domain =
  struct

    
  (*========================*)
  (** {3 Lattice structure} *)
  (*========================*)


  module Abs = Framework.Lattices.Total_map.Make(Var)(Ptr)
  include Abs
  (** Assign a pointer information to each variable. *)

  let print fmt x =
    Format.fprintf fmt "ptr: @[%a@]@\n" print x
        

  (*================*)
  (** {3 Utilities} *)
  (*================*)


  let apexpr_of_ptr (a:Ptr.t) (flow:'a flow) : (apexpr, 'a) evals =
    match Ptr.enumerate a with
    | TOP | Nt [] -> eval_singleton (None, flow, [])
    | Nt (a::r) ->
       List.fold_left
         (fun e x ->
           eval_join e (eval_singleton (Some x, flow, []))
         )
         (eval_singleton (Some a, flow, []))
         r

            
        
  (*=========================*)
  (** {3 Transfer functions} *)
  (*=========================*)

    
  let init man ctx prog flow =
    ctx, set_domain_cur top man flow
    

  (* evaluates a pointer expression *)
    
  let rec eval_ptr
      (man: ('a, t) manager) ctx
      (exp: expr)
      (flow: 'a flow)
    : (Ptr.t, 'a) evals option =

    let range = exp.erange in
    debug "%a@\neval_ptr %a in@\n@[%a@]" pp_range range pp_expr exp man.flow.print flow;

    match ekind exp with
    | E_constant (C_int n) when Z.equal n Z.zero ->
       (* NULL constant *)
       let pt = Ptr.null in
       debug "eval_ptr NULL: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
      
    | E_constant C_c_invalid ->
       (* invalid constant *)
       let pt = Ptr.invalid in
       debug "eval_ptr C_c_invalid: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
      
    | E_addr addr ->
       (* address constant *)
       let pt = Ptr.base (A addr) in
       debug "eval_ptr E_addr: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
      
    | E_var p when is_c_pointer_type p.vtyp ->
       (* pointer info available in our environment *)
       let a = get_domain_cur man flow in
       if not (mem p a) then debug "eval_ptr Not_found %a" pp_var p;
       let pt = find p a in
       debug "eval_ptr E_var %a pointer: %a" pp_var p Ptr.print pt;
       if Ptr.is_bottom pt then oeval_singleton (None, flow, [])
       else oeval_singleton (Some pt, flow, [])
       
    | E_var ({vkind = V_orig} as a) when is_c_array_type a.vtyp ->
       (* array -> pointer to its first element *)
       let ar = Ptr.base (V a) in
       let pt = Ptr.append_array (under_array_type a.vtyp) zero_slice ar in
       debug "eval_ptr E_var array #1: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
       
    | E_var a when is_c_array_type a.vtyp ->
       (match man.ask ctx (QExtractVarAccessPath a) flow with
        | Some ap ->
           let pt = Ptr.append_array (under_array_type a.vtyp) zero_slice ap in
           debug "eval_ptr E_var array #2: %a" Ptr.print pt;
           oeval_singleton (Some pt, flow, [])
        | None ->
           assert false
       )
            
    | E_c_address_of(e) when is_c_array_type e.etyp || is_c_function_type e.etyp ->
       (* address of array or function -> bypass & operator *)
       debug "eval_ptr E_c_address_of array or function";
       man.eval ctx e flow |> eval_compose (eval_ptr man ctx)
      
    | E_c_address_of(e) ->
       (* address of lvalue *)
       debug "eval_ptr E_c_address_of non-array, non-function";
       man.eval ctx e flow |>
       eval_compose (eval_ref man ctx)
      
    | E_binop((O_plus _ | O_minus _ as op), e1, e2) when
           ((is_c_pointer_type e1.etyp || is_c_array_type e1.etyp) && (is_c_int_type e2.etyp || is_int_type e2.etyp)) ||
           ((is_c_pointer_type e2.etyp || is_c_array_type e2.etyp) && (is_c_int_type e1.etyp || is_int_type e1.etyp))
      ->
       (* pointer arithmetic, as array access *)
       let p, i = if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp then e1, e2 else e2, e1 in
       let vp = eval_compose (eval_ptr man ctx) (man.eval ctx p flow)
       and vi = man.ask ctx (Universal.Numeric.Query.QIntInterval i) flow
       in
       let sub = match op with
         | O_plus _ -> false
         | O_minus _ -> true
         | _ -> assert false
       in
       (match vp, vi with
        | Some pp,
          Some (Nb (Intervals.IntBound.Finite lo,
                    Intervals.IntBound.Finite hi)) ->
           eval_compose
             (fun pt flow ->
               let x = Ptr.ptr_add (under_pointer_type p.etyp) sub (Nt (lo,hi)) pt in
               debug "eval_ptr pointer add: %a +/- %a -> %a" Ptr.print pt Z.pp_print lo Ptr.print x;
               oeval_singleton (Some x, flow, [])
             )
             pp
        | Some pp, Some _ ->
           eval_compose
             (fun pt flow ->
               let x = Ptr.ptr_add (under_pointer_type p.etyp) sub TOP pt in
               debug "eval_ptr pointer add: %a +/- %a -> %a" Ptr.print pt pp_expr i Ptr.print x;
               oeval_singleton (Some x, flow, [])
             )
             pp
        | _ ->
           assert false
       )
       
    | E_c_cast(p', _) when is_c_array_type exp.etyp || is_c_function_type exp.etyp || is_c_pointer_type exp.etyp ->
       (* cast (transparent) *)
       debug "eval_ptr E_c_cast %a" pp_expr p';
       man.eval ctx p' flow |>
       eval_compose (eval_ptr man ctx)
    (*       eval_ptr man ctx p' flow*)
       
    | E_c_function f ->
       (* function constant *)
       let pt = Ptr.fn f in
       debug "eval_ptr E_c_function: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
      
    | _ ->
       panic "eval_ptr: unsupported expression %a in %a" pp_expr exp pp_range_verbose exp.erange



  (* evaluates the address of a lvalue *)
    
  and eval_ref
      (man: ('a, t) manager) ctx
      (exp: expr)
      (flow: 'a flow)
    : (Ptr.t, 'a) evals option =

    let range = exp.erange in
    debug "%a@\neval_ref %a in@\n@[%a@]" pp_range range pp_expr exp man.flow.print flow;

    match ekind exp with
    | E_var v ->
       (
         (* address of variable *)
         match man.ask ctx (QExtractVarAccessPath v) flow with
         | Some ap ->
            debug "eval_ref E_var: %a" Ptr.print ap;
            oeval_singleton (Some ap, flow, [])
            
         | None ->
            assert false
       )
      
    | E_c_function f ->
       (* function constant *)
       let pt = Ptr.fn f in
       debug "eval_ref E_c_function: %a" Ptr.print pt;
       oeval_singleton (Some pt, flow, [])
      
      
    | _ ->
       panic "eval_ref: unsupported expression %a in %a" pp_expr exp pp_range_verbose exp.erange

    
  let rec exec man ctx stmt flow =
    let range = srange stmt in
    debug "%a@\nexec %a@\n@[%a@]" pp_range range pp_stmt stmt man.flow.print flow;
    match skind stmt with

    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
       debug "exec S_c_local_declaration pointer %a" pp_var p;
       man.eval ctx (mk_var p range) flow |>
       eval_to_oexec
         (fun exp flow ->
           match ekind exp with
           | E_var v ->
              map_domain_cur (add v Ptr.invalid) man flow |>
              return
             
           | _ -> assert false
         )
         (man.exec ctx) man.flow

    | S_assign({ekind = E_var p}, q, mode) when is_c_pointer_type p.vtyp ->
       debug "exec S_assign %a = %a" pp_var p pp_expr q;
       eval_ptr man ctx q flow |>
       oeval_to_oexec
         (fun ptr flow ->
           debug "exec S_assign %a -> %a" pp_var p Ptr.print ptr;
           let ptr = match mode with
             | STRONG | EXPAND -> ptr
             | WEAK ->
                let cur = get_domain_cur man flow in
                if not (mem p cur) then debug "exec S_assign Not_found %a" pp_var p;
                Ptr.join ptr (find p cur)
           in
           map_domain_cur (add p ptr) man flow |>
           return
        ) (man.exec ctx) man.flow

    | S_remove_var(p) when is_c_pointer_type p.vtyp ->
       debug "exec S_remove_var pointer %a" pp_var p;
       map_domain_cur (remove p) man flow |>
       return

    | _ -> None


  let eval man ctx exp flow =
    let range = exp.erange in
    debug "%a@\neval %a" pp_range range pp_expr exp;
    match ekind exp with

    | E_c_resolve_pointer p ->
      man.eval ctx p flow |>
      eval_compose (eval_ptr man ctx) |>
      oeval_compose (fun pt flow ->
          let exp' = {exp with ekind = E_c_access_path pt} in
          debug "eval E_c_resolve_pointer %a: %a" pp_expr p Ptr.print pt;
          oeval_singleton (Some exp', flow, [])
        )
      
    | _ -> None

         
  let ask _ _ _ _ = None

end


  
let setup () =
  register_domain name (module Domain);
  (* E_c_resolve_pointer registered by pointer.ml *)
  Framework.Pp.register_pp_expr (fun next fmt exp ->
      match ekind exp with
      (*      | E_c_access_path ap -> Format.fprintf fmt "points-to %a" pp_apexpr ap*)
      | E_c_access_path ap -> Format.fprintf fmt "points-to %a" Ptr.print ap
      | _ -> next fmt exp
    );
  Framework.Visitor.register_expr_visitor (fun next exp ->
      match ekind exp with
      | E_c_access_path _ -> leaf exp
      | _ -> next exp
    )
