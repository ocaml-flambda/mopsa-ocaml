(**
  ListExt - Adds a few useful functions to OCaml lists


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


include List
(** Import everything from List *)

          
(** {2 Operations} *)
          
let rec last = function
  | [] -> invalid_arg "listUtil.last: empty list"
  | [a] -> a
  | a::b -> last b
(** Last eleemnt of a list. Raises invalid_arg if the list is empty. *)                 

let map_tail f l = rev (rev_map f l)
(** Tail-recursive version of map. Useful for large lists. *)

let append_tail a b = rev_append (List.rev a) b
(** Tail-recursive version of append. Useful if the first list is large.. *)
                                 
let map_merge (f:'a -> 'b list) (l:'a list) : 'b list =
  List.rev
    (List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l)
(** Maps [e1;e2;...;en] to (f e1)@(f e2)@...@(f en) *)
    
let map_filter (f:'a -> 'b option) (l: 'a list) : 'b list =
  let rec doit acc l = match l with
    | [] -> rev acc
    | a::r -> doit (match f a with Some x -> x::acc | None -> acc) r
  in
  doit [] l
(** Applies f to every element of the list and kepp only those that return some value. 
    The order of the argument list is preserved.
 *)
       
                             
(** {2 Printing} *)

type list_printer = {
    print_empty: string; (** Special text for empty lists *)
    print_begin: string; (** Text before the first element. *)
    print_sep: string;   (** Text between two elements *)
    print_end: string;   (** Text after the last element *)
  }
(** Tells how to print a list. *)

let printer_list = { print_empty="[]"; print_begin="["; print_sep=";"; print_end="]"; }
(** Print as OCaml list: [a;b;c]. *)
                     
let printer_tuple = { print_empty="()"; print_begin="("; print_sep=","; print_end=")"; }
(** Print as OCaml tuple: (a,b,c). *)

let printer_set = { print_empty="{}"; print_begin="{"; print_sep=";"; print_end="}"; }
(** Print as set: {a;b;c}. *)

let print_gen o printer elem ch l =
  match l with
  | [] -> o ch printer.print_empty
  | a::rest ->
     o ch printer.print_begin;
     elem ch a;
     List.iter (fun e -> o ch printer.print_sep; elem ch e) rest;
     o ch printer.print_end
(* internal printing helper *)
       
let print printer elem ch l = print_gen output_string printer elem ch l
let bprint printer elem ch l = print_gen Buffer.add_string printer elem ch l
let fprint printer elem ch l = print_gen Format.pp_print_string printer elem ch l

let to_string printer elem l =
  let b = Buffer.create 10 in
  print_gen (fun () s -> Buffer.add_string b s) printer (fun () e -> elem e) () l;
  Buffer.contents b
