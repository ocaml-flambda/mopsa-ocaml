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
  ListExt - Adds a few useful functions to OCaml lists
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

let split (l:'a list) : 'a list * 'a list =
  let rec doit acc l nb =
    if nb <= 0 then (List.rev acc), l
    else doit ((List.hd l)::acc) (List.tl l) (nb-1)
  in
  doit [] l ((List.length l + 1) / 2)
(** [split l] cuts the list [l] into two halves.
    Either the two halves have the same size, or the first list is
    one element larger.
    If [a,b = split l], then [l = a @ b].
 *)


(** {2 Printing} *)

type list_printer = {
    print_empty: string; (** Special text for empty lists *)
    print_begin: string; (** Text before the first element. *)
    print_sep: string;   (** Text between two elements *)
    print_end: string;   (** Text after the last element *)
  }
(** Tells how to print a list. *)

let printer_plain = { print_empty=""; print_begin=""; print_sep=" "; print_end=""; }
(** Print as a space-sparated list, no delimiters. *)

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

let print printer elem ch l =
  print_gen output_string printer elem ch l

let bprint printer elem ch l =
  print_gen Buffer.add_string printer elem ch l

let fprint printer elem ch l =
  print_gen (fun fmt s -> Format.fprintf fmt "%s@," s) printer elem ch l

let to_string printer elem l =
  let b = Buffer.create 10 in
  print_gen (fun () s -> Buffer.add_string b s) printer (fun () e -> elem e) () l;
  Buffer.contents b

let rec compare cmp a b = match a,b with
  | [],[] -> 0
  | [],_ -> 1
  | _,[] -> -1
  | x::s, y::t ->
     let r =  cmp x y in
     if r = 0 then compare cmp s t else r

let rec mem_compare cmp e l = match l with
  | p::q when cmp p e = 0 -> true
  | p::q -> mem_compare cmp e q
  | [] -> false



(** {2 Parallel functions} *)

        (*
Disabled for now.
Not used, yet causes compiler warnings about threading...

let par_iteri (nb_threads:int) (f: int -> 'a -> unit) (l:'a list) : unit =
  if nb_threads <= 1 || List.length l <= 1
  then List.iteri f l
  else
    let exn = ref None in (* exception thrown in thread *)
    let mtx = Mutex.create () in
    let i = ref 0 in
    let ll = ref l in
    let rec consumer () = 
      Mutex.lock mtx;
      match !ll with
      | a::b when !exn = None->
         (* eat one *)
         let err = ref false in
         ll := b;
         let ii = !i in
         incr i;
         Mutex.unlock mtx;
         (try
             f ii a
           with x ->
             (* remember exception for main thread *)
             Mutex.lock mtx;
             exn := Some x;
             Mutex.unlock mtx;
             err := true
         );
         if not !err then consumer ()
      | _ ->
         (* the end *)
         Mutex.unlock mtx
    in
    Array.init
      (min nb_threads (List.length l))
      (fun _ -> Thread.create consumer ())
    |>  Array.iter Thread.join;
    match !exn with
    | None -> ()
    | Some x -> raise x (* rethrow exception from thread *)
         *)
(**
   As List.iter, but in parallel using nb_threads threads.
   As threads are used, this only makes sense if the iterated function
   calls a C function that temporarily lifts the global interpreter lock
   (e.g., the Clang parser)
 *)
