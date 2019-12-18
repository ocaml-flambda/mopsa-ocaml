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

(** Command-line options.
    Replacement for [Arg] from the standard library 
 *)

type spec =
  | Unit of (unit -> unit)

  | Unit_delayed of (unit -> unit)
  (** [Unit_delayed] functions are only executed after parsing all 
      the arguments, in the order they appear on the command-line.
   *)

  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref

  | Int of (int -> unit)
  | Set_int of int ref

  | String of (string -> unit)
  | Set_string of string ref

  | Set_string_list of string list ref

  | Symbol of string list * (string -> unit)

type arg = {
  key: string;
  doc: string;
  category: string;
  default: string;
  spec: spec;
}


(** Replacement for [Arg.parse]. 
    Adds delayed Unit arguments.
 *)
let parse (args:arg list) (handler:string -> unit) (rest:string list -> unit) (msg:string) (help:unit -> unit) : unit =
  (* separate arg into program name and actual command-line arguments *)
  let progname, opts =
    if Array.length Sys.argv < 1 then "?", []
    else Sys.argv.(0), List.tl (Array.to_list Sys.argv)
  in
  (* Unit_delayed actions are delayed into everything is parsed *)
  let delayed = ref [] in
  (* utilities *)
  let to_bool a v =
    try bool_of_string v with _ ->
      Printf.eprintf "%s: option %s requires a boolean argument (true or false)\n" progname a;
      help ();
      exit 2
  and to_int a v =
    try int_of_string v with _ ->
      Printf.eprintf "%s: option %s requires an integer argument\n" progname a;
      help ();
      exit 2
  in
  (* eat argument list *)
  let rec eat = function
    | [] -> ()
    | a::tl ->
       if a = "" then
         eat tl
       else if a = "--" then
         rest tl
       else if a.[0] != '-' then (
         handler a;
         eat tl
       )
       else (
         (* cut option at '=' if necessary *)
         let opt, arg =
           if String.contains a '=' then
             let i = String.index a '=' in
             String.sub a 0 i,
             Some (String.sub a (i+1) (String.length a - i - 1))
           else
             a, None
         in
         (* get option argument, either after '=' or in the next 
            command-line argument *)
         let get_arg () = match arg, tl with
           | Some x, tl -> x, tl
           | None, x::tl -> x, tl
           | None, [] ->
              Printf.eprintf "%s: option %s requires an argument\n" progname opt;
              help ();
              exit 2
         and noarg () =
           if arg <> None then (
             Printf.eprintf "%s: option %s has no argument\n" progname opt;
             help ();
             exit 2
           )
         and get_arg_list () = match arg, tl with
           | Some x, tl -> String.split_on_char ',' x, tl
           | None, x::tl -> String.split_on_char ',' x, tl
           | None, [] ->
             Printf.eprintf "%s: option %s requires an argument\n" progname opt;
             help ();
             exit 2
         in
         if List.exists (fun x -> x.key = opt) args then (
           let arg = List.find (fun x -> x.key = opt) args in
           match arg.spec with
           
           | Unit_delayed f ->
              noarg ();
              delayed := (!delayed)@[f];
              eat tl
              
           | Unit f ->
              noarg ();
              f ();
              eat tl
              
           | Set r ->
              noarg ();
              r := true;
              eat tl
              
           | Clear r ->
              noarg ();
              r := false;
              eat tl
              
           | Bool f ->
              let v, tl = get_arg () in
              f (to_bool a v);
              eat tl
            
           | Int f ->
              let v, tl = get_arg () in
              f (to_int a v);
              eat tl
            
           | Set_int f ->
              let v, tl = get_arg () in
              f := to_int a v;
              eat tl
            
           | String f ->
              let v, tl = get_arg () in
              f v;
              eat tl
            
           | Set_string f ->
              let v, tl = get_arg () in
              f := v;
              eat tl
              
           | Set_string_list f ->
              let v, tl = get_arg_list () in
              f := (!f)@v;
              eat tl
              
           | Symbol (l,f) ->
              let v, tl = get_arg () in
              if not (List.mem v l) then (
                Printf.eprintf
                  "%s: option %s requires an argument in the list: [%a]\n"
                  progname a
                  (ListExt.print ListExt.printer_plain output_string) l;
                help ();
                exit 2
              );
              f v;
              eat tl
         )
         else (
           Printf.eprintf "%s: unknown option %s\n" progname a;
           help ();
           exit 2
         )
       )
  in
  eat opts;
  (* now execute all delayed actions *)
  List.iter (fun f -> f ()) !delayed
         
