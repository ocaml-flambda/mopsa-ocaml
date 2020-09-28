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

(** Print - pretty-printing of expressions values *)

open Ast.Var
open Ast.Expr
open Yojson.Basic


module StringMap = MapExt.StringMap


type print_symbols = {
  sym_begin  : string;
  sym_sep    : string;
  sym_end    : string;
}

type print_object =
  | Empty
  | Bool   of bool
  | Int    of Z.t
  | Float  of float
  | String of string
  | Map    of print_object MapExt.StringMap.t * print_symbols
  | List   of print_object list * print_symbols

type printer = {
  mutable body : print_object;
  mutable prev_exprs: ExprSet.t;
}

let empty_printer () =
  { body = Empty;
    prev_exprs = ExprSet.empty }

let get_printed_exprs printer =
  ExprSet.elements printer.prev_exprs

let add_printed_expr printer exp =
  printer.prev_exprs <- ExprSet.add exp printer.prev_exprs

let mem_printed_expr printer exp =
  ExprSet.mem exp printer.prev_exprs


type print_selector =
  | Key    of string
  | Index  of int

type print_path = print_selector list

let find_print_object printer path =
  let rec iter p s =
    match p,s with
    | [],_ -> s
    | Key k::tl, Map (m,_) -> iter tl (StringMap.find k m)
    | Key k::tl, Empty -> Empty
    | Key _::_,_ -> Exceptions.panic "find_print_object: key selector on non-map object"
    | Index i::tl, List (l,_) -> begin try iter tl (List.nth l i) with Failure _ -> raise Not_found end
    | Index i::tl, Empty -> Empty
    | Index _::_,_ -> Exceptions.panic "find_print_object: index selector on non-list object"
  in
  iter path printer.body

let pp_obj ?(path=[]) printer obj =
  let rec iter p o =
    match p,o with
    | [],_ -> obj
    | Key k::tl, Map (m,sym) -> Map (StringMap.add k (iter tl (try StringMap.find k m with Not_found -> Empty)) m, sym)
    | Key k::tl, Empty -> Map (StringMap.singleton k (iter tl Empty), {sym_begin = ""; sym_sep = ","; sym_end = ""})
    | Key _::_,_ -> Exceptions.panic "print: key selector on non-map object"
    | Index i::tl, List (l,sym) -> List (List.mapi (fun j e -> if i = j then iter tl e else e) l, sym)
    | Index i::tl, Empty -> List (List.init (i+1) (fun j -> if i = j then iter tl Empty else Empty), {sym_begin = ""; sym_sep = ","; sym_end = ""})
    | Index _::_,_ -> Exceptions.panic "print: index selector on non-list object"
  in
  printer.body <- iter path printer.body


let rec fprint_obj fmt = function
  | Empty    -> ()
  | Bool b   -> Format.pp_print_bool fmt b
  | Int n    -> Z.pp_print fmt n
  | Float f  -> Format.pp_print_float fmt f
  | String s -> Format.pp_print_string fmt s
  | Map (m,sym) ->
    Format.(
      fprintf fmt "@[<hov>%s%a%s@]"
        sym.sym_begin
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s@ " sym.sym_sep)
           (fun fmt (k,v) ->
              fprintf fmt "@[<hov2>%s: @,%a@]"
                k
                fprint_obj v
           )
        ) (StringMap.bindings m)
        sym.sym_end
    )
  | List (l,sym) ->
    Format.(
      fprintf fmt "@[<hov>%s%a%s@]"
        sym.sym_begin
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s@ " sym.sym_sep)
           fprint_obj
        ) l
        sym.sym_end
    )

let fprint fmt printer =
  fprint_obj fmt printer.body

let format f fmt x =
  let printer = empty_printer () in
  f printer x;
  fprint fmt printer

let pp_int ?(path=[]) printer n =
  pp_obj ~path printer (Int (Z.of_int n))

let pp_z ?(path=[]) printer z =
  pp_obj ~path printer (Int z)

let pp_bool ?(path=[]) printer b =
  pp_obj ~path printer (Bool b)

let pp_float ?(path=[]) printer f =
  pp_obj ~path printer (Float f)

let pp_string ?(path=[]) printer str =
  pp_obj ~path printer (String str)

let boxed f x =
  let printer = empty_printer () in
  f printer x;
  printer.body

let boxed_format fmt =
  Format.kasprintf (fun str ->
      boxed pp_string str
    ) fmt

let pp_obj_list ?(path=[]) ?(sym_begin="[") ?(sym_sep=",") ?(sym_end="]") printer l =
  pp_obj ~path printer
    (List (l, {sym_begin; sym_sep; sym_end}))

let pp_list ?(path=[]) ?(sym_begin="[") ?(sym_sep=",") ?(sym_end="]") f printer l =
  pp_obj_list ~path ~sym_begin ~sym_sep ~sym_end printer
    (List.map (boxed f) l)

let pp_append_obj_list ?(path=[]) printer o =
  let l,s =
    match find_print_object printer path with
    | Empty -> [o], {sym_begin=""; sym_sep=""; sym_end=""}
    | List(l,s) -> o::l,s
    | _ ->  Exceptions.panic "pp_append_obj_list: non-list object"
  in
  pp_obj_list printer l ~path ~sym_begin:s.sym_begin ~sym_sep:s.sym_sep ~sym_end:s.sym_end

let pp_append_list ?(path=[]) f printer e =
  pp_append_obj_list ~path printer (boxed f e)

let pp_obj_map ?(path=[]) ?(sym_begin="{") ?(sym_sep=",") ?(sym_end="}") printer l =
  let l =
    List.map (fun (k,v) ->
        let s =
          match k with
          | String s -> s
          | _ -> Format.asprintf "%a" (format pp_obj) k
        in
        (s,v)
      ) l
  in
  let m = StringMap.of_list l in
  pp_obj ~path printer (Map (m, {sym_begin; sym_sep; sym_end}))

let pp_map ?(path=[]) ?(sym_begin="{") ?(sym_sep=",") ?(sym_end="}") fk fv printer l =
  pp_obj_map ~path ~sym_begin ~sym_sep ~sym_end printer
    (List.map (fun (k,v) -> (boxed fk k,boxed fv v)) l)

let pp_set = pp_list ~sym_begin:"{" ~sym_end:"}"

let pp_obj_set = pp_obj_list ~sym_begin:"{" ~sym_end:"}"

let pp_tuple = pp_list ~sym_begin:"(" ~sym_end:")"

let pp_obj_tuple = pp_obj_list ~sym_begin:"(" ~sym_end:")"

let pp_sequence = pp_list ~sym_begin:"" ~sym_end:""

let pp_obj_sequence = pp_obj_list ~sym_begin:"" ~sym_end:""

let pp_boxed ?(path=[]) f printer x =
  pp_obj ~path printer (boxed f x)

let pp_boxed_format ?(path=[]) printer fmt =
  Format.kasprintf (fun str ->
      pp_string ~path printer str
    ) fmt

let rec print_object_to_json = function
  | Empty     -> `Null
  | Bool b    -> `Bool b
  | Int n     -> `Int (Z.to_int n)
  | Float f   -> `Float f
  | String s  -> `String s
  | Map (m,_) ->
    `Assoc (
      StringMap.bindings m |>
      List.map (fun (k,v) -> k, print_object_to_json v)
    )
  | List (l,_) -> `List (List.map print_object_to_json l)


let printer_to_json printer =
  print_object_to_json printer.body

let fkey fmt =
  Format.kasprintf (fun str ->
      Key str
    ) fmt

let pkey f x =
  fkey "%a" (format f) x

let unformat ?(path=[]) f printer x =
  Format.kasprintf (fun str ->
      pp_string ~path printer str
    ) "%a" f x
