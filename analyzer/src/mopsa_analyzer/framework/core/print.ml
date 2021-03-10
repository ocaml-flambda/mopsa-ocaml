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

open Mopsa_utils
open Ast.Var
open Ast.Expr
open Yojson.Basic
module StringMap = MapExt.StringMap



(****************************************************************************)
(**                          {1 Print objects}                              *)
(****************************************************************************)

type map_symbols = {
  mopen : string;
  msep   : string;
  mbind  : string;
  mclose   : string;
}

type list_symbols = {
  lopen : string;
  lsep   : string;
  lclose   : string;
}

type print_object =
  | Empty
  | Bool   of bool
  | Int    of Z.t
  | Float  of float
  | String of string
  | Map    of print_object StringMap.t * map_symbols
  | List   of print_object list * list_symbols

let default_map_symbols = { mopen = ""; msep = ","; mclose = ""; mbind = ":" }

let default_list_symbols = { lopen = ""; lsep = ","; lclose = "" }

(****************************************************************************)
(**                            {1 Printers}                                 *)
(****************************************************************************)

type printer = {
  mutable body : print_object;
  mutable prev_exprs: ExprSet.t;
}

let get_printed_object printer = printer.body

let empty_printer () =
  { body = Empty;
    prev_exprs = ExprSet.empty }

let get_printed_exprs printer =
  ExprSet.elements printer.prev_exprs

let add_printed_expr printer exp =
  printer.prev_exprs <- ExprSet.add exp printer.prev_exprs

let mem_printed_expr printer exp =
  ExprSet.mem exp printer.prev_exprs


(****************************************************************************)
(**                           {1 Print paths}                               *)
(****************************************************************************)

type print_selector =
  | Key    of string
  | Index  of int
  | Head
  | Tail

type print_path = print_selector list

let rec find_print_object path obj =
  match path, obj with
  | [],_ -> obj

  (* Maps *)
  | Key k::tl, Map (m,_) -> find_print_object tl (StringMap.find k m)
  | Key k::tl, Empty -> Empty
  | Key _::_,_ -> Exceptions.panic "find_print_object: key selector on non-map object"

  (* Lists *)
  | Index i::tl, List (l,_) -> begin try find_print_object tl (List.nth l i) with Failure _ -> raise Not_found end
  | Index i::tl, Empty -> Empty
  | Index _::_,_ -> Exceptions.panic "find_print_object: index selector on non-list object"

  | Head::tl, List (l,_) -> begin try find_print_object tl (List.hd l) with Failure _ -> raise Not_found end
  | Head::tl, Empty -> Empty
  | Head::_,_ -> Exceptions.panic "find_print_object: head selector on non-list object"

  | Tail::tl, List (l,_) -> begin try find_print_object tl (ListExt.last l) with Failure _ -> raise Not_found end
  | Tail::tl, Empty -> Empty
  | Tail::_,_ -> Exceptions.panic "find_print_object: tail selector on non-list object"

let rec match_print_object_keys re obj =
  match obj with
  | Map (map,sym) ->
    let map' =
      StringMap.fold
        (fun k v acc ->
           if Str.string_match re k 0
           then StringMap.add k v acc
           else acc
        ) map StringMap.empty in
    if StringMap.is_empty map' then Empty else Map(map',sym)

  | List(list,sym) ->
    let list' =
      List.fold_left
        (fun acc v ->
           let v' = match_print_object_keys re v in
           match v' with
           | Empty -> acc
           | _     -> v' :: acc
        ) [] list in
    if list' = [] then Empty else List(List.rev list',sym)

  | Empty | Int _ | Bool _ | Float _ | String _ -> obj




(****************************************************************************)
(**                    {1 Generic print functions}                          *)
(****************************************************************************)

let pprint ?(path=[]) printer obj =
  let rec iter p o =
    match p,o with
    | Key k::tl, Map (m,sym) -> Map (StringMap.add k (iter tl (try StringMap.find k m with Not_found -> Empty)) m, sym)
    | Key k::tl, Empty -> Map (StringMap.singleton k (iter tl Empty), default_map_symbols)
    | Key _::_,_ -> Exceptions.panic "print: key selector on non-map object"
    | Index i::tl, List (l,sym) -> List (List.mapi (fun j e -> if i = j then iter tl e else e) l, sym)
    | Index i::tl, Empty -> List (List.init (i+1) (fun j -> if i = j then iter tl Empty else Empty), default_list_symbols)
    | Index _::_,_ -> Exceptions.panic "print: index selector on non-list object"
    | Head::tl, List (l,sym) -> List (iter tl Empty::l, sym)
    | Head::tl, Empty -> List ([iter tl Empty], default_list_symbols)
    | Head::_,_ -> Exceptions.panic "print: head selector on non-list object"
    | Tail::tl, List (l,sym) -> List (l@[iter tl Empty], sym)
    | Tail::tl, Empty -> List ([iter tl Empty], default_list_symbols)
    | Tail::_,_ -> Exceptions.panic "print: tail selector on non-list object"
    | [],_ ->
      match o,obj with
      | Map(m1,s1),Map(m2,s2) -> Map(StringMap.map2zo (fun k v1 -> v1) (fun k v2 -> v2) (fun k v1 v2 -> v2) m1 m2, s2)
      | List(l1,s1),List(l2,s2) -> List(l1@l2, s2)
      | _ -> obj
  in
  printer.body <- iter path printer.body

let rec pp_print_object fmt = function
  | Empty    -> ()
  | Bool b   -> Format.pp_print_bool fmt b
  | Int n    -> Z.pp_print fmt n
  | Float f  -> Format.pp_print_float fmt f
  | String s -> Format.pp_print_string fmt s
  | Map (m,sym) ->
    Format.(
      fprintf fmt "@[<hv>%s%a%s@]"
        sym.mopen
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s@ " sym.msep)
           (fun fmt (k,v) ->
              fprintf fmt "@[<hov2>%s %s @,%a@]"
                k sym.mbind
                pp_print_object v
           )
        ) (StringMap.bindings m)
        sym.mclose
    )
  | List (l,sym) ->
    Format.(
      fprintf fmt "@[<hov>%s%a%s@]"
        sym.lopen
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s@ " sym.lsep)
           pp_print_object
        ) l
        sym.lclose
    )

let pflush fmt printer = pp_print_object fmt printer.body

let format f fmt x =
  let printer = empty_printer () in
  f printer x;
  pflush fmt printer

let unformat ?(path=[]) f printer x =
  Format.kasprintf (fun str ->
      pprint printer (String str) ~path
    ) "%a" f x

let pbox f x =
  let printer = empty_printer () in
  f printer x;
  printer.body

let fbox fmt =
  Format.kasprintf (fun str ->
      pbox (fun printer str -> pprint printer (String str)) str
    ) fmt

let fprint ?(path=[]) printer fmt =
  Format.kasprintf (fun str ->
      pprint ~path printer (String str)
    ) fmt

let sprint f x =
  Format.asprintf "%a" (format f) x

let fkey fmt =
  Format.kasprintf (fun str ->
      Key str
    ) fmt

let pkey f x =
  fkey "%a" (format f) x


(****************************************************************************)
(**                      {1 Typed print functions}                          *)
(****************************************************************************)

let pp_int ?(path=[]) printer n =
  pprint ~path printer (Int (Z.of_int n))

let pp_z ?(path=[]) printer z =
  pprint ~path printer (Int z)

let pp_bool ?(path=[]) printer b =
  pprint ~path printer (Bool b)

let pp_float ?(path=[]) printer f =
  pprint ~path printer (Float f)

let pp_string ?(path=[]) printer str =
  pprint ~path printer (String str)

let pp_obj_list ?(path=[]) ?(lopen="") ?(lsep=",") ?(lclose="") printer l =
  pprint ~path printer
    (List (l, {lopen; lsep; lclose;}))

let pp_list ?(path=[]) ?(lopen="") ?(lsep=",") ?(lclose="") f printer l =
  pp_obj_list ~path ~lopen ~lsep ~lclose printer
    (List.map (pbox f) l)

let pp_obj_smap ?(path=[]) ?(mopen="") ?(msep=",") ?(mclose="") ?(mbind=":") printer l =
  let m = StringMap.of_list l in
  pprint ~path printer (Map (m, {mopen; msep; mclose; mbind}))

let pp_obj_map ?(path=[]) ?(mopen="") ?(msep=",") ?(mclose="") ?(mbind=":") printer l =
  let m = List.map (fun (k,v) -> (sprint pprint k, v)) l |>
          StringMap.of_list in
  pprint ~path printer (Map (m, {mopen; msep; mclose; mbind}))

let pp_smap ?(path=[]) ?(mopen="") ?(msep=",") ?(mclose="") ?(mbind=":") fv printer l =
  pp_obj_smap ~path ~mopen ~msep ~mclose ~mbind printer
    (List.map (fun (k,v) -> (k,pbox fv v)) l)

let pp_map ?(path=[]) ?(mopen="") ?(msep=",") ?(mclose="") ?(mbind=":") fk fv printer l =
  pp_obj_map ~path ~mopen ~msep ~mclose ~mbind printer
    (List.map (fun (k,v) -> (pbox fk k,pbox fv v)) l)



(****************************************************************************)
(**                              {1 JSON}                                   *)
(****************************************************************************)

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

let rec json_to_print_object = function
  | `Null -> Empty
  | `Bool b -> Bool b
  | `Int n -> Int (Z.of_int n)
  | `Float f -> Float f
  | `String s -> String s
  | `Assoc a ->
    let m = List.map (fun (k,v) -> (k,json_to_print_object v)) a |>
            StringMap.of_list in
    Map(m,default_map_symbols)
  | `List l -> List(List.map json_to_print_object l,default_list_symbols)
