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

(** Printer - pretty-printing of expressions values *)

open Ast.Var
open Ast.Expr
open Yojson.Basic


module StringMap = MapExt.StringMap

type domain = string

type section =
  | Map    of section StringMap.t
  | List   of section list
  | String of string

type printer = {
  mutable sections : section StringMap.t;
  mutable prev_exprs: ExprSet.t;
}

let empty_printer =
  { sections = StringMap.empty;
    prev_exprs = ExprSet.empty }

let get_prev_exprs printer =
  ExprSet.elements printer.prev_exprs

let add_expr printer exp =
  printer.prev_exprs <- ExprSet.add exp printer.prev_exprs

let mem_prev_expr printer exp =
  ExprSet.mem exp printer.prev_exprs

let set_section printer domain s =
  printer.sections <- StringMap.add domain s printer.sections

let get_section printer domain =
  StringMap.find domain printer.sections

let get_section_opt printer domain =
  StringMap.find_opt domain printer.sections

let pp_string printer domain s =
  set_section printer domain (String s)

let pp_map_binding printer domain key value =
  let map =
    match get_section_opt printer domain with
    | None -> StringMap.empty
    | Some (Map m) -> m
    | _ -> Exceptions.panic "add_to_map_section called on a non-map section"
  in
  set_section printer domain (Map (StringMap.add key value map))

let pp_map printer domain bindings =
  set_section printer domain (Map (StringMap.of_list bindings))

let pp_list_element printer domain value =
  let l =
    match get_section_opt printer domain with
    | None -> []
    | Some (List l) -> l
    | _ -> Exceptions.panic "add_to_list_section called on a non-list section"
  in
  set_section printer domain (List (value::l))

let pp_list printer domain l =
  set_section printer domain (List l)

let rec flush_section fmt = function
  | String s -> Format.pp_print_string fmt s
  | Map m ->
    Format.(
      fprintf fmt "@[<v>%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt (k,v) ->
              fprintf fmt "@[<hov2>%s: @,%a@]"
                k
                flush_section v
           )
        ) (StringMap.bindings m)
    )
  | List l ->
    Format.(
      fprintf fmt "@[<v>%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           flush_section
        ) l
    )

let flush fmt printer =
  Format.(
    fprintf fmt "@[<v>%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (domain,sec) ->
            fprintf fmt "%s:@,  @[<v>%a@]"
              domain
              flush_section sec
         )
      ) (StringMap.bindings printer.sections)
  )

let rec section_to_json = function
  | String s -> `String s
  | Map m ->
    `Assoc (
      StringMap.bindings m |>
      List.map (fun (k,v) -> k,section_to_json v)
    )
  | List l -> `List (List.map section_to_json l)


let printer_to_json printer =
  `Assoc (
    StringMap.bindings printer.sections |>
    List.map (fun (domain,sec) -> domain,section_to_json sec)
  )
