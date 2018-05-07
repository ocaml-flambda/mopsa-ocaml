(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Utility module for initializing variables. *)

open Framework.Ast
open Framework.Manager
open Framework.Flow
open Framework.Pp
open Universal.Ast
open Ast


type 'a init_manager = {
  scalars : expr -> expr -> range -> 'a flow -> 'a flow;
  arrays  : expr -> bool -> c_init option list -> range -> 'a flow -> 'a flow;
  structs  : expr -> bool -> c_init option list -> range -> 'a flow -> 'a flow;
}


(** Initialization of scalar expressions *)
let rec init_scalar man v is_global init range flow =
  match init with
  (* Uninitialized local pointers are invalid *)
  | None when not is_global && is_c_pointer_type v.etyp ->
    man.scalars v (Pointer.mk_c_invalid range) range flow

  (* Local uninitialized variables are kept ⟙ *)
  | None when not is_global ->
    flow

  (* Globals are initialized to 0 *)
  | None when is_global ->
    man.scalars v (mk_zero range) range flow

  (* Initialization with an expression *)
  | Some (C_init_expr e) ->
    man.scalars v e range flow

  | _ -> assert false


and init_array man a is_global init range flow =
  debug "init array %a" Framework.Pp.pp_expr a;
  let n = get_array_constant_length a.etyp in
  let el =
    match init with
    | None ->
      let rec aux i =
        if i = n then []
        else None :: aux (i + 1)
      in
      aux 0

    | Some (C_init_list (l, filler)) ->
      let rec aux i =
        if i = n then []
        else
          let init = if i < List.length l then Some (List.nth l i) else filler in
          init :: aux (i + 1)
      in
      aux 0

    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _))}) ->
      let rec aux i =
        if i = n then []
        else
          let init = if i < String.length s then Some (C_init_expr (mk_c_character (String.get s i) range)) else Some (C_init_expr (mk_c_character (char_of_int 0) range)) in
          init :: aux (i + 1)
      in
      aux 0

    | _ ->
      Framework.Exceptions.panic "Array initialization not supported"
  in
  man.arrays a is_global el range flow

and init_union  man u is_global init range flow =
  match init with
  | None when not is_global -> flow
  | _ ->
    Framework.Exceptions.panic "Initialization of union not supported"


and init_struct man s is_global init range flow =
  debug "init struct %a" Framework.Pp.pp_expr s;
  let n =
    match remove_typedef s.etyp |> remove_qual with
    | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.length c_record_fields
    | _ -> assert false
  in
  let el =
    match init with
    | None ->
      let rec aux i =
        if i = n then []
        else None :: aux (i + 1)
      in
      aux 0

    | Some (C_init_list(l, None)) ->
      let rec aux i =
        if i = n then []
        else
          let init = if i < List.length l then Some (List.nth l i) else None in
          init :: aux (i + 1)
      in
      aux 0

    | Some (C_init_expr e) -> Framework.Exceptions.panic "init: initialization of structs not supported"

    | _ -> assert false
  in
  man.structs s is_global el range flow


and init_expr man e is_global (init: c_init option) (range: range) flow =
  if is_c_scalar_type e.etyp then init_scalar man e is_global init range flow else
  if is_c_array_type e.etyp then  init_array man e is_global init range flow else
  if is_c_struct_type e.etyp then init_struct man e is_global init range flow else
  if is_c_union_type e.etyp then init_union man e is_global init range flow else
    Framework.Exceptions.panic "Unsupported initialization of %a" pp_expr e


let fold_globals ctx man globals flow =
  (* Initialize string symbols as global variables *)
  let range = mk_fresh_range () in
  let table = Program.find_string_table ctx in
  let globals = Program.StringTable.fold (fun s v acc ->
      let init = C_init_expr (mk_c_string s range) in
      (v, Some init) :: acc
    ) table globals
  in
  (* Initialize global variables *)
  List.fold_left (fun flow (v, init) ->
      let range = mk_fresh_range () in
      let v = mk_var v range in
      init_expr man v true init range flow
    ) flow globals

let init_local man v init range flow =
  let v = mk_var v range in
  init_expr man v false init range flow
