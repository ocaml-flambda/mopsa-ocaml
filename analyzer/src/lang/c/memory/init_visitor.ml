(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Visitor of initialization expressions. *)

open Framework.Essentials
open Universal.Ast
open Ast

let debug fmt = Debug.debug ~channel:"c.memory.init" fmt

type record_init =
  | Expr of expr
  | Parts of c_init option list

type 'a visitor = {
  scalar : expr -> expr -> range -> 'a flow -> 'a flow;
  array  : expr -> bool -> c_init option list -> range -> 'a flow -> 'a flow;
  record  : expr -> bool -> record_init -> range -> 'a flow -> 'a flow;
}


(** Initialization of scalar expressions *)
let rec init_scalar visitor v is_global init range flow =
  match init with
  (* Local uninitialized pointers are invalidated *)
  | None when not is_global && is_c_pointer_type v.etyp ->
    visitor.scalar v (mk_constant C_c_invalid ~etyp:(T_c_pointer T_c_void) range) range flow

  (* Other local uninitialized variables are kept ⟙ *)
  | None when not is_global ->
    flow

  (* Globals are initialized to 0 *)
  | None when is_global ->
    visitor.scalar v (mk_zero range) range flow

  (* Initialization with an expression *)
  | Some (C_init_expr e) ->
    visitor.scalar v e range flow

  | _ -> assert false


and init_array visitor a is_global init range flow =
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
  visitor.array a is_global el range flow

and init_union visitor u is_global init range flow =
  match init with
  | None when not is_global -> flow
  | _ ->
    Framework.Exceptions.panic "Initialization of union not supported"


and init_record visitor s is_global init range flow =
  let n =
    match remove_typedef s.etyp |> remove_qual with
    | T_c_record{c_record_fields} -> List.length c_record_fields
    | _ -> assert false
  in
  let el =
    match init with
    | None ->
      let rec aux i =
        if i = n then []
        else None :: aux (i + 1)
      in
      Parts (aux 0)

    | Some (C_init_list(l, None)) ->
      let rec aux i =
        if i = n then []
        else
          let init = if i < List.length l then Some (List.nth l i) else None in
          init :: aux (i + 1)
      in
      Parts (aux 0)

    | Some (C_init_expr e) ->
      Expr e

    | _ -> assert false
  in
  visitor.record s is_global el range flow


and init_expr visitor e is_global (init: c_init option) (range: range) flow =
  if is_c_scalar_type e.etyp then init_scalar visitor e is_global init range flow else
  if is_c_array_type e.etyp then  init_array visitor e is_global init range flow else
  if is_c_record_type e.etyp then init_record visitor e is_global init range flow else
    Framework.Exceptions.panic "Unsupported initialization of %a" pp_expr e


let init_global visitor v init range flow =
  let v = mk_var v range in
  init_expr visitor v true init range flow

let init_local visitor v init range flow =
  let v = mk_var v range in
  init_expr visitor v false init range flow
