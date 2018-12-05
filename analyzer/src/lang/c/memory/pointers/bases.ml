(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Pointer bases as a non-relational value abstraction *)

open Framework.Essentials
open Framework.Value
open Universal.Ast
open Common.Base

(** Pointer bases *)
module PB =
struct
  type t =
    | PB_fun of Ast.c_fundec
    | PB_block of base
    | PB_null
    | PB_invalid

  let print fmt = function
    | PB_fun f -> Format.fprintf fmt "(fun: %a)" pp_var f.Ast.c_func_var
    | PB_block base -> Format.fprintf fmt "(var: %a)" pp_base base
    | PB_null-> Format.pp_print_string fmt "NULL"
    | PB_invalid -> Format.pp_print_string fmt "Invalid"

  let compare p1 p2 =
    match p1, p2 with
    | PB_fun f1, PB_fun f2 -> compare_var f1.Ast.c_func_var f2.Ast.c_func_var
    | PB_block b1, PB_block b2 -> compare_base b1 b2
    | _, _ -> Pervasives.compare p1 p2
end

module Value =
struct

  module PBS = Framework.Lattices.Powerset.Make(PB)

  include PBS

  let name = "c.memory.pointers.bases", "bases"

  type _ value += V_c_pointers_bases : t value

  let id = V_c_pointers_bases

  let identify : type a. a value -> (t, a) eq option =
    function
    | V_c_pointers_bases -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:(fst name) fmt
    
  let zone = Z_any

  let null = singleton PB_null

  let invalid = singleton PB_invalid

  let block b = singleton (PB_block b)

  let bfun f = singleton (PB_fun f)

  let of_constant _ c =
    match c with
    | C_int n when Z.equal n Z.zero -> null
    | _ -> top

  let unop _ op v = top

  let binop _ op v1 v2 = top

  let bwd_unop = default_bwd_unop_simple
  let bwd_binop = default_bwd_binop_simple

  let filter _ v b =
    if b then diff v null
    else meet () v null

  let is_singleton v =
    not (is_top v) &&
    cardinal v == 1

  let compare _ op v1 v2 r =
    let op = if r then op else negate_comparison op in        
    match op with
    | O_eq -> let v = meet () v1 v2 in v, v
    | O_ne ->
      if is_singleton v1 then v1, diff v2 v1
      else if is_singleton v2 then diff v1 v2, v2
      else v1, v2
    | _ -> v1, v2

  let ask _ _ = None

end

let () =
  register_simple_value (module Value)
