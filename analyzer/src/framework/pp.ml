(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Pretty-printer of the AST. *)


open Format
open Ast


let rec pp_vkind_chain : (formatter -> var_kind -> unit) ref = ref (fun fmt vk ->
    match vk with
    | V_orig -> ()
    | _ -> failwith "Pp: Unknown variable kind"
  )

and register_pp_vkind pp = pp_vkind_chain := pp !pp_vkind_chain

let pp_var_kind fmt vk = !pp_vkind_chain fmt vk

let pp_var fmt v =
  match v.vkind with
  | V_orig -> fprintf fmt "%s@%d" v.vname v.vuid
  | _ -> fprintf fmt "%s:%a@%d" v.vname pp_var_kind v.vkind v.vuid

(* Processing chain for the extensible type [Ast.expr] *)
let rec pp_expr_chain : (Format.formatter -> expr -> unit) ref =
  ref (fun fmt expr ->
      match ekind expr with
      | E_constant c -> pp_constant fmt c
      | E_var(v) -> pp_var fmt v
      | E_unop(op, e) -> fprintf fmt "%a (%a)" pp_operator op pp_expr e
      | E_binop(op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_operator op pp_expr e2
      | _ -> failwith "Pp: Unknown expression"
    )

(* Processing chain for the extensible type [Ast.stmt] *)
and pp_stmt_chain : (Format.formatter -> stmt -> unit) ref =
  ref (fun fmt stmt ->
      match skind stmt with
      | S_program prog -> pp_program fmt prog
      | _ -> failwith "Pp: Unknown statement"
    )

(* Processing chain for the extensible type [Ast.program] *)
and pp_program_chain : (Format.formatter -> program -> unit) ref =
  ref (fun fmt prg ->
      failwith "Pp: Unknown program"
    )

(* Processing chain for the extensible type [Ast.stmt] *)
and pp_typ_chain : (Format.formatter -> typ -> unit) ref =
  ref (fun fmt typ ->
      match typ with
      | T_any -> fprintf fmt "?"
      | _ -> failwith "Pp: Unknown type"
    )

(* Processing chain for the extensible type [Ast.operator] *)
and pp_operator_chain : (Format.formatter -> operator -> unit) ref =
  ref (fun fmt op ->
      failwith "Pp: Unknown operator"
    )

(* Processing chain for the extensible type [Ast.constant] *)
and pp_constant_chain : (Format.formatter -> constant -> unit) ref =
  ref (fun fmt c ->
      failwith "Pp: Unknown constant"
    )



(* To register a new pp, we just give the previous chain as argument to pp
 * so that it can call the chain when it can not handle the given case
 *)

and register_pp_expr pp = pp_expr_chain := pp !pp_expr_chain
and register_pp_stmt pp = pp_stmt_chain := pp !pp_stmt_chain
and register_pp_program pp = pp_program_chain := pp !pp_program_chain
and register_pp_typ pp = pp_typ_chain := pp !pp_typ_chain
and register_pp_operator pp = pp_operator_chain := pp !pp_operator_chain
and register_pp_constant pp = pp_constant_chain := pp !pp_constant_chain

(* These functions start the chain processing *)
and pp_expr fmt expr = !pp_expr_chain fmt expr

and pp_stmt fmt stmt = !pp_stmt_chain fmt stmt

and pp_program fmt prg = !pp_program_chain fmt prg

and pp_typ fmt typ = !pp_typ_chain fmt typ

and pp_location fmt loc =
  Format.fprintf fmt "%d:%d" loc.loc_line loc.loc_column

and pp_location_verbose fmt loc =
  Format.fprintf fmt "file %s, line %d:%d"
    loc.loc_file loc.loc_line loc.loc_column

and pp_range fmt range =
  match range with
  | Range_origin r ->
    Format.fprintf fmt "%a-%a"
      pp_location r.range_begin pp_location r.range_end
  | Range_file f ->
    Format.fprintf fmt "%s" f
  | Range_fresh uid ->
    Format.fprintf fmt "!%d" uid
  | Range_tagged (t, r) ->
    Format.fprintf fmt "%a<%s>" pp_range r t

and pp_range_verbose fmt range =
  match range with
  | Range_origin r ->
    pp_location_verbose fmt r.range_begin
  | Range_file f ->
    Format.fprintf fmt "File %s" f
  | Range_fresh uid ->
    Format.fprintf fmt "!%d" uid
  | Range_tagged(t, r) ->
    fprintf fmt "%a<%s>" pp_range_verbose r t

and pp_operator fmt op = !pp_operator_chain fmt op

and pp_constant fmt c = !pp_constant_chain fmt c
