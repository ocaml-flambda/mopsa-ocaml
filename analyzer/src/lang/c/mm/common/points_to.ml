(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Common constructs for points-to evaluations. *)

open Mopsa
open Base

(* Points-to results *)
(* ================= *)

type points_to =
  | P_fun of Ast.c_fundec
  | P_block of base (** base *) * expr (** offset *)
  | P_null
  | P_invalid
  | P_top

let pp_points_to fmt = function
  | P_fun f -> Format.fprintf fmt "(fp %a)" pp_var f.Ast.c_func_var
  | P_block(base, offset) -> Format.fprintf fmt "(%a, %a)" pp_base base pp_expr offset
  | P_null -> Format.pp_print_string fmt "NULL"
  | P_invalid -> Format.pp_print_string fmt "Invalid"
  | P_top -> Format.pp_print_string fmt "⊺"

let compare_points_to p1 p2 =
  match p1, p2 with
  | P_fun f1, P_fun f2 -> compare_var f1.Ast.c_func_var f2.Ast.c_func_var
  | P_block (b1, o1), P_block (b2, o2) ->
    Compare.compose [
      (fun () -> compare_base b1 b2);
      (fun () -> compare_expr o1 o2);
    ]
  | _, _ -> Pervasives.compare p1 p2


type expr_kind +=
  | E_c_points_to of points_to  (* Reply to a points-to evaluation *)

let mk_c_points_to_bloc b o range =
  mk_expr (E_c_points_to (P_block (b, o))) range

let mk_c_points_to_top range =
  mk_expr (E_c_points_to P_top) range

let mk_c_points_to_null range =
  mk_expr (E_c_points_to P_null) range

let mk_c_points_to_invalid range =
  mk_expr (E_c_points_to P_invalid) range

let mk_c_points_to_fun f range =
  mk_expr (E_c_points_to (P_fun f)) range


let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_c_points_to p1, E_c_points_to p2 -> compare_points_to p1 p2

        | _ -> next e1 e2
      );
    print = (fun next fmt e ->
        match ekind e with
        | E_c_points_to p -> Format.fprintf fmt "⇝ %a" pp_points_to p
        | _ -> next fmt e
      );
    visit = (fun next e ->
        let open Framework.Visitor in
        match ekind e with
        | E_c_points_to p -> leaf e (* FIXME: do we need to visit the offset expression? *)
        | _ -> next e
      )
  };
  ()

type zone +=
  | Z_c_points_to

let () =
  register_zone {
    zone = Z_c_points_to;
    name = "C/Points-To";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_c_points_to _ -> Keep
        | _ -> Process
      );
  }
