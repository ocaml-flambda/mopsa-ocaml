(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Pretty-printing for CFG. *)

open Framework.Essentials
open Universal.Ast
open Ast


let cfg_printer = {
    CFG.print_node = (fun fmt n ->
      Format.fprintf fmt "%a:@;" pp_location (CFG.node_id n)
    );
    CFG.print_edge = (fun fmt e ->
      Format.fprintf fmt "  @[<v>%a@]@;" pp_stmt (CFG.edge_data e)
    );
    CFG.print_src = (fun fmt n port e -> 
      Format.fprintf
        fmt "  %a --[%a]-->@;"
        pp_location (CFG.node_id n) pp_token port
    );
    CFG.print_dst = (fun fmt e port n -> 
      Format.fprintf
        fmt "  --[%a]--> %a@;"
        pp_token port pp_location (CFG.node_id n)
    );
    CFG.print_entry = (fun fmt n port ->
      Format.fprintf
        fmt "  entry --[%a]--> %a@;"
        pp_token port pp_location (CFG.node_id n)
    );
    CFG.print_exit = (fun fmt n port ->
      Format.fprintf
        fmt "  %a --[%a]--> exit@;"
        pp_location (CFG.node_id n) pp_token port
    );
  }

           
let () =
  register_pp_stmt (fun next fmt s ->
      match s.skind with
      | S_cfg g -> Format.fprintf fmt "@[<v>%a@]" (CFG.print cfg_printer) g
      | S_test e -> Format.fprintf fmt "test %a" pp_expr e
      | S_skip -> Format.pp_print_string fmt "skip"
      | _ -> next fmt s
    )
  

let () =
  register_token
    { compare = (fun next t1 t2 ->
        match t1, t2 with
        | T_loc l1, T_loc l2 -> compare_location l1 l2
        | T_true, T_true -> 0
        | T_false, T_false -> 0
        | _ -> next t1 t2
      );
      print = (fun next fmt t ->
        match t with
        | T_loc l -> pp_location fmt l
        | T_true -> Format.pp_print_string fmt "true"
        | T_false -> Format.pp_print_string fmt "true"
        | _ -> next fmt t
      );
    }
