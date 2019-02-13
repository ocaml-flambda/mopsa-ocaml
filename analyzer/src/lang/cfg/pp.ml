(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(** Pretty-printing for CFG. *)

open Mopsa
open Universal.Ast
open Ast

  
(*==========================================================================*)
                         (** {2 Text printing} *)
(*==========================================================================*)

   
let cfg_printer = {
    CFG.print_node = (fun fmt n ->
      Format.fprintf fmt "%a:@;" pp_node_id (CFG.node_id n)
    );
    CFG.print_edge = (fun fmt e ->
      Format.fprintf fmt "  @[<v>%a@]@;" pp_stmt (CFG.edge_data e)
    );
    CFG.print_src = (fun fmt n port e -> 
      Format.fprintf
        fmt "  %a --[%a]-->@;"
        pp_node_id (CFG.node_id n) pp_token port
    );
    CFG.print_dst = (fun fmt e port n -> 
      Format.fprintf
        fmt "  --[%a]--> %a@;"
        pp_token port pp_node_id (CFG.node_id n)
    );
    CFG.print_entry = (fun fmt n port ->
      Format.fprintf
        fmt "  entry --[%a]--> %a@;"
        pp_token port pp_node_id (CFG.node_id n)
    );
    CFG.print_exit = (fun fmt n port ->
      Format.fprintf
        fmt "  %a --[%a]--> exit@;"
        pp_node_id (CFG.node_id n) pp_token port
    );
  }

           
let () =
  register_stmt_pp (fun next fmt s ->
      match s.skind with
      | S_cfg g ->
         Format.fprintf fmt "@[<v>%a@]" (CFG.print cfg_printer) g.cfg_graph
      | S_test e ->
         Format.fprintf fmt "test %a" pp_expr e
      | S_skip ->
         Format.pp_print_string fmt "skip"
      | _ -> next fmt s
    )
  

let () =
  register_token
    { compare = (fun next t1 t2 ->
        match t1, t2 with
        | T_node l1, T_node l2 -> compare_node_id l1 l2
        | T_true, T_true -> 0
        | T_false, T_false -> 0
        | _ -> next t1 t2
      );
      print = (fun next fmt t ->
        match t with
        | T_node l -> pp_node_id fmt l
        | T_true -> Format.pp_print_string fmt "true"
        | T_false -> Format.pp_print_string fmt "false"
        | _ -> next fmt t
      );
    }


    
(*==========================================================================*)
                        (** {2 DOT output} *)
(*==========================================================================*)

let pp_location = Location.pp_position
  
let dot_printer =
  { CFG.dot_pp_node =
      (fun fmt n -> pp_location fmt (node_loc (CFG.node_id n)));
    CFG.dot_pp_edge =
      (fun fmt e -> pp_stmt fmt (CFG.edge_data e));
    CFG.dot_pp_port =
      (fun fmt t -> pp_token fmt t);
    CFG.dot_filter_node = (fun x -> true);
    CFG.dot_filter_edge = (fun x -> true);
    CFG.dot_filter_port = (fun x -> true);
  }


(** Prints the graph in dot format in the specified filename. *)
let output_dot (title:string) (filename:string) (cfg:cfg) : unit =
  let f = open_out filename in
  let fmt = Format.formatter_of_out_channel f in
  CFG.print_dot dot_printer title fmt cfg.cfg_graph;
  Format.pp_print_flush fmt ();
  close_out f

  
  

  
