(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Visitors for CFG. *)

open Framework.Essentials
open Framework.Visitor
open Universal.Ast
open Ast
   

let () =
    register_stmt_visitor (fun default stmt ->
      match skind stmt with
      | S_skip ->
         leaf stmt

      | S_test e ->
         { exprs = [e]; stmts = []; },
         (function { exprs = [e] } -> { stmt with skind = S_test e }
         | _ -> failwith "invalid S_test visitor")

      | S_cfg c ->
         let cfg = c.cfg_graph in
         (* get edges *)
         let l = CFG.edge_list cfg in

         (* construct map from edge id to index in l *)
         let idxmap = Hashtbl.create 16 in
         let rec mkmap i l = match l with
           | [] -> ()
           | a::b -> Hashtbl.add idxmap (CFG.edge_id a) i; mkmap (i+1) b
         in
         mkmap 0 l;
         
         { exprs = []; stmts = List.map CFG.edge_data l; },
         (function { stmts } ->
            (* copy *)
            let cfg = CFG.clone cfg in
            (* update statements *)
            CFG.iter_edges
              (fun id e ->
                let d = List.nth stmts (Hashtbl.find idxmap id) in
                CFG.edge_set_data e d
              )
              cfg;
            mk_cfg { c with cfg_graph = cfg; } (srange stmt)
         )
         
      | _ -> default stmt
      )
  
