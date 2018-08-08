(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for integer intervals and octagon. *)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions.State_reduction
open Framework.Domains.Reduced_product.Pool
open Ast

let name = "universal.numeric.reductions.hidden_post_assume_relation"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : REDUCTION =
struct

  module I = Values.Intervals.Value
  module O = Relational.Oct
  
  let reduce stmt dman nman man flow : 'a flow =
    match skind stmt with
    | S_assume(e) ->
      let vars = Framework.Visitor.expr_vars e in
      let oct = Flow.get T_cur man flow |>
                dman.get_state O.id
      in
      List.fold_left (fun acc v ->
          O.var_relations v oct |>
          List.fold_left (fun acc v' ->
              let cur = Flow.get T_cur man acc in
              let itv = nman.get I.id v' cur in
              let itv' = O.interval v' oct in
              let cur' = nman.set I.id v' (I.meet (get_annot flow) itv itv') cur in
              Flow.set T_cur cur' man acc
            ) acc
        ) flow vars
    | _ -> flow

end


let () =
  register_reduction name (module Reduction)
