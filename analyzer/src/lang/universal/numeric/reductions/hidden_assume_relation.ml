(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
   Reduction operator for refining the interval of a variable x when
   another variable y is filtered and a relation exists between the
   two variables x and y. 
*)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions.Post_reduction
open Framework.Domains.Reduced_product.Pool
open Ast

let name = "universal.numeric.reductions.hidden_assume_relation"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : REDUCTION =
struct

  module I = Values.Intervals.Value
  module O = Relational.Oct

  let trigger = None
  
  let reduce stmt dman nrman man flow : 'a flow =
    match skind stmt with
    | S_assume(e) ->
      (* Get variables involved explicitly in the filter statement *)
      let vars = Framework.Visitor.expr_vars e in

      let oct = Flow.get T_cur man flow |>
                dman.get_env O.id
      in

      (* Fold over the relations of the variables *)
      List.fold_left (fun acc v ->
          O.var_relations v oct |>
          List.fold_left (fun acc v' ->
              (* Get the interval of the variable [v] in relation with [v'] *)
              let cur = Flow.get T_cur man acc in
              let itv = nrman.get_var_value I.id v' cur in

              (* Compare [itv] with the interval in relational domains *)
              let itv' = O.get_interval v' oct in
              let cur' = nrman.set_var_value I.id v' (I.meet (get_annot flow) itv itv') cur in
              Flow.set T_cur cur' man acc
            ) acc
        ) flow vars
    | _ -> flow

end


let () =
  register_reduction name (module Reduction)
