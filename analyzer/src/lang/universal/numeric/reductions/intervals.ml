(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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

(** Reduction operator for computing the most precise interval in a
    post-condition and inform the intervals domain about it.
*)

open Mopsa
open Core.Sig.Domain.Reduction


module Reduction =
struct

  let name = "universal.numeric.reductions.intervals"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Values.Intervals.Integer.Value

  (** Get the list of modified variables *)
  let modified_vars stmt =
    match skind stmt with
    | S_assign({ekind = E_var (v, _)}, _) -> [v]
    | S_assume e -> Visitor.expr_vars e
    | _ -> []

  (** Reduction operator *)
  let reduce stmt (man: 'a man) (a: 'a) : 'a =
    (* Get the modified variables *)
    let vars = modified_vars stmt in

    (* Refine the interval of each variable *)
    List.fold_left (fun a var ->
        (* Get the interval of var in the box domain *)
        let itv = man.get_value I.id var a in

        (* Get the interval in all domain *)
        let itv' = man.ask (I.Q_interval (mk_var var stmt.srange)) a in

        (* Check if box is less precise *)
        if not (I.subset itv itv')
        then
          let () = debug "interval of %a reduced from %a to %a" pp_var var I.print itv I.print itv' in
          man.set_value I.id var itv' a
        else a

      ) a vars

end


let () =
  register_reduction (module Reduction)
