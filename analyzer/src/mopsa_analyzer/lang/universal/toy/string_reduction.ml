open Mopsa
open Ast
open Sig.Reduction.Eval

module Reduction =
  struct

    let name = "universal.toy.string_reduction"


    let reduce exp _ _ _ results flow =
      let rec aux acc flow = function
        | [] -> Eval.singleton acc flow
        | hd::tl ->
           match ekind acc, ekind hd with
           | E_var _, E_constant (C_int_interval _) -> aux acc flow tl
           | E_constant (C_int_interval _), E_var _ -> aux hd flow tl
           | _ -> aux acc flow tl
      in
      match results with
      | [] -> Some (Eval.empty flow)
      | hd::tl ->
         let r = aux hd flow tl in
         debug "reduce results=%a into %a" (Format.pp_print_list pp_expr) results Eval.print r;
         Some r

  end

let () = register_eval_reduction (module Reduction)
