(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cache of post-conditions and evaluations *)

open Manager
open Ast

let debug fmt = Debug.debug ~channel:"framework.cache" fmt

module Make(Domain: sig type t end) =
struct
  let exec_cache : ((stmt * Domain.t flow) * Domain.t flow) list ref = ref []

  let eval_cache : ((expr * Domain.t flow) * (Domain.t, expr) evl) list ref = ref []

  let add_to_cache : type a. a list ref -> a -> unit =
    fun cache x ->
      cache := x :: (
          if List.length !cache < Options.(common_options.cache) then !cache
          else List.rev @@ List.tl @@ List.rev !cache
        )

  let exec (f: stmt -> Domain.t flow -> Domain.t flow) stmt (flow: Domain.t flow) : Domain.t flow =
    if Options.(common_options.cache) == 0 then
      f stmt flow
    else
      try
        let ret = List.assoc (stmt, flow) !exec_cache in
        debug "exec from cache";
        ret
      with Not_found ->
        let flow' = f stmt flow in
        add_to_cache exec_cache ((stmt, flow), flow');
        flow'

  let eval (f: expr -> Domain.t flow -> (Domain.t, expr) evl) exp (flow:Domain.t flow) : (Domain.t, expr) evl =
    if Options.(common_options.cache) == 0 then f exp flow
    else
      try
        let ret = List.assoc (exp, flow) !eval_cache in
        debug "eval from cache";
        ret
      with Not_found ->
        let evals = f exp flow in
        add_to_cache eval_cache ((exp, flow), evals);
        Eval.iter (fun case ->
            match case.expr with
            | Some e -> add_to_cache eval_cache ((e, flow), Eval.singleton e flow);
            | None -> ()
          ) evals;
        evals

end
