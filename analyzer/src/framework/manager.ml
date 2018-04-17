(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Lattice
open Flow

(**
   A manager provides to a domain:
   - the operators of global flow abstraction and its the underlying environment
   abstraction,
   - the accessor structure to its own abstraction
   - and the transfer functions of the top-level analyzer.
*)


let debug fmt = Debug.debug ~channel:"framework.manager" fmt

(*==========================================================================*)
(**                            {2 Evaluations}                              *)
(*==========================================================================*)


type ('a, 'b) eval_case = 'a option * 'b flow * Ast.stmt list
type ('a, 'b) evals = ('a, 'b) eval_case Dnf.t

let eval_singleton (case: ('a, 'b) eval_case) : ('a, 'b) evals =
  Dnf.singleton case

let eval_map
    (f: ('a, 'b) eval_case -> ('c, 'd) eval_case)
    (evl: ('a, 'b) evals) : ('c, 'd) evals
  =
  Dnf.map f evl

let eval_join (e1: ('a, 'b) evals) (e2: ('a, 'b) evals) : ('a, 'b) evals =
  Dnf.mk_or e1 e2

let eval_meet (e1: ('a, 'b) evals) (e2: ('a, 'b) evals) : ('a, 'b) evals =
  Dnf.mk_and e1 e2

let pp_evals print fmt (evals: ('a, 'b) evals) =
  let l = Dnf.to_list evals in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt conj ->
       Format.fprintf fmt "(%a)"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋀@;")
            (fun fmt (x, _, _) ->
               match x with
               | None -> Format.pp_print_string fmt "none"
               | Some x -> print fmt x
            )
         ) conj
    )
    fmt l


(*==========================================================================*)
(**                            {2 Accessors}                                *)
(*==========================================================================*)


(**
   An accessor of type [('a, 'b) accessor] allows retrieving/updating a domain
    abstraction of type ['b] within the global analyzer abstraction ['a]
*)
type ('a, 'b) accessor = {
  get : 'a -> 'b; (** Returns the domain's abstract element. *)
  set : 'b -> 'a -> 'a; (** Modifies the domain's abstract element and returns
                            the updated global abstraction . *)
}



(*==========================================================================*)
(**                            {2 Manager}                                  *)
(*==========================================================================*)


(** An instance of type [('a, 'b) manager] encapsulates the lattice operators
    of the global environment abstraction ['a] and its flow abstraction
    ['a Flow.t], the top-level transfer functions [exec], [eval] and [ask],
    and the accessor to the domain abstraction ['b] within ['a].
*)
type ('a, 'b) manager = {
  (** Environment abstraction. *)
  env : 'a lattice_manager;

  (** Flow abstraction. *)
  flow : 'a flow_manager;

  (** Statement transfer function. *)
  exec :
    Ast.stmt -> Context.context -> 'a flow ->
    'a flow;

  (** Expression transfer function. *)
  eval :
    Ast.expr -> Context.context -> 'a flow ->
    (Ast.expr, 'a) evals;

  (** Query transfer function. *)
  ask : 'r. 'r Query.query -> Context.context -> 'a flow -> 'r option;

  (** Domain accessor. *)
  ax : ('a, 'b) accessor;
}

(** Update the domain abstraction of the TCur flow *)
let map_domain_cur f man flow =
  let cur = man.flow.get TCur flow in
  let a = man.ax.get cur in
  let a' = f a in
  let cur' = man.ax.set a' cur in
  man.flow.set TCur cur' flow

let set_domain_cur a man flow =
  map_domain_cur (fun _ -> a) man flow

(** Retrieve the domain abstraction of the TCur flow *)
let get_domain_cur man flow =
    man.flow.get TCur flow |>
    man.ax.get

