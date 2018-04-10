(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Domains with global view on the full abstraction.

   This is the low-level domain signature that gives full access to the
   overall flow abstraction and analysis manager.
*)

open Lattice
open Flow
open Manager

(** {Optional flows} *)

let fail = None

let return x = Some x

let oflow_extract = Option.none_to_exn

let oflow_extract_dfl dfl = function
  | None -> dfl
  | Some flow -> flow

let oflow_join man =
  Option.option_neutral2 man.flow.join


let oflow_map f flow = Option.option_lift1 f flow

let oflow_merge f1 f2 f12 none flow1 flow2 = Option.option_apply2 f1 f2 f12 none flow1 flow2

(*==========================================================================*)
(**                       {2 Optional evaluations}                          *)
(*==========================================================================*)

let oeval_singleton (ev: ('a, 'b) eval_case) : ('a, 'b) evals option =
  Some (eval_singleton ev)

let oeval_map
    (f: ('a, 'b) eval_case -> ('c, 'd) eval_case)
    (oevl: ('a, 'b) evals option) : ('c, 'd) evals option
  =
  Option.option_lift1 (eval_map f) oevl

let oeval_join
    (oevl1: ('a, 'b) evals option)
    (oevl2: ('a, 'b) evals option) : ('a, 'b) evals option
  =
  Option.option_neutral2 eval_join oevl1 oevl2

let oeval_meet
    (oevl1: ('a, 'b) evals option)
    (oevl2: ('a, 'b) evals option) : ('a, 'b) evals option
  =
  Option.option_neutral2 eval_meet oevl1 oevl2


let oeval_merge
    (f: ('a, 'b) eval_case -> 'c)
    (join: 'c -> 'c -> 'c)
    (meet: 'c -> 'c -> 'c)
    (none: unit -> 'c)
    (oevl: ('a, 'b) evals option) : 'c
  =
  Option.option_dfl1 none (Dnf.substitute f join meet) oevl

let oeval_merge2
    (f1: ('a, 'b) evals -> 'e)
    (f2: ('c, 'd) evals -> 'e)
    (f12: ('a, 'b) evals -> ('c, 'd) evals -> 'e)
    (none: unit -> 'e)
    (oevl1: ('a, 'b) evals option) (oevl2: ('c, 'd) evals option) : 'e =
  Option.option_apply2 f1 f2 f12 none oevl1 oevl2


let oeval_substitute
    (f: ('a, 'b) eval_case -> ('c, 'd) evals option)
    (oevl: ('a, 'b) evals option) : ('c, 'd) evals option =
  oeval_merge f
    (oeval_merge2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_join evl1 evl2))
       (fun () -> None)
    )
    (oeval_merge2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_meet evl1 evl2))
       (fun () -> None)
    )
    (fun () -> None)
    oevl


(*==========================================================================*)
                        (** {2 Standlone domains} *)
(*==========================================================================*)

(** Low level abstract domain. *)
module type DOMAIN =
sig

  include Lattice.LATTICE

  val init :
    Ast.program -> ('a, t) manager -> Context.context -> 'a flow ->
    Context.context * 'a flow

  (** Abstract transfer function of statements. *)
  val exec:
    Ast.stmt -> ('a, t) manager -> Context.context -> 'a flow ->
    'a flow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval:
    Ast.expr -> ('a, t) manager -> Context.context -> 'a flow ->
    (Ast.expr, 'a) evals option

  (** Handler of generic queries. *)
  val ask:
    'r Query.query -> ('a, t) manager -> Context.context -> 'a flow ->
    'r option
end


(** Low level functor abstract domain. *)
module type FUNCTOR = functor(_ : DOMAIN) -> DOMAIN


module EmptyDomain : DOMAIN =
struct
  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let is_top _ = true
  let leq _ _ = true
  let unify _ a1 a2 = (a1, a2)
  let join _ _ = top
  let meet _ _ = top
  let widening _ _ _ = top
  let print _ _ = ()
  let init _ _ ctx x = ctx, x
  let exec _ _ _ _ = None
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None
end

let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains

let () = register_domain "empty" (module EmptyDomain)


let functors : (string * (module FUNCTOR)) list ref = ref []

let register_functor name modl =
  functors := (name, modl) :: !functors

let find_functor name = List.assoc name !functors

let mk_lattice_manager (type a) (domain: (module DOMAIN with type t = a)) : a lattice_manager =
  let module Domain = (val domain) in
  {
    bottom = Domain.bottom;
    top = Domain.top;
    is_bottom = Domain.is_bottom;
    is_top = Domain.is_top;
    leq = Domain.leq;
    join = Domain.join;
    meet = Domain.meet;
    widening = Domain.widening;
    print = Domain.print;
  }


(*==========================================================================*)
                        (** {2 Stack domains} *)
(*==========================================================================*)


(** Low level stack abstract domain. *)
module type STACK_DOMAIN =
  functor(Sub: DOMAIN) ->
  sig
    include Lattice.LATTICE

    val init :
      Ast.program -> ('a, t) manager -> ('a, Sub.t) manager -> Context.context -> 'a flow ->
      Context.context * 'a flow

    (** Abstract transfer function of statements. *)
    val exec:
      Ast.stmt -> ('a, t) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      'a flow option

    (** Abstract (symbolic) evaluation of expressions. *)
    val eval:
      Ast.expr -> ('a, t) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      (Ast.expr, 'a) evals option

    (** Handler of generic queries. *)
    val ask:
      'r Query.query -> ('a, t) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      'r option

  end


module MakeStack =
  functor(Domain: DOMAIN) ->
  functor(Sub: DOMAIN) ->
  struct
    type t = Domain.t
    let bottom = Domain.bottom
    let top = Domain.top
    let is_bottom = Domain.is_bottom
    let is_top = Domain.is_top
    let leq = Domain.leq
    let join = Domain.join
    let meet = Domain.meet
    let widening = Domain.widening
    let print = Domain.print

    let init _ _ _ ctx x = ctx, x

    let exec stmt man subman ctx gabs =
      Domain.exec stmt man ctx gabs

    let eval exp man subman ctx gabs =
      Domain.eval exp man ctx gabs

    let ask request man subman ctx gabs =
      Domain.ask request man ctx gabs
  end

let stack_domains : (string * (module STACK_DOMAIN)) list ref = ref []

let register_stack_domain name modl =
  stack_domains := (name, modl) :: !stack_domains

let find_stack_domain name =
  List.assoc name !stack_domains
