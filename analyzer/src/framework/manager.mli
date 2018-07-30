(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Managers provide access to operators and transfer functions over the
   global abstract environment.
*)


(*==========================================================================*)
(**                            {2 Flows}                                    *)
(*==========================================================================*)

type token = ..
(** Flow tokens are used to tag abstract elements when encountered in a
    relevant control point *)

type token += T_cur
(** Token of current (active) execution flow *)

type token_info = {
  compare : (token -> token -> int) -> token -> token -> int;
  print   : (Format.formatter -> token -> unit) -> Format.formatter -> token -> unit;
}

val regiter_token : token_info -> unit

module FlowMap : sig
  include MapExtSig.S with type key = token
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
(** Map of flows binding tokens to abstract elements *)

type 'a fmap = 'a FlowMap.t Top.with_top

type 'a flow = {
  map   : 'a fmap;
  annot : 'a Annotation.t;
}
(** An abstract flow is a flow map augmented with an annotation *)

val set_annot : 'a Annotation.t -> 'a flow -> 'a flow
(** Change the annotation associated to a flow *)

val get_annot : 'a flow -> 'a Annotation.t
(** Returns the annotation associated to a flow *)


(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('a, 'e) evl_case = {
  expr : 'e option;
  flow: 'a flow;
  cleaners: Ast.stmt list;
}

type ('a, 'e) evl = ('a, 'e) evl_case Dnf.t



(*==========================================================================*)
(**                             {2 Managers}                                *)
(*==========================================================================*)



(**
   An instance of type [('a, 't) man] encapsulates the lattice
   operators of the global environment abstraction ['a], the top-level
   transfer functions [exec], [eval] and [ask], and the accessor to
   the domain abstraction ['t] within ['a].
*)
type ('a, 't) man = {
  (* Functions on the global abstract element *)
  bottom    : 'a;
  top       : 'a;
  is_bottom : 'a -> bool;
  subset    : 'a -> 'a -> bool;
  join      : 'a Annotation.t -> 'a -> 'a -> 'a;
  meet      : 'a Annotation.t -> 'a -> 'a -> 'a;
  widen     : 'a Annotation.t -> 'a -> 'a -> 'a;
  print     : Format.formatter -> 'a -> unit;

  (* Accessors to abstract element ['t] of the domain *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Transfer functions *)
  exec : ?zone:Zone.t -> Ast.stmt -> 'a flow -> 'a flow;
  eval : ?zone:(Zone.t * Zone.t) -> Ast.expr -> 'a flow -> ('a, Ast.expr) evl;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;
}
