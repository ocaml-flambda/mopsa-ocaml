(**
  C_simplify - C AST to C AST simplification

  We optionnal apply a set of transformations to remove some constuctions
  and replace them with simpler ones. 
  One application is to ensure that each expression has at most one 
  side-effect (a call or an assignment).

  Note: these transormations make hepotheses on the order of evaluation of
  side-effects in expression.
  TODO: use a represantation that is more neutral to evaluation order
  (a partial order?)
  

  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.
  @author Antoine Miné
*)


open C_AST
       
type context
(** Global context for the simplification procedure. *)

val create_context : unit -> context
(** Creates a new context. *)

val simplify_func : context -> func -> unit
(** Simplifies (in-place) a single function. *)
                                         
