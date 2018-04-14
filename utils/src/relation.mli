(**
  Relation - Relations (or multimaps) between ordered sets.

  Copyright (C) 2018 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)

open RelationSig

module Make(Dom: OrderedType)(CoDom: OrderedType) :
S with
         type dom = Dom.t and
         type codom = CoDom.t and
         module CoDomSet = SetExt.Make(CoDom)
(** Generic functor to build a relation data-type from an ordered
    domain to an ordered codomain.
 *)
     
