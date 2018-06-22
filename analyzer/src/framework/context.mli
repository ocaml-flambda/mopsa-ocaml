(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(**
   Heterogenous data structure to store/retrieve extra information about
   the analysis state.

   To allow the context supporting a new type of information, extend the [key]
   type by providing a name and a type of the new content. For example:
   {[type _ key +=
     | KCurFuncName : string key
     | KCurLine : int key
   ]}

   The extraction process of cells values requires the definition of
   a witness of key equality as follows:
   {[
     register_key_equality {
       case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
                 fun chain k1 k2 ->
                   match k1, k2 with
                   | KCurFuncName, KCurFuncName -> Some Eq
                   | KCurLine, KCurLine -> Some Eq
                   | _ -> chain.check k1 k2
               in
               f);
     }
   ]}

   After that, given a context [ctx : context], inserting/retrieving values
   from it can be done as follows:
   {[
     let ctx = add KCurFuncName "toto" ctx
     let ctx = add KCurLine 240 ctx
     let f = find KCurFuncName ctx
     let l = find KCurLine ctx
     let ctx = remove KCurFuncName ctx
   ]}
*)

(*==========================================================================*)
(**                         {2 Data structure}                              *)
(*==========================================================================*)


(** Extensible GADT for cells keys. *)
type _ key = ..

(** A context cell with a key and a content. *)
type 'a cell

(** Wrapped context cell. *)
type xcell

(** A context is a list of wrapped cells. *)
type context = xcell list


(*==========================================================================*)
(**                   {2 Keys equality constraint}                          *)
(*==========================================================================*)

(** Key equality witness. *)
type (_, _) eq = Eq : ('a, 'a) eq

(** [chain.check k1 k2] verifies that the type of both keys is equal. *)
type chain = {
  check : 'a 'b. 'a key -> 'b key -> ('a, 'b) eq option
}

(** An equality witness case, defined by domains. *)
type case = {
  case : 'a 'b. chain -> 'a key -> 'b key -> ('a, 'b) eq option
}

(** Add a witness case to the chain. *)
val register_key_equality : case -> unit
  
(** Cast a cell to the type of a given key. *)
val cast : 'a key -> 'b cell -> 'a option 

(*==========================================================================*)
(**                        {2 Cell management}                              *)
(*==========================================================================*)


(** Empty context. *)
val empty : context
  
(** Create a context with a single cell. *)
val singleton : 'a key -> 'a -> context

(** Return the cell value of a given key. Raise [Not_found] if there is
    no cell associated to that key.
*)
val find : 'a key -> context -> 'a

(** Remove the cell associated to a given key. The context is unchanged
    if the key is not present.
*)
val remove : 'a key -> context -> context

(** Add a new cell to a context. If a cell already exists with the given key,
    it is removed before the insertion.
*)
val add : 'a key -> 'a -> context -> context
