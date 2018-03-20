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
type 'a cell = 'a key * 'a


(** Wrapped context cell. *)
type xcell = Cell : 'a cell -> xcell

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

(** Chain of witnesses. *)
let chain : chain ref = ref {
  check = (fun _ _ -> None)
}

(** An equality witness case, defined by domains. *)
type case = {
  case : 'a 'b. chain -> 'a key -> 'b key -> ('a, 'b) eq option
}

(** Add a witness case to the chain. *)
let register_key_equality (c : case) =
  let old = !chain in
  let check : type a b. a key -> b key -> (a, b) eq option =
    fun k1 k2 -> c.case old k1 k2
  in
  chain := {check}

(** Cast a cell to the type of a given key. *)
let cast : type a b. a key -> b cell -> a option =
  fun k1 (k2, v) ->
    match !chain.check k1 k2 with
    | None -> None
    | Some Eq -> Some v


(*==========================================================================*)
(**                        {2 Cell management}                              *)
(*==========================================================================*)


(** Empty context. *)
let empty : context = []

(** Create a context with a single cell. *)
let singleton : type a. a key -> a -> context =
  fun k v -> [Cell (k, v)]

(** Return the cell value of a given key. Raise [Not_found] if there is
    no cell associated to that key.
*)
let rec find : type a b. a key -> context -> a =
  fun k -> function
    | [] -> raise Not_found
    | hd :: tl ->
      let Cell (k', v) = hd in
      match cast k (k', v) with
      | Some v' -> v'
      | None -> find k tl

(** Remove the cell associated to a given key. The context is unchanged
    if the key is not present.
*)
let rec remove : type a. a key -> context -> context =
  fun k -> function
    | [] -> []
    | hd :: tl ->
      let Cell (k', v) = hd in
      match cast k (k', v) with
      | Some _ -> tl
      | None -> hd :: (remove k tl)

(** Add a new cell to a context. If a cell already exists with the given key,
    it is removed before the insertion.
*)
let add : type a. a key -> a -> context -> context =
  fun k v cells ->
    let cells' = remove k cells in
    Cell (k, v) :: cells'
