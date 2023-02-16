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


(** Generators of identifiers for domains and values *)

open Mopsa_utils
open Eq


type _ id = ..

type _ id += I_stateful_domain : 'a id -> 'a id
type _ id += I_stateless_domain : 'a id -> 'a id
type _ id += I_value : 'a id -> 'a id


type witness = {
  eq :  'a 'b. 'a id -> 'b id -> ('a,'b) eq option;
}

type witness_chain = {
  eq :  'a 'b. witness -> 'a id -> 'b id -> ('a,'b) eq option;
}

let empty_witness : witness = {
  eq = (fun _ _ -> None)
}

type pool =
  { mutable stateful : witness;
    mutable stateless: witness;
    mutable value: witness;
    mutable others: witness; }

let pool =
  { stateful = empty_witness;
    stateless = empty_witness;
    value = empty_witness;
    others = empty_witness; }


let register_id (w:witness_chain) =
  let old = pool.others in
  pool.others <- { eq = (fun (type a) (type b) (id1:a id) (id2:b id) -> w.eq old id1 id2) }

let register_stateful_id (w:witness_chain) =
  let old = pool.stateful in
  pool.stateful <- { eq = (fun (type a) (type b) (id1:a id) (id2:b id) -> w.eq old id1 id2) }

let register_stateless_id (w:witness_chain) =
  let old = pool.stateless in
  pool.stateless <- { eq = (fun (type a) (type b) (id1:a id) (id2:b id) -> w.eq old id1 id2) }

let register_value_id (w:witness_chain) =
  let old = pool.value in
  pool.value <- { eq = (fun (type a) (type b) (id1:a id) (id2:b id) -> w.eq old id1 id2) }


(** Equality witness of domain identifiers *)
let equal_id (id1:'a id) (id2:'b id) : ('a,'b) eq option =
  match id1, id2 with
  | I_stateful_domain i1, I_stateful_domain i2 ->
    pool.stateful.eq i1 i2

  | I_stateful_domain _, _ | _, I_stateful_domain _ -> None

  | I_stateless_domain i1, I_stateless_domain i2 ->
    pool.stateless.eq i1 i2

  | I_stateless_domain _, _ | _, I_stateless_domain _ -> None

  | I_value i1, I_value i2 ->
    pool.value.eq i1 i2

  | I_value _, _ | _, I_value _ -> None

  | _ -> pool.others.eq id1 id2


(** Generator of a new identifier *)

module GenId(Spec:sig
    type t
  end) =
struct

  type _ id += Id : Spec.t id

  let id = Id

  let () = register_id {
      eq = (
        let f : type a b. witness -> a id -> b id -> (a,b) eq option = fun next id1 id2 ->
          match id1, id2 with
          | Id, Id -> Some Eq
          | _      -> next.eq id1 id2
        in
        f
      );
    }

end

module GenDomainId(Spec:sig
    type t
    val name : string
  end) =
struct

  type _ id += Id : Spec.t id

  let id = I_stateful_domain Id

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let () = register_stateful_id {
      eq = (
        let f : type a b. witness -> a id -> b id -> (a,b) eq option = fun next id1 id2 ->
          match id1, id2 with
          | Id, Id -> Some Eq
          | _      -> next.eq id1 id2
        in
        f
      );
    }
end


(** Generator of a new identifier for stateless domains *)
module GenStatelessDomainId(Spec:sig
    val name : string
  end)
=
struct

  type _ id += Id : unit id

  let id = I_stateless_domain Id

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let () = register_stateless_id {
      eq = (
        let f : type a b. witness -> a id -> b id -> (a,b) eq option = fun next id1 id2 ->
          match id1, id2 with
          | Id, Id -> Some Eq
          | _      -> next.eq id1 id2
        in
        f
      );
    }

end


(** Generator of a new value identifier *)
module GenValueId(Spec:sig
    type t
    val name : string
    val display : string
  end) =
struct

  type _ id += Id : Spec.t id

  let id = I_value Id

  let name = Spec.name

  let display = Spec.display

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let () = register_value_id {
      eq = (
        let f : type a b. witness -> a id -> b id -> (a,b) eq option = fun next id1 id2 ->
          match id1, id2 with
          | Id, Id -> Some Eq
          | _      -> next.eq id1 id2
        in
        f
      );
    }
end
