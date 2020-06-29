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

open Eq


type _ id = ..

type _ id += I_stateful_domain : 'a id -> 'a id
type _ id += I_stateless_domain : 'a id -> 'a id
type _ id += I_value : 'a id -> 'a id


type 'a witness = {
  eq :  'b. 'b id -> ('b,'a) eq option;
}

(** Pool of witnesses *)
type pool =
  | [] :   pool
  | (::) : 'a witness * pool -> pool

type typed_pool =
  { mutable stateful : pool;
    mutable stateless: pool;
    mutable value: pool;
    mutable others: pool; }

let typed_pool =
  { stateful = [];
    stateless = [];
    value = [];
    others = []; }


let register_id (info:'a witness) =
  typed_pool.others <- info :: typed_pool.others

let register_stateful_id (info:'a witness) =
  typed_pool.stateful <- info :: typed_pool.stateful

let register_stateless_id (info:'a witness) =
  typed_pool.stateless <- info :: typed_pool.stateless

let register_value_id (info:'a witness) =
  typed_pool.value <- info :: typed_pool.value



let rec find_witnesss : type a. a id -> pool -> a witness = fun id pool ->
  match pool with
  | [] -> raise Not_found
  | info :: tl ->
    match info.eq id with
    | Some Eq -> info
    | None -> find_witnesss id tl


(** Equality witness of domain identifiers *)
let equal_id (id1:'a id) (id2:'b id) : ('a,'b) eq option =
  match id1, id2 with
  | I_stateful_domain i1, I_stateful_domain i2 ->
    let witness = find_witnesss i2 typed_pool.stateful in
    witness.eq i1

  | I_stateful_domain _, _ | _, I_stateful_domain _ -> None

  | I_stateless_domain i1, I_stateless_domain i2 ->
    let witness = find_witnesss i2 typed_pool.stateless in
    witness.eq i1

  | I_stateless_domain _, _ | _, I_stateless_domain _ -> None

  | I_value i1, I_value i2 ->
    let witness = find_witnesss i2 typed_pool.value in
    witness.eq i1

  | I_value _, _ | _, I_value _ -> None

  | _ ->
    let witness = find_witnesss id2 typed_pool.others in
    witness.eq id1


(** Generator of a new identifier *)

module GenId(Spec:sig
    type t
  end) =
struct

  type _ id += Id : Spec.t id

  let id = Id

  let eq : type a. a id -> (a,Spec.t) eq option =
    function
    | Id -> Some Eq
    | _ -> None

  let () =
    register_id { eq }

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

  let eq : type a. a id -> (a,Spec.t) eq option =
    function
    | Id -> Some Eq
    | _ -> None

  let () =
    register_stateful_id { eq }

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

  let eq : type a. a id -> (a,unit) eq option =
    function
    | Id -> Some Eq
    | _ -> None

  let () =
    register_stateless_id { eq }

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

  let eq : type a. a id -> (a,Spec.t) eq option =
    function
    | Id -> Some Eq
    | _ -> None

  let () =
    register_value_id { eq }

end
