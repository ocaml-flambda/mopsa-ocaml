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


(****************************************************************************)
(**                        {2 Domain identifiers}                           *)
(****************************************************************************)

(** Domain identifier *)
type _ domain = ..

(** Descriptor of a domain identifier *)
type 'a domain_info = {
  eq :  'b. 'b domain -> ('b,'a) eq option;
}

(** Pool of descriptors of domain identifiers *)
type domain_pool =
  | [] :   domain_pool
  | (::) : 'a domain_info * domain_pool -> domain_pool

let domain_pool : domain_pool ref = ref []

(** Register a new descriptor *)
let register_domain_id (info:'a domain_info) =
  domain_pool := info :: !domain_pool

(** Find descriptor of a domain identifier *)
let find_domain (domain:'a domain) =
  let rec aux : type b. b domain -> domain_pool -> b domain_info =
    fun domain domain_pool ->
      match domain_pool with
      | [] -> raise Not_found
      | domain_info :: tl ->
        match domain_info.eq domain with
        | Some Eq -> domain_info
        | None -> aux domain tl
  in
  aux domain !domain_pool

(** Equality witness of domain identifiers *)
let domain_id_eq (domain1:'a domain) (domain2:'b domain) : ('a,'b) eq option =
  let domain_info2 = find_domain domain2 in
  domain_info2.eq domain1

(** Generator of a new domain identifier *)
module GenDomainId(Spec:sig
    type t
    val name : string
  end) =
struct

  type _ domain += Domain : Spec.t domain

  let id = Domain

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a domain -> (a,Spec.t) eq option =
    function
    | Domain -> Some Eq
    | _ -> None

  let () =
    register_domain_id { eq }

end


(** Generator of a new identifier for stateless domains *)
module GenStatelessDomainId(Spec:sig
    val name : string
  end) =
struct

  type _ domain += Domain : unit domain

  let id = Domain

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a domain -> (a,unit) eq option =
    function
    | Domain -> Some Eq
    | _ -> None

  let () =
    register_domain_id { eq }

end


(****************************************************************************)
(**                         {2 Value identifiers}                           *)
(****************************************************************************)


(** Value identifier *)
type _ value = ..

(** Descriptor of a value identifier *)
type 'a value_info = {
  eq :  'b. 'b value -> ('b,'a) eq option;
}

(** Pool of descriptors of value identifiers *)
type value_pool =
  | [] :   value_pool
  | (::) : 'a value_info * value_pool -> value_pool

let value_pool : value_pool ref = ref []

(** Register a new descriptor *)
let register_value_id (value_info:'a value_info) =
  value_pool := value_info :: !value_pool

(** Find descriptor of a value identifier *)
let find_value (value:'a value) =
  let rec aux : type b. b value -> value_pool -> b value_info =
    fun value value_pool ->
      match value_pool with
      | [] -> raise Not_found
      | value_info :: tl ->
        match value_info.eq value with
        | Some Eq -> value_info
        | None -> aux value tl
  in
  aux value !value_pool

(** Equality witness of value identifiers *)
let value_id_eq (value1:'a value) (value2:'b value) : ('a,'b) eq option =
  let value_info2 = find_value value2 in
  value_info2.eq value1

(** Generator of a new value identifier *)
module GenValueId(Spec:sig
    type t
    val name : string
    val display : string
  end) =
struct

  type _ value += Value : Spec.t value

  let id = Value

  let name = Spec.name

  let display = Spec.display

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a value -> (a,Spec.t) eq option =
    function
    | Value -> Some Eq
    | _ -> None

  let () =
    register_value_id { eq }

end
