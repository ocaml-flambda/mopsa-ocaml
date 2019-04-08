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
type 'a dinfo = {
  domain : 'a domain;
  eq :  'b. 'b domain -> ('b,'a) eq option;
}

(** Pool of descriptors of domain identifiers *)
type dpool =
  | [] :   dpool
  | (::) : 'a dinfo * dpool -> dpool

let dpool : dpool ref = ref []

(** Register a new descriptor *)
let register_domain_id (dinfo:'a dinfo) =
  dpool := dinfo :: !dpool

(** Find descriptor of a domain identifier *)
let dfind (domain:'a domain) =
  let rec aux : type b. b domain -> dpool -> b dinfo =
    fun domain dpool ->
      match dpool with
      | [] -> raise Not_found
      | dinfo :: tl ->
        match dinfo.eq domain with
        | Some Eq -> dinfo
        | None -> aux domain tl
  in
  aux domain !dpool

(** Equality witness of domain identifiers *)
let deq (domain1:'a domain) (domain2:'b domain) : ('a,'b) eq option =
  let dinfo2 = dfind domain2 in
  dinfo2.eq domain1

(** Generator of a new domain identifier *)
module GenDomainId(Spec:sig
    type typ
    val name : string
  end) =
struct

  type _ domain += Domain : Spec.typ domain

  let id = Domain

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a domain -> (a,Spec.typ) eq option =
    function
    | Domain -> Some Eq
    | _ -> None

  let () =
    register_domain_id { domain = id; eq }

end


(****************************************************************************)
(**                         {2 Value identifiers}                           *)
(****************************************************************************)


(** Value identifier *)
type _ value = ..

(** Descriptor of a value identifier *)
type 'a vinfo = {
  value : 'a value;
  eq :  'b. 'b value -> ('b,'a) eq option;
}

(** Pool of descriptors of value identifiers *)
type vpool =
  | [] :   vpool
  | (::) : 'a vinfo * vpool -> vpool

let vpool : vpool ref = ref []

(** Register a new descriptor *)
let register_value_id (vinfo:'a vinfo) =
  vpool := vinfo :: !vpool

(** Find descriptor of a value identifier *)
let vfind (value:'a value) =
  let rec aux : type b. b value -> vpool -> b vinfo =
    fun value vpool ->
      match vpool with
      | [] -> raise Not_found
      | vinfo :: tl ->
        match vinfo.eq value with
        | Some Eq -> vinfo
        | None -> aux value tl
  in
  aux value !vpool

(** Equality witness of value identifiers *)
let veq (value1:'a value) (value2:'b value) : ('a,'b) eq option =
  let vinfo2 = vfind value2 in
  vinfo2.eq value1

(** Generator of a new value identifier *)
module GenValueId(Spec:sig
    type typ
    val name : string
    val display : string
  end) =
struct

  type _ value += Value : Spec.typ value

  let id = Value

  let name = Spec.name

  let display = Spec.display

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a value -> (a,Spec.typ) eq option =
    function
    | Value -> Some Eq
    | _ -> None

  let () =
    register_value_id { value = id; eq }

end

