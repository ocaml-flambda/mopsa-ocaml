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
type _ did = ..

(** Descriptor of a domain identifier *)
type 'a dinfo = {
  did : 'a did;
  eq :  'b. 'b did -> ('b,'a) eq option;
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
let dfind (did:'a did) =
  let rec aux : type b. b did -> dpool -> b dinfo =
    fun did dpool ->
      match dpool with
      | [] -> raise Not_found
      | dinfo :: tl ->
        match dinfo.eq did with
        | Some Eq -> dinfo
        | None -> aux did tl
  in
  aux did !dpool

(** Equality witness of domain identifiers *)
let deq (did1:'a did) (did2:'b did) : ('a,'b) eq option =
  let dinfo2 = dfind did2 in
  dinfo2.eq did1

(** Generator of a new domain identifier *)
module GenDomainId(Spec:sig
    type typ
    val name : string
  end) =
struct

  type _ did += DId : Spec.typ did

  let id = DId

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a did -> (a,Spec.typ) eq option =
    function
    | DId -> Some Eq
    | _ -> None

  let () =
    register_domain_id { did = id; eq }

end


(****************************************************************************)
(**                         {2 Value identifiers}                           *)
(****************************************************************************)


(** Value identifier *)
type _ vid = ..

(** Descriptor of a value identifier *)
type 'a vinfo = {
  vid : 'a vid;
  eq :  'b. 'b vid -> ('b,'a) eq option;
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
let vfind (vid:'a vid) =
  let rec aux : type b. b vid -> vpool -> b vinfo =
    fun vid vpool ->
      match vpool with
      | [] -> raise Not_found
      | vinfo :: tl ->
        match vinfo.eq vid with
        | Some Eq -> vinfo
        | None -> aux vid tl
  in
  aux vid !vpool

(** Equality witness of value identifiers *)
let veq (vid1:'a vid) (vid2:'b vid) : ('a,'b) eq option =
  let vinfo2 = vfind vid2 in
  vinfo2.eq vid1

(** Generator of a new value identifier *)
module GenValueId(Spec:sig
    type typ
    val name : string
    val display : string
  end) =
struct

  type _ vid += VId : Spec.typ vid

  let id = VId

  let name = Spec.name

  let display = Spec.display

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a vid -> (a,Spec.typ) eq option =
    function
    | VId -> Some Eq
    | _ -> None

  let () =
    register_value_id { vid = id; eq }

end

