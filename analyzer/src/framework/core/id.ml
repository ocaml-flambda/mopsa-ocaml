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


type 'a witness = {
  eq :  'b. 'b id -> ('b,'a) eq option;
}

(** Pool of witnesses *)
type pool =
  | [] :   pool
  | (::) : 'a witness * pool -> pool

let pool : pool ref = ref []


let register_id (info:'a witness) =
  pool := info :: !pool


let find_witnesss (id:'a id) =
  let rec aux : type b. b id -> pool -> b witness =
    fun id pool ->
      match pool with
      | [] -> raise Not_found
      | info :: tl ->
        match info.eq id with
        | Some Eq -> info
        | None -> aux id tl
  in
  aux id !pool

(** Equality witness of domain identifiers *)
let equal_id (id1:'a id) (id2:'b id) : ('a,'b) eq option =
  let info2 = find_witnesss id2 in
  info2.eq id1


(** Generator of a new identifier *)
module GenId(Spec:sig
    type t
    val name : string
  end) =
struct

  type _ id += Id : Spec.t id

  let id = Id

  let name = Spec.name

  let debug fmt = Debug.debug ~channel:Spec.name fmt

  let eq : type a. a id -> (a,Spec.t) eq option =
    function
    | Id -> Some Eq
    | _ -> None

  let () =
    register_id { eq }

end


module GenDomainId = GenId

(** Generator of a new identifier for stateless domains *)
module GenStatelessDomainId(Spec:sig
    val name : string
  end)
=
struct

  include GenId(struct
      type t = unit
      let name = Spec.name
    end)

end


(** Generator of a new value identifier *)
module GenValueId(Spec:sig
    type t
    val name : string
    val display : string
  end) =
struct

  include GenId(Spec)

  let display = Spec.display

end
