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

(** Flow-insensitive context *)

open Eq


(****************************************************************************)
(**                          {2 Unit contexts}                              *)
(****************************************************************************)

(** Key of a unit context value *)
type _ ukey = ..

(** Descriptor of a unit context key *)
type 'v udesc = {
  eq : 'vv. 'vv ukey -> ('vv, 'v) eq option;
  print : Format.formatter -> 'v -> unit;
}

(** Pool of unit context descriptors *)
type upool =
  | [] : upool
  | (::) : 'v udesc * upool -> upool

let upool : upool ref = ref []


let register_udesc udesc =
  upool := udesc :: !upool


(** Generate a unit (stateless) key *)
module GenUnitKey(V:sig
    type t
    val print : Format.formatter -> t -> unit
  end)
=
struct

  type _ ukey += UKey : V.t ukey

  let key = UKey

  let eq : type v. v ukey -> (v, V.t) eq option =
    function
    | UKey -> Some Eq
    | _ -> None

  let () =
    register_udesc {
      eq = eq;
      print = V.print;
    }

end


type uctx =
  | [] : uctx
  | (::) : ('v ukey * 'v) * uctx -> uctx


let find_udesc (k: 'v ukey) () : 'v udesc =
  let rec iter : type v. v ukey -> upool -> v udesc =
    fun k -> function
      | [] -> raise Not_found
      | hd :: tl ->
        match hd.eq k with
        | Some Eq -> hd
        | None -> iter k tl
  in
  iter k !upool

let ufind (k: 'v ukey) (uctx:uctx) : 'v =
  let udesc = find_udesc k () in
  let rec iter : type v. v ukey -> v udesc -> uctx -> v =
    fun k udesc -> function
      | [] -> raise Not_found
      | (k',v) :: tl ->
        match udesc.eq k' with
        | Some Eq -> v
        | None -> iter k udesc tl
  in
  iter k udesc uctx

let uempty : uctx = []

let uadd (k:'v ukey) (v:'v) (uctx:uctx) : uctx =
  let rec iter : type v. v ukey -> v -> v udesc -> uctx -> uctx =
    fun k v udesc -> function
      | [] -> [(k, v)]
      | hd :: tl ->
        let (k', _) = hd in
        match udesc.eq k' with
        | Some Eq -> (k, v) :: tl
        | None -> hd :: (iter k v udesc tl)
  in
  iter k v (find_udesc k ()) uctx


let umem (k:'v ukey) (uctx:uctx) : bool =
  let rec iter : type v. v ukey -> v udesc -> uctx -> bool =
    fun k udesc -> function
      | [] -> false
      | hd :: tl ->
        let (k', _) = hd in
        match udesc.eq k' with
        | Some Eq -> true
        | None -> iter k udesc tl
  in
  iter k (find_udesc k ()) uctx


let uremove (k:'v ukey) (uctx:uctx) : uctx =
  let rec iter : type v. v ukey -> v udesc -> uctx -> uctx =
    fun k udesc -> function
      | [] -> []
      | hd :: tl ->
        let (k', _) = hd in
        match udesc.eq k' with
        | Some Eq -> tl
        | None -> hd :: iter k udesc tl
  in
  iter k (find_udesc k ()) uctx


let uprint fmt uctx =
  let rec iter fmt uctx =
    match uctx with
    | [] -> ()
    | [(k,v)] ->
      let udesc = find_udesc k () in
      Format.fprintf fmt "%a" udesc.print v
    | (k,v) :: tl ->
      let udesc = find_udesc k () in
      Format.fprintf fmt "%a@\n%a" udesc.print v iter tl
  in
  iter fmt uctx



(****************************************************************************)
(**                       {2 Polymorphic contexts}                          *)
(****************************************************************************)


(** Key of a polymorphic context value *)
type ('a, _) pkey = ..


(** Descriptor of a polymorphic context key *)
type ('a, 'v) pdesc = {
  eq : 'vv. ('a, 'vv) pkey -> ('vv, 'v) eq option;
  print : Format.formatter -> 'v -> unit;
}

type 'a pctx =
  | [] : 'a pctx
  | (::) : (('a,'v) pdesc * 'v option) * 'a pctx -> 'a pctx

let pempty : 'a pctx = []

let pfind (k: ('a, 'v) pkey) (pctx: 'a pctx) : 'v =
  let rec iter : type v. ('a, v) pkey -> 'a pctx -> v =
    fun k -> function
      | [] -> raise Not_found
      | (desc,v) :: tl ->
        match desc.eq k with
        | None -> iter k tl
        | Some Eq ->
          match v with
          | Some vv -> vv
          | None -> raise Not_found
  in
  iter k pctx

let padd (k:('a,'v) pkey) (v:'v) (pctx:'a pctx) : 'a pctx =
  let rec iter : type v. ('a,v) pkey -> v -> 'a pctx -> 'a pctx =
    fun k v -> function
      | [] -> raise Not_found
      | hd :: tl ->
        let (desc, _) = hd in
        match desc.eq k with
        | Some Eq -> (desc, Some v) :: tl
        | None -> hd :: (iter k v tl)
  in
  iter k v pctx


let pmem (k:('a,'v) pkey) (pctx:'a pctx) : bool =
  let rec iter : type v. ('a,v) pkey -> 'a pctx -> bool =
    fun k -> function
      | [] -> false
      | hd :: tl ->
        let (desc, v) = hd in
        match desc.eq k with
        | None -> iter k tl
        | Some Eq ->
          match v with
          | None -> false
          | Some _ -> true
  in
  iter k pctx


let premove (k:('a,'v) pkey) (pctx:'a pctx) : 'a pctx =
  let rec iter : type v. ('a,v) pkey -> 'a pctx -> 'a pctx =
    fun k -> function
      | [] -> []
      | hd :: tl ->
        let (desc, _) = hd in
        match desc.eq k with
        | Some Eq -> (desc, None) :: tl
        | None -> hd :: iter k tl
  in
  iter k pctx

let pconcat (pctx:'a pctx) (pctx':'a pctx) : 'a pctx =
  let rec iter = function
    | [] -> pctx'
    | hd :: tl ->
    hd :: iter tl
  in
  iter pctx


let pprint fmt pctx =
  let rec iter fmt pctx =
    match pctx with
    | [] -> ()
    | (_, None) :: tl -> iter fmt tl
    | [(desc, Some v)] -> Format.fprintf fmt "%a" desc.print v
    | (desc, Some v) :: tl -> Format.fprintf fmt "%a@\n%a" desc.print v iter tl
  in
  iter fmt pctx


(****************************************************************************)
(**                            {2 Contexts}                                 *)
(****************************************************************************)

type 'a ctx = {
  ctx_unit : uctx;
  ctx_poly : 'a pctx;
}

(** Generate a polymorphic (stateless) key *)
module GenPolyKey(V:sig
    type 'a t
    val print : Format.formatter -> 'a t -> unit
  end)
=
struct

  type ('a,_) pkey += PKey : ('a, 'a V.t) pkey

  let key = PKey

  let eq : type a v. (a,v) pkey -> (v, a V.t) eq option =
    function
    | PKey -> Some Eq
    | _ -> None

  let init =
    let desc = {
      eq = eq;
      print = V.print;
    }
    in
    [(desc,None)]

end


let empty : 'a ctx = {
  ctx_unit = uempty;
  ctx_poly = pempty;
}

let unit ctx = ctx.ctx_unit

let find_unit (k: 'v ukey) (ctx:'a ctx) : 'v =
  ufind k ctx.ctx_unit

let find_poly (k: ('a,'v) pkey) (ctx:'a ctx) : 'v =
  pfind k ctx.ctx_poly

let mem_unit (k: 'v ukey) (ctx:'a ctx) : bool =
  umem k ctx.ctx_unit

let mem_poly (k: ('a,'v) pkey) (ctx:'a ctx) : bool =
  pmem k ctx.ctx_poly

let add_unit (k: 'v ukey) (v:'v) (ctx:'a ctx) : 'a ctx =
  { ctx with ctx_unit = uadd k v ctx.ctx_unit }

let add_poly (k: ('a,'v) pkey) (v:'v) (ctx:'a ctx) : 'a ctx =
  { ctx with ctx_poly = padd k v ctx.ctx_poly }

let remove_unit (k: 'v ukey) (ctx:'a ctx) : 'a ctx =
  { ctx with ctx_unit = uremove k ctx.ctx_unit }

let remove_poly (k: ('a,'v) pkey) (ctx:'a ctx) : 'a ctx =
  { ctx with ctx_poly = premove k ctx.ctx_poly }

let init_poly (pctx:'a pctx) (ctx:'a ctx) : 'a ctx =
  { ctx with ctx_poly = pconcat ctx.ctx_poly pctx }

let print fmt ctx =
  Format.fprintf fmt "%a%a" uprint ctx.ctx_unit pprint ctx.ctx_poly

let get_unit ctx = ctx.ctx_unit

let set_unit unit ctx = { ctx with ctx_unit = unit }
