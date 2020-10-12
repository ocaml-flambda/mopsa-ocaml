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

type ('a,_) ctx_key = ..

type 'a ctx_list =
  | Empty
  | Cons : (('a,'v) ctx_key * 'v * 'a ctx_list) -> 'a ctx_list

type 'a ctx = {
  list: 'a ctx_list;
  timestamp : int;
}

type ctx_pool = {
  ctx_pool_equal: 'a 'v 'w. ('a,'v) ctx_key -> ('a,'w) ctx_key -> ('v,'w) eq option;
  ctx_pool_print: 'a 'v. (Print.printer -> 'a -> unit) -> Format.formatter -> ('a,'v) ctx_key -> 'v -> unit;
}

let pool = ref {
  ctx_pool_equal = (fun _ _ -> None);
  ctx_pool_print = (fun pp fmt key v -> raise Not_found);
}

let counter = ref 0

let next list =
  incr counter;
  { list; timestamp = !counter }

let empty_ctx = { list = Empty; timestamp = 0 }

let singleton_ctx_list k v = Cons(k,v,Empty)

let singleton_ctx k v = next (singleton_ctx_list k v)

let rec mem_ctx_list : type v.('a,v) ctx_key -> 'a ctx_list -> bool =
  fun k -> function
  | Empty -> false
  | Cons(k',_,tl) ->
    match !pool.ctx_pool_equal k k' with
    | None    -> mem_ctx_list k tl
    | Some Eq -> true

let mem_ctx k ctx = mem_ctx_list k ctx.list

let rec find_ctx_list_opt : type v.('a,v) ctx_key -> 'a ctx_list -> v option =
  fun k -> function
    | Empty -> None
    | Cons(k',v,tl) ->
      match !pool.ctx_pool_equal k k' with
      | None    -> find_ctx_list_opt k tl
      | Some Eq -> Some v

let find_ctx_opt k ctx = find_ctx_list_opt k ctx.list

let find_ctx k ctx =
  match find_ctx_opt k ctx with
  | None   -> raise Not_found
  | Some v -> v

let rec add_ctx_list : type v.('a,v) ctx_key -> v -> 'a ctx_list -> 'a ctx_list =
  fun k v -> function
  | Empty -> singleton_ctx_list k v
  | Cons(k',v',tl) ->
    match !pool.ctx_pool_equal k k' with
    | None    -> Cons(k',v',add_ctx_list k v tl)
    | Some Eq -> Cons(k,v,tl)

let add_ctx k v ctx = next (add_ctx_list k v ctx.list)

let rec remove_ctx_list : type v. ('a,v) ctx_key -> 'a ctx_list -> 'a ctx_list =
  fun k -> function
  | Empty -> Empty
  | Cons(k',v',tl) ->
    match !pool.ctx_pool_equal k k' with
    | None    -> Cons(k',v',remove_ctx_list k tl)
    | Some Eq -> tl

let remove_ctx k ctx = next (remove_ctx_list k ctx.list)

let most_recent_ctx ctx1 ctx2 =
  if ctx1.timestamp >= ctx2.timestamp then ctx1 else ctx2

let pp_ctx pp fmt ctx =
  let rec iter = function
    | Empty -> []
    | Cons(k,v,tl) ->
      (fun fmt -> !pool.ctx_pool_print pp fmt k v) :: iter tl
  in
  let fl = iter ctx.list in
  Format.(fprintf fmt "@[<v>%a@]"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt "@,")
               (fun fmt f -> f fmt)
            ) fl
         )

type ctx_info = {
  ctx_equal : 'a 'v 'w. ctx_pool -> ('a,'v) ctx_key -> ('a,'w) ctx_key -> ('v,'w) eq option;
  ctx_print : 'a 'v. ctx_pool -> (Print.printer -> 'a -> unit) -> Format.formatter -> ('a,'v) ctx_key -> 'v -> unit;
}


let register_ctx info =
  let old_pool = !pool in
  pool := {
    ctx_pool_equal =
      (fun (type a v w) (k1:(a,v) ctx_key) (k2:(a,w) ctx_key) ->
         info.ctx_equal old_pool k1 k2);
    ctx_pool_print =
      (fun (type a v) pp fmt (k:(a,v) ctx_key) (v:v) ->
         info.ctx_print old_pool pp fmt k v)
  }

module GenContextKey
    (Value:sig
       type 'a t
       val print : (Print.printer -> 'a -> unit) -> Format.formatter -> 'a t -> unit
     end)
  : sig
    val key : ('a,'a Value.t) ctx_key
  end
=
struct
  type ('a,_) ctx_key += MyKey : ('a,'a Value.t) ctx_key
  let key = MyKey
  let () =
    register_ctx {
      ctx_equal = (
        let f: type a v w. ctx_pool -> (a,v) ctx_key -> (a,w) ctx_key -> (v,w) eq option =
          fun pool k1 k2 ->
            match k1, k2 with
            | MyKey, MyKey -> Some Eq
            | _            -> pool.ctx_pool_equal k1 k2
        in f
      );
      ctx_print = (
        let f : type a v. ctx_pool -> (Print.printer -> a -> unit) -> Format.formatter -> (a,v) ctx_key -> v -> unit =
          fun pool pp fmt k v ->
            match k with
            | MyKey -> Value.print pp fmt v
            | _     -> pool.ctx_pool_print pp fmt k v
        in f
      )
    }

end

open Callstack

module CallstackKey = GenContextKey
    (struct
      type 'a t = callstack
      let print pp fmt cs = pp_callstack fmt cs
    end)

let callstack_ctx_key = CallstackKey.key
