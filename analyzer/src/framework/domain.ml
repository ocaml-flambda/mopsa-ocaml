(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Signature of an abstract domain.

   This is the low-level domain signature that gives full access to the
   overall flow abstraction and analysis manager.
*)

open Manager
open Eval

type 'a interface = {
  export : 'a list;
  import : 'a list;
}

module type DOMAIN =
sig

  include Lattice.LATTICE

  val init : Ast.program -> ('a, t) man -> 'a flow -> 'a flow option

  val exec_interface : Zone.t interface
  val eval_interface : (Zone.t * Zone.t) interface

  val exec : Zone.t -> Ast.stmt -> ('a, t) man -> 'a flow -> 'a Post.post option
  val eval : (Zone.t * Zone.t) -> Ast.expr -> ('a, t) man -> 'a flow -> ('a, Ast.expr) evl option
  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
end

type _ id = ..
type (_, _) eq = Eq : ('a, 'a) eq

type domain = Domain : {
    name : string;
    id : 'a id;
    domain : (module DOMAIN with type t = 'a);
    eq : 'b. 'b id -> ('a, 'b) eq option;
  } -> domain

let domains : domain list ref = ref []

let register dom () =
  domains := dom :: !domains

let find_by_name d =
  let Domain {domain} = List.find (function Domain {name} -> name = d) !domains in
  let module D = (val domain : DOMAIN with type t = 'a) in
  (module D : DOMAIN)

let find_by_id : type a. a id -> (module DOMAIN with type t = a) = fun id ->
  let rec aux : type a. a id -> domain list -> (module DOMAIN with type t = a) = fun id l ->
    match l with
    | [] -> raise Not_found
    | hd :: tl ->
      let Domain {eq; domain} = hd in
      match eq id with
      | Some Eq -> domain
      | None -> aux id tl
  in
  aux id !domains
