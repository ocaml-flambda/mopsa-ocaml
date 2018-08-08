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

type _ domain = ..
type (_, _) eq = Eq : ('a, 'a) eq


module type DOMAIN =
sig

  include Lattice.LATTICE

  val id : t domain
  val name : string
  val identify : 'a domain -> (t, 'a) eq option

  val init : Ast.program -> ('a, t) man -> 'a flow -> 'a flow option

  val exec_interface : Zone.zone interface
  val eval_interface : (Zone.zone * Zone.zone) interface

  val exec : Zone.zone -> Ast.stmt -> ('a, t) man -> 'a flow -> 'a Post.post option
  val eval : (Zone.zone * Zone.zone) -> Ast.expr -> ('a, t) man -> 'a flow -> ('a, Ast.expr) evl option
  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
end


(*==========================================================================*)
(**                         {2 Registration} *)
(*==========================================================================*)


let domains : (module DOMAIN) list ref = ref []

let register_domain info = domains := info :: !domains

let find_domain name =
  let rec aux = function
    | [] -> raise Not_found
    | hd :: tl ->
      let module D = (val hd : DOMAIN) in
      if D.name = name then
        (module D : DOMAIN)
      else aux tl
  in
  aux !domains

let find_pool (names: string list) : (module DOMAIN) list =
  List.filter (fun d ->
      let module D = (val d : DOMAIN) in
      List.mem D.name names
    ) !domains

let mem_domain name =
  List.exists (fun d ->
      let module D = (val d : DOMAIN) in
      D.name = name
    ) !domains
