(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unified signature of abstract domains.

    This is the low-level domain signature that gives full access to
    the flow abstraction and global analysis manager.
*)

open Manager
open Eval
open Post

(** Zone interface of a transfer function *)
type 'a interface = {
  export : 'a list;
  import : 'a list;
}


(** Domain identifer *)
type _ domain = ..


(** Identifier equality witness *)
type (_, _) eq = Eq : ('a, 'a) eq


(** Unified signature of abstract domains *)
module type DOMAIN =
sig

  (** Lattice structure *)
  include Lattice.LATTICE

  (** Domain identifier *)
  val id : t domain

  (** Name of the domain *)
  val name : string

  (** Check the identity of the domain *)
  val identify : 'a domain -> (t, 'a) eq option

  (** Initialization of the domain's abstract element *)
  val init : Ast.program -> ('a, t) man -> 'a flow -> 'a flow option

  (** Interface of the [exec] transfer function *)
  val exec_interface : Zone.zone interface

  (** Interface of the eval transfer function *)
  val eval_interface : (Zone.zone * Zone.zone) interface

  (** Transfer function for computing post-conditions of statements *)
  val exec : Zone.zone -> Ast.stmt -> ('a, t) man -> 'a flow -> 'a post option

  (** Transfer function for evaluating expressions *)
  val eval : (Zone.zone * Zone.zone) -> Ast.expr -> ('a, t) man -> 'a flow -> ('a, Ast.expr) evl option

  (** Handler of queries *)
  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
end


(*==========================================================================*)
(**                          {2 Registration}                               *)
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
