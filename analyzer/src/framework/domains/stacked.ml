(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Stacked domains are functor allowing doing unifications before
   binary operators (⊆, ∪, ∩, and ▽) over an argument sub-domain.

   Note that the abstract element of the sub-domain is outside the
   scope of the stacked domain. An additional manager is therefore
   given to transfer functions.
*)

open Manager

type 'a interface = 'a Domain.interface


(** Abstract domain signature. *)
module type DOMAIN = functor(SubDomain: Domain.DOMAIN) ->
sig

  include Lattice.LATTICE

  val leq: t * SubDomain.t-> t * SubDomain.t-> bool * SubDomain.t * SubDomain.t
  val join: t * SubDomain.t -> t * SubDomain.t -> t * SubDomain.t * SubDomain.t
  val meet: t * SubDomain.t -> t * SubDomain.t -> t * SubDomain.t * SubDomain.t
  val widening: Context.context -> t * SubDomain.t -> t * SubDomain.t -> t * SubDomain.t * SubDomain.t

  val init : Ast.program -> ('a, t) manager -> Context.context -> 'a Flow.flow -> (Context.context * 'a Flow.flow) option

  (** Abstract transfer function of statements. *)
  val exec_interface : Zone.t interface
  val exec: Zone.t -> Ast.stmt -> ('a, t) manager -> ('a, SubDomain.t) manager -> Context.context -> 'a Flow.flow -> 'a Post.post option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval_interface : Zone.path interface
  val eval: Zone.path -> Ast.expr -> ('a, t) manager -> ('a, SubDomain.t) manager -> Context.context -> 'a Flow.flow -> (Ast.expr, 'a) Eval.eval option

  (** Handler of generic queries. *)
  val ask: 'r Query.query -> ('a, t) manager -> ('a, SubDomain.t) manager -> Context.context -> 'a Flow.flow -> 'r option

end


(** Create a stateful domain from a stacked domain and its sub-domain *)
module Make(StackDomain: DOMAIN)(SubDomain: Domain.DOMAIN) : Domain.DOMAIN =
struct

  module StackDomain = StackDomain(SubDomain)

  type t = StackDomain.t * SubDomain.t

  let head_man man = {
    man with ax = {
      get = (fun gabs -> fst @@ man.ax.get gabs);
      set = (fun hd gabs -> man.ax.set (hd, snd @@ man.ax.get gabs) gabs);
    }
  }

  let tail_man man = {
    man with ax = {
      get = (fun gabs -> snd @@ man.ax.get gabs);
      set = (fun tl gabs -> man.ax.set (fst @@ man.ax.get gabs, tl) gabs);
    }
  }


  let bottom = StackDomain.bottom, SubDomain.bottom

  let is_bottom (hd, tl) = StackDomain.is_bottom hd || SubDomain.is_bottom tl

  let top = StackDomain.top, SubDomain.top

  let is_top (hd, tl) = StackDomain.is_top hd && SubDomain.is_top tl

  let leq a1 a2 =
    let ret, s1', s2' = StackDomain.leq a1 a2 in
    ret && SubDomain.leq s1' s2'

  let join a1 a2 =
    let d, s1', s2' = StackDomain.join a1 a2 in
    d, SubDomain.join s1' s2'

  let meet a1 a2 =
    let d, s1', s2' = StackDomain.meet a1 a2 in
    d, SubDomain.meet s1' s2'

  let widening ctx a1 a2 =
    let d, s1', s2' = StackDomain.widening ctx a1 a2 in
    d, SubDomain.widening ctx s1' s2'

  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" StackDomain.print hd SubDomain.print tl

  let init prog man ctx flow =
    let ctx, flow = match StackDomain.init prog (head_man man) ctx flow with
      | None -> ctx, flow
      | Some x -> x
    in
    SubDomain.init prog (tail_man man) ctx flow


  let exec_interface = Domain.{
    import = StackDomain.exec_interface.import @ SubDomain.exec_interface.import;
    export = StackDomain.exec_interface.export @ SubDomain.exec_interface.export;
  }

  let exec zone =
    match List.find_all (fun z -> Zone.leq z zone) StackDomain.exec_interface.Domain.export, List.find_all (fun z -> Zone.leq z zone) SubDomain.exec_interface.Domain.export with
    | [], [] -> raise Not_found

    | l, [] ->
      Analyzer.mk_exec_of_zone_list l (fun zone stmt man ctx flow -> StackDomain.exec zone stmt (head_man man) (tail_man man) ctx flow)

    | [], l ->
      Analyzer.mk_exec_of_zone_list l (fun zone stmt man ctx flow -> SubDomain.exec zone stmt (tail_man man) ctx flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_exec_of_zone_list l1 (fun zone stmt man ctx flow -> StackDomain.exec zone stmt (head_man man) (tail_man man) ctx flow) in
      let f2 = Analyzer.mk_exec_of_zone_list l2 (fun zone stmt man ctx flow -> SubDomain.exec zone stmt (tail_man man) ctx flow) in
      (fun stmt man ctx flow ->
         match f1 stmt man ctx flow with
         | Some post -> Some post
         | None -> f2 stmt man ctx flow
      )

  let eval_interface = Domain.{
    import = StackDomain.eval_interface.import @ SubDomain.eval_interface.import;
    export = StackDomain.eval_interface.export @ SubDomain.eval_interface.export;
  }

  let eval zpath =
    match List.find_all (fun p -> Zone.path_leq p zpath) StackDomain.eval_interface.Domain.export, List.find_all (fun p -> Zone.path_leq p zpath) SubDomain.eval_interface.Domain.export with
    | [], [] -> raise Not_found

    | l, [] ->
      Analyzer.mk_eval_of_zone_path_list l (fun zone exp man ctx flow -> StackDomain.eval zone exp (head_man man) (tail_man man) ctx flow)

    | [], l ->
      Analyzer.mk_eval_of_zone_path_list l (fun zone exp man ctx flow -> SubDomain.eval zone exp (tail_man man) ctx flow)

    | l1, l2 ->
      let f1 = Analyzer.mk_eval_of_zone_path_list l1 (fun zone exp man ctx flow -> StackDomain.eval zone exp (head_man man) (tail_man man) ctx flow) in
      let f2 = Analyzer.mk_eval_of_zone_path_list l2 (fun zone exp man ctx flow -> SubDomain.eval zone exp (tail_man man) ctx flow) in
      (fun exp man ctx flow ->
         match f1 exp man ctx flow with
         | Some evl -> Some evl
         | None -> f2 exp man ctx flow
      )



  let ask query man ctx flow =
    let head_reply = StackDomain.ask query (head_man man) (tail_man man) ctx flow in
    let tail_reply = SubDomain.ask query (tail_man man) ctx flow in
    Query.join query head_reply tail_reply


end



let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains
