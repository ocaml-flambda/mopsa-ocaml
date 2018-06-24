(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Generic n-ary reduction mechanism of abstract domains. *)

open Manager

(** Keys for identifying domains within a pool *)
type _ key = ..

(** Key equality witness *)
type (_,_) eq = Eq : ('a, 'a) eq

(** A pool of abstract domains *)
module Pool =
struct
  (** A domain is composed of a key and a module of type Domain.DOMAIN *)
  type 'a domain = Domain : 'a key * (module Domain.DOMAIN with type t = 'a) -> 'a domain

  (** A pool is encoded as GADT tuples *)
  type 'a t =
  | [] : unit t
  | (::) : 'a domain * 'b t -> ('a * 'b) t

end

type 'a product_manager = {
  get : 't. 't key -> ('a, 't) Manager.manager;
}

module ProductPost =
struct
  type ('a, 't) v = Post : 't key * 'a Post.post option -> ('a, 't) v

  type ('a, 't) t =
    | [] : ('a, unit) t
    | (::) : ('a, 't) v * ('a, 'u) t -> ('a, 't * 'u) t

end

module ProductEval =
struct
  type ('a, 't) v = Eval : 't key * (Ast.expr, 'a) eval option -> ('a, 't) v

  type ('a, 't) t =
    | [] : ('a, unit) t
    | (::) : ('a, 't) v * ('a, 'u) t -> ('a, 't * 'u) t

end

type ('a, 't) eval_accessor = {
  get : 'u. 'u key -> ('a, 't) ProductEval.t -> (Ast.expr, 'a) eval option;
}


(** Signature for reductions *)
module type REDUCTION =
sig
  val exec : Ast.stmt -> 'a product_manager -> Context.context -> 'a Flow.flow -> 'a Post.post option

  val eval : Ast.expr -> 'a product_manager -> ('a, 't) eval_accessor -> Context.context -> ('a, 't) ProductEval.t -> (Ast.expr, 'a) eval option
end

(** Functor module to create a reduced product abstract domain given a
   pool of abstract domains and a reduction operator *)
module Make
    (P: sig
       type t
       val pool : t Pool.t
       val eq : 'a key -> 'b key -> ('a, 'b) eq option
     end)
    (Reduction: REDUCTION) : Domain.DOMAIN =
struct

  type t = P.t

  let bottom =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let Pool.Domain (_,d) = hd in
        let module D = (val d) in
        let tl = aux tl in
        D.bottom, tl
    in
    aux P.pool

  let top =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        let tl = aux tl in
        V.top, tl
    in
    aux P.pool

  let is_bottom v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> false
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.is_bottom vhd
        || aux tl vtl
    in
    aux P.pool v

  let is_top v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> true
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.is_top vhd
        && aux tl vtl
    in
    aux P.pool v

  let leq v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> true
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.leq vhd1 vhd2
        && aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let join v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.join vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let meet v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.meet vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let widening ctx v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        V.widening ctx vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let print fmt v =
    let rec aux : type a. a Pool.t -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Pool.Domain (_,d) = hd in
        let module V = (val d) in
        Format.fprintf fmt "%a, %a" V.print vhd (aux tl) vtl
    in
    aux P.pool fmt v

  let head_man man = Manager.{
      man with
      ax = {
        get = (fun flow -> man.ax.get flow |> fst);
        set = (fun a flow -> man.ax.set (a, man.ax.get flow |> snd) flow);
      }
    }

  let tail_man man = Manager.{
    man with
    ax = {
      get = (fun flow -> man.ax.get flow |> snd);
      set = (fun a flow -> man.ax.set (man.ax.get flow |> fst, a) flow);
    }
  }

  let init prog man ctx flow =
    let rec aux: type t. t Pool.t -> ('a, t) Manager.manager -> _ -> _ -> (Context.context * 'a Flow.flow) option =
      fun pool man ctx flow ->
        match pool with
        | Pool.[] -> None
        | Pool.(hd :: tl) ->
          let Pool.Domain (_,d) = hd in
          let module D = (val d) in
          match D.init prog (head_man man) ctx flow with
          | None -> aux tl (tail_man man) ctx flow
          | Some (ctx, flow) -> aux tl (tail_man man) ctx flow
    in
    aux P.pool man ctx flow

  let product_manager (man: ('a, P.t) Manager.manager) : 'a product_manager =
    let get : type b. b key -> ('a, b) Manager.manager = fun k ->
      let rec aux : type t. t Pool.t -> ('a, t) Manager.manager -> ('a, b) Manager.manager = fun pool man ->
        match pool with
        | Pool.[] -> raise Not_found
        | Pool.(hd :: tl) ->
          let Pool.Domain (k',_) = hd in
          match P.eq k k' with
          | Some Eq -> head_man man
          | None -> aux tl (tail_man man)
      in
      aux P.pool man
    in
    { get }
  
  (** FIXME: support only the case when all domains of the pool have
     the same exec_interface *)
  let exec_interface =
    match P.pool with
    | Pool.(hd :: tl) ->
      let Pool.Domain (_,d) = hd in
      let module D = (val d) in
      D.exec_interface
    | _ -> assert false

  let exec zone stmt man ctx flow =
    let pman = product_manager man in
    (* Dispatch statement to domains in a point-wise way *)
    let rec dispatch: type t. t Pool.t -> ('a, t) ProductPost.t =
      fun pool ->
        match pool with
        | Pool.[] -> ProductPost.[]
        | Pool.(hd :: tl) ->
          let Pool.Domain (k,d) = hd in
          let module D = (val d) in
          let post = D.exec zone stmt (pman.get k) ctx flow in
          ProductPost.((Post (k, post)) :: (dispatch tl))
    in
    let post = dispatch P.pool in
    (* Compute meet mergers *)
    let rec set_mergers : type t. ('a, t) ProductPost.t -> Ast.stmt list -> Ast.stmt list * ('a, t) ProductPost.t = fun ppost before ->
      match ppost with
      | ProductPost.[] -> [], ppost
      | ProductPost.(hd :: tl) ->
        let ProductPost.Post (k, post) = hd in
        match post with
        | None ->
          let after, ret = set_mergers tl before in
          after, ProductPost.((Post (k, post)) :: ret)
        | Some post ->
          let this = post.Post.mergers in
          let after, ret = set_mergers tl (before @ this) in
          let post' = Post.{post with mergers = before @ after} in
          (after @ this), ProductPost.((Post (k, Some post')) :: ret)
    in
    let _, post' = set_mergers post [] in
    (* Merge post-conditions *)
    let rec merge : type t. ('a, t) ProductPost.t -> 'a Flow.flow = fun ppost ->
      match ppost with
      | ProductPost.[] -> man.flow.top
      | ProductPost.(hd :: tl) ->
        let ProductPost.Post (k, post) = hd in
        match post with
        | None -> merge tl
        | Some post ->
          let mergers = post.Post.mergers in
          let man = pman.get k in
          let flow' = List.fold_left (fun flow stmt -> man.exec stmt ctx flow) post.Post.flow mergers in
          man.flow.meet flow' (merge tl)
    in
    let flow' = merge post' in
    Reduction.exec stmt pman ctx flow'

  (** FIXME: support only the case when all domains of the pool have
     the same exec_interface *)

  let eval_accessor evl =
    let get : type b c. b key -> ('a, c) ProductEval.t -> (Ast.expr, 'a) eval option = fun k eval ->
      let rec aux : type d. ('a, d) ProductEval.t -> (Ast.expr, 'a) eval option = fun eval ->
        match eval with
        | ProductEval.[] -> raise Not_found
        | ProductEval.(hd :: tl) ->
          let ProductEval.Eval (k', evl) = hd in
          match P.eq k k' with
          | Some Eq -> evl
          | None -> aux tl
      in
      aux eval
    in
    ProductEval.{ get }

  (** FIXME: support only the case when all domains of the pool have
     the same eval_interface *)
  let eval_interface =
    match P.pool with
    | Pool.(hd :: tl) ->
      let Pool.Domain (_,d) = hd in
      let module D = (val d) in
      D.eval_interface
    | _ -> assert false
  
  let eval zpath exp man ctx flow =
    let pman = product_manager man in
    (* Dispatch exp to domains in a point-wise way *)
    let rec dispatch: type t. t Pool.t -> ('a, t) ProductEval.t =
      fun pool ->
        match pool with
        | Pool.[] -> ProductEval.[]
        | Pool.(hd :: tl) ->
          let Pool.Domain (k,d) = hd in
          let module D = (val d) in
          let evl = D.eval zpath exp (pman.get k) ctx flow in
          ProductEval.((Eval (k, evl)) :: (dispatch tl))
    in
    let eval = dispatch P.pool in
    Reduction.eval exp pman (eval_accessor eval) ctx eval

  let ask query man ctx flow = None

end

(** Polymorphic record type describing a product pool, used during registration *)
type 'a info = {
  pool: 'a Pool.t;
  eq : 'b 'c. 'b key -> 'c key -> ('b, 'c) eq option
}

(** Register a reduced product value abstraction *)
let register_reduction (type a) (name: string) (info: a info) (reduction: (module REDUCTION)) =
  let module D = Make(struct
      type t = a
      let pool = info.pool
      let eq = info.eq
    end)
      (val reduction : REDUCTION) in
  Domain.register_domain name (module D)
