(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of abstract abstractions. *)

open Essentials
open Domain


(** Pool of abstract domains *)
(** ************************ *)

module Pool =
struct

  (** A pool is encoded as GADT tuples *)
  type 'a t =
  | [] : unit t
  | (::) : (module DOMAIN with type t = 'a) * 'b t -> ('a * 'b) t

end


(* Pool manager defines get/set functions to access the abstract elements of a
   domain within the pool via its identifier *)
type 'a pool_man = {
  get : 't. 't domain -> 'a -> 't;
  set : 't. 't domain -> 't -> 'a -> 'a;
}


(** Reduction rules *)
(** *************** *)

module type REDUCTION =
sig
  val exec : Ast.stmt -> 'a pool_man -> ('a, 'b) man -> 'a flow -> 'a flow
end


let reductions : (string * (module REDUCTION)) list ref = ref []

let register_reduction name rule =
  reductions := (name, rule) :: !reductions

let find_reduction name = List.assoc name !reductions



(** Functor of reduced products *)
(** *************************** *)

module Make
    (Config:
     sig
       type t
       val pool : t Pool.t
       val rules : (module REDUCTION) list
     end
    ) : DOMAIN =
struct
  type t = Config.t


  (* Domain identification *)
  (* ********************* *)

  type _ domain += D_reduced_product : t domain

  let name = "framework.domains.reduced_product"

  let id = D_reduced_product

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_reduced_product -> Some Eq
    | _ -> None


  (* Lattice definition *)
  (* ****************** *)  

  let bottom : t =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux Config.pool

  let top : t =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool

  let is_bottom (v:t) : bool = 
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> false
      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool v

  let subset (v1:t) (v2:t) : bool =
    let rec aux : type a. a Pool.t -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> true
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.subset vhd1 vhd2 && aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let join annot (v1:t) (v2:t) : t =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.join annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let meet annot (v1:t) (v2:t) =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.meet annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let widen annot v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.widen annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let print fmt v =
    let rec aux : type a. a Pool.t -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.[hd], (vhd, ()) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a" V.print vhd

      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a ∧ %a" V.print vhd (aux tl) vtl
    in
    aux Config.pool fmt v


  (* Managers *)
  (* ******** *)

  let head_man (man: ('a, ('b * 'c)) man) : ('a, 'b) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
  }

  let tail_man (man: ('a, ('b * 'c)) man) : ('a, 'c) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun a flow -> man.set (man.get flow |> fst, a) flow);
  }

  let pool_man (man:('a, t) man) : 'a pool_man = {
    get = (
      let f : type b. b domain -> 'a -> b = fun id env ->
        let rec aux : type b c. b domain -> c Pool.t -> ('a, c) man -> b = fun id pool man ->
          match pool with
          | Pool.[] -> raise Not_found
          | Pool.(hd::tl) ->
            let module D = (val hd) in
            match D.identify id with
            | Some Eq ->
              let man' = head_man man in
              man'.get env
            | None -> aux id tl (tail_man man)
        in
        aux id Config.pool man
      in
      f
    );
    set = (
      let f : type b. b domain -> b -> 'a -> 'a = fun id a env ->
        let rec aux : type b c. b domain -> b -> c Pool.t -> ('a, c) man -> 'a =
          fun id a pool man ->
            match pool with
            | Pool.[] -> raise Not_found
            | Pool.(hd::tl) ->
              let module D = (val hd) in
              match D.identify id with
              | Some Eq ->
                let man' = head_man man in
                man'.set a env
              | None -> aux id a tl (tail_man man)
        in
        aux id a Config.pool man
      in
      f
    );
  }


  (* Initialization *)
  (* ************** *)

  let init prog man flow =
    let rec aux: type t. t Pool.t -> ('a, t) man -> 'a flow -> 'a flow option =
      fun pool man flow ->
        match pool with
        | Pool.[] -> None
        | Pool.(hd :: tl) ->
          let module D = (val hd) in
          match D.init prog (head_man man) flow with
          | None -> aux tl (tail_man man) flow
          | Some flow -> aux tl (tail_man man) flow
    in
    aux Config.pool man flow


  (* Post-condition computation *)
  (* ************************** *)

  (* FIXME: we support only the case when all domains of the pool have
     the same exec_interface *)
  let exec_interface =
    match Config.pool with
    | Pool.(hd :: tl) ->
      let module D = (val hd) in
      D.exec_interface
    | _ -> assert false


  let reduce_exec stmt man flow =
    let pman = pool_man man in
    let rec apply flow (l: (module REDUCTION) list) =
      match l with
      | [] -> flow
      | hd :: tl ->
        let module R = (val hd : REDUCTION) in
        apply (R.exec stmt pman man flow) tl
    in
    let rec lfp flow =
      let flow' = apply flow Config.rules in
      if Flow.subset man flow flow' then flow else lfp flow'
    in
    lfp flow

  let exec zone stmt man flow =
    (* Point-wise exec *)
    let posts =
      let rec aux: type t. t Pool.t -> ('a, t) man -> 'a post option list =
        fun pool man ->
          match pool with
          | Pool.[] -> []
          | Pool.(hd :: tl) ->
            let module D = (val hd) in
            let post = D.exec zone stmt  (head_man man) flow in
            post :: (aux tl (tail_man man))
      in
      aux Config.pool man
    in
    (* Compute meet mergers *)
    let mergers =
      let rec aux posts before =
        match posts with
        | [] -> [], []
        | None :: tl ->
          let after, ret = aux tl before in
          after, [] :: ret
        | Some post :: tl ->
          let this = post.Post.mergers in
          let after, ret = aux tl (before @ this) in
          (after @ this), (before @ after) :: ret
      in
      snd @@ aux posts []
    in
    (* Apply mergers *)
    let flow' =
      let rec aux : type t. t Pool.t -> ('a, t) man -> 'a post option list -> Ast.stmt list list -> 'a Flow.flow option =
        fun pool man posts mergers  ->
          match pool, posts, mergers with
          | Pool.[], _, _ -> None
          | Pool.(hd :: tl), (None :: ptl), (mhd :: mtl) ->
            aux tl (tail_man man) ptl mtl
          | Pool.(hd :: tl), (Some post :: ptl), (mhd :: mtl) ->
            let man' = head_man man in
            let flow' = List.fold_left (fun flow stmt -> man'.exec stmt flow) post.Post.flow mhd in
            (match aux tl (tail_man man) ptl mtl with
             | None -> Some flow'
             | Some flow'' -> Some (Flow.meet man flow' flow'')
            )
          | _ -> assert false
      in
      aux Config.pool man posts mergers
    in
    match flow' with
    | None -> None
    | Some flow -> Some (
        reduce_exec stmt man flow |>
        Post.singleton
      )


  (* Evaluations *)
  (* *********** *)

  (* FIXME: we support only the case when all domains of the pool have
     the same eval_interface *)
  let eval_interface =
    match Config.pool with
    | Pool.(hd :: tl) ->
      let module D = (val hd) in
      D.eval_interface
    | _ -> assert false

  let eval zone exp man flow =
    (* Point-wise evaluation *)
    let evls =
      let rec aux: type t. t Pool.t -> ('a, t) man -> ('a, Ast.expr) evl option list =
        fun pool man ->
          match pool with
          | Pool.[] -> []
          | Pool.(hd :: tl) ->
            let module D = (val hd) in
            let evl = D.eval zone exp  (head_man man) flow in
            evl :: (aux tl (tail_man man))
      in
      aux Config.pool man
    in
    assert false


  (* Queries *)
  (* ******** *)

  let ask = assert false

end


(* Domain factory from string identifiers *)
(* ************************************** *)

type xpool = P : 'a Pool.t -> xpool

let of_string (values: string list) (rules: string list) : (module DOMAIN) =
  let pool = find_pool values in
  let rules = List.map find_reduction rules in

  let type_domain (type a) (d : (module DOMAIN with type t = a)) =
    let module D = (val d) in
    (module D : DOMAIN with type t = a)
  in

  let open Pool in
  let rec type_pool : (module DOMAIN) list -> xpool = function
    | [] -> P []
    | hd :: tl ->
      let module D = (val hd) in
      let d = type_domain (module D) in
      let P tl = type_pool tl in
      P (d :: tl)
  in

  let create_product (type a) (pool: a Pool.t) =
    let module D = Make(struct type t = a let pool = pool let rules = rules end) in
    (module D : DOMAIN)
  in

  let P pool = type_pool pool in
  create_product pool
