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

type 'a pool =
  | Nil : unit pool
  | Cons : (module DOMAIN with type t = 'a) * 'b pool -> ('a * 'b) pool

(** Pool evaluations *)
(** **************** *)

(** Each conjunction of point-wise evaluations are reduced individually *)
type 'a econj = ('a, Ast.expr) evl_case option list

(** Transform a conjunction into an evaluation *)
let econj_to_eval (c: 'a econj) : ('a, Ast.expr) evl option =
  match List.partition (function None -> true | _ -> false) c with
  | _, [] -> None
  | _, l ->
    Some (
      List.map (function Some c -> Eval.case c | None -> assert false) l |>
      Eval.meet_list
    )



(** Pool manager *)
(** ************ *)

type 'a pool_man = {
  get_state : 't. 't domain -> 'a -> 't;
  set_state : 't. 't domain -> 't -> 'a -> 'a;
  get_eval : 't. 't domain -> 'a econj -> (Ast.expr option * 'a flow) option;
  set_eval : 't. 't domain -> Ast.expr -> 'a flow -> 'a econj -> 'a econj;
  remove_eval : 't. 't domain -> 'a econj -> 'a econj;

}



(** State reductions *)
(** **************** *)

(** Operator signature *)
module type STATE_REDUCTION =
sig
  val reduce : Ast.stmt -> 'a pool_man -> ('a, 'b) man -> 'a flow -> 'a flow
end

(** Registration *)
let state_reductions : (string * (module STATE_REDUCTION)) list ref = ref []
let register_state_reduction name rule = state_reductions := (name, rule) :: !state_reductions
let find_state_reduction name = List.assoc name !state_reductions



(** Evaluation reductions *)
(** ********************* *)

(** Operator signature *)
module type EVAL_REDUCTION =
sig
  val reduce : Ast.expr -> 'a pool_man -> ('a, 'b) man -> 'a econj -> 'a econj
end

(** Registration *)
let eval_reductions : (string * (module EVAL_REDUCTION)) list ref = ref []
let register_eval_reduction name rule = eval_reductions := (name, rule) :: !eval_reductions
let find_eval_reduction name = List.assoc name !eval_reductions



(** Domain identification *)
(** ********************* *)

type _ domain +=
  | D_empty : unit domain
  | D_reduced_product : 'a domain * 'b domain -> ('a * 'b) domain



(** Domain functor *)
(** ************** *)

module Make
    (Config:
     sig
       type t
       val pool : t pool
       val state_rules : (module STATE_REDUCTION) list
       val eval_rules : (module EVAL_REDUCTION) list
     end
    ) : DOMAIN =
struct
  type t = Config.t


  (* Domain instance identification *)
  (* ****************************** *)

  let name = "framework.domains.reduced_product"

  let id =
    let rec aux : type a. a pool -> a domain =
      function
      | Nil -> D_empty
      | Cons(hd, tl) ->
        let module V = (val hd) in
        D_reduced_product(V.id, aux tl)
    in
    aux Config.pool

  let identify : type a. a domain -> (t, a) eq option =
    fun id ->
    let rec aux : type a b. a pool -> b domain -> (a, b) eq option =
      fun pool id ->
        match pool, id with
        | Nil, D_empty -> Some Eq
        | Cons(hd, tl), D_reduced_product(id1, id2) ->
          let module D = (val hd) in
          begin match D.identify id1, aux tl id2 with
            | Some Eq, Some Eq -> Some Eq
            | _ -> None
          end
        | _ -> None
    in
    aux Config.pool id


  (* Lattice definition *)
  (* ****************** *)

  let bottom : t =
    let rec aux : type a. a pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux Config.pool

  let top : t =
    let rec aux : type a. a pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool

  let is_bottom (v:t) : bool =
    let rec aux : type a. a pool -> a -> bool = fun pool v ->
      match pool, v with
      | Nil, () -> false
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool v

  let subset (v1:t) (v2:t) : bool =
    let rec aux : type a. a pool -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> true
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.subset vhd1 vhd2 && aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let join annot (v1:t) (v2:t) : t =
    let rec aux : type a. a pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.join annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let meet annot (v1:t) (v2:t) =
    let rec aux : type a. a pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.meet annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let widen annot v1 v2 =
    let rec aux : type a. a pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.widen annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let print fmt v =
    let rec aux : type a. a pool -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Nil, () -> ()
      | Cons(hd, Nil), (vhd, ()) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a" V.print vhd

      | Cons(hd, tl), (vhd, vtl) ->
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
    get_state = (
      let f : type b. b domain -> 'a -> b = fun id env ->
        let rec aux : type b c. b domain -> c pool -> ('a, c) man -> b = fun id pool man ->
          match pool with
          | Nil -> raise Not_found
          | Cons(hd, tl) ->
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
    set_state = (
      let f : type b. b domain -> b -> 'a -> 'a = fun id a env ->
        let rec aux : type b c. b domain -> b -> c pool -> ('a, c) man -> 'a =
          fun id a pool man ->
            match pool with
            | Nil -> raise Not_found
            | Cons(hd, tl) ->
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
    get_eval = (let f : type t. t domain -> 'a econj -> (Ast.expr option * 'a flow) option =
                  fun id econj ->
                    let rec aux : type s t. s pool -> t domain -> 'a econj -> (Ast.expr option * 'a flow) option =
                      fun pool id econj ->
                        match pool, econj with
                        | Nil, [] -> raise Not_found
                        | Cons(hd, tl), c :: ctl ->
                          let module D = (val hd) in
                          begin match D.identify id, c with
                            | Some Eq, None -> None
                            | Some Eq, Some {expr; flow} -> Some (expr, flow)
                            | _ -> aux tl id ctl
                          end
                        | _ -> assert false
                    in
                    aux Config.pool id econj
                in
                f);
    set_eval = (let f : type t. t domain -> Ast.expr -> 'a flow -> 'a econj -> 'a econj =
                  fun id exp flow econj ->
                    let rec aux : type s t. s pool -> t domain -> 'a econj -> 'a econj =
                      fun pool id econj ->
                        match pool, econj with
                        | Nil, [] -> []
                        | Cons(hd, tl), c :: ctl ->
                          let module D = (val hd) in
                          begin match D.identify id with
                            | Some Eq -> Some {expr = Some exp; flow; cleaners = []} :: ctl
                            | None -> c :: (aux tl id ctl)
                          end
                        | _ -> assert false
                    in
                    aux Config.pool id econj
                in
                f);
    remove_eval = (let f : type t. t domain -> 'a econj -> 'a econj =
                     fun id econj ->
                       let rec aux : type s t. s pool -> t domain -> 'a econj -> 'a econj =
                         fun pool id econj ->
                           match pool, econj with
                           | Nil, [] -> []
                           | Cons(hd, tl), c :: ctl ->
                             let module D = (val hd) in
                             begin match D.identify id with
                               | Some Eq -> None :: ctl
                               | None -> c :: (aux tl id ctl)
                             end
                           | _ -> assert false
                       in
                       aux Config.pool id econj
                   in
                   f
                  );
  }


  (* Initialization *)
  (* ************** *)

  let init prog man flow =
    let rec aux: type t. t pool -> ('a, t) man -> 'a flow -> 'a flow option =
      fun pool man flow ->
        match pool with
        | Nil -> None
        | Cons(hd, tl) ->
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
    | Cons(hd, tl) ->
      let module D = (val hd) in
      D.exec_interface
    | _ -> assert false


  let reduce_exec stmt man flow =
    let pman = pool_man man in
    let rec apply flow (l: (module STATE_REDUCTION) list) =
      match l with
      | [] -> flow
      | hd :: tl ->
        let module R = (val hd : STATE_REDUCTION) in
        apply (R.reduce stmt pman man flow) tl
    in
    let rec lfp flow =
      let flow' = apply flow Config.state_rules in
      if Flow.subset man flow flow' then flow else lfp flow'
    in
    lfp flow

  let exec zone stmt man flow =
    (* Point-wise exec *)
    let posts =
      let rec aux: type t. t pool -> ('a, t) man -> 'a post option list =
        fun pool man ->
          match pool with
          | Nil -> []
          | Cons(hd, tl) ->
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
      let rec aux : type t. t pool -> ('a, t) man -> 'a post option list -> Ast.stmt list list -> 'a Flow.flow option =
        fun pool man posts mergers  ->
          match pool, posts, mergers with
          | Nil, _, _ -> None
          | Cons(hd, tl), (None :: ptl), (mhd :: mtl) ->
            aux tl (tail_man man) ptl mtl
          | Cons(hd, tl), (Some post :: ptl), (mhd :: mtl) ->
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
    | Cons(hd, tl) ->
      let module D = (val hd) in
      D.eval_interface
    | _ -> assert false

  let reduce_eval exp man pevl =
    let pman = pool_man man in
    let rec apply pevl (l: (module EVAL_REDUCTION) list) =
      match l with
      | [] -> pevl
      | hd :: tl ->
        let module R = (val hd : EVAL_REDUCTION) in
        apply (R.reduce exp pman man pevl) tl
    in
    apply pevl Config.eval_rules


  let eval zone exp man flow =
    (* Point-wise evaluation *)
    let evls =
      let rec aux: type t. t pool -> ('a, t) man -> ('a, Ast.expr) evl option list =
        fun pool man ->
          match pool with
          | Nil -> []
          | Cons(hd, tl) ->
            let module D = (val hd) in
            let evl = D.eval zone exp  (head_man man) flow in
            evl :: (aux tl (tail_man man))
      in
      aux Config.pool man
    in

    (* Transform list of evaluations into list of conjunctions *)
    let lconj =
      let rec aux : type t. t pool -> ('a, Ast.expr) evl option list -> 'a econj list =
        fun pool evls ->
          match pool, evls with
          | Nil, [] -> [[]]
          | Cons(hd, tl), evl :: evls ->
            let evl' = aux tl evls in
            begin match evl with
              | None -> List.map (fun c -> None :: c) evl'
              | Some evl ->
                let l = Eval.to_dnf evl |>
                        Dnf.to_list
                in
                List.fold_left (fun acc c ->
                    match c with
                    | [e] -> (List.map (fun c -> Some e :: c) evl') @ acc
                    | _ -> failwith "Domains in a reduced product cannot return a conjunction"
                  ) [] l
            end
          | _ -> assert false
      in
      aux Config.pool evls
    in

    (* Reduce each conjunction *)
    let lconj' = List.map (reduce_eval exp man) lconj in

    (* Combine conjunctions into an evaluation *)
    List.fold_left (fun acc c ->
        Option.option_neutral2 Eval.join acc (econj_to_eval c)
      ) None lconj'


  (* Queries *)
  (* ******** *)

  let ask query man flow =
    let rec aux : type t r. t pool -> ('a, t) man -> r Query.query -> r option =
      fun pool man query ->
        match pool with
        | Nil -> None
        | Cons(hd, tl) ->
          let module D = (val hd) in
          let r1 = D.ask query (head_man man) flow in
          let r2 = aux tl (tail_man man) query in
          Query.meet query r1 r2
    in
    aux Config.pool man query
                  

end


(* Domain factory from string identifiers *)
(* ************************************** *)

type xpool = P : 'a pool -> xpool

let make (pool: (module DOMAIN) list) (rules: string list) : (module DOMAIN) =
  let state_rules, eval_rules = List.partition (fun rule -> List.mem_assoc rule !state_reductions) rules in
  let state_rules = List.map find_state_reduction state_rules in
  let eval_rules = List.map find_eval_reduction eval_rules in

  let type_domain (type a) (d : (module DOMAIN with type t = a)) =
    let module D = (val d) in
    (module D : DOMAIN with type t = a)
  in

  let rec type_pool : (module DOMAIN) list -> xpool = function
    | [] -> P Nil
    | hd :: tl ->
      let module D = (val hd) in
      let d = type_domain (module D) in
      let P tl = type_pool tl in
      P (Cons (d, tl))
  in

  let create_product (type a) (pool: a pool) =
    let module D = Make(struct type t = a let pool = pool let state_rules = state_rules let eval_rules = eval_rules end) in
    (module D : DOMAIN)
  in

  let P pool = type_pool pool in
  create_product pool



(** Utility functions *)
(** ***************** *)

type 'a ffold = {
  doit : 't. 'a -> 't domain -> 'a;
}

let rec fold : type t. 'a ffold -> 'a -> t pool -> 'a =
  fun f init pool ->
    match pool with
    | Nil -> init
    | Cons(hd, tl) ->
      let module D = (val hd) in
      fold f (f.doit init D.id) tl


type fiter = {
  doit : 't. 't domain -> unit;
}

let rec iter : type t. fiter -> t pool -> unit =
  fun f pool ->
    match pool with
    | Nil -> ()
    | Cons(hd, tl) ->
      let module D = (val hd) in
      f.doit D.id;
      iter f tl
