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
open Pool

(** Domain identification *)
(** ********************* *)

type _ domain +=
  | D_empty_product : unit domain
  | D_reduced_product : 'a domain * 'b domain -> ('a * 'b) domain

let debug fmt = Debug.debug ~channel:"framework.domains.reduced_product.products.domain_product" fmt

(** Domain functor *)
(** ************** *)

module Make
    (Config:
     sig
       type t
       type v
       val pool : t domain_pool
       val nonrel_man : ('a, t) man -> ('a, v) nonrel_man
       val state_rules : (module Reductions.State_reduction.REDUCTION) list
       val eval_rules : (module Reductions.Eval_reduction.REDUCTION) list
     end
    ) : DOMAIN =
struct
  type t = Config.t


  (* Domain instance identification *)
  (* ****************************** *)

  let name = "framework.domains.reduced_product"

  let id =
    let rec aux : type a. a domain_pool -> a domain =
      function
      | Nil -> D_empty_product
      | Cons(hd, tl) ->
        let module V = (val hd) in
        D_reduced_product(V.id, aux tl)
    in
    aux Config.pool

  let identify : type a. a domain -> (t, a) eq option =
    fun id ->
    let rec aux : type a b. a domain_pool -> b domain -> (a, b) eq option =
      fun pool id ->
        match pool, id with
        | Nil, D_empty_product -> Some Eq
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
    let rec aux : type a. a domain_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux Config.pool

  let top : t =
    let rec aux : type a. a domain_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool

  let is_bottom (v:t) : bool =
    let rec aux : type a. a domain_pool -> a -> bool = fun pool v ->
      match pool, v with
      | Nil, () -> false
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool v

  let subset (v1:t) (v2:t) : bool =
    let rec aux : type a. a domain_pool -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> true
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.subset vhd1 vhd2 && aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let join annot (v1:t) (v2:t) : t =
    let rec aux : type a. a domain_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.join annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let meet annot (v1:t) (v2:t) =
    let rec aux : type a. a domain_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.meet annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let widen annot v1 v2 =
    let rec aux : type a. a domain_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.widen annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let print fmt v =
    let rec aux : type a. a domain_pool -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Nil, () -> ()
      | Cons(hd, Nil), (vhd, ()) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a" V.print vhd

      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a%a" V.print vhd (aux tl) vtl
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

  let domain_man man = {
    pool = Config.pool;
    
    get_state = (
      let f : type b. b domain -> 'a -> b = fun id env ->
        let rec aux : type b c. b domain -> c domain_pool -> ('a, c) man -> b = fun id pool man ->
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
        let rec aux : type b c. b domain -> b -> c domain_pool -> ('a, c) man -> 'a =
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

    get_eval = (
      let f : type t. t domain -> 'a evl_conj -> (Ast.expr option * 'a flow) option =
        fun id econj ->
          let rec aux : type s t. s domain_pool -> t domain -> 'a evl_conj -> (Ast.expr option * 'a flow) option =
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
    set_eval = (
      let f : type t. t domain -> Ast.expr -> 'a flow -> 'a evl_conj -> 'a evl_conj =
        fun id exp flow econj ->
          let rec aux : type s t. s domain_pool -> t domain -> 'a evl_conj -> 'a evl_conj =
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
    remove_eval = (
      let f : type t. t domain -> 'a evl_conj -> 'a evl_conj =
        fun id econj ->
          let rec aux : type s t. s domain_pool -> t domain -> 'a evl_conj -> 'a evl_conj =
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
    let rec aux: type t. t domain_pool -> ('a, t) man -> 'a flow -> 'a flow option =
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


  let reduce_exec stmt channels man flow =
    let dman = domain_man man in
    let rec apply flow (l: (module Reductions.State_reduction.REDUCTION) list) =
      match l with
      | [] -> flow
      | hd :: tl ->
        let module R = (val hd : Reductions.State_reduction.REDUCTION) in
        match R.trigger with
        | Some ch when not (List.mem ch channels) -> apply flow tl
        | Some ch -> apply (R.reduce stmt dman (Config.nonrel_man man) man flow) tl
        | None -> apply (R.reduce stmt dman (Config.nonrel_man man) man flow) tl
    in
    let rec lfp flow =
      let flow' = apply flow Config.state_rules in
      if Flow.subset man flow flow' then flow else lfp flow'
    in
    lfp flow

  let exec zone stmt man flow =
    (* Point-wise exec *)
    let posts =
      let rec aux: type t. t domain_pool -> ('a, t) man -> 'a annot -> 'a post option list =
        fun pool man annot ->
          match pool with
          | Nil -> []
          | Cons(hd, tl) ->
            let module D = (val hd) in
            debug "Exec on %s" D.name;
            match D.exec zone stmt  (head_man man) flow with
            | None -> None :: (aux tl (tail_man man) annot)
            | Some post ->
              let annot' = get_annot post.Post.flow in
              (Some post) :: (aux tl (tail_man man) annot')
            
      in
      aux Config.pool man (get_annot flow)
    in

    (* Merge post conditions. 
       Each domain applies its merger statements on the post-conditions of the other domains,
       and restores its local abstract element from its own post-condition.
    *)
    let merged_flows, channels =
      let rec aux : type t. t domain_pool -> ('a, t) man -> 'a post option list -> 'a flow option list -> 'a flow option list * Post.channel list =
        fun pool man posts ret ->
          match pool, posts with
          | Nil, [] -> ret, []
          | Cons(hd, tl), None :: ptl -> aux tl (tail_man man) ptl ret
          | Cons(hd, tl), (Some post) :: ptl ->
            let module D = (val hd) in
            let hman = head_man man in
            let mergers = post.Post.mergers in
            let flow = post.Post.flow in
            let channels = post.Post.channels in
            (* Merge [ret] with the post-condition of [D] *)
            let rec aux2: type u. u domain_pool -> 'a flow option list -> 'a flow option list =
              fun pool' ret' ->
                match pool', ret' with
                | Nil, [] -> []
                | Cons(hd', tl'), None :: ftl' -> None :: aux2 tl' ftl'
                | Cons(hd', tl'), Some flow' :: ftl' ->
                  let module D' = (val hd') in
                  let flow' = match D.identify D'.id with
                    | Some Eq -> flow'
                    | None ->
                      (* Apply mergers of D on the post-condition [post] *)
                      let mflow' = List.fold_left (fun flow stmt -> man.exec stmt flow) flow' mergers in
                      (* Restore the abstract element of D *)
                      Flow.merge (fun tk a1 a2 ->
                          match a1, a2 with
                          | None, _ | _, None -> None
                          | Some a1, Some a2 -> Some (hman.set (hman.get a1) a2)
                        ) hman flow mflow'
                  in
                  Some flow' :: aux2 tl' ftl'
                | _ -> assert false
            in
            let ret' = aux2 Config.pool ret in
            let ret'', channels' = aux tl (tail_man man) ptl ret' in
            ret'', channels @ channels'
          | _ -> assert false
      in
      aux Config.pool man posts (List.map (Option.option_lift1 (fun post -> post.Post.flow)) posts)  
    in

    (* Meet merged flows *)
    let flow' =
      let rec aux = function
        | [] -> None
        | None :: tl -> aux tl
        | Some flow :: tl ->
          match aux tl with
          | None -> Some flow
          | Some flow' -> Some (Flow.meet man flow flow')
      in
      aux merged_flows
    in
    match flow' with
    | None -> None
    | Some flow -> Some (
        reduce_exec stmt channels man flow |>
        Post.of_flow
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
    let dman = domain_man man in
    let rec apply pevl (l: (module Reductions.Eval_reduction.REDUCTION) list) =
      match l with
      | [] -> pevl
      | hd :: tl ->
        let module R = (val hd : Reductions.Eval_reduction.REDUCTION) in
        apply (R.reduce exp dman (Config.nonrel_man man) man pevl) tl
    in
    apply pevl Config.eval_rules


  let eval zone exp man flow =
    (* Point-wise evaluation *)
    let evls =
      let rec aux: type t. t domain_pool -> ('a, t) man -> ('a, Ast.expr) evl option list =
        fun pool man ->
          match pool with
          | Nil -> []
          | Cons(hd, tl) ->
            let module D = (val hd) in
            match D.eval zone exp  (head_man man) flow with
            | None -> None :: aux tl (tail_man man)
            | Some evl -> (Some evl) :: aux tl (tail_man man)
            
      in
      aux Config.pool man
    in

    (* Transform list of evaluations into list of conjunctions *)
    let lconj =
      let rec aux : type t. t domain_pool -> ('a, Ast.expr) evl option list -> 'a evl_conj list =
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
        Option.option_neutral2 Eval.join acc (conj_to_evl c)
      ) None lconj'


  (* Queries *)
  (* ******** *)

  let ask query man flow =
    let rec aux : type t r. t domain_pool -> ('a, t) man -> r Query.query -> r option =
      fun pool man query ->
        match pool with
        | Nil -> None
        | Cons(hd, tl) ->
          let module D = (val hd) in
          let r1 = D.ask query (head_man man) flow in
          let r2 = aux tl (tail_man man) query in
          Option.option_neutral2 (Query.meet query) r1 r2
    in
    aux Config.pool man query
                  

end
