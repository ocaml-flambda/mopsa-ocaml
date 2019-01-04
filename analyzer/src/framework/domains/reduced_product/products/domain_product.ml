(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of abstract abstractions. *)

open Manager
open Annotation
open Domain
open Post
open Pool

(** Domain identification *)
(** ********************* *)

type _ domain +=
  | D_empty_product : 'o domain -> (unit * 'o) domain
  | D_reduced_product : 'a domain * ('b * 'o) domain -> (('a * 'b) * 'o) domain

let debug fmt = Debug.debug ~channel:"framework.domains.reduced_product.products.domain_product" fmt

(** Domain functor *)
(** ************** *)

module Make
    (Over: DOMAIN)
    (Config:
     sig
       type t
       type v
       val pool : t domain_pool
       val nonrel_man : ('a, t * Over.t) man -> ('a, v) nonrel_man
       val post_rules : (module Reductions.Post_reduction.REDUCTION) list
       val eval_rules : (module Reductions.Eval_reduction.REDUCTION) list
     end
    )
  : DOMAIN =
struct
  type t = Config.t * Over.t


  (* Domain instance identification *)
  (* ****************************** *)

  let name = "framework.domains.reduced_product"

  let id =
    let rec aux : type a. a domain_pool -> (a * Over.t) domain =
      function
      | Nil -> D_empty_product Over.id
      | Cons(hd, tl) ->
        let module V = (val hd) in
        D_reduced_product(V.id, aux tl)
    in
    aux Config.pool

  let identify : type a. a domain -> (t, a) eq option =
    fun id ->
      let rec aux : type a b. a domain_pool -> b domain -> (a * Over.t, b) eq option =
        fun pool id ->
          match pool, id with
          | Nil, D_empty_product(o) ->
            begin match Over.identify o with
              | Some Eq -> Some Eq
              | None -> None
            end
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
    aux Config.pool, Over.bottom

  let top : t =
    let rec aux : type a. a domain_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool, Over.top

  let is_bottom ((a,o):t) : bool =
    let rec aux : type a. a domain_pool -> a -> bool = fun pool v ->
      match pool, v with
      | Nil, () -> false
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool a || Over.is_bottom o


  (* Managers *)
  (* ******** *)

  let head_man (man: ('a, ('b * 'c) * Over.t) man) : ('a, 'b) man = {
    man with
    get = (fun flow -> man.get flow |> fst |> fst);
    set = (fun b flow ->
        let ((_, c), o) = man.get flow in
        man.set ((b, c), o) flow
      );
  }

  let tail_man (man: ('a, ('b * 'c) * Over.t) man) : ('a, 'c * Over.t) man = {
    man with
    get = (fun flow ->
        let ((_, c), o) = man.get flow in
        (c, o)
      );
    set = (fun (c, o) flow ->
        let ((b, _), _) = man.get flow in
        man.set ((b, c), o) flow
      );
  }

  let over_man (man: ('a, t) man) : ('a, Over.t) man = {
    man with
    get = (fun flow ->
        let (_, o) = man.get flow in
        o
      );
    set = (fun o flow ->
        let (a, _) = man.get flow in
        man.set (a, o) flow
      );
  }

  module LocalAnalyzer = Analyzer.Make(Over)

  let join annot ((a1,o1):t) ((a2,o2):t) : t =
    let rec aux : type a. a domain_pool -> a * Over.t -> a * Over.t -> a * Over.t * Over.t = fun pool (a1,o1) (a2,o2) ->
      match pool, a1, a2 with
      | Nil, (), () -> (), o1, o2
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        let hd', o1', o2' =
          Stacked.lift_to_flow LocalAnalyzer.man
            (fun o1 o2 -> V.join annot LocalAnalyzer.man (vhd1, o1) (vhd2, o2))
            o1 o2
        in
        let tl', o1'', o2'' = aux tl (vtl1, o1') (vtl2, o2') in
        (hd', tl'), o1'', o2''
    in
    let a', o1', o2' = aux Config.pool (a1,o1) (a2,o2) in
    a', Over.join annot o1' o2'

  let meet annot ((a1,o1):t) ((a2,o2):t) =

    let rec aux : type a. a domain_pool -> a * Over.t -> a * Over.t -> a * Over.t * Over.t = fun pool (a1,o1) (a2,o2) ->
      match pool, a1, a2 with
      | Nil, (), () -> (), o1, o2
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        let hd', o1', o2' =
          Stacked.lift_to_flow LocalAnalyzer.man
            (fun o1 o2 -> V.meet annot LocalAnalyzer.man (vhd1, o1) (vhd2, o2))
            o1 o2
        in
        let tl', o1'', o2'' = aux tl (vtl1, o1') (vtl2, o2') in
        (hd', tl'), o1'', o2''
    in
    let a', o1', o2' = aux Config.pool (a1,o1) (a2,o2) in
    a', Over.meet annot o1' o2'

  let widen annot ((a1,o1):t) ((a2,o2):t) =
    let rec aux : type a. a domain_pool -> bool -> a * Over.t -> a * Over.t -> a * bool * Over.t * Over.t = fun pool is_stable (a1,o1) (a2,o2) ->
      match pool, a1, a2 with
      | Nil, (), () -> (), is_stable, o1, o2
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        let hd', is_stable', o1', o2' =
          Stacked.lift_widen_to_flow LocalAnalyzer.man
            (fun o1 o2 -> V.widen annot LocalAnalyzer.man (vhd1, o1) (vhd2, o2))
            o1 o2
        in
        let tl', is_stable, o1'', o2'' = aux tl (is_stable && is_stable') (vtl1, o1') (vtl2, o2') in
        (hd', tl'), is_stable, o1'', o2''
    in
    let a', is_stable, o1', o2' = aux Config.pool true (a1,o1) (a2,o2) in
    a', if is_stable then Over.widen annot o1' o2' else Over.join annot o1' o2'

  let subset ((a1,o1):t) ((a2,o2):t) : bool =
    let rec aux : type a. a domain_pool -> a * Over.t -> a * Over.t -> bool * Over.t * Over.t  = fun pool (a1, o1) (a2, o2) ->
      match pool, a1, a2 with
      | Nil, (), () -> true , o1, o2
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        let (b, o1' ,o2') =
          Stacked.lift_to_flow LocalAnalyzer.man
            (fun o1 o2 -> V.subset LocalAnalyzer.man (vhd1, o1) (vhd2, o2))
            o1 o2
        in
        let (b', o1'' ,o2'') = aux tl (vtl1, o1') (vtl2, o2') in
        (b && b', o1'', o2'')
    in
    let b, o1, o2 = aux Config.pool (a1, o1) (a2, o2) in
    Over.subset o1 o2

  let print fmt (a,o) =
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
    Format.fprintf fmt "%a%a" (aux Config.pool) a Over.print o


  let domain_man man = {
    pool = Config.pool;
    get_env = (
      let f : type b. b domain -> 'a -> b = fun id env ->
        let rec aux : type b c. b domain -> c domain_pool -> ('a, c * Over.t) man -> b = fun id pool man ->
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
    set_env = (
      let f : type b. b domain -> b -> 'a -> 'a = fun id a env ->
        let rec aux : type b c. b domain -> b -> c domain_pool -> ('a, c * Over.t) man -> 'a =
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

    fold = (
      let f : type b. b fold_fun -> b -> b = fun g init ->
        let rec aux : type c. c domain_pool -> b -> b =
          fun pool acc ->
            match pool with
            | Nil -> acc
            | Cons(hd, tl) ->
              let module D = (val hd) in
              let acc' = g.doit D.id acc in
              aux tl acc'
        in
        aux Config.pool init
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
                  | Some Eq ->
                    let cleaners = match c with
                      | None -> []
                      | Some {cleaners} -> cleaners
                    in
                    Some {expr = Some exp; flow; cleaners} :: ctl
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
    let rec aux: type b. b domain_pool -> 'a flow_callback option -> ('a, b * Over.t) man -> 'a flow -> 'a flow_callback option =
      fun pool ret man flow ->
        match pool with
        | Nil -> ret
        | Cons(hd, tl) ->
          let module D = (val hd) in
          match D.init prog (head_man man) flow with
          | None -> aux tl ret (tail_man man) flow
          | Some flow' ->
            aux tl (Some flow') (tail_man man) flow'.flow |>
            OptionExt.lift (fun ret' -> { ret' with callbacks = flow'.callbacks @ ret'.callbacks })
            
    in
    aux Config.pool None man flow


  (* Post-condition computation *)
  (* ************************** *)

  let exec_interface =
    let rec aux : type a. a domain_pool -> Zone.zone interface =
      function
      | Nil -> Over.exec_interface
      | Cons(hd, tl) ->
        let module D = (val hd) in
        let after = aux tl in
        {
          import = D.exec_interface.import @ after.import;
          export = D.exec_interface.export @ after.export;
        }
    in
    aux Config.pool

  let reduce_exec stmt channels man flow =
    let dman = domain_man man in
    let rec apply flow (l: (module Reductions.Post_reduction.REDUCTION) list) =
      match l with
      | [] -> flow
      | hd :: tl ->
        let module R = (val hd : Reductions.Post_reduction.REDUCTION) in
        match R.trigger with
        | Some ch when not (List.mem ch channels) -> apply flow tl
        | Some ch -> apply (R.reduce stmt dman (Config.nonrel_man man) man flow) tl
        | None -> apply (R.reduce stmt dman (Config.nonrel_man man) man flow) tl
    in
    let rec lfp flow =
      let flow' = apply flow Config.post_rules in
      if Flow.subset man flow flow' then flow else lfp flow'
    in
    lfp flow

  let exec zone =
    (* Check coverage of exported zones in the pool *)
    let coverage =
      let rec aux: type t. t domain_pool -> bool list =
        function
        | Nil -> []
        | Cons(hd, tl) ->
          let module D = (val hd) in
          let b = List.exists (fun z -> Zone.sat_zone z zone) D.exec_interface.Domain.export in
          b :: aux tl
      in
      aux Config.pool
    in
    (fun stmt man flow ->
       (* Point-wise exec *)
       let posts =
         let rec aux: type b. b domain_pool -> bool list -> ('a, b * Over.t) man -> 'a annot -> 'a post option list =
           fun pool coverage man annot ->
             match pool, coverage with
             | Nil, _ -> []
             | Cons(hd, tl), false :: ctl -> None :: (aux tl ctl (tail_man man) annot)
             | Cons(hd, tl), true :: ctl ->
               begin
                 let module D = (val hd) in
                 match D.exec zone stmt (head_man man) flow with
                 | None -> None :: (aux tl ctl (tail_man man) annot)
                 | Some post ->
                   let annot' = Flow.get_all_annot post.Post.flow in
                   (Some post) :: (aux tl ctl (tail_man man) annot')
               end
             | _ -> assert false

         in
         aux Config.pool coverage man (Flow.get_all_annot flow)
       in

       (* Merge post conditions.
          Each domain applies its merger statements on the post-conditions of the other domains,
          and restores its local abstract element from its own post-condition.
       *)
       let merged_flows, channels =
         let rec aux : type b. b domain_pool -> ('a, b * Over.t) man -> 'a post option list -> 'a flow option list -> 'a flow option list * Post.channel list =
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
                         let mflow' = List.fold_left (fun flow (stmt, zone) -> man.exec ~zone:zone stmt flow) flow' mergers in
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
         aux Config.pool man posts (List.map (OptionExt.option_lift1 (fun post -> post.Post.flow)) posts)
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
       | None -> Over.exec zone stmt (over_man man) flow
       | Some flow -> Some (
           reduce_exec stmt channels man flow |>
           Post.of_flow
         )
    )

  (* Evaluations *)
  (* *********** *)

  let eval_interface =
    let rec aux : type a. a domain_pool -> (Zone.zone * Zone.zone) interface =
      function
      | Nil -> Over.eval_interface
      | Cons(hd, tl) ->
        let module D = (val hd) in
        let after = aux tl in
        {
          import = D.eval_interface.import @ after.import;
          export = D.eval_interface.export @ after.export;
        }
    in
    aux Config.pool

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


  let eval zone =
    (* Computing zoning coverage of the pool *)
    let coverage =
      let rec aux: type b. b domain_pool -> bool list =
        function
        | Nil -> []
        | Cons(hd, tl) ->
          let module D = (val hd) in
          let b = List.exists (fun z -> Zone.sat_zone2 z zone) D.eval_interface.Domain.export in
          b :: aux tl
      in
      aux Config.pool
    in
    (fun exp man flow ->
       (* Point-wise evaluation *)
       let evls =
         let rec aux: type b. b domain_pool -> bool list -> ('a, b * Over.t) man -> ('a, Ast.expr) evl option list =
           fun pool coverage man ->
             match pool, coverage with
             | Nil, _ -> []
             | Cons(hd, tl), false :: ctl -> None :: aux tl ctl (tail_man man)
             | Cons(hd, tl), true :: ctl ->
               begin
                 let module D = (val hd) in
                 match D.eval zone exp (head_man man) flow with
                 | None -> None :: aux tl ctl (tail_man man)
                 | Some evl -> (Some evl) :: aux tl ctl (tail_man man)
               end
             | _ -> assert false

         in
         aux Config.pool coverage man
       in
       (* Transform list of evaluations into list of conjunctions *)
       let lconj =
         let rec aux : type b. b domain_pool -> ('a, Ast.expr) evl option list -> 'a evl_conj list =
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
       let res = List.fold_left (fun acc c ->
           OptionExt.option_neutral2 Eval.join acc (conj_to_evl c)
         ) None lconj'
       in

       match res with
       | None -> Over.eval zone exp (over_man man) flow
       | _ -> res
    )


  (* Queries *)
  (* ******** *)

  let ask query man flow =
    let rec aux : type b r. b domain_pool -> ('a, b * Over.t) man -> r Query.query -> r option =
      fun pool man' query ->
        match pool with
        | Nil -> Over.ask query (over_man man) flow
        | Cons(hd, tl) ->
          let module D = (val hd) in
          let r1 = D.ask query (head_man man') flow in
          let r2 = aux tl (tail_man man') query in
          OptionExt.option_neutral2 (Query.meet query) r1 r2
    in
    aux Config.pool man query


end
