(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** A stacked combination of D1 over D2 allows domain D1 to unify the
    state of D2 before lattice binary operators *)

open Ast
open Manager
open Annotation
open Domain
open Post

module type S =
sig
  (* Most parts are unchanged from DOMAIN signature *)
  include Domain.DOMAIN

  (* Binary lattice operators can unify the state of the underneath domain *)
  val join: 'a annot -> ('b, 'b) man -> t * ('b flow) -> t * ('b flow) -> t * ('b flow) * ('b flow)
  val meet: 'a annot -> ('b, 'b) man -> t * ('b flow) -> t * ('b flow) -> t * ('b flow) * ('b flow)
  val widen: 'a annot -> ('b, 'b) man -> t * ('b flow) -> t * ('b flow) -> t * bool * ('b flow) * ('b flow)
  val subset: ('b, 'b) man -> t * ('b flow) -> t * ('b flow) -> bool * ('b flow) * ('b flow)

end

let create_flow_cur_only (b: 'b) (man: ('b, 'b) man): 'b flow =
  (* FIXME: this empty annotation may lead to unsuccessful lookups of flow-insensitive information *)
  Flow.bottom Annotation.empty
  |> Flow.add T_cur b man

let get_env_from_flow_cur_only (f: 'b flow) (man: ('b, 'b) man): 'b =
  Flow.get T_cur man f

let lift_to_flow man f =
  fun b1 b2 ->
    let b1', b2' = create_flow_cur_only b1 man, create_flow_cur_only b2 man in
    let (a, b1'', b2'') = f b1' b2' in
    (a, get_env_from_flow_cur_only b1'' man, get_env_from_flow_cur_only b2'' man)

let lift_widen_to_flow man f =
  fun b1 b2 ->
    let flow1 = create_flow_cur_only b1 man in
    let flow2 = create_flow_cur_only b2 man in
    let a, is_stable, flow1', flow2' = f flow1 flow2 in
    let b1' = get_env_from_flow_cur_only flow1' man in
    let b2' = get_env_from_flow_cur_only flow2' man in
    a, is_stable, b1', b2'

(** Combine two domains with a stack configuration. *)
module Make(D1: S)(D2: Domain.DOMAIN) : Domain.DOMAIN =
struct

  module LocalAnalyzer = Analyzer.Make(D2)

  (* Lattice operators *)
  (* ================= *)

  type t = D1.t * D2.t

  let bottom = D1.bottom, D2.bottom

  let top = D1.top, D2.top

  let is_bottom (a,b) = D1.is_bottom a || D2.is_bottom b

  let join annot (a1, b1) (a2, b2) =
    let a, b1', b2' = lift_to_flow LocalAnalyzer.man (fun b1 b2 -> D1.join annot LocalAnalyzer.man (a1, b1) (a2, b2)) b1 b2 in
    a, D2.join annot b1' b2'

  let meet annot (a1, b1) (a2, b2) =
    let a, b1', b2' = lift_to_flow LocalAnalyzer.man (fun b1 b2 -> D1.meet annot LocalAnalyzer.man (a1, b1) (a2, b2)) b1 b2  in
    a, D2.meet annot b1' b2'

  let widen annot (a1, b1) (a2, b2) =
    let a, is_stable, b1', b2' =
      lift_widen_to_flow LocalAnalyzer.man (fun b1 b2 -> D1.widen annot LocalAnalyzer.man (a1, b1) (a2, b2)) b1 b2
    in
    a, if is_stable then D2.widen annot b1' b2' else D2.join annot b1' b2'

  let subset (a1, b1) (a2, b2) =
    let b, b1', b2' = lift_to_flow LocalAnalyzer.man (fun b1 b2 -> D1.subset LocalAnalyzer.man (a1, b1) (a2, b2)) b1 b2  in
    b && (D2.subset b1' b2')

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" D1.print a D2.print b


  (* Domain identification *)
  (* ===================== *)

  type _ domain += D_stacked : t domain
  let name = D1.name ^ "/" ^ D2.name
  let id = D_stacked
  let identify : type b. b domain -> (t, b) eq option =
    function
    | D_stacked -> Some Eq
    | _ -> None


  (* Managers definition *)
  (* =================== *)

  let man1 man = {
    man with
    get = (fun flow -> fst @@ man.get flow);
    set = (fun hd flow -> man.set (hd, snd @@ man.get flow) flow);
  }

  let man2 man = {
    man with
    get = (fun flow -> snd @@ man.get flow);
    set = (fun tl flow -> man.set (fst @@ man.get flow, tl) flow);
  }


  (* Initial states *)
  (* ============== *)

  let init prog man flow =
    let flow', b = match D1.init prog (man1 man) flow with
      | None -> flow, false
      | Some flow' -> flow', true
    in
    match D2.init prog (man2 man) flow', b with
    | None, false -> None
    | None, true -> Some flow'
    | x, _ -> x


  (* Computation of post-conditions *)
  (* ============================== *)

  let exec_interface = Domain.{
    import = D1.exec_interface.import @ D2.exec_interface.import;
    export = D1.exec_interface.export @ D2.exec_interface.export;
  }

  let exec zone =
    match List.exists (fun z -> Zone.sat_zone z zone) D1.exec_interface.Domain.export,
          List.exists (fun z -> Zone.sat_zone z zone) D2.exec_interface.Domain.export
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = D1.exec zone in
      (fun stmt man flow -> f stmt (man1 man) flow)

    | false, true ->
      let f = D2.exec zone in
      (fun stmt man flow -> f stmt (man2 man) flow)

    | true, true ->
      let f1 = D1.exec zone in
      let f2 = D2.exec zone in
      (fun stmt man flow ->
         match f1 stmt (man1 man) flow with
         | Some post -> Some post
         | None -> f2 stmt (man2 man) flow
      )


  (* Evaluation of expressions *)
  (* ========================= *)

  let eval_interface = Domain.{
      import = D1.eval_interface.import @ D2.eval_interface.import;
      export = D1.eval_interface.export @ D2.eval_interface.export;
    }

  let eval zpath =
    match List.exists (fun p -> Zone.sat_zone2 p zpath) D1.eval_interface.Domain.export,
          List.exists (fun p -> Zone.sat_zone2 p zpath) D2.eval_interface.Domain.export
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = D1.eval zpath in
      (fun exp man flow -> f exp (man1 man) flow)

    | false, true ->
      let f = D2.eval zpath in
      (fun exp man flow -> f exp (man2 man) flow)

    | true, true ->
      let f1 = D1.eval zpath in
      let f2 = D2.eval zpath in
      (fun exp man flow ->
         match f1 exp (man1 man)  flow with
         | Some evl -> Some evl
         | None -> f2 exp (man2 man) flow
      )


  (* Query handler *)
  (* ============= *)

  let ask query man flow =
    let reply1 = D1.ask query (man1 man) flow in
    let reply2 = D2.ask query (man2 man) flow in
    OptionExt.option_neutral2 (Query.join query) reply1 reply2

end


(* Create a stacked domain from a non-stacked one *)
module MakeStacked(D: DOMAIN)=
struct
  (* Most values are similar *)
  include D

  (* Except binary operators *)
  let join annot man (a1, b1) (a2, b2) = D.join annot a1 a2, b1, b2
  let meet annot man (a1, b1) (a2, b2) = D.meet annot a1 a2, b1, b2
  let widen annot man (a1, b1) (a2, b2) = D.widen annot a1 a2, true, b1, b2
  let subset man (a1, b1) (a2, b2) = D.subset a1 a2, b1, b2
end


(* Registration of a stacked domain *)
(* ================================ *)

let domains : (module S) list ref = ref []

let register_domain f = domains := f :: !domains

let rec find_domain name =
  let rec aux = function
  | [] -> raise Not_found
  | hd :: tl ->
    let module D = (val hd : S) in
    if D.name = name then (module D : S) else aux tl
  in
  aux !domains

let mem_domain name =
  List.exists (fun d ->
      let module D = (val d : S) in
      D.name = name
    ) !domains
