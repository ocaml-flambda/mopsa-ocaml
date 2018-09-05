(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow abstraction for Goto statements. *)

open Framework.Essentials
open Ast


(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type token +=
  | T_goto of string
  (** Goto environments *)


(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

(* goto number of iteration before widening *)
let goto_wid_param = ref 3

module Domain : Framework.Domains.Stateless.S =
struct
  (* Save TCur env in T_goto s token, then set T_cur to bottom. *)
  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_flows_goto : unit domain
  let id = D_c_flows_goto
  let name = "c.flows.switch"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_flows_goto -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let zone = Zone.Z_c
  let import_exec = []
  let import_eval = []

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    None

  let exec stmt man flow =
    match skind stmt with
    | S_c_goto s ->
      (* Save TCur env in T_goto s token, then set T_cur to bottom. *)
      let cur = Flow.get T_cur man flow in
      let flow0 = Flow.add (T_goto s) cur man flow |>
                  Flow.remove T_cur man
      in
      Post.return flow0

    | S_c_label s ->
      (* Moves flow in goto label inside current *)
      let fromlbl = Flow.get (T_goto s) man flow in
      let flow0 = Flow.add T_cur fromlbl man flow |>
                  Flow.remove (T_goto s) man
      in
      Post.return flow0

    | S_c_goto_stab stmt' ->
      (* Stabilization statement for backward gotos *)
      begin
        let annot = Flow.get_annot flow in
        let nogotos, gotos = Flow.fold (fun (nogotos, gotos) k v ->
            match k with
            | T_goto s -> (nogotos, Flow.add k v man gotos)
            | _       -> (Flow.add k v man nogotos, gotos)
          ) (Flow.bottom annot, Flow.top annot) man flow in
        let next f f' i wid_limit =
          let get_gotos f = Flow.filter
              (fun t e -> match t with | T_goto s -> true | _ -> false) man f
          in
          let f1, f1' = get_gotos f, get_gotos f' in
          if Flow.subset man f1' f1 then
            None
          else if i >= wid_limit then
            Some (Flow.widen man f1 (Flow.join man f1 f1'))
          else Some (Flow.join man f1 f1')
        in
        let rec stabilization f i wid_limit =
          let f' = man.exec stmt' f in
          match next f f' i wid_limit with
          | None -> f'
          | Some f'' -> stabilization f'' (i+1) wid_limit
        in
        let flow1 = stabilization nogotos 0 3 in
        let flow1_minus_gotos = Flow.filter (fun k v -> match k with
            | T_goto s -> false | _ -> true) man flow1 in
        let flow2 = Flow.join man flow1_minus_gotos gotos in
        Post.return flow2
      end

    | _ -> None

  let eval man exp flow = None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain);
  Framework.Options.register_option ("-goto-wid",
                                     Arg.Int (fun i -> goto_wid_param := i),
                                     "Number of iteration allowed for wideining iteration");
  register_token
    { print = (fun next fmt -> function
          | T_goto str -> Format.fprintf fmt "goto %s" str
          | tk -> next fmt tk
        );
      compare = (fun next a b ->
          match a,b with
          | T_goto x, T_goto y -> compare x y
          | _ -> next a b
        )
    }
