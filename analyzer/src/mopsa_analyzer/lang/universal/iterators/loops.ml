(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Loops iterator with widening *)


open Mopsa
open Sig.Abstraction.Stateless
open Ast

let name = "universal.iterators.loops"


(*==========================================================================*)
(**                         {2 Loops flow token}                            *)
(*==========================================================================*)

type token +=
  | T_break
  (** Control flows reaching a break statement *)

  | T_continue
  (** Control flows reaching a continue statement *)

let () =
  register_token {
    compare = (fun next tk1 tk2 -> next tk1 tk2);
    print   = (fun next fmt tk ->
        match tk with
        | T_break -> Format.pp_print_string fmt "break"
        | T_continue -> Format.pp_print_string fmt "cont"
        | _ -> next fmt tk
      );
  }


(*==========================================================================*)
(**                        {2 Unrolling strategy}                           *)
(*==========================================================================*)

type unrolling = {
  mutable unroll_global_nb : int option (* unrolling numbers *);
  mutable unroll_locals : local_unrolling list;
}

and local_unrolling = {
  unroll_local_file : string option;
  unroll_local_line : int;
  unroll_local_nb : int option;
}

let opt_unrolling = {
  unroll_global_nb = Some 1;
  unroll_locals = [];
}


(** Parse local unrolling specification string *)
let parse_unroll_local (spec:string) : local_unrolling =
  if not Str.(string_match (regexp "^\\(\\([a-zA-Z][^:]*\\):\\)?\\([0-9]+\\):\\([0-9]+\\)$") spec 0) then
    panic "incorrect argument '%s' for option -loop-unrolling-at" spec
  ;
  let file = try Some (Str.matched_group 2 spec) with Not_found -> None in
  let line = Str.matched_group 3 spec |> int_of_string in
  let nb = Some (Str.matched_group 4 spec |> int_of_string) in
  { unroll_local_file = file;
    unroll_local_line = line;
    unroll_local_nb = nb; }

(** Parse local full unrolling specification string *)
let parse_full_unroll_local (spec:string) : local_unrolling =
  if not Str.(string_match (regexp "^\\(\\([a-zA-Z][^:]*\\):\\)?\\([0-9]+\\)$") spec 0) then
    panic "incorrect argument '%s' for option -loop-full-unrolling-at" spec
  ;
  let file = try Some (Str.matched_group 2 spec) with Not_found -> None in
  let line = Str.matched_group 3 spec |> int_of_string in
  { unroll_local_file = file;
    unroll_local_line = line;
    unroll_local_nb = None; }

(** Get the unrolling limit for a given loop location *)
let get_range_unrolling range : int option =
  let range = untag_range range in
  let is_matching u =
    match_range_line u.unroll_local_line range
    && match u.unroll_local_file with
    | None -> true
    | Some file -> match_range_file file range in
  match List.find_opt is_matching opt_unrolling.unroll_locals with
  | Some u -> u.unroll_local_nb
  | None -> opt_unrolling.unroll_global_nb


(*==========================================================================*)
(**                       {2 Command line options}                          *)
(*==========================================================================*)

let opt_loop_widening_delay : int ref = ref 0
(** Number of iterations before applying a widening. *)

let opt_loop_use_cache : bool ref = ref false

let opt_loop_decreasing_it : bool ref = ref false

let () =
  register_domain_option name {
    key = "-widening-delay";
    category = "Loops";
    doc = " number of iterations before applying a widening";
    spec = ArgExt.Set_int opt_loop_widening_delay;
    default = "0";
  };
  register_domain_option name {
    key = "-loop-unrolling";
    category = "Loops";
    doc = " number of unrolling iterations before joining the environments";
    spec = ArgExt.Int (fun n -> opt_unrolling.unroll_global_nb <- Some n);
    default = "1";
  };
  register_domain_option name {
    key = "-loop-unrolling-at";
    category = "Loops";
    doc = " number of unrolling iterations at specific program location (syntax: [file.]line:unrolling)";
    spec = ArgExt.String_list (fun specs -> opt_unrolling.unroll_locals <- List.map parse_unroll_local specs);
    default = "";
  };
  register_domain_option name {
    key = "-loop-full-unrolling";
    category = "Loops";
    doc = " unroll loops without applying widening";
    spec = ArgExt.Bool (fun b -> opt_unrolling.unroll_global_nb <- (if b then None else opt_unrolling.unroll_global_nb));
    default = "false";
  };
  register_domain_option name {
    key = "-loop-full-unrolling-at";
    category = "Loops";
    doc = " fully unroll loop at specific program location (syntax: [file.]line)";
    spec = ArgExt.String_list (fun specs -> opt_unrolling.unroll_locals <- List.map parse_full_unroll_local specs);
    default = "";
  };
  register_domain_option name {
    key = "-loop-no-cache";
    category = "Loops";
    doc = " do not use cache for loops";
    spec = ArgExt.Clear opt_loop_use_cache;
    default = "use cache";
  };
  register_domain_option name {
    key = "-loop-decr-it";
    category = "Loops";
    doc = " enable decreasing iteration";
    spec = ArgExt.Set opt_loop_decreasing_it;
    default = "disabled";
  }

(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

let nestedness = ref 0

module Domain  =
struct

  (** {3 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId(struct
      let name = name
    end)

  let checks = []

  (** {3 Cache of last fixpoint} *)
  (** ************************** *)

  (** Fixpoints are attached to the callstack and the location of the loop head *)
  module LoopHeadMap = MapExt.Make(
    struct
      type t = (callstack * range)
      let compare (cs1, r1) (cs2, r2) =
        Compare.compose
          [(fun () -> compare_range r1 r2);
           (fun () -> compare_callstack cs1 cs2);
          ]
      let print fmt (cs, r) = Format.fprintf fmt "[%a, %a]" pp_range r pp_callstack cs end)

  (** Cache of the last fixpoints at loop heads *)
  module LastFixpointCtx = GenContextKey(
    struct
        type 'a t = 'a flow LoopHeadMap.t
        let print l fmt ctx = Format.fprintf fmt "Lfp cache context: %a"
            (LoopHeadMap.fprint
               MapExt.printer_default
               (fun fmt (cs, r) -> pp_callstack fmt cs; pp_range fmt r)
               (fun fmt flow -> format (TokenMap.print l) fmt (Flow.get_token_map flow))) ctx
      end
    )


  (** Search the last fixpoint attached to a loop *)
  let search_last_fixpoint (srange, scs) man flow =
    try
      let m = find_ctx LastFixpointCtx.key (Flow.get_ctx flow) in
      let mf = LoopHeadMap.filter (fun (cs, range) _ ->
                   compare_callstack cs scs = 0 &&
                     compare_range srange range = 0
                 ) m in
      Some (LoopHeadMap.choose mf |> snd |> Flow.join man.lattice flow)

    with Not_found ->
      (debug "no ctx found";
       None)


  (** Update the last fixpoint attached to a loop *)
  let store_fixpoint man flow (range, cs) =
    let old_lfp_ctx =
      try
        let r = find_ctx LastFixpointCtx.key (Flow.get_ctx flow) in
        if LoopHeadMap.cardinal r > 5 then
          LoopHeadMap.remove_min_binding r
        else r
      with Not_found -> LoopHeadMap.empty in
    let stripped_flow =
      Flow.bottom (Flow.get_ctx flow) (Flow.get_report flow) |>
      Flow.add T_cur (Flow.get T_cur man.lattice flow) man.lattice |>
      Flow.add T_continue (Flow.get T_continue man.lattice flow) man.lattice |>
      Flow.add T_break (Flow.get T_break man.lattice flow) man.lattice
    in
    Debug.debug ~channel:(name ^ ".cache") "@(%a, %a): adding %a" pp_range range pp_callstack cs (format (Flow.print man.lattice.print)) stripped_flow;
    let lfp_ctx = LoopHeadMap.add (cs, range) stripped_flow old_lfp_ctx in
    Flow.set_ctx (add_ctx LastFixpointCtx.key lfp_ctx (Flow.get_ctx flow)) flow

  let join_w_old_lfp man flow range =
    debug "searching in cache";
    match search_last_fixpoint range man flow with
    | None -> None
    | Some old_lfp ->
      let res = Flow.join man.lattice old_lfp flow in
      Debug.debug ~channel:"universal.iterators.loops.cache" "cache: %a join %a = %a@\n" (format (Flow.print man.lattice.print)) old_lfp (format (Flow.print man.lattice.print)) flow (format (Flow.print man.lattice.print)) res;
      Some res
    (* flow *)


  (** Merge tokens T_cur and T_continue into T_cur *)
  let merge_cur_and_continue man flow =
    let cur = Flow.get T_cur man.lattice flow in
    let cont = Flow.get T_continue man.lattice flow in
    let ctx = Flow.get_ctx flow in
    Flow.set T_cur (man.lattice.join ctx cur cont) man.lattice flow |>
    Flow.remove T_continue


  let init prog man flow =
    Flow.map_ctx (add_ctx LastFixpointCtx.key LoopHeadMap.empty) flow

  let decr_iteration cond body man flow_init flow =
    Flow.remove T_continue flow |>
    Flow.remove T_break |>
    Flow.remove_report |>
    man.exec (mk_assume cond cond.erange) >>%
    man.exec body >>% fun flow ->
    merge_cur_and_continue man flow |>
    Flow.join man.lattice flow_init |>
    Post.return

  let rec lfp count delay cond body man flow_init flow =
    debug "lfp called, range = %a, count = %d" pp_range body.srange count;
    (* Ignore continue and break flows of the previous iterations *)
    Flow.remove T_continue flow |>
    Flow.remove T_break |>
    (* Ignore report of the previous iterations *)
    Flow.set_report (Flow.get_report flow_init) |>
    (* Apply condition and execute body *)
    man.exec (mk_assume cond cond.erange) >>%
    man.exec body >>% fun f ->
    (* Merge cur and continue flows *)
    let flow' = merge_cur_and_continue man f |>
                Flow.join man.lattice flow_init
    in
    (* Check convergence of cur flow *)
    let cur = Flow.get T_cur man.lattice flow in
    let cur' = Flow.get T_cur man.lattice flow' in
    let is_sub = man.lattice.subset (Flow.get_ctx flow') cur' cur in
    debug "lfp range %a is_sub: %b" pp_range body.srange is_sub;
    if is_sub then
      (* Convergence *)
      Post.return flow'
    else if delay = 0 then
      (* Widen *)
      let wflow = Flow.widen man.lattice flow flow' in
      lfp (count+1) !opt_loop_widening_delay cond body man flow_init wflow
    else
      (* Delay *)
      lfp (count+1) (delay - 1) cond body man flow_init flow'


  let rec unroll i cond body man flow =
    debug "unrolling iteration %a in %a"
      (OptionExt.print ~none:"" ~some:"" Format.pp_print_int) i
      pp_range (srange body);
    if i = Some 0 then
      Cases.singleton (false, Flow.bottom (Flow.get_ctx flow) (Flow.get_report flow)) flow
    else
      let post1 =
        man.exec {skind = S_assume cond; srange = cond.erange} flow >>%
        man.exec body >>% fun f ->
        merge_cur_and_continue man f |>
        Post.return
      in

      let post2 =
        man.exec (mk_assume (mk_not cond cond.erange) cond.erange) (Flow.set_ctx (Cases.get_ctx post1) flow)
      in

      post1 >>% fun flow1 ->
      post2 >>% fun flow2 ->
      if Flow.subset man.lattice flow1 flow then
        let () = debug "stabilisation reached in unrolling!" in
        Cases.singleton (true, flow2) flow1
      else
        unroll (OptionExt.lift (fun ii -> (ii - 1)) i) cond body man (Flow.copy_ctx flow2 flow1) >>$ fun (flag, flow2') flow1' ->
        Cases.singleton (flag, Flow.join man.lattice flow2 flow2') flow1'


  let rec exec stmt man flow =
    match skind stmt with
    | S_while(cond, body) ->
      incr nestedness;
      Debug.debug ~channel:"nested" "nestedness: %d" !nestedness;
      debug "while %a:" (* @\nflow = @[%a@] *) pp_range stmt.srange (* (Flow.print man.lattice) flow *);

      let flow0 = Flow.remove T_continue flow |>
                  Flow.remove T_break
      in

      begin if !opt_loop_use_cache then
          match join_w_old_lfp man flow0 (stmt.srange, Flow.get_callstack flow0) with
          | Some flow0 -> Cases.singleton (false, Flow.bottom (Flow.get_ctx flow0) (Flow.get_report flow0)) flow0
          | None -> unroll (get_range_unrolling stmt.srange) cond body man flow0
        else
          unroll (get_range_unrolling stmt.srange) cond body man flow0
      end >>$? fun (is_fp, flow_out) flow_init ->

      begin
        if is_fp then
          Post.return flow_init
        else
          lfp 0 !opt_loop_widening_delay cond body man flow_init flow_init >>% fun flow_lfp ->
          let flow_lfp = if !opt_loop_use_cache then
                           store_fixpoint man flow_lfp (stmt.srange, Flow.get_callstack flow_lfp) else flow_lfp in
          Post.return flow_lfp
      end >>%? fun flow_lfp ->
      man.exec (mk_assume (mk_not cond cond.erange) cond.erange) flow_lfp >>%? fun f ->
      let res0 = Flow.join man.lattice flow_out f in

      debug "while post abs %a:" (* @\nres0 = @[%a@] *) pp_range stmt.srange (* (Flow.print man.lattice) res0 *);

      let res1 = Flow.add T_cur (Flow.get T_break man.lattice res0) man.lattice res0 |>
                 Flow.set T_break (Flow.get T_break man.lattice flow) man.lattice |>
                 Flow.set T_continue (Flow.get T_continue man.lattice flow) man.lattice
      in

      decr nestedness;
      Some (Post.return res1)

    | S_break ->
      let cur = Flow.get T_cur man.lattice flow in
      let flow' = Flow.add T_break cur man.lattice flow |>
                  Flow.remove T_cur
      in
      Some (Post.return flow')

    | S_continue ->
      let cur = Flow.get T_cur man.lattice flow in
      let flow' = Flow.add T_continue cur man.lattice flow |>
                  Flow.remove T_cur
      in
      Some (Post.return flow')

    | _ -> None

  let eval _ _ _ = None

  let ask _ _ _ = None

  let print_expr man flow printer exp = ()

end

(*==========================================================================*)
(**                               {2 Setup}                                 *)
(*==========================================================================*)


let () =
  register_stateless_domain (module Domain)
