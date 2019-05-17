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
open Ast
open Zone

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
(**                       {2 Command line options}                          *)
(*==========================================================================*)

let opt_loop_widening_delay : int ref = ref 0
(** Number of iterations before applying a widening. *)

let opt_loop_unrolling : int ref = ref 1
(** Number of unrolling iterations before joining the environments. *)

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
    spec = ArgExt.Set_int opt_loop_unrolling;
    default = "1";
  }


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain  =
struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt


  let interface = {
    iexec = { provides = [Z_u]; uses = [] };
    ieval = { provides = []; uses = [] };
  }


  module Lctx = Context.GenPolyKey(
    struct
        type 'a t = (range * 'a) list
        let ctx = Context.empty
        let print fmt ctx =
        Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
          (fun fmt (range, env) -> Format.fprintf fmt "%a -> TODO" pp_range range) fmt ctx
      end
    )

  let search_lctx srange flow =
    try
      let l = Context.find_poly Lctx.key (Flow.get_ctx flow) in
      List.find_opt (fun (range, _) ->
        let srange = untag_range srange and range = untag_range range in
        match srange, range with
        | R_orig (s1, e1), R_orig (s2, e2) -> s1.pos_file = s2.pos_file && abs (s1.pos_line - s2.pos_line) <= 1
        | _ -> false
      ) l
    with Not_found -> None

  let init prog man flow = flow

  let rec exec zone stmt (man:('a,unit) man) flow =
    match skind stmt with
    | S_while(cond, body) ->
      (* debug "while:@\n abs = @[%a@]" (Flow.print man.lattice) flow; *)

      let flow0 = Flow.remove T_continue flow |>
                  Flow.remove T_break
      in

      let flow_init, flow_out = unroll cond body man flow0 in

      let flow_init = Option.apply flow_init (fun (_, old_lfp) ->
          let old_flow = Flow.add T_cur old_lfp man.lattice (Flow.bottom (Flow.get_ctx flow_init)) in
          Flow.join man.lattice old_flow flow_init) (search_lctx stmt.srange flow_init) in

      (* debug "post unroll:@\n abs0 = @[%a@]@\n abs out = @[%a@]"
       *   (Flow.print man.lattice) flow_init
       *   (Flow.print man.lattice) flow_out
       * ; *)

      let flow_lfp = lfp 0 !opt_loop_widening_delay cond body man flow_init flow_init in
      let old_lfpctx = try Context.find_poly Lctx.key (Flow.get_ctx flow_lfp) with Not_found -> [] in
      let lfpctx = (stmt.srange, Flow.get T_cur man.lattice flow_lfp) :: old_lfpctx in
      let flow_lfp = Flow.set_ctx (Context.add_poly Lctx.key lfpctx (Flow.get_ctx flow_lfp)) flow_lfp in

      let res0 =
        man.exec (mk_assume (mk_not cond cond.erange) cond.erange) flow_lfp |>
        Flow.join man.lattice flow_out
      in

      let res1 = Flow.add T_cur (Flow.get T_break man.lattice res0) man.lattice res0 |>
                 Flow.set T_break (Flow.get T_break man.lattice flow) man.lattice |>
                 Flow.set T_continue (Flow.get T_continue man.lattice flow) man.lattice
      in

      (* debug "while post abs %a:@\n abs = @[%a@]" pp_range stmt.srange (Flow.print man.lattice) res1; *)

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

  and lfp count delay cond body man flow_init flow =
    debug "lfp called, range = %a, count = %d@\nflow = %a@\n" pp_range body.srange count (Flow.print man.lattice) flow;
    let flow0 = Flow.remove T_continue flow |>
                Flow.remove T_break
    in

    (* debug "lfp:@\n delay = %d@\n abs = @[%a@]"
     *   delay (Flow.print man.lattice) flow0
     * ; *)

    let flow1 = man.exec (mk_assume cond cond.erange) flow0 |>
                man.exec body
    in

    let flow2 = merge_cur_and_continue man flow1 in

    (* debug "lfp post:@\n res = @[%a@]" (Flow.print man.lattice) flow1; *)

    let flow3 = Flow.join man.lattice flow_init flow2 in

    (* debug "lfp join: %a@\n res = @[%a@]" pp_range body.srange (Flow.print man.lattice) flow3; *)
    let is_sub = Flow.subset man.lattice flow3 flow in
    debug "lfp range %a is_sub: %b@\n" pp_range body.srange is_sub;
    if is_sub then flow3
    else if delay = 0 then
      let wflow = Flow.widen man.lattice flow flow3 in
      let () = debug
          "widening: %a@\n abs =@\n@[  %a@]@\n abs' =@\n@[  %a@]@\n res =@\n@[  %a@]"
          pp_range body.srange
          (Flow.print man.lattice) flow
          (Flow.print man.lattice) flow3
          (Flow.print man.lattice) wflow
      in
      lfp (count+1) !opt_loop_widening_delay cond body man flow_init wflow
    else
      lfp (count+1) (delay - 1) cond body man flow_init flow3

  and unroll cond body man flow =
    let rec loop i flow =
      debug "unrolling iteration %d in %a" i pp_range (srange body);
      let annot = Flow.get_ctx flow in
      if i = 0 then (flow, Flow.bottom annot)
      else
        let flow1 =
          man.exec {skind = S_assume cond; srange = cond.erange} flow |>
          man.exec body |>
          merge_cur_and_continue man
        in

        let flow2 =
          man.exec (mk_assume (mk_not cond cond.erange) cond.erange) (Flow.copy_ctx flow1 flow)
        in
        let flow1', flow2' = loop (i - 1) (Flow.copy_ctx flow2 flow1) in
        flow1', Flow.join man.lattice flow2 flow2'
    in
    loop !opt_loop_unrolling flow

  and merge_cur_and_continue man flow =
    Flow.map (fun tk eabs ->
        match tk with
        | T_cur ->
          let cont = Flow.get T_continue man.lattice flow in
          man.lattice.join eabs cont
        | T_continue -> man.lattice.bottom
        | _ -> eabs
      ) flow

  let eval _ _ _ _ = None

  let ask _ _ _ = None

end

(*==========================================================================*)
(**                               {2 Setup}                                 *)
(*==========================================================================*)


let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
