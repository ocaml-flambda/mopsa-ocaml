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

(** Common utility definitions used by combiners *)

open Core.All
open Mopsa_utils


(** Kinds of domain combiners *)
type combiner =
  | Sequence
  | Compose
  | Product

(** GADT identifiers used by domain combiners *)
type _ id +=
  | C_empty : unit id
  | C_pair : combiner * 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | C_empty, C_empty -> Some Eq
        | C_pair(c1,x1,y1), C_pair(c2,x2,y2) ->
          if c1 = c2 then
            match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          else
            None
        | _ -> next.eq id1 id2
      in
      f
    );
  }

(** GADT identifiers used by value combiners *)
type _ id += V_empty : unit id
type _ id += V_pair  : 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | V_empty, V_empty -> Some Eq
        | V_pair(x1,y1), V_pair(x2,y2) ->
          begin match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          end
        | _ -> next.eq id1 id2
      in
      f
    );
  }


(** [sat_targets ~domains ~targets] checks whether a combiner containing [domains] satisifies some route [targets] *)
let sat_targets ~targets ~domains =
  match targets with
  | None   -> true
  | Some s -> not (DomainSet.is_empty (DomainSet.inter s domains))


(** Manager of the left argument in a pair of domains *)
let fst_pair_man (man:('a, 'b * 'c) man) : ('a, 'b) man = {
  man with
  get = get_pair_fst man;
  set = set_pair_fst man;
  get_effects = (fun geffects -> man.get_effects geffects |> get_left_teffect);
  set_effects = (fun effects geffects -> man.set_effects (
      mk_teffect empty_effect effects (man.get_effects geffects |> get_right_teffect)
    ) geffects);
}

(** Manager of the right argument in a pair of domains *)
let snd_pair_man (man:('a, 'b * 'c) man) : ('a, 'c) man = {
  man with
  get = get_pair_snd man;
  set = set_pair_snd man;
  get_effects = (fun geffects -> man.get_effects geffects |> get_right_teffect);
  set_effects = (fun effects geffects -> man.set_effects (
      mk_teffect empty_effect (man.get_effects geffects |> get_left_teffect) effects
    ) geffects);
}


(** Find the manager of a domain given the manager of a combiner containing it *)
let rec find_domain_man : type b c. target:b id -> tree:c id -> ('a,c) man -> ('a,b) man = fun ~target ~tree man ->
  match tree with
  | C_empty -> raise Not_found
  | C_pair(_,left,right) ->
    begin
      try find_domain_man ~target ~tree:left (fst_pair_man man)
      with Not_found -> find_domain_man ~target ~tree:right (snd_pair_man man)
    end
  | _ ->
    match equal_id target tree with
    | Some Eq -> man
    | None -> raise Not_found

(** Check whether a domain is part of a combiner tree *)
let rec mem_domain : type b c. target:b id -> tree:c id -> bool = fun ~target ~tree ->
  match tree with
  | C_empty -> false
  | C_pair(_,left,right) -> mem_domain ~target ~tree:left || mem_domain ~target ~tree:right
  | _ ->
    match equal_id target tree with
    | Some Eq -> true
    | None -> false

let pp_domains fmt s =
  Format.(pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            pp_print_string)
    fmt
    (DomainSet.elements s)

(** Apply transfer functions [f1] and [f2] in cascade. Function [f1]
    is called first. When [f1] returns [None] or not-handled cases,
    [f2] is called. Note that not-handled cases are joined in order to
    call [f2] only once. *)
let cascade_call targets f1 domains1 f2 domains2 =
  match sat_targets ~targets ~domains:domains1,
        sat_targets ~targets ~domains:domains2
  with
  | false, false ->
    (* Both domains do not provide an [exec] for such targets *)
    Exceptions.panic "switch: targets '%a' not found in %a nor %a"
      (OptionExt.print pp_domains) targets
      pp_domains domains1
      pp_domains domains2

  | true, false ->
    (* Only [D1] provides a transfer function for such targets *)
    let f = f1 targets in
    (fun cmd man flow ->
       f cmd (fst_pair_man man) flow)

  | false, true ->
    (* Only [D2] provides a transfer function for such targets *)
    let f = f2 targets in
    (fun cmd man flow ->
       f cmd (snd_pair_man man) flow)

  | true, true ->
    (* Both [D1] and [D2] provide a transfer function for such targets *)
    let ff1 = f1 targets in
    let ff2 = f2 targets in
    (fun cmd man flow ->
       match ff1 cmd (fst_pair_man man) flow with
       | None ->
         ff2 cmd (snd_pair_man man) flow

       | Some ret1 ->
         (* Collect cases not handled by [D1] and pass them to [D2] *)
         match Cases.partition
                 (fun c flow ->
                    match c with NotHandled -> true | _ -> false)
                 ret1
         with
         | None, _ -> Some ret1
         | Some not_handled1, handled1 ->
           (* Fusion all not-handled cases to speedup the analysis *)
           let not_handled1' = Cases.remove_duplicates compare man.lattice not_handled1 in
           (* Call [D2] on not-handled cases *)
           let ret2 = not_handled1' >>=? fun _ flow -> ff2 cmd (snd_pair_man man) flow in
           OptionExt.neutral2 Cases.join handled1 ret2)

let cascade_stateless_call targets f1 domains1 f2 domains2 =
  match sat_targets ~targets ~domains:domains1,
        sat_targets ~targets ~domains:domains2
  with
  | false, false ->
    (* Both domains do not provide an [exec] for such targets *)
    Exceptions.panic "switch: targets '%a' not found in %a nor %a"
      (OptionExt.print pp_domains) targets
      pp_domains domains1
      pp_domains domains2

  | true, false ->
    (* Only [D1] provides a transfer function for such targets *)
    f1 targets

  | false, true ->
    (* Only [D2] provides a transfer function for such targets *)
    f2 targets

  | true, true ->
    (* Both [D1] and [D2] provide a transfer function for such targets *)
    let ff1 = f1 targets in
    let ff2 = f2 targets in
    (fun cmd man flow ->
       match ff1 cmd man flow with
       | None ->
         ff2 cmd man flow

       | Some ret1 ->
         (* Collect cases not handled by [D1] and pass them to [D2] *)
         match Cases.partition
                 (fun c flow ->
                    match c with NotHandled -> true | _ -> false)
                 ret1
         with
         | None, _ -> Some ret1
         | Some not_handled1, handled1 ->
           (* Fusion all not-handled cases to speedup the analysis *)
           let not_handled1' = Cases.remove_duplicates compare man.lattice not_handled1 in
           (* Call [D2] on not-handled cases *)
           let ret2 = not_handled1' >>=? fun _ flow -> ff2 cmd man flow in
           OptionExt.neutral2 Cases.join handled1 ret2)

let broadcast_call targets f1 domains1 f2 domains2 =
  match sat_targets ~targets ~domains:domains1,
        sat_targets ~targets ~domains:domains2
  with
  | false, false ->
    (* Both domains do not provide an [exec] for such targets *)
    Exceptions.panic "broadcast_call: targets '%a' not found in %a nor %a"
      (OptionExt.print pp_domains) targets
      pp_domains domains1
      pp_domains domains2

  | true, false ->
    (* Only [D1] provides a transfer function for such targets *)
    let f = f1 targets in
    (fun cmd man flow ->
       f cmd (fst_pair_man man) flow)

  | false, true ->
    (* Only [D2] provides a transfer function for such targets *)
    let f = f2 targets in
    (fun cmd man flow ->
       f cmd (snd_pair_man man) flow)

  | true, true ->
    (* Both [D1] and [D2] provide a transfer function for such targets *)
    let ff1 = f1 targets in
    let ff2 = f2 targets in
    (fun cmd man flow ->
       OptionExt.neutral2 Cases.join 
         (ff1 cmd (fst_pair_man man) flow)
         (ff2 cmd (snd_pair_man man) flow))

let broadcast_stateless_call targets f1 domains1 f2 domains2 =
  match sat_targets ~targets ~domains:domains1,
        sat_targets ~targets ~domains:domains2
  with
  | false, false ->
    (* Both domains do not provide an [exec] for such targets *)
    Exceptions.panic "broadcast_stateless_call: targets '%a' not found in %a nor %a"
      (OptionExt.print pp_domains) targets
      pp_domains domains1
      pp_domains domains2

  | true, false ->
    (* Only [D1] provides a transfer function for such targets *)
    f1 targets

  | false, true ->
    (* Only [D2] provides a transfer function for such targets *)
    f2 targets

  | true, true ->
    (* Both [D1] and [D2] provide a transfer function for such targets *)
    let ff1 = f1 targets in
    let ff2 = f2 targets in
    (fun cmd man flow ->
       OptionExt.neutral2 Cases.join 
         (ff1 cmd man flow)
         (ff2 cmd man flow))
