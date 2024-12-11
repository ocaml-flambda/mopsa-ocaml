(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2023 The MOPSA Project.                               *)
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

(** State partitioning depending on the value of target integer variables *)

open Mopsa
open Sig.Abstraction.Partitioning
open Ast

module Domain =
struct

  type v =
    | Equal of Z.t
    | Other

  type t = v option

  let name = "universal.partitioning.int-var"

  let checks = []

  let opt_target_name = ref ""

  let opt_target_values = ref []

  let parse_option s =
    match String.split_on_char ':' s with
    | [v] ->
      opt_target_name := v;
      opt_target_values := [Z.zero; Z.one]
    | [v;vals] ->
      opt_target_name := v;
      opt_target_values := String.split_on_char ',' vals |> List.map Z.of_string
    | _ ->
      panic "incorrect syntax for option -state-partition-var"

  let () = register_domain_option name {
      key = "-state-partition-int-var";
      doc = "name of the variable used to partition the states";
      spec = ArgExt.String parse_option;
      category = "PARTITIONING";
      default = "";
    }

  let print fmt = function
    | None ->
      Format.fprintf fmt "✘ %s" !opt_target_name
    | Some (Equal n) ->
      Format.fprintf fmt "%s = %a" !opt_target_name Z.pp_print n
    | Some Other ->
      Format.fprintf fmt "%s ∉ {%a}"
        !opt_target_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           Z.pp_print
        ) !opt_target_values

  let compare =
    Compare.option (fun v1 v2 ->
        match v1, v2 with
        | Equal n1, Equal n2 -> Z.compare n1 n2
        | Other, Other -> 0
        | _ -> compare v1 v2
      )

  let init = None

  let add marker p = p

  let is_target_var v =
    match vtyp v with
    | T_int | T_bool ->
      let name = Format.asprintf "%a" pp_var v in
      name = !opt_target_name
    | _ ->
      false

  let sat man post =
    Cases.exists
      (fun _ flow ->
         not (man.lattice.is_bottom (Flow.get T_cur man.lattice flow))
      ) post

  let exec_and_partition stmt v man flow =
    man.exec stmt flow ~route:(Below name) >>% fun flow ->
    let range = stmt.srange in
    let var = mk_var v range in
    let eq_cases =
      !opt_target_values |> List.fold_left
        (fun acc n ->
           let p' = Some (Equal n) in
           let cond = eq var (mk_z n range) range in
           let post =
             man.exec (mk_assume cond range) flow >>% fun flow ->
             set_env T_cur p' man flow
           in
           if sat man post then post :: acc else acc
        ) []
    in
    let ne_case =
      let p' = Some Other in
      let cond =
        !opt_target_values |> List.tl |> List.fold_left
          (fun acc n ->
             log_and acc (ne var (mk_z n range) range) range
          ) (ne (mk_var v range) (mk_z (List.hd !opt_target_values) range) range)
      in
      let post =
        man.exec (mk_assume cond range) flow >>% fun flow ->
        set_env T_cur p' man flow
      in
      if sat man post then [post] else []
    in
    Cases.join_list (eq_cases @ ne_case) ~empty:(fun () -> Cases.empty flow)

  let exec_and_remove stmt v man flow =
    man.exec stmt flow ~route:(Below name) >>% fun flow ->
    set_env T_cur None man flow

  let exec stmt man flow =
    match skind stmt with
    | S_add{ekind = E_var(v, None)} when is_target_var v ->
      exec_and_partition stmt v man flow |>
      Option.some

    | S_remove{ekind = E_var(v, None)} when is_target_var v ->
      exec_and_remove stmt v man flow |>
      Option.some

    | S_assign({ekind = E_var(v, mode)}, _) when is_target_var v && var_mode v mode = STRONG ->
      exec_and_partition stmt v man flow |>
      Option.some

    | S_forget{ekind = E_var(v, mode)} when is_target_var v ->
      panic "%s: forget not supported" name

    | S_fold({ekind = E_var(v, mode)}, _) when is_target_var v ->
      panic "%s: fold not supported" name

    | _ ->
      None

  let eval _ _ _ = None
  let ask _ _ _ = None
end

let () = register_partitioning (module Domain)
