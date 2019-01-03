(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow partitioning domain, based on flow manipulations *)

open Mopsa
open Ast

type partitioning_location_label = int
let print_partitioning_location_label = Format.pp_print_int
let compare_partitioning_location_label = (-)
(** labels of partioning location *)

type son_labels = int
let print_son_labels = Format.pp_print_int
let compare_son_labels = (-)
module SonLabelMap = MapExt.Make(struct type t = son_labels
                                     let print = print_son_labels
                                     let compare = compare_son_labels
                              end)
(** labels of sons in the partitioning tree *)

module History = Map.Make(
  struct
    type t = partitioning_location_label
    let print = print_partitioning_location_label
    let compare = compare_partitioning_location_label
  end
  )

type history = son_labels History.t
let print_history fmt h = Format.fprintf fmt "{%a}"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (x, y) -> Format.fprintf fmt "%a ~> %a"
           print_partitioning_location_label x
           print_son_labels y
       )
    ) (History.bindings h)
let subset_history a b =
  History.for_all (fun x y ->
      try
        compare_son_labels (History.find x b) y = 0
      with
      | Not_found -> false
    ) a
let compare_history h h' =
  if subset_history h h' then
    begin
      if subset_history h' h then
        0
      else -1
    end
  else
    begin
      if subset_history h' h then
        1
      else -2
    end

let empty_history = History.empty
module HistoryMap = Map.Make(struct type t = history let compare = compare_history end)

(*==========================================================================*)
(**                    {2 Partitioning Annotations}                         *)
(*==========================================================================*)

type ('a, _) Annotation.key +=
  | A_cf_part: ('a, partitioning_location_label) Annotation.key

let () =
  Annotation.(register_stateless_annot {
                  eq = (let f: type a b. (a, b) key -> (partitioning_location_label, b) eq option =
                          function
                          | A_cf_part -> Some Eq
                          | _ -> None
                        in
                        f);
                  print = (fun fmt l -> Format.fprintf fmt "Cf_partitioning: %a"
                                          print_partitioning_location_label l
                ) ;
  }) ();
  ()

(*==========================================================================*)
(**                       {2 Partitioning tokens}                           *)
(*==========================================================================*)


type token +=
   | T_part of (history * token)
(** tokens used to store partitioned environments *)

let () =
  register_token {
      compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_part (h1, tk1), T_part(h2, tk2) ->
           Compare.compose
             [(fun () -> compare_history h1 h2);
              (fun () -> compare_token tk1 tk2)]
        | _ -> next tk1 tk2
      );
      print   = (fun next fmt tk ->
        match tk with
        | T_part(h, tk) -> Format.fprintf fmt "part(%a, %a)"
                                 print_history h
                                 pp_token tk
        | _ -> next fmt tk
      );
    }

exception AlreadyExistingPartitioning
exception NoPartitioning
let add_history_choice (h: history) (l: partitioning_location_label) (son: son_labels): history =
  if History.mem l h then
    raise AlreadyExistingPartitioning
  else History.add l son h

let partition_flow man (l: partitioning_location_label) (part_keys: son_labels list) (f: 'a Flow.flow) =
  let fmap =
    Flow.fold (fun map tk env ->
        match tk with
        | T_part (h, tk_org) ->
           List.fold_left (fun map part_key ->
               FlowMap.add (T_part(add_history_choice h l part_key, tk_org)) env map
             ) map part_keys
        | tk_org ->
           List.fold_left (fun map part_key ->
               FlowMap.add (T_part(add_history_choice empty_history l part_key, tk_org)) env map
             ) map part_keys
      ) FlowMap.empty man f
  in
  Flow.({f with map = Nt fmap})

let remove_partitition_history (l : partitioning_location_label) (h: history) =
  History.remove l h

let select_partition_flow man (l: partitioning_location_label) part_keys (f: 'a Flow.flow) =
  let fmap = Flow.fold (fun map tk env ->
      match tk with
      | T_part (h, tk_org) ->
        begin
          try
            let son = History.find l h in
            if ListExt.mem_compare compare_son_labels son part_keys then
              FlowMap.add (T_part(remove_partitition_history l h, tk_org)) env map
            else map
          with
          | Not_found -> raise NoPartitioning
         end
      | _ -> raise NoPartitioning
    ) FlowMap.empty man f
  in
  Flow.({f with map = Nt fmap})

let select_all_partition_flow man (l: partitioning_location_label) (f: 'a Flow.flow) =
  let fmap = Flow.fold (fun (map: 'a FlowMap.t SonLabelMap.t) tk (env: 'a) ->
      match tk with
      | T_part (h, tk_org) ->
        begin
          try
            let i = History.find l h in
            let h' = History.remove l h in
            let cur_flow_i = try SonLabelMap.find i map with | Not_found -> FlowMap.empty in
            SonLabelMap.add i (FlowMap.add (T_part(h', tk_org)) env cur_flow_i) map
          with
          | Not_found -> raise NoPartitioning
         end
      | _ ->
        let cur_flow = try SonLabelMap.find (-1) map with | Not_found -> FlowMap.empty in
        SonLabelMap.add (-1) (FlowMap.add tk env cur_flow) map
    ) SonLabelMap.empty man f
  in
  fmap

let is_flow_partitioned man f =
  let exception Partitioned in
  try
    Flow.fold (fun () tk _ ->
        match tk with
        | T_part _ -> raise Partitioned
        | _ -> ()
      ) () man f;
    false
  with
  | Partitioned -> true

let partitioning_of_flow man f =
  Flow.fold (fun map tk env ->
      let h, tk_org =
        match tk with
        | T_part (h, tk_org) -> h, tk_org
        | _ -> empty_history, tk
      in
      let flow_h =
        try HistoryMap.find h map with | Not_found -> FlowMap.empty
      in
      HistoryMap.add h (FlowMap.add tk_org env flow_h) map
    ) HistoryMap.empty man f

let flow_of_partitioning p annot =
  let fmap =
    HistoryMap.fold (fun h flow acc ->
        let hn = compare_history h empty_history = 0 in
        FlowMap.fold (fun tk_org env acc' ->
            let tk = if hn then tk_org else T_part (h, tk_org) in
            FlowMap.add tk env acc'
          ) flow acc
      ) p FlowMap.empty
  in
  Flow.make annot fmap

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)

module Domain : Framework.Domains.Stateless.S =
  struct

    (*==========================================================================*)
    (**               {2 Domain definition and identification}                  *)
    (*==========================================================================*)

    type _ domain += D_universal_cf_partitioning : unit domain

    let id = D_universal_cf_partitioning
    let name = "universal.partitioning.cf.cf_partitioning"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_universal_cf_partitioning -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    (*==========================================================================*)
    (**                           {2 Interface}                                 *)
    (*==========================================================================*)

    let exec_interface = {export = [any_zone]; import = []}
    let eval_interface = {export = []; import = []}

    (*==========================================================================*)
    (**                       {2 Lattice structure}                             *)
    (*==========================================================================*)

    let cf_partitioning_waiting flow =
      try
        Some (Flow.get_annot A_cf_part flow )
      with
      | Not_found -> None

    let rec exec zone stmt man flow =
      match skind stmt with
      | S_if (cond, s_then, s_else) ->
         begin
           match cf_partitioning_waiting flow with
           (* Control flow partitioning should be done on this if
              statement. *)
           | Some l ->
              let range = srange stmt in

              (* The flow is partitioned *)
              let flow' = partition_flow man l [0; 1] flow in

              (* We get the flows for then and else branches *)
              let flow_then = select_partition_flow man l [0] flow' in
              let flow_else = select_partition_flow man l [1] flow' in

              let flow_then' = man.exec (mk_assume cond range) flow_then |>
                                 man.exec s_then
              in
              let flow_else' = man.exec (mk_assume cond range) flow_else |>
                                 man.exec s_else
              in
              Some (Flow.join man flow_then' flow_else' |> Post.of_flow)
           (* No control flow partitioning should be done on this
              statement, therefore we do not handle it and rely on the
              classical iterators *)
           | None   -> None
         end

      | S_cf_part_start(i) ->
         (* Starting a new control flow partition on next statement,
            we replace the currently waiting label (which should not
            exist) by [i] *)
         let annot' = i in
         Some (Flow.set_annot A_cf_part annot' flow |> Post.of_flow)

      | S_cf_part_merge(i) ->
         let part = select_all_partition_flow man i flow in
         Some (
             SonLabelMap.fold (fun _ fmap joined ->
                 Flow.join man Flow.({flow with map = Nt fmap}) joined
               ) part (Flow.bottom (Flow.get_all_annot flow))  |> Post.of_flow
           )

      | _ when is_flow_partitioned man flow ->
         let part = partitioning_of_flow man flow in
         let part', annot = HistoryMap.fold (fun h fmap (acc_hmap, acc_annot) ->
                         let flow' = man.exec ~zone:zone stmt Flow.({map = Nt fmap; annot = acc_annot}) in
                         (HistoryMap.add h Flow.(flow'.map |> Top.detop) acc_hmap, Flow.get_all_annot flow')
                       ) part (HistoryMap.empty, Flow.get_all_annot flow)
         in
         Some (flow_of_partitioning part' annot |> Post.of_flow)
      | _ -> None

    let eval zone exp man flow = None

    let init prog man flow = None

    let ask query man flow = None

  end
