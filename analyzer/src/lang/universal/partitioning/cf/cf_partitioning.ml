(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow partitioning domain, based on flow manipulations *)

open Framework.Essentials
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

type part_tpath = (partitioning_location_label * son_labels) list
let print_part_tpath fmt x =
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "·")
       (fun fmt (x, y) -> Format.fprintf fmt "(%a, %a)"
                            print_partitioning_location_label x
                            print_son_labels y
       )
    ) x
let compare_part_tpath =
  Compare.list (Compare.pair compare_partitioning_location_label compare_son_labels)
module TPathMap = MapExt.Make(struct type t = part_tpath
                                     let print = print_part_tpath
                                     let compare = compare_part_tpath
                              end)
(** tree path from the root of the partitioning tree to the flow *)

type ('a, 'b) partition_tree =
  | Part of 'a * (('a, 'b) partition_tree) list
  | Env of 'b
let rec print_partition_tree pa pb fmt x =
  match x with
  | Part(a, l) ->
     Format.fprintf fmt "@[<v 2>%a@,%a@]"
       pa a
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
          (print_partition_tree pa pb)
       ) l
  | Env b -> pb b
(** partitioning tree *)

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
   | T_part of (part_tpath * token)
(** tokens used to store partitioned environments *)

let () =
  register_token {
      compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_part (tpath1, tk1), T_part(tpath2, tk2) ->
           Compare.compose
             [(fun () -> compare_part_tpath tpath1 tpath2);
              (fun () -> compare_token tk1 tk2)]
        | _ -> next tk1 tk2
      );
      print   = (fun next fmt tk ->
        match tk with
        | T_part(tpath, tk) -> Format.fprintf fmt "part(%a, %a)"
                                 print_part_tpath tpath
                                 pp_token tk
        | _ -> next fmt tk
      );
    }

let partition_flow man (l: partitioning_location_label) part_keys (f: 'a flow) =
  let fmap =
    Flow.fold (fun map tk env ->
        match tk with
        | T_part (tpath, tk_org) ->
           List.fold_left (fun map part_key ->
               FlowMap.add (T_part((l, part_key) :: tpath, tk_org)) env map
             ) map part_keys
        | tk_org ->
           List.fold_left (fun map part_key ->
               FlowMap.add (T_part((l, part_key) :: [], tk_org)) env map
             ) map part_keys
      ) FlowMap.empty man f
  in
  Flow.({f with map = Nt fmap})

let select_tpath (l : partitioning_location_label) part_keys (tpath: part_tpath)
    : part_tpath option =
  let rec aux tpath beg =
    match tpath with
    | (l', i')::r when compare_partitioning_location_label l l' = 0 ->
       if ListExt.mem_compare compare_son_labels i' part_keys then
         Some (List.rev_append beg r)
       else None
    | hd::r -> aux r (hd :: beg)
    | [] -> None
  in
  aux tpath []

let remove_partitition_tpath (l : partitioning_location_label) (tpath: part_tpath) =
  let rec aux tpath beg =
    match tpath with
    | (l', i')::r when compare_partitioning_location_label l l' = 0 ->
       Some (i', List.rev_append beg r)
    | hd::r -> aux r (hd :: beg)
    | [] -> None
  in
  aux tpath []

let select_partition_flow man (l: partitioning_location_label) part_keys (f: 'a flow) =
  let fmap = Flow.fold (fun map tk env ->
      match tk with
      | T_part (tpath, tk_org) ->
         begin
           match select_tpath l part_keys tpath with
           | Some tpath' -> FlowMap.add (T_part(tpath', tk_org)) env map
           | None -> map
         end
      | _ -> map
    ) FlowMap.empty man f
  in
  Flow.({f with map = Nt fmap})

let select_all_partition_flow man (l: partitioning_location_label) (f: 'a flow) =
  let fmap = Flow.fold (fun (map: 'a FlowMap.t SonLabelMap.t) tk (env: 'a) ->
      match tk with
      | T_part (tpath, tk_org) ->
         begin
           match remove_partitition_tpath l tpath with
           | Some(i, tp) ->
              let cur_flow_i = try SonLabelMap.find i map with | Not_found -> FlowMap.empty in
              SonLabelMap.add i (FlowMap.add (T_part(tp, tk_org)) env cur_flow_i) map
           | None ->
              let cur_flow = try SonLabelMap.find (-1) map with | Not_found -> FlowMap.empty in
              SonLabelMap.add (-1) (FlowMap.add (T_part(tpath, tk_org)) env cur_flow) map
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
      let tp, tk_org =
        match tk with
        | T_part (tp, tk_org) -> tp, tk_org
        | _ -> [], tk
      in
      let flow_tp =
        try TPathMap.find tp map with | Not_found -> FlowMap.empty
      in
      TPathMap.add tp (FlowMap.add tk_org env flow_tp) map
    ) TPathMap.empty man f

let flow_of_partitioning p annot =
  let fmap =
    TPathMap.fold (fun tp flow acc ->
        let tpn = tp = [] in
        FlowMap.fold (fun tk_org env acc' ->
            let tk = if tpn then tk_org else T_part (tp, tk_org) in
            FlowMap.add tk env acc'
          ) flow acc
      ) p FlowMap.empty
  in
  Flow.({map = Nt fmap; annot = annot})

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
         let part', annot = TPathMap.fold (fun tp fmap (acc_tpmap, acc_annot) ->
                         let flow' = man.exec ~zone:zone stmt Flow.({map = Nt fmap; annot = acc_annot}) in
                         (TPathMap.add tp Flow.(flow'.map |> Top.detop) acc_tpmap, Flow.get_all_annot flow')
                       ) part (TPathMap.empty, Flow.get_all_annot flow)
         in
         Some (flow_of_partitioning part' annot |> Post.of_flow)
      | _ -> None

    let eval zone exp man flow = None

    let init prog man flow = None

    let ask query man flow = None

  end
