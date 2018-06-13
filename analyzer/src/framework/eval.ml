(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Evaluations of expressions *)


open Ast
open Flow

type ('e, 'a) clause = {
  case: 'e option;
  flow: 'a flow;
  cleaner: Ast.stmt list;
}

type ('e, 'a) t = ('e, 'a) clause list

let singleton (case: 'e option) ?(cleaner = []) (flow: 'a flow) : ('e, 'a) t =
  [{case; flow; cleaner}]

let join (evl1: ('e, 'a) t)  (evl2: ('e, 'a) t) : ('e, 'a) t =
  evl1 @ evl2

let append_cleaner (cleaner: Ast.stmt list) (evl: ('e, 'a) t) : ('e, 'a) t =
  List.map (fun ev ->
      {ev with cleaner = ev.cleaner @ cleaner}
    ) evl

let map_clause
    (f: 'e -> 'a flow -> Ast.stmt list -> ('x, 'a) t)
    (evls: ('e, 'a) t)
  : ('x, 'a) t =
  List.map (fun ev ->
      match ev.case with
      | None -> singleton None ev.flow
      | Some ret -> f ret ev.flow ev.cleaner
    ) evls
  |>
  List.concat


let map
    (f: 'e -> 'a flow -> ('x, 'a) t)
    (evls: ('e, 'a) t)
  : ('x, 'a) t =
  map_clause (fun case flow cleaners ->
      let ev' = f case flow in
      append_cleaner cleaners ev'
    ) evls

let iter
    (f: 'e -> 'a flow -> unit)
    (evls: ('e, 'a) t)
  : unit =
  List.iter (fun ev ->
      match ev.case with
      | None -> ()
      | Some ret -> f ret ev.flow
    ) evls

let merge
    (f: ('e, 'a) clause -> 'b)
    ~(join: 'b -> 'b -> 'b)
    (evals: ('e, 'a) t)
  : 'b =
  let l = List.map f evals in
  match l with
  | [] -> assert false
  | hd :: tl ->
    List.fold_left join hd tl


let print fmt (evals: ('e, 'a) t) ~(print_case: Format.formatter -> 'e -> unit) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt ev ->
       match ev.case with
       | None -> Format.pp_print_string fmt "ϵ"
       | Some x -> print_case fmt x
    )
    fmt
    evals
