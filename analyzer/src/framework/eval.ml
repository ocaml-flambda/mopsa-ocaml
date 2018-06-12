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

type ('e, 'a) evals = ('e, 'a) clause list

let singleton (case: 'e option) ?(cleaner = []) (flow: 'a flow) : ('e, 'a) evals =
  [{case; flow; cleaner}]

let join (evl1: ('e, 'a) evals)  (evl2: ('e, 'a) evals) : ('e, 'a) evals =
  evl1 @ evl2

let append_cleaner (evl: ('e, 'a) evals) (cleaner: Ast.stmt list) : ('e, 'a) evals =
  List.map (fun ev ->
      {ev with cleaner = ev.cleaner @ cleaner}
    ) evl

let map_clause
    (f: 'e -> 'a flow -> Ast.stmt list -> ('x, 'a) evals)
    (evls: ('e, 'a) evals)
  : ('x, 'a) evals =
  List.map (fun ev ->
      match ev.case with
      | None -> singleton None ev.flow
      | Some ret -> f ret ev.flow ev.cleaner
    ) evls
  |>
  List.concat


let map
    (f: 'e -> 'a flow -> ('x, 'a) evals)
    (evls: ('e, 'a) evals)
  : ('x, 'a) evals =
  map_clause (fun case flow cleaners ->
      let ev' = f case flow in
      append_cleaner ev' cleaners
    ) evls

let iter
    (f: 'e -> 'a flow -> unit)
    (evls: ('e, 'a) evals)
  : unit =
  List.iter (fun ev ->
      match ev.case with
      | None -> ()
      | Some ret -> f ret ev.flow
    ) evls

let merge
    (f: ('e, 'a) clause -> 'b)
    ~(join: 'b -> 'b -> 'b)
    (evals: ('e, 'a) evals)
  : 'b =
  let l = List.map f evals in
  match l with
  | [] -> assert false
  | hd :: tl ->
    List.fold_left join hd tl


let print fmt (evals: ('e, 'a) evals) ~(print_case: Format.formatter -> 'e -> unit) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt ev ->
       match ev.case with
       | None -> Format.pp_print_string fmt "ϵ"
       | Some x -> print_case fmt x
    )
    fmt
    evals
