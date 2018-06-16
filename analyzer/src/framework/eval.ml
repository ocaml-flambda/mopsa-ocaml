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

type ('e, 'a) case = {
  result    : 'e option;
  flow      : 'a flow;
  cleaners  : Ast.stmt list;
}

type ('e, 'a) t = ('e, 'a) case list

let singleton (result: 'e option) ?(cleaners = []) (flow: 'a flow) : ('e, 'a) t =
  [{result; flow; cleaners}]

let join (evl1: ('e, 'a) t)  (evl2: ('e, 'a) t) : ('e, 'a) t =
  evl1 @ evl2

let add_cleaners (cleaners: Ast.stmt list) (evl: ('e, 'a) t) : ('e, 'a) t =
  List.map (fun ev ->
      {ev with cleaners = ev.cleaners @ cleaners}
    ) evl

let map
    (f: 'e -> 'a flow -> Ast.stmt list -> ('x, 'a) t)
    (evls: ('e, 'a) t)
  : ('x, 'a) t =
  List.map (fun ev ->
      match ev.result with
      | None -> singleton None ev.flow
      | Some result -> f result ev.flow ev.cleaners
    ) evls
  |>
  List.concat


let iter
    (f: 'e -> 'a flow -> unit)
    (evls: ('e, 'a) t)
  : unit =
  List.iter (fun ev ->
      match ev.result with
      | None -> ()
      | Some ret -> f ret ev.flow
    ) evls

let fold
    (f: 'b -> ('e, 'a) case -> 'b)
    (init: 'b)
    (evals: ('e, 'a) t)
  : 'b =
  List.fold_left f init evals

let merge
    (f: ('e, 'a) case -> 'b)
    ~(join: 'b -> 'b -> 'b)
    (evals: ('e, 'a) t)
  : 'b =
  let l = List.map f evals in
  match l with
  | [] -> assert false
  | hd :: tl ->
    List.fold_left join hd tl


let print fmt (evals: ('e, 'a) t) ~(print_result: Format.formatter -> 'e -> unit) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt ev ->
       match ev.result with
       | None -> Format.pp_print_string fmt "ϵ"
       | Some x -> print_result fmt x
    )
    fmt
    evals
