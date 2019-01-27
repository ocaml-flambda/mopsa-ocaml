(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Call stacks are represented as sequences of call sites
   (ranges). They are saved as annotations in flows. *)

open Location

type t = range list

let pp_call_stack fmt cs =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " → ")
    pp_range
    fmt cs

let pp_call_stack_newlines fmt cs =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_range
    fmt cs


let compare_call_stack cs cs' =
  Compare.list compare_range cs cs'

type ('a, _) Annotation.key +=
  | A_call_stack: ('a, t) Annotation.key

let empty : t = []

let get flow : t =
  Flow.get_annot A_call_stack flow

let set cs flow =
  Flow.set_annot A_call_stack cs flow

let push range flow =
  let cs = get flow in
  set (range :: cs) flow

let pop flow =
  let cs = get flow in
  List.hd cs, set (List.tl cs) flow

let () =
  Annotation.(register_stateless_annot {
      eq = (let f: type a b. (a, b) key -> (t, b) eq option =
              function
              | A_call_stack -> Some Eq
              | _ -> None
            in
            f);
      print = (fun fmt cs -> Format.fprintf fmt "Call stack: %a" pp_call_stack cs);
    }) ();
  ()
