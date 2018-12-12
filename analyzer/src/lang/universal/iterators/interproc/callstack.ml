(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Call stacks are represented as sequences of call sites
   (ranges). They are saved as annotations into flows. *)

open Mopsa

type cs = range list

let pp_call_stack =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_range

let compare_call_stack cs cs' =
  Compare.list compare_range cs cs'

type ('a, _) Annotation.key +=
  | A_call_stack: ('a, cs) Annotation.key

let () =
  Annotation.(register_stateless_annot {
      eq = (let f: type a b. (a, b) key -> (cs, b) eq option =
              function
              | A_call_stack -> Some Eq
              | _ -> None
            in
            f);
      print = (fun fmt cs -> Format.fprintf fmt "Call stack: %a" pp_call_stack cs);
    }) ();
  ()
