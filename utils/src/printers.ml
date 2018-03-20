(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** [print_option printer] lifts the 'a printer [printer] to 'a option printer*)
let print_option printer =
  fun fmt e ->  match e with
  | Some a -> Format.fprintf fmt "%a" printer a
  | None   -> Format.fprintf fmt "None"
