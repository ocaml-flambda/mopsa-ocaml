(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Locations and ranges *)

type loc = {
    file: string;
    line: int;
    col: int;
  }

type range = loc * loc

type 'a with_range = {
  kind: 'a;
  range: range;
}

let with_range a range =
  {
    kind = a;
    range;
  }

let bind_range (a: 'a with_range) (f: 'a -> 'b) : 'b with_range =
  { a with kind = f a.kind }

let bind2_range (a: 'a with_range) (f: 'a -> 'b * 'c) : 'b with_range * 'c =
  let b, c = f a.kind in
  { kind = b; range = a.range }, c
