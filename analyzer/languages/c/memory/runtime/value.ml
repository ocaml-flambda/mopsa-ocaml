(** Auxillary definitions for OCaml runtime check *)


open Mopsa
open Sig.Abstraction.Value
open Universal.Ast
open Ast
open Common.Points_to
open Common.Base
open Top

open Framework.Lattices.Partial_inversible_map


module Roots =
struct
  type t =
    Rooted | NotRooted

  let compare a b =
    match a, b with
    | NotRooted, NotRooted -> 0
    | Rooted, Rooted -> 0
    | _, _ -> Stdlib.compare a b


  let to_string s =
    match s with
    | Rooted-> "root"
    | NotRooted -> "not rooted"

  let print printer b = pp_string printer (to_string b)
end


module RuntimeLock =
struct
  type t =
    Locked | Unlocked

  let compare a b =
    match a, b with
    | Locked, Locked -> 0
    | Unlocked, Unlocked -> 0
    | _, _ -> Stdlib.compare a b


  let to_string s =
    match s with
    | Locked -> "locked"
    | Unlocked-> "unlocked"

  let print printer b = pp_string printer (to_string b)
end
