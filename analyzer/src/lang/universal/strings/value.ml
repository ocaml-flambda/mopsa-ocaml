(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Constant string abstraction of string values *)

open Framework.Essentials
open Framework.Value
open Ast

module Value =
struct
  type t =
    | T | B
    | V of string

  let zone = Zone.Z_universal_string

  type _ Framework.Query.query +=
    | Q_string : Framework.Ast.expr -> t Framework.Query.query

  type _ value += V_string_constant : t value

  let id = V_string_constant
  let name = "universal.strings.value", "strings"

  let identify : type a. a value -> (t, a) eq option =
    function
    | V_string_constant -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:(fst @@ name) fmt

  let bottom = B
  let top = T
  let is_bottom = function
    | B -> true
    | _ -> false
  let is_top = function
    | T -> true
    | _ -> false
  let join annot u v = match u, v with
    | T, _ -> T
    | _, T -> T
    | B, _ -> v
    | _, B -> u
    | V u, V v when u = v -> V u
    | _ -> T
  let meet annot u v = match u, v with
    | T, _ -> v
    | _, T -> u
    | B, _ -> B
    | _, B -> B
    | V u, V v when u = v -> V u
    | _ -> B

  let subset u v = match u, v with
    | _, T -> true
    | B, _ -> true
    | V u, V v when u = v -> true
    | _ -> false

  let widen = join
  let print fmt = function
    | T -> Format.fprintf fmt "top"
    | B -> Format.fprintf fmt "bot"
    | V u -> Format.fprintf fmt "%s" u
  let of_constant = function
    | C_string s -> V s
    | _ -> T
  let unop op a = return T
  let binop op a1 a2 =
    return (
      match op, a1, a2 with
      | O_concat, B, _ -> B
      | O_concat, _, B -> B
      | O_concat, V u, V v -> V (u ^ v)
      | _ -> T
    )
  let filter _ _ = return T
  let bwd_unop _ _ _ = return T
  let bwd_binop _ _ _ _ = return (T, T)
  let compare _ _ _ = return (T, T)

  let ask : type r. r Framework.Query.query -> (expr -> t) -> r option =
    fun query eval ->
      match query with
      | Q_string e ->
        Some (eval e)
      | _ -> None


end

let () =
  register_value (module Value)
