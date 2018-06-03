(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstraction of Python values. *)

open Framework.Domains
open Framework.Domains.Reduce.Domain
open Framework.Flow
open Framework.Manager
open Framework.Query
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Framework.Utils
open Universal.Ast
open Ast
open Addr

let name = "python.memory.nonrel"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                             {2 Values}                                  *)
(*==========================================================================*)

module N = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_none]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
  )

module B = Framework.Lattices.Enum.Make(struct
    type t = bool
    let values = [true; false]
    let print = Format.pp_print_bool
  end)

module S = Framework.Lattices.Top_set.Make(struct
    type t = string
    let compare = compare
    let print fmt s = Format.fprintf fmt "\"%s\"" s
  end)


module NI = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_not_implemented]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
  )

module I = Universal.Numeric.Values.Int

module F = Universal.Numeric.Values.Float

module Value =
struct

  type t = {
    none: N.t;
    bool: B.t;
    string: S.t;
    int : I.t;
    float : F.t;
    notimplem: NI.t;
  }

  let bottom = {
    none = N.bottom;
    bool = B.bottom;
    string = S.bottom;
    int = I.bottom;
    float = F.bottom;
    notimplem = NI.bottom;
  }


  let top = {
    none = N.top;
    bool = B.top;
    string = S.top;
    int = I.top;
    float = F.top;
    notimplem = NI.top;
  }

  let is_bottom abs =
    N.is_bottom abs.none &&
    B.is_bottom abs.bool &&
    S.is_bottom abs.string &&
    I.is_bottom abs.int &&
    F.is_bottom abs.float &&
    F.is_bottom abs.float &&
    NI.is_bottom abs.notimplem

  let is_top abs =
    N.is_top abs.none &&
    B.is_top abs.bool &&
    S.is_top abs.string &&
    I.is_top abs.int &&
    F.is_top abs.float &&
    NI.is_top abs.notimplem


  let init = top

  let print fmt abs =
    let open Format in
    if is_top abs then
      fprintf fmt "⊤"
    else
    if is_bottom abs then
      pp_print_string fmt " ⊥ "
    else (
      fprintf fmt "@[<h>(|";
      let pred = ref false in

      if not (I.is_bottom abs.int) then (
        fprintf fmt " i: %a " I.print abs.int;
        pred := true;
      );

      if not (F.is_bottom abs.float) then (
        fprintf fmt "%s f: %a " (if !pred then "∨" else "") F.print abs.float;
        pred := true;
      );

      if not (B.is_bottom abs.bool) then (
        fprintf fmt "%s b: %a " (if !pred then "∨" else "") B.print abs.bool;
        pred := true;
      );

      if not (S.is_bottom abs.string) then (
        fprintf fmt "%s s: %a " (if !pred then "∨" else "") S.print abs.string;
        pred := true;
      );

      if not (N.is_bottom abs.none) then (
        fprintf fmt "%s n: %a " (if !pred then "∨" else "") N.print abs.none;
        pred := true;
      );

      if not (NI.is_bottom abs.notimplem) then
        fprintf fmt "%s ni: %a " (if !pred then "∨" else "") NI.print abs.notimplem;

      fprintf fmt "|)@]"
    )


  let leq abs1 abs2 =
    N.leq abs1.none abs2.none &&
    B.leq abs1.bool abs2.bool &&
    S.leq abs1.string abs2.string &&
    I.leq abs1.int abs2.int &&
    F.leq abs1.float abs2.float &&
    NI.leq abs1.notimplem abs2.notimplem

  let join abs1 abs2 = {
    none = N.join abs1.none abs2.none;
    bool = B.join abs1.bool abs2.bool;
    string = S.join abs1.string abs2.string;
    int = I.join abs1.int abs2.int;
    float = F.join abs1.float abs2.float;
    notimplem = NI.join abs1.notimplem abs2.notimplem;
  }
  
  let meet abs1 abs2 = {
    none = N.meet abs1.none abs2.none;
    bool = B.meet abs1.bool abs2.bool;
    string = S.meet abs1.string abs2.string;
    int = I.meet abs1.int abs2.int;
    float = F.meet abs1.float abs2.float;
    notimplem = NI.meet abs1.notimplem abs2.notimplem;
  }

  let widening ctx abs1 abs2 = {
    none = N.widening ctx abs1.none abs2.none;
    bool = B.widening ctx abs1.bool abs2.bool;
    string = S.widening ctx abs1.string abs2.string;
    int = I.widening ctx abs1.int abs2.int;
    float = F.widening ctx abs1.float abs2.float;
    notimplem = NI.widening ctx abs1.notimplem abs2.notimplem;
  }

  (** Creation of uniquely typed values *)
  let boolean b = {
    bottom with
    bool = b;
  }

  let none n = {
    bottom with
    none = n;
  }

  let integer i = {
    bottom with
    int = i;
  }

  let float i = {
    bottom with
    float = i;
  }

  let string s = {
    bottom with
    string = s
  }

  let notimplem ne = {
    bottom with
    notimplem = ne
  }

  (** Creation of a value from a program constant *)
  let of_constant c =
    match c with
    | C_true -> boolean (B.singleton true)
    | C_false -> boolean (B.singleton false)
    | C_top T_bool -> boolean B.top

    | C_py_none | C_top T_py_none -> none  (N.singleton c)

    | C_int n -> integer (I.of_constant c)
    | C_int_interval _ -> integer (I.of_constant c)
    | C_top T_int -> integer I.top

    | C_float n -> float (F.of_constant c)
    | C_float_interval _ -> float (F.of_constant c)
    | C_top T_float -> float F.top

    | C_py_not_implemented | C_top T_py_not_implemented -> notimplem  (NI.singleton c)

    | C_string s -> string (S.singleton s)
    | C_top T_string -> string S.top

    | _ -> top

  (** Boolean predicates *)
  let can_be_true abs =
    (B.mem true abs.bool) ||
    (S.exists (fun s -> s <> "") abs.string) ||
    (I.can_be_true abs.int) ||
    (F.can_be_true abs.float) ||
    (not @@ NI.is_bottom abs.notimplem)

  let can_be_false abs =
    (not @@ N.is_bottom abs.none) ||
    (S.exists (fun s -> s = "") abs.string) ||
    (B.mem false abs.bool) ||
    (I.can_be_false abs.int) ||
    (F.can_be_false abs.float)


  (** Forward evaluation of unary operators *)
  let fwd_unop op abs =
    match op with
    | O_sqrt ->
      { bottom with float = F.fwd_unop op abs.float }

    | O_plus T_int | O_minus T_int ->
      {bottom with int = I.fwd_unop op abs.int}

    | O_plus T_float | O_minus T_float ->
      {bottom with float = F.fwd_unop op abs.float}

    | _ ->
      Debug.fail "fwd_unop %a not implemented" Framework.Pp.pp_operator op

  (** Filtering functions *)
  let assume_true abs = {
    none = N.bottom;
    bool = B.meet abs.bool (B.singleton true);
    string = S.filter (fun s -> s <> "") abs.string;
    int = I.assume_true abs.int;
    float = F.assume_true abs.float;
    notimplem = abs.notimplem;
  }


  let assume_false abs = {
    none = abs.none;
    bool = B.meet abs.bool (B.singleton false);
    string = S.filter (fun s -> s = "") abs.string;
    int = I.assume_false abs.int;
    float = F.assume_false abs.float;
    notimplem = NI.bottom;
  }

  (** Forwared evaluation of binary operators *)
  let fwd_binop op abs1 abs2 =
    match op with
    | O_plus T_int | O_minus T_int | O_mult T_int | O_mod T_int  ->
      {
        bottom with
        int = I.fwd_binop op abs1.int abs2.int;
      }

    | O_plus T_float | O_minus T_float | O_mult T_float | O_mod T_float ->
      {
        bottom with
        float = F.fwd_binop op abs1.float abs2.float;
      }

    | O_pow ->
      {
        bottom with
        int = I.fwd_binop op abs1.int abs2.int;
        float = F.fwd_binop op abs1.float abs2.float;
      }

    | O_div T_float ->
      {
        bottom with
        float = F.fwd_binop op abs1.float abs2.float;
      }

    | O_py_floor_div ->
      {
        bottom with
        (* FIXME : use operator types *)
        int = I.fwd_binop math_div abs1.int abs2.int;
      }

    | O_bit_and
    | O_bit_or
    | O_bit_xor
    | O_bit_lshift
    | O_bit_rshift
      ->
      {
        bottom with
        int = I.fwd_binop op abs1.int abs2.int;
        bool = B.fold (fun b1 acc ->
            B.fold (fun b2 acc ->
                let op =
                  match op with
                    O_bit_and -> (&&)
                  | O_bit_or -> (||)
                  | O_bit_xor -> (<>)
                  | _ -> assert false
                in
                B.singleton (op b1 b2) |> B.join acc
              ) abs2.bool acc
          ) abs1.bool B.bottom
      }

    | _ ->
      Debug.fail "fwd_binop %a not implemented" Framework.Pp.pp_operator op

  let mk_true = boolean (B.singleton true)
  let mk_false = boolean (B.singleton false)

  let fwd_filter op abs1 abs2 =
    match op with
    | O_eq -> not @@ is_bottom @@ meet abs1 abs2
    | O_ne ->
      (is_bottom @@ meet abs1 abs2) ||
      (
        not @@ S.is_bottom abs1.string ||
        not @@ S.is_bottom abs2.string &&
        (
          S.is_bottom @@ S.meet abs1.string abs2.string ||
          try
            (S.cardinal abs1.string > 1) ||
            (S.cardinal abs2.string > 1)
          with Top.Found_TOP -> true
        )
      ) ||
      (I.fwd_filter op abs1.int abs2.int) ||
      (F.fwd_filter op abs1.float abs2.float)

    | O_lt ->
      (B.mem false abs1.bool && B.mem true abs1.bool) ||
      (I.fwd_filter op abs1.int abs2.int) ||
      (F.fwd_filter op abs1.float abs2.float)
    | O_le ->
      (not @@ B.is_bottom @@ B.meet abs1.bool abs2.bool) ||
      (B.mem false abs1.bool && B.mem true abs1.bool) ||
      (I.fwd_filter op abs1.int abs2.int) ||
      (F.fwd_filter op abs1.float abs2.float)
    | O_gt ->
      (B.mem true abs1.bool && B.mem false abs1.bool) ||
      (I.fwd_filter op abs1.int abs2.int) ||
      (F.fwd_filter op abs1.float abs2.float)
    | O_ge ->
      (not @@ B.is_bottom @@ B.meet abs1.bool abs2.bool) ||
      (B.mem true abs1.bool && B.mem false abs1.bool) ||
      (I.fwd_filter op abs1.int abs2.int) ||
      (F.fwd_filter op abs1.float abs2.float)

    | _ -> assert false


  let bwd_unop op abs rabs =
    match op with
    | O_sqrt ->
      Framework.Exceptions.panic "bwd evaluation of sqrt not supported"
    (* FIXME : use operator types *)
    | O_minus _ ->
      {bottom with int = I.bwd_unop op abs.int rabs.int; float = F.bwd_unop op abs.float rabs.float}
    (* FIXME : use operator types *)
    | O_plus _ ->
      {bottom with int = abs.int; float = abs.float}

    | _ ->
      assert false

  (** Backward evaluation of binary operators *)
  let bwd_binop_default abs1 abs2 = (abs1, abs2)
  let bwd_binop op abs1 abs2 rabs =
    match op with
    (* FIXME : use operator types *)
    | O_plus _ | O_minus _ | O_mult _ | O_div _ | O_mod _ ->
      let int1, int2 = I.bwd_binop op abs1.int abs2.int rabs.int in
      let float1, float2 = F.bwd_binop op abs1.float abs2.float rabs.float in
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}
    (* FIXME : use operator types *)
    | O_pow
    | O_py_and
    | O_py_or
    | O_py_floor_div
    | O_bit_and
    | O_bit_or
    | O_bit_xor
    | O_bit_lshift
    | O_bit_rshift
    | O_py_is
    | O_py_is_not
      ->
      bwd_binop_default abs1 abs2

    | _ ->
      assert false

  (** Backward filters of comparison operators *)
  let bwd_filter op abs1 abs2 =
    let int1, int2 = I.bwd_filter op abs1.int abs2.int in
    let float1, float2 = F.bwd_filter op abs1.float abs2.float in

    match op with
    | O_eq ->
      let abs = meet abs1 abs2 in
      {abs with int = int1; float = float1},
      {abs with int = int2; float = float2}

    | O_ne ->
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}

    | O_lt ->
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}

    | O_le ->
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}

    | O_gt ->
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}

    | O_ge ->
      {abs1 with int = int1; float = float1},
      {abs2 with int = int2; float = float2}

    | _ -> assert false

end

module Domain =
struct

  include Universal.Nonrel.Domain.Make(Value)

end

let setup () =
  register_domain name (module Domain);
  ()
