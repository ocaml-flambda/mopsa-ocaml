(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Value abstraction of atomic Python types. *)

open Framework.Ast
open Universal.Ast
open Ast

let debug fmt = Debug.debug ~channel:"python.environment.value" fmt

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

module A = struct
  module SSet = Framework.Lattices.Top_set.Make
      (struct type t = addr let compare = compare_addr let print = Universal.Pp.pp_addr end)
  include SSet
  let widening ctx a b = join a b
end

module NI = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_not_implemented]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)

module I = Universal.Numeric.Values.Int

module F = Universal.Numeric.Values.Float

module E = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_empty]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)


(** Lattice structure *)

type t = {
  none: N.t;
  bool: B.t;
  string: S.t;
  int : I.t;
  float : F.t;
  addr: A.t;
  notimplem: NI.t;
  emptyvalue: E.t;
}

let bottom = {
  none = N.bottom;
  bool = B.bottom;
  string = S.bottom;
  int = I.bottom;
  float = F.bottom;
  addr = A.bottom;
  notimplem = NI.bottom;
  emptyvalue = E.bottom;
}


let top = {
  none = N.top;
  bool = B.top;
  string = S.top;
  int = I.top;
  float = F.top;
  addr = A.top;
  notimplem = NI.top;
  emptyvalue = E.top;
}

let is_bottom abs =
  N.is_bottom abs.none &&
  B.is_bottom abs.bool &&
  S.is_bottom abs.string &&
  I.is_bottom abs.int &&
  F.is_bottom abs.float &&
  F.is_bottom abs.float &&
  A.is_bottom abs.addr &&
  NI.is_bottom abs.notimplem &&
  E.is_bottom abs.emptyvalue

let is_top abs =
  N.is_top abs.none &&
  B.is_top abs.bool &&
  S.is_top abs.string &&
  I.is_top abs.int &&
  F.is_top abs.float &&
  A.is_top abs.addr &&
  NI.is_top abs.notimplem &&
  E.is_top abs.emptyvalue


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

      if not (A.is_bottom abs.addr) then (
        fprintf fmt "%s a: %a " (if !pred then "∨" else "") A.print abs.addr;
        pred := true;
      );

      if not (N.is_bottom abs.none) then (
        fprintf fmt "%s n: %a " (if !pred then "∨" else "") N.print abs.none;
        pred := true;
      );

      if not (NI.is_bottom abs.notimplem) then
        fprintf fmt "%s ni: %a " (if !pred then "∨" else "") NI.print abs.notimplem;

      if not (E.is_bottom abs.emptyvalue) then
        fprintf fmt "%s %a " (if !pred then "∨" else "") E.print abs.emptyvalue;

      fprintf fmt "|)@]"
    )


let unify op abs1 abs2 = abs1, abs2

let leq abs1 abs2 =
  N.leq abs1.none abs2.none &&
  B.leq abs1.bool abs2.bool &&
  S.leq abs1.string abs2.string &&
  I.leq abs1.int abs2.int &&
  F.leq abs1.float abs2.float &&
  A.leq abs1.addr abs2.addr &&
  NI.leq abs1.notimplem abs2.notimplem &&
  E.leq abs1.emptyvalue abs2.emptyvalue

let join abs1 abs2 =
  let r = {
    none = N.join abs1.none abs2.none;
    bool = B.join abs1.bool abs2.bool;
    string = S.join abs1.string abs2.string;
    int = I.join abs1.int abs2.int;
    float = F.join abs1.float abs2.float;
    addr = A.join abs1.addr abs2.addr;
    notimplem = NI.join abs1.notimplem abs2.notimplem;
    emptyvalue = E.join abs1.emptyvalue abs2.emptyvalue;
  }
  in
  debug "join@\nabs1 =@ @[ %a@]@\nabs2 =@ @[ %a@]@\nres =@ @[ %a@]"
    print abs1
    print abs2
    print r
  ;
  r

let meet abs1 abs2 = {
  none = N.meet abs1.none abs2.none;
  bool = B.meet abs1.bool abs2.bool;
  string = S.meet abs1.string abs2.string;
  int = I.meet abs1.int abs2.int;
  float = F.meet abs1.float abs2.float;
  addr = A.meet abs1.addr abs2.addr;
  notimplem = NI.meet abs1.notimplem abs2.notimplem;
  emptyvalue = E.meet abs1.emptyvalue abs2.emptyvalue;
}

let widening ctx abs1 abs2 = {
  none = N.widening ctx abs1.none abs2.none;
  bool = B.widening ctx abs1.bool abs2.bool;
  string = S.widening ctx abs1.string abs2.string;
  int = I.widening ctx abs1.int abs2.int;
  float = F.widening ctx abs1.float abs2.float;
  addr = A.widening ctx abs1.addr abs2.addr;
  notimplem = NI.widening ctx abs1.notimplem abs2.notimplem;
  emptyvalue = E.widening ctx abs1.emptyvalue abs2.emptyvalue;
}

(** Creation of uniquely typed valus *)
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


let addr a = {
  bottom with
  addr = a;
}

let string s = {
  bottom with
  string = s
}

let notimplem ne = {
  bottom with
  notimplem = ne
}

let emptyvalue ev = {
  bottom with
  emptyvalue = ev
}

(** Creation of a value from a program constant *)
let of_constant c =
  match c with
  | C_true -> boolean (B.singleton true)
  | C_false -> boolean (B.singleton false)
  | C_py_none -> none  (N.singleton c)
  | C_int n -> integer (I.of_constant c)
  | C_int_interval _ -> integer (I.of_constant c)
  | C_float n -> float (F.of_constant c)
  | C_float_interval _ -> float (F.of_constant c)
  | C_py_not_implemented -> notimplem  (NI.singleton c)
  | C_empty -> emptyvalue  (E.singleton c)
  | C_string s -> string (S.singleton s)
  | _ -> top

(** Type predicates *)
let can_be_none abs = not (N.is_bottom abs.none)
let can_be_bool abs = not (B.is_bottom abs.bool)
let can_be_int abs = not (I.is_bottom abs.int)
let can_be_float abs = not (F.is_bottom abs.float)
let can_be_string abs = not (S.is_bottom abs.string)
let can_be_addr abs = not (A.is_bottom abs.addr)
let can_be_notimplem abs = not (NI.is_bottom abs.notimplem)
let can_be_emptyvalue abs = not (E.is_bottom abs.emptyvalue)


(** Compute the type of a given value abstraction. *)
let type_of abs =
  (if can_be_none abs then [T_py_none] else []) |>
  (fun acc -> if can_be_bool abs then Universal.Ast.T_bool :: acc else acc ) |>
  (fun acc -> if can_be_int abs then Universal.Ast.T_int :: acc else acc) |>
  (fun acc -> if can_be_float abs then Universal.Ast.T_float :: acc else acc) |>
  (fun acc -> if can_be_string abs then Universal.Ast.T_string :: acc else acc) |>
  (fun acc -> if can_be_addr abs then T_addr :: acc else acc) |>
  (fun acc -> if can_be_notimplem abs then T_py_not_implemented :: acc else acc)  |>
  (fun acc -> if can_be_emptyvalue abs then T_empty :: acc else acc)

let fold_type f x abs =
  (if can_be_none abs then [T_py_none, {bottom with none = abs.none}] else []) |>
  (fun acc -> if can_be_bool abs then (Universal.Ast.T_bool, {bottom with bool = abs.bool}) :: acc else acc ) |>
  (fun acc -> if can_be_int abs then (Universal.Ast.T_int, {bottom with int = abs.int}) :: acc else acc) |>
  (fun acc -> if can_be_float abs then (Universal.Ast.T_float, {bottom with float = abs.float}) :: acc else acc) |>
  (fun acc -> if can_be_string abs then (Universal.Ast.T_string, {bottom with string = abs.string}) :: acc else acc) |>
  (fun acc -> if can_be_addr abs then (T_addr, {bottom with addr = abs.addr}) :: acc else acc) |>
  (fun acc -> if can_be_notimplem abs then (T_py_not_implemented, {bottom with notimplem = abs.notimplem}) :: acc else acc) |>
  (fun acc -> if can_be_emptyvalue abs then (T_empty, {bottom with emptyvalue = abs.emptyvalue}) :: acc else acc) |>
  List.fold_left f x

let is_type_upgradable t1 t2 =
  if t1 = t2 then false
  else
    match t1, t2 with
    | T_bool, T_int
    | T_bool, T_float -> true

    | T_int, T_float -> true

    | _ -> false

let upgrade_type types abs =
  types |> List.fold_left (fun acc (t, t') ->
      debug "upgrading %a to %a" Framework.Pp.pp_typ t Framework.Pp.pp_typ t';
      match (t, t') with
      | T_bool, T_int ->
        { abs with
          int = abs.int |>
                I.join (if B.mem true abs.bool then I.of_constant (C_int Z.one) else I.bottom) |>
                I.join (if B.mem false  abs.bool then I.of_constant (C_int Z.zero) else I.bottom);
        }

      | T_bool, T_float ->
        { abs with
          float = abs.float |>
                F.join (if B.mem true abs.bool then F.of_constant (C_float 1.) else F.bottom) |>
                F.join (if B.mem false  abs.bool then F.of_constant (C_float 0.) else F.bottom);
        }

      | T_int, T_float ->
        { abs with

          float = abs.float |>
                  F.join (F.of_int_interval abs.int) |>
                  F.join (if B.mem true abs.bool then F.of_constant (C_float 1.) else F.bottom) |>
                  F.join (if B.mem false  abs.bool then F.of_constant (C_float 0.) else F.bottom)
        }

      | _ -> assert false
    ) abs

let rec remove_types types abs =
  match types with
  | [] -> abs
  | t :: tl ->
    debug "removing type t = %a from %a" Framework.Pp.pp_typ t print abs;
    let abs' =
      match t with
      | T_int -> {abs with int = I.bottom}
      | T_bool -> {abs with bool = B.bottom}
      | _ -> assert false
    in
    remove_types tl abs'

let coerce abs1 abs2 =
  debug "coerce@\n abs1 = @[%a@]@\n abs2 = @[%a@]" print abs1 print abs2;

  let types1 = type_of abs1 and types2 = type_of abs2 in

  let upgrades1 = types1 |>
                  List.fold_left (fun acc t1 ->
                      debug "checking t1 = %a" Framework.Pp.pp_typ t1;
                      let types2' = types2 |> List.filter (is_type_upgradable t1) in
                      let r = types2' |> List.map (fun t2 -> (t1, t2)) in
                      r @ acc
                    ) []
  in

  let removes1 = upgrades1 |>
                 List.filter (fun (t1, _) -> not (List.mem t1 types2)) |>
                 List.map fst
  in

  let upgrades2 = types2 |>
                  List.fold_left (fun acc t2 ->
                      debug "checking t2 = %a" Framework.Pp.pp_typ t2;
                      let types1' = types1 |> List.filter (is_type_upgradable t2) in
                      let r = types1' |> List.map (fun t1 -> (t2, t1)) in
                      r @ acc
                    ) []
  in

  let removes2 = upgrades2 |>
                 List.filter (fun (t2, _) -> not (List.mem t2 types1)) |>
                 List.map fst
  in


  debug "upgrading abs1";
  let abs1 = upgrade_type upgrades1 abs1 |> remove_types removes1 in
  debug "abs1' = %a" print abs1;

  debug "upgrading abs2";
  let abs2 = upgrade_type upgrades2 abs2  |> remove_types removes2 in
  debug "abs2' = %a" print abs2;

  abs1, abs2

let cast typ abs =
  match typ with
  | T_float ->
    let abs = upgrade_type [(T_bool, T_float); (T_int, T_float)] abs in
    { bottom with float = abs.float }

  | T_int ->
    { bottom with int = I.join abs.int (F.to_int abs.float) }

  | _ ->
    assert false

(** Boolean predicates *)
let can_be_true abs =
  (B.mem true abs.bool) ||
  (S.exists (fun s -> s <> "") abs.string) ||
  (I.can_be_true abs.int) ||
  (F.can_be_true abs.float) ||
  (not @@ A.is_bottom abs.addr) ||
  (not @@ NI.is_bottom abs.notimplem)

let can_be_false abs =
  let b = (not @@ N.is_bottom abs.none) ||
  (S.exists (fun s -> s = "") abs.string) ||
  (B.mem false abs.bool) ||
  (I.can_be_false abs.int) ||
          (F.can_be_false abs.float)
  in
  debug "can_be_false = %b" b;
  b

let assume_is_type typ abs =
  let abs' = match typ with
    | T_int -> integer I.top
    | T_float -> float F.top
    | T_bool -> boolean B.top
    | T_string -> string S.top
    | T_addr -> addr A.top
    | T_py_not_implemented -> notimplem NI.top
    | T_py_none -> none N.top
    | _ -> assert false
  in
  meet abs abs'

let assume_is_not_type typ abs =
  let abs' = match typ with
    | T_int -> {top with int = I.bottom}
    | T_float -> {top with float = F.bottom}
    | T_bool -> {top with bool = B.bottom}
    | T_string -> {top with string = S.bottom}
    | T_addr -> {top with addr = A.bottom}
    | T_py_not_implemented -> {top with notimplem = NI.bottom}
    | T_py_none -> {top with none = N.bottom}
    | _ -> assert false
  in
  meet abs abs'



(** Forward evaluation of unary operators *)
let fwd_unop op abs =
  match op with
  | O_sqrt ->
    let abs, _ = coerce abs (float F.top) in
    { bottom with float = F.fwd_unop op abs.float }

  | O_plus T_int | O_minus T_int ->
    let abs = upgrade_type [(T_bool, T_int)] abs in
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
  addr = abs.addr;
  notimplem = abs.notimplem;
  emptyvalue = abs.emptyvalue;
}


let assume_false abs = {
  none = abs.none;
  bool = B.meet abs.bool (B.singleton false);
  string = S.filter (fun s -> s = "") abs.string;
  int = I.assume_false abs.int;
  float = F.assume_false abs.float;
  addr = A.bottom;
  notimplem = NI.bottom;
  emptyvalue = E.bottom;
}

(** Forwared evaluation of binary operators *)
let fwd_binop op abs1 abs2 =
  match op with
  | O_py_and ->
    begin
      match can_be_false abs1, can_be_true abs1 with
      | true, false -> assume_false abs1
      | false, true -> abs2
      | true, true -> join (assume_false abs1) abs2
      | false, false -> bottom
    end

  | O_py_or ->
    begin
      match can_be_false abs1, can_be_true abs1 with
      | true, false -> abs2
      | false, true -> assume_true abs1
      | true, true -> join (assume_true abs1) abs2
      | false, false -> bottom
    end

  | O_plus T_int | O_minus T_int | O_mult T_int | O_mod T_int  ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = I.fwd_binop op abs1.int abs2.int;
    }

  | O_plus T_float | O_minus T_float | O_mult T_float | O_mod T_float ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      float = F.fwd_binop op abs1.float abs2.float;
    }

  | O_pow ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = I.fwd_binop op abs1.int abs2.int;
      float = F.fwd_binop op abs1.float abs2.float;
    }

  (* FIXME : use operator types *)
  | O_div T_float ->
    let abs1, abs2 = coerce abs1 abs2 in
    let abs1 = cast T_float abs1 and abs2 = cast T_float abs2 in
    {
      bottom with
      float = F.fwd_binop op abs1.float abs2.float;
    }

  | O_py_floor_div ->
    let abs1, abs2 = coerce abs1 abs2 in
    let abs1 = cast T_int abs1 and abs2 = cast T_int abs2 in
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
    let abs1 = { abs1 with float = F.bottom } and abs2 = { abs2 with float = F.bottom } in
    let abs1, abs2 = coerce abs1 abs2 in
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

  | O_py_is ->
    let can_be_true = not @@ is_bottom @@ meet abs1 abs2 in
    (* FIXME: weak addresses make "is" returns false *)
    let can_be_false =
      (not @@ S.is_bottom abs1.string && not @@ S.is_bottom abs2.string) ||
      (not @@ is_bottom abs1 && not @@ is_bottom abs2 && is_bottom @@ meet abs1 abs2)
    in
    debug "is forward evaluation: can_be_true = %b, can_be_false = %b" can_be_true can_be_false;
    (match can_be_true, can_be_false with
     | true, false -> of_constant C_true
     | false, true -> of_constant C_false
     | true, true -> boolean B.top
     | false, false -> bottom)

  | O_py_is_not ->
    let can_be_false = not @@ is_bottom @@ meet abs1 abs2 in
    let can_be_true =
      (not @@ S.is_bottom abs1.string && not @@ S.is_bottom abs2.string) ||
      (not @@ is_bottom abs1 && not @@ is_bottom abs2 && is_bottom @@ meet abs1 abs2)
    in
    debug "is not forward evaluation: can_be_true = %b, can_be_false = %b" can_be_true can_be_false;
    (match can_be_true, can_be_false with
     | true, false -> of_constant C_true
     | false, true -> of_constant C_false
     | true, true -> boolean B.top
     | false, false -> bottom)

  | _ ->
    Debug.fail "fwd_binop %a not implemented" Framework.Pp.pp_operator op

let mk_true = boolean (B.singleton true)
let mk_false = boolean (B.singleton false)

let is_not_same_type abs1 abs2 =
  let type1 = type_of abs1 and type2 = type_of abs2 in
  List.for_all (fun t -> not (List.mem t type2)) type1


let fwd_filter op abs1 abs2 =
  let abs1, abs2 = coerce abs1 abs2 in

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
    let abs1, abs2 = coerce abs1 abs2 in
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
  let abs1, abs2 = coerce abs1 abs2 in
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


let rebase_addr a1 a2 abs =
  if not (A.mem a1 abs.addr) then abs else
  { abs with addr = A.remove a1 abs.addr |> A.add a2 }
