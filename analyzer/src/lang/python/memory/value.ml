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

module NoneLattice = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_none]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)

module BoolLattice = Framework.Lattices.Enum.Make(struct
    type t = bool
    let values = [true; false]
    let print = Format.pp_print_bool
  end)

module StringLattice = Framework.Lattices.Top_set.Make(struct
    type t = string
    let compare = compare
    let print fmt s = Format.fprintf fmt "\"%s\"" s
  end)

module AddrLattice = struct
  module SSet = Framework.Lattices.Top_set.Make
      (struct type t = addr let compare = compare_addr let print = Universal.Pp.pp_addr end)
  include SSet
  let widening ctx a b = join a b
end

module NotImplementedLattice = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_not_implemented]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)

module IntLattice = Universal.Numeric.Integers.Value

module FloatLattice = Universal.Numeric.Floats

module EmptyValueLattice = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_empty]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)

module UndefinedLattice = Framework.Lattices.Enum.Make(struct
    type t = Framework.Ast.constant
    let values = [C_py_undefined]
    let print fmt c = Framework.Pp.pp_constant fmt c
  end
)

(** Lattice structure *)

type t = {
  none: NoneLattice.t;
  bool: BoolLattice.t;
  string: StringLattice.t;
  int : IntLattice.t;
  float : FloatLattice.t;
  addr: AddrLattice.t;
  notimplem: NotImplementedLattice.t;
  emptyvalue: EmptyValueLattice.t;
}

let bottom = {
  none = NoneLattice.bottom;
  bool = BoolLattice.bottom;
  string = StringLattice.bottom;
  int = IntLattice.bottom;
  float = FloatLattice.bottom;
  addr = AddrLattice.bottom;
  notimplem = NotImplementedLattice.bottom;
  emptyvalue = EmptyValueLattice.bottom;
}


let top = {
  none = NoneLattice.top;
  bool = BoolLattice.top;
  string = StringLattice.top;
  int = IntLattice.top;
  float = FloatLattice.top;
  addr = AddrLattice.top;
  notimplem = NotImplementedLattice.top;
  emptyvalue = EmptyValueLattice.top;
}

let is_bottom abs =
  NoneLattice.is_bottom abs.none &&
  BoolLattice.is_bottom abs.bool &&
  StringLattice.is_bottom abs.string &&
  IntLattice.is_bottom abs.int &&
  FloatLattice.is_bottom abs.float &&
  FloatLattice.is_bottom abs.float &&
  AddrLattice.is_bottom abs.addr &&
  NotImplementedLattice.is_bottom abs.notimplem &&
  EmptyValueLattice.is_bottom abs.emptyvalue

let is_top abs =
  NoneLattice.is_top abs.none &&
  BoolLattice.is_top abs.bool &&
  StringLattice.is_top abs.string &&
  IntLattice.is_top abs.int &&
  FloatLattice.is_top abs.float &&
  AddrLattice.is_top abs.addr &&
  NotImplementedLattice.is_top abs.notimplem &&
  EmptyValueLattice.is_top abs.emptyvalue


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

      if not (IntLattice.is_bottom abs.int) then (
        fprintf fmt " i: %a " IntLattice.print abs.int;
        pred := true;
      );

      if not (FloatLattice.is_bottom abs.float) then (
        fprintf fmt "%s f: %a " (if !pred then "∨" else "") FloatLattice.print abs.float;
        pred := true;
      );

      if not (BoolLattice.is_bottom abs.bool) then (
        fprintf fmt "%s b: %a " (if !pred then "∨" else "") BoolLattice.print abs.bool;
        pred := true;
      );

      if not (StringLattice.is_bottom abs.string) then (
        fprintf fmt "%s s: %a " (if !pred then "∨" else "") StringLattice.print abs.string;
        pred := true;
      );

      if not (AddrLattice.is_bottom abs.addr) then (
        fprintf fmt "%s a: %a " (if !pred then "∨" else "") AddrLattice.print abs.addr;
        pred := true;
      );

      if not (NoneLattice.is_bottom abs.none) then (
        fprintf fmt "%s n: %a " (if !pred then "∨" else "") NoneLattice.print abs.none;
        pred := true;
      );

      if not (NotImplementedLattice.is_bottom abs.notimplem) then
        fprintf fmt "%s ni: %a " (if !pred then "∨" else "") NotImplementedLattice.print abs.notimplem;

      if not (EmptyValueLattice.is_bottom abs.emptyvalue) then
        fprintf fmt "%s %a " (if !pred then "∨" else "") EmptyValueLattice.print abs.emptyvalue;

      fprintf fmt "|)@]"
    )


let unify op abs1 abs2 = abs1, abs2

let leq abs1 abs2 =
  NoneLattice.leq abs1.none abs2.none &&
  BoolLattice.leq abs1.bool abs2.bool &&
  StringLattice.leq abs1.string abs2.string &&
  IntLattice.leq abs1.int abs2.int &&
  FloatLattice.leq abs1.float abs2.float &&
  AddrLattice.leq abs1.addr abs2.addr &&
  NotImplementedLattice.leq abs1.notimplem abs2.notimplem &&
  EmptyValueLattice.leq abs1.emptyvalue abs2.emptyvalue

let join abs1 abs2 =
  let r = {
    none = NoneLattice.join abs1.none abs2.none;
    bool = BoolLattice.join abs1.bool abs2.bool;
    string = StringLattice.join abs1.string abs2.string;
    int = IntLattice.join abs1.int abs2.int;
    float = FloatLattice.join abs1.float abs2.float;
    addr = AddrLattice.join abs1.addr abs2.addr;
    notimplem = NotImplementedLattice.join abs1.notimplem abs2.notimplem;
    emptyvalue = EmptyValueLattice.join abs1.emptyvalue abs2.emptyvalue;
  }
  in
  debug "join@\nabs1 =@ @[ %a@]@\nabs2 =@ @[ %a@]@\nres =@ @[ %a@]"
    print abs1
    print abs2
    print r
  ;
  r

let meet abs1 abs2 = {
  none = NoneLattice.meet abs1.none abs2.none;
  bool = BoolLattice.meet abs1.bool abs2.bool;
  string = StringLattice.meet abs1.string abs2.string;
  int = IntLattice.meet abs1.int abs2.int;
  float = FloatLattice.meet abs1.float abs2.float;
  addr = AddrLattice.meet abs1.addr abs2.addr;
  notimplem = NotImplementedLattice.meet abs1.notimplem abs2.notimplem;
  emptyvalue = EmptyValueLattice.meet abs1.emptyvalue abs2.emptyvalue;
}

let widening ctx abs1 abs2 = {
  none = NoneLattice.widening ctx abs1.none abs2.none;
  bool = BoolLattice.widening ctx abs1.bool abs2.bool;
  string = StringLattice.widening ctx abs1.string abs2.string;
  int = IntLattice.widening ctx abs1.int abs2.int;
  float = FloatLattice.widening ctx abs1.float abs2.float;
  addr = AddrLattice.widening ctx abs1.addr abs2.addr;
  notimplem = NotImplementedLattice.widening ctx abs1.notimplem abs2.notimplem;
  emptyvalue = EmptyValueLattice.widening ctx abs1.emptyvalue abs2.emptyvalue;
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
  | C_true -> boolean (BoolLattice.singleton true)
  | C_false -> boolean (BoolLattice.singleton false)
  | C_py_none -> none  (NoneLattice.singleton c)
  | C_int n -> integer (IntLattice.of_constant c)
  | C_int_range _ -> integer (IntLattice.of_constant c)
  | C_float n -> float (FloatLattice.of_constant c)
  | C_float_range _ -> float (FloatLattice.of_constant c)
  | C_py_not_implemented -> notimplem  (NotImplementedLattice.singleton c)
  | C_empty -> emptyvalue  (EmptyValueLattice.singleton c)
  | C_string s -> string (StringLattice.singleton s)
  | _ -> top

(** Type predicates *)
let can_be_none abs = not (NoneLattice.is_bottom abs.none)
let can_be_bool abs = not (BoolLattice.is_bottom abs.bool)
let can_be_int abs = not (IntLattice.is_bottom abs.int)
let can_be_float abs = not (FloatLattice.is_bottom abs.float)
let can_be_string abs = not (StringLattice.is_bottom abs.string)
let can_be_addr abs = not (AddrLattice.is_bottom abs.addr)
let can_be_notimplem abs = not (NotImplementedLattice.is_bottom abs.notimplem)
let can_be_emptyvalue abs = not (EmptyValueLattice.is_bottom abs.emptyvalue)


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
                IntLattice.join (if BoolLattice.mem true abs.bool then IntLattice.of_constant (C_int Z.one) else IntLattice.bottom) |>
                IntLattice.join (if BoolLattice.mem false  abs.bool then IntLattice.of_constant (C_int Z.zero) else IntLattice.bottom);
        }

      | T_bool, T_float ->
        { abs with
          float = abs.float |>
                FloatLattice.join (if BoolLattice.mem true abs.bool then FloatLattice.of_constant (C_float 1.) else FloatLattice.bottom) |>
                FloatLattice.join (if BoolLattice.mem false  abs.bool then FloatLattice.of_constant (C_float 0.) else FloatLattice.bottom);
        }

      | T_int, T_float ->
        { abs with

          float = abs.float |>
                  FloatLattice.join (FloatLattice.of_int_interval abs.int) |>
                  FloatLattice.join (if BoolLattice.mem true abs.bool then FloatLattice.of_constant (C_float 1.) else FloatLattice.bottom) |>
                  FloatLattice.join (if BoolLattice.mem false  abs.bool then FloatLattice.of_constant (C_float 0.) else FloatLattice.bottom)
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
      | T_int -> {abs with int = IntLattice.bottom}
      | T_bool -> {abs with bool = BoolLattice.bottom}
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
    { bottom with int = IntLattice.join abs.int (FloatLattice.to_int abs.float) }

  | _ ->
    assert false

(** Boolean predicates *)
let can_be_true abs =
  (BoolLattice.mem true abs.bool) ||
  (StringLattice.exists (fun s -> s <> "") abs.string) ||
  (IntLattice.can_be_true abs.int) ||
  (FloatLattice.can_be_true abs.float) ||
  (not @@ AddrLattice.is_bottom abs.addr) ||
  (not @@ NotImplementedLattice.is_bottom abs.notimplem)

let can_be_false abs =
  let b = (not @@ NoneLattice.is_bottom abs.none) ||
  (StringLattice.exists (fun s -> s = "") abs.string) ||
  (BoolLattice.mem false abs.bool) ||
  (IntLattice.can_be_false abs.int) ||
          (FloatLattice.can_be_false abs.float)
  in
  debug "can_be_false = %b" b;
  b

let assume_is_type typ abs =
  let abs' = match typ with
    | T_int -> integer IntLattice.top
    | T_float -> float FloatLattice.top
    | T_bool -> boolean BoolLattice.top
    | T_string -> string StringLattice.top
    | T_addr -> addr AddrLattice.top
    | T_py_not_implemented -> notimplem NotImplementedLattice.top
    | T_py_none -> none NoneLattice.top
    | _ -> assert false
  in
  meet abs abs'

let assume_is_not_type typ abs =
  let abs' = match typ with
    | T_int -> {top with int = IntLattice.bottom}
    | T_float -> {top with float = FloatLattice.bottom}
    | T_bool -> {top with bool = BoolLattice.bottom}
    | T_string -> {top with string = StringLattice.bottom}
    | T_addr -> {top with addr = AddrLattice.bottom}
    | T_py_not_implemented -> {top with notimplem = NotImplementedLattice.bottom}
    | T_py_none -> {top with none = NoneLattice.bottom}
    | _ -> assert false
  in
  meet abs abs'



(** Forward evaluation of unary operators *)
let fwd_unop op abs =
  match op with
  | O_sqrt ->
    let abs, _ = coerce abs (float FloatLattice.top) in
    { bottom with float = FloatLattice.fwd_unop op abs.float }

  | O_plus | O_minus ->
    let abs = upgrade_type [(T_bool, T_int)] abs in
    {bottom with int = IntLattice.fwd_unop op abs.int; float = FloatLattice.fwd_unop op abs.float}

  | _ ->
    Debug.fail "fwd_unop %a not implemented" Framework.Pp.pp_operator op

(** Filtering functions *)
let assume_true abs = {
  none = NoneLattice.bottom;
  bool = BoolLattice.meet abs.bool (BoolLattice.singleton true);
  string = StringLattice.filter (fun s -> s <> "") abs.string;
  int = IntLattice.assume_true abs.int;
  float = FloatLattice.assume_true abs.float;
  addr = abs.addr;
  notimplem = abs.notimplem;
  emptyvalue = abs.emptyvalue;
}


let assume_false abs = {
  none = abs.none;
  bool = BoolLattice.meet abs.bool (BoolLattice.singleton false);
  string = StringLattice.filter (fun s -> s = "") abs.string;
  int = IntLattice.assume_false abs.int;
  float = FloatLattice.assume_false abs.float;
  addr = AddrLattice.bottom;
  notimplem = NotImplementedLattice.bottom;
  emptyvalue = EmptyValueLattice.bottom;
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

  | O_plus ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = IntLattice.fwd_binop op abs1.int abs2.int;
      float = FloatLattice.fwd_binop op abs1.float abs2.float;
      string =
        try
          StringLattice.fold (fun s1 acc ->
              StringLattice.fold (fun s2 acc ->
                  StringLattice.add (s1 ^ s2) acc
                ) abs2.string acc
            ) abs1.string StringLattice.bottom;
        with Top.Found_TOP -> StringLattice.top
    }

  | O_mult ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = IntLattice.fwd_binop op abs1.int abs2.int;
      float = FloatLattice.fwd_binop op abs1.float abs2.float;
      string = if StringLattice.is_bottom abs1.string || IntLattice.is_bottom abs2.int then StringLattice.bottom else assert false;
    }

  | O_mod | O_minus | O_pow ->
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = IntLattice.fwd_binop op abs1.int abs2.int;
      float = FloatLattice.fwd_binop op abs1.float abs2.float;
    }

  | O_div ->
    let abs1, abs2 = coerce abs1 abs2 in
    let abs1 = cast T_float abs1 and abs2 = cast T_float abs2 in
    {
      bottom with
      float = FloatLattice.fwd_binop op abs1.float abs2.float;
    }

  | O_py_floor_div ->
    let abs1, abs2 = coerce abs1 abs2 in
    let abs1 = cast T_int abs1 and abs2 = cast T_int abs2 in
    {
      bottom with
      int = IntLattice.fwd_binop O_div abs1.int abs2.int;
    }

  | O_bit_and
  | O_bit_or
  | O_bit_xor
  | O_bit_lshift
  | O_bit_rshift
    ->
    let abs1 = { abs1 with float = FloatLattice.bottom } and abs2 = { abs2 with float = FloatLattice.bottom } in
    let abs1, abs2 = coerce abs1 abs2 in
    {
      bottom with
      int = IntLattice.fwd_binop op abs1.int abs2.int;
      bool = BoolLattice.fold (fun b1 acc ->
          BoolLattice.fold (fun b2 acc ->
              let op =
                match op with
                  O_bit_and -> (&&)
                | O_bit_or -> (||)
                | O_bit_xor -> (<>)
                | _ -> assert false
              in
              BoolLattice.singleton (op b1 b2) |> BoolLattice.join acc
            ) abs2.bool acc
        ) abs1.bool BoolLattice.bottom
    }

  | O_py_is ->
    let can_be_true = not @@ is_bottom @@ meet abs1 abs2 in
    (* FIXME: weak addresses make "is" returns false *)
    let can_be_false =
      (not @@ StringLattice.is_bottom abs1.string && not @@ StringLattice.is_bottom abs2.string) ||
      (not @@ is_bottom abs1 && not @@ is_bottom abs2 && is_bottom @@ meet abs1 abs2)
    in
    debug "is forward evaluation: can_be_true = %b, can_be_false = %b" can_be_true can_be_false;
    (match can_be_true, can_be_false with
     | true, false -> of_constant C_true
     | false, true -> of_constant C_false
     | true, true -> boolean BoolLattice.top
     | false, false -> bottom)

  | O_py_is_not ->
    let can_be_false = not @@ is_bottom @@ meet abs1 abs2 in
    let can_be_true =
      (not @@ StringLattice.is_bottom abs1.string && not @@ StringLattice.is_bottom abs2.string) ||
      (not @@ is_bottom abs1 && not @@ is_bottom abs2 && is_bottom @@ meet abs1 abs2)
    in
    debug "is not forward evaluation: can_be_true = %b, can_be_false = %b" can_be_true can_be_false;
    (match can_be_true, can_be_false with
     | true, false -> of_constant C_true
     | false, true -> of_constant C_false
     | true, true -> boolean BoolLattice.top
     | false, false -> bottom)

  | _ ->
    Debug.fail "fwd_binop %a not implemented" Framework.Pp.pp_operator op

let mk_true = boolean (BoolLattice.singleton true)
let mk_false = boolean (BoolLattice.singleton false)

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
      not @@ StringLattice.is_bottom abs1.string ||
      not @@ StringLattice.is_bottom abs2.string &&
      (
        StringLattice.is_bottom @@ StringLattice.meet abs1.string abs2.string ||
        try
          (StringLattice.cardinal abs1.string > 1) ||
          (StringLattice.cardinal abs2.string > 1)
        with Top.Found_TOP -> true
      )
    ) ||
    (IntLattice.fwd_filter op abs1.int abs2.int) ||
    (FloatLattice.fwd_filter op abs1.float abs2.float)

  | O_lt ->
    (BoolLattice.mem false abs1.bool && BoolLattice.mem true abs1.bool) ||
    (IntLattice.fwd_filter op abs1.int abs2.int) ||
    (FloatLattice.fwd_filter op abs1.float abs2.float)
  | O_le ->
    (not @@ BoolLattice.is_bottom @@ BoolLattice.meet abs1.bool abs2.bool) ||
    (BoolLattice.mem false abs1.bool && BoolLattice.mem true abs1.bool) ||
    (IntLattice.fwd_filter op abs1.int abs2.int) ||
    (FloatLattice.fwd_filter op abs1.float abs2.float)
  | O_gt ->
    (BoolLattice.mem true abs1.bool && BoolLattice.mem false abs1.bool) ||
    (IntLattice.fwd_filter op abs1.int abs2.int) ||
    (FloatLattice.fwd_filter op abs1.float abs2.float)
  | O_ge ->
    (not @@ BoolLattice.is_bottom @@ BoolLattice.meet abs1.bool abs2.bool) ||
    (BoolLattice.mem true abs1.bool && BoolLattice.mem false abs1.bool) ||
    (IntLattice.fwd_filter op abs1.int abs2.int) ||
    (FloatLattice.fwd_filter op abs1.float abs2.float)

  | _ -> assert false


let bwd_unop op abs rabs =
  match op with
  | O_sqrt ->
    panic "bwd evaluation of sqrt not supported"

  | O_minus ->
    {bottom with int = IntLattice.bwd_unop op abs.int rabs.int; float = FloatLattice.bwd_unop op abs.float rabs.float}

  | O_plus ->
    {bottom with int = abs.int; float = abs.float}

  | _ ->
    assert false

(** Backward evaluation of binary operators *)
let bwd_binop_default abs1 abs2 = (abs1, abs2)
let bwd_binop op abs1 abs2 rabs =
  match op with
  | O_plus | O_minus | O_mult | O_div | O_mod ->
    let abs1, abs2 = coerce abs1 abs2 in
    let int1, int2 = IntLattice.bwd_binop op abs1.int abs2.int rabs.int in
    let float1, float2 = FloatLattice.bwd_binop op abs1.float abs2.float rabs.float in
    {abs1 with int = int1; float = float1},
    {abs2 with int = int2; float = float2}

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
  let int1, int2 = IntLattice.bwd_filter op abs1.int abs2.int in
  let float1, float2 = FloatLattice.bwd_filter op abs1.float abs2.float in

  match op with
  | O_eq ->
    let abs = meet abs1 abs2 in
    {abs with int = int1; float = float1},
    {abs with int = int2; float = float2}

  | O_ne ->
    (* let addr1, addr2 = AddrLattice.bwd_neq abs1.addr abs2.addr in
     * let string1, string2 = StringLattice.bwd_neq abs1.string abs2.string in
     * {abs1 with int = int1; float = float1; addr = addr1; string = string1},
     * {abs2 with int = int2; float = float2; addr = addr2; string = string2} *)
    assert false

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
  if not (AddrLattice.mem a1 abs.addr) then abs else
  { abs with addr = AddrLattice.remove a1 abs.addr |> AddrLattice.add a2 }
