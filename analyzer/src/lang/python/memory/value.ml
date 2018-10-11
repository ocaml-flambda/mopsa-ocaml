(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstraction of Python values. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Addr_env
open Framework.Value

module Value =
  struct

    (*==========================================================================*)
    (**                           {2 Lattices}                                  *)
    (*==========================================================================*)

    (** NoneType singleton lattice *)
    module N = Framework.Lattices.Finite_powerset.Make(struct
                   type t = Framework.Ast.constant
                   let values = [C_py_none]
                   let print fmt c = Framework.Ast.pp_constant fmt c
                 end
                 )

    (** Powerset lattice of finite strings *)
    module S = Framework.Lattices.Powerset.Make(struct
                   type t = string
                   let compare = compare
                   let print fmt s = Format.fprintf fmt "\"%s\"" s
                 end)

    (** NotImplementedType singleton lattice *)
    module NI = Framework.Lattices.Finite_powerset.Make(struct
                    type t = Framework.Ast.constant
                    let values = [C_py_not_implemented]
                    let print fmt c = Framework.Ast.pp_constant fmt c
                  end
                  )

    (** Empty value singleton lattice *)
    module E = Framework.Lattices.Finite_powerset.Make(struct
                   type t = Framework.Ast.constant
                   let values = [C_py_empty]
                   let print fmt c = Framework.Ast.pp_constant fmt c
                 end
                 )

    (** Integer intervals lattice *)
    module I = Universal.Numeric.Values.Intervals.Value

    (** Float intervals lattices *)
    module F = Universal.Numeric.Values.Float_intervals.Value



    (*==========================================================================*)
    (**                      {2 Value abstraction}                              *)
    (*==========================================================================*)


    type t = {
        none: N.t;
        string: S.t;
        int : I.t;
        float : F.t;
        notimplem: NI.t;
        empty: E.t;
      }

    type _ value += V_py_values : t value

    let id = V_py_values
    let name = "python.memory.value", "py_values"

    let identify : type a. a value -> (t, a) eq option =
      function
      | V_py_values -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:(fst name) fmt
    let zone = any_zone


    let bottom = {
        none = N.bottom;
        string = S.bottom;
        int = I.bottom;
        float = F.bottom;
        notimplem = NI.bottom;
        empty = E.bottom;
      }


    let top = {
        none = N.top;
        string = S.top;
        int = I.top;
        float = F.top;
        notimplem = NI.top;
        empty = E.top;
      }

    let is_bottom abs =
      N.is_bottom abs.none &&
        S.is_bottom abs.string &&
          I.is_bottom abs.int &&
            F.is_bottom abs.float &&
              NI.is_bottom abs.notimplem &&
                E.is_bottom abs.empty

    (* let is_top abs =
     *   N.is_top abs.none &&
     *   S.is_top abs.string &&
     *   I.is_top abs.int &&
     *   (\* F.is_top abs.float && *\)
     *   NI.is_top abs.notimplem &&
     *   E.is_top abs.empty *)


    let init = top

    let print fmt abs =
      let open Format in
      (* if is_top abs then
       *   fprintf fmt "⊤"
       * else *)
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


    let subset abs1 abs2 =
      N.subset abs1.none abs2.none &&
        S.subset abs1.string abs2.string &&
          I.subset abs1.int abs2.int &&
            F.subset abs1.float abs2.float &&
            NI.subset abs1.notimplem abs2.notimplem &&
              E.subset abs1.empty abs2.empty

    let join annot abs1 abs2 = {
        none = N.join annot abs1.none abs2.none;
        string = S.join annot abs1.string abs2.string;
        int = I.join annot abs1.int abs2.int;
        float = F.join annot abs1.float abs2.float;
        notimplem = NI.join annot abs1.notimplem abs2.notimplem;
        empty = E.join annot abs1.empty abs2.empty;
      }

    let meet annot abs1 abs2 = {
        none = N.meet annot abs1.none abs2.none;
        string = S.meet annot abs1.string abs2.string;
        int = I.meet annot abs1.int abs2.int;
        float = F.meet annot abs1.float abs2.float;
        notimplem = NI.meet annot abs1.notimplem abs2.notimplem;
        empty = E.meet annot abs1.empty abs2.empty;
      }

    let widen annot abs1 abs2 = {
        none = N.widen annot abs1.none abs2.none;
        string = S.widen annot abs1.string abs2.string;
        int = I.widen annot abs1.int abs2.int;
        float = F.widen annot abs1.float abs2.float;
        notimplem = NI.widen annot abs1.notimplem abs2.notimplem;
        empty = E.widen annot abs1.empty abs2.empty;
      }

    (** Creation of uniquely typed values *)
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

    let empty e = {
        bottom with
        empty = e
      }

    (** Creation of a value from a program constant *)
    let of_constant c =
      match c with
      | C_bool true -> integer (I.of_constant (C_int Z.one))
      | C_bool false -> integer (I.of_constant (C_int Z.zero))
      | C_top T_bool -> integer (I.of_constant (C_int_interval (Z.zero, Z.one)))

      | C_py_none | C_top T_py_none -> none  (N.singleton c)

      | C_int n -> integer (I.of_constant c)
      | C_int_interval _ -> integer (I.of_constant c)
      | C_top T_int -> integer I.top

      | C_float n -> float (F.of_constant c)
      | C_float_interval _ -> float (F.of_constant c)
      | C_top (T_float _) (* FIXME *) -> float F.top

      | C_py_not_implemented | C_top T_py_not_implemented -> notimplem  (NI.singleton c)

      | C_string s -> string (S.singleton s)
      | C_top T_string -> string S.top

      | C_py_empty | C_top T_py_empty -> empty  (E.singleton c)

      | _ -> top


    (** Forward evaluation of unary operators *)
    let unop op (abs:t) =
      let c = I.unop op abs.int in
      let cf = F.unop op abs.float in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) {abs with int = c.value; float = cf.value}

    (** Forward evaluation of binary operators *)
    let binop op abs1 abs2 =
      let c = I.binop op abs1.int abs2.int in
      let cf = F.binop op abs1.float abs2.float in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) {bottom with int = c.value; float = cf.value }

    let mk_true = integer (I.of_constant (C_int Z.one))
    let mk_false = integer (I.of_constant (C_int Z.zero))

    let filter a b =
      let c = I.filter a.int b in
      let cf = F.filter a.float b in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) {bottom with int = c.value; float = cf.value }

    let bwd_unop op abs rabs =
      let c = I.bwd_unop op abs.int rabs.int in
      let cf = F.bwd_unop op abs.float rabs.float in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) {bottom with int = c.value; float = cf.value }

    (** Backward evaluation of binary operators *)
    let bwd_binop op abs1 abs2 rabs =
      let c = I.bwd_binop op abs1.int abs2.int rabs.int in
      let cf = F.bwd_binop op abs1.float abs2.float rabs.float in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) ({bottom with int = fst c.value; float = fst cf.value}, {bottom with int = snd c.value; float = snd cf.value})

    (** Backward filters of comparison operators *)
    let compare op abs1 abs2 r =
      let c = I.compare op abs1.int abs2.int r in
      let cf = F.compare op abs1.float abs2.float r in
      let open Framework.Channel in
      return ~channels:(c.channels @ cf.channels) ({bottom with int = fst c.value; float = fst cf.value}, {bottom with int = snd c.value; float = snd cf.value})

    let ask : type r. r Framework.Query.query -> (expr -> t) -> r option = fun _ _ -> None (*FIXME ?*)
  end

let () = Framework.Value.register_value (module Value)
