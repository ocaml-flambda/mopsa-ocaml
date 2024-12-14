(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Finite sets of possibly included or definitely excluded integer constants *)

open Mopsa
open Sig.Abstraction.Simplified_value
open Ast
open Common


module SimplifiedValue =
struct

  (** {2 Types} *)
  (** ********* *)

  module Set = SetExt.Make(
    struct
      type t = Z.t
      let compare = Z.compare
      let print = unformat Z.pp_print
    end
  )

  type t =
    | In    of Set.t
    | NotIn of Set.t

  let print printer a =
    let print_set prefix s =
      if Set.is_empty s then pp_string printer (prefix^"∅")
      else
        pp_set
          (unformat Z.pp_print)
          printer
          (Set.to_poly_set s)
          ~sopen:(prefix ^ "{") ~ssep:"," ~sclose:"}" in
    match a with
    | In s    -> print_set "∈ " s
    | NotIn s -> print_set "∉ " s

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.powersets.excluded"
        let display = "excluded"
    end)

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let bottom = In Set.empty

  let top = NotIn Set.empty

  let is_bottom a =
    match a with
    | In s    -> Set.is_empty s
    | NotIn _ -> false

  let is_top a =
    match a with
    | In _    -> false
    | NotIn s -> Set.is_empty s

  (** {2 Options} *)
  (** *********** *)

  let opt_max_intset = ref 10

  let () =
    register_domain_option name {
      key = "-max-excluded-set-size";
      category = "Numeric";
      doc = " maximum size of integer sets for the excluded powerset";
      spec = ArgExt.Set_int opt_max_intset;
      default = string_of_int !opt_max_intset;
    }

  (** {2 Utilities} *)
  (** ************* *)

  let zero = In (Set.singleton Z.zero)
  let one = In (Set.singleton Z.one)

  (** [bound a] is [a] if the number of elements in [a] is lesser or equal to
      the maximum number of elements allowed, otherwise it is [top]. Does not
      bound the size of [NotIn], as there are no infinite ascending chains for
      [NotIn]. *)
  let bound_size (a:t) : t =
    match a with
    | NotIn _ -> a
    | In s    -> if Set.cardinal s <= !opt_max_intset then a else top

  (** [of_bounds l r] is [{l, l+1, ..., r}] if this set has size up to the
      maximum number of elements, otherwise it is [NotIn {l-1, r+1}].
      This is a precision optimization compared to simply return top, as we
      already know that [l - 1] and [r + 1]  will not be in the set of elements.
      Due to this optimization, top is often represented as
      [∉{MININT - 1, MAXINT + 1}] in C programs. *)
  let of_bounds (l:Z.t) (h:Z.t) : t =
    let rec doit acc i =
      if Z.gt i h then In acc
      else doit (Set.add i acc) (Z.succ i)
    in
    if Z.sub h l >= Z.of_int !opt_max_intset
    then NotIn (Set.of_list [Z.pred l; Z.succ h])
    else doit Set.empty l

  let is_zero = function
    | In s    -> Set.equal s (Set.singleton Z.zero)
    | NotIn _ -> false

  let contains_zero = function
    | In s    -> Set.mem Z.zero s
    | NotIn s -> Set.mem Z.zero s |> not

  let contains_non_zero = function
    | In s    -> Set.remove Z.zero s |> Set.is_empty |> not
    | NotIn s -> true (* An infinite set always contains non-zero values. *)

  let remove_zero = function
    | In s    -> In    (Set.remove Z.zero s)
    | NotIn s -> NotIn (Set.add Z.zero s)

  (** [of_bool t f] is:
      - [∅]     if [t = false] and [f = false]
      - [{0}]   if [t = false] and [f = true]
      - [{1}]   if [t = true]  and [f = false]
      - [{0,1}] if [t = true]  and [f = true] *)
  let of_bool t f = match t,f with
    | false, false -> bottom
    | true, false  -> In (Set.singleton Z.one)
    | false, true  -> In (Set.singleton Z.zero)
    | true, true   -> In (Set.of_list [Z.zero; Z.one]) |> bound_size

  let to_itv (a:t) : int_itv =
    if is_top a    then Nb I.minf_inf else
    if is_bottom a then BOT           else
    match a with
    | In s    -> Nb (I.of_z (Set.min_elt s) (Set.max_elt s))
    | NotIn _ -> Nb I.minf_inf

  let map f = function
    | In s    -> In (Set.map f s)
    | NotIn s -> NotIn (Set.map f s)

  (** [combine combiner s1 s2] is [{combiner x1 x2 | x1 ∈ s1, x2 ∈ s2}]. *)
  let combine combiner (s1:Set.t) (s2:Set.t) =
      Set.fold
        (fun n1 acc ->
           Set.fold (fun n2 -> Set.add (combiner n1 n2)) s2 acc
        ) s1 Set.empty

  let combine_opt combiner (s1:Set.t) (s2:Set.t) =
    Set.fold
      (fun n1 acc ->
         Set.fold (fun n2 acc ->
             match combiner n1 n2 with
             | None -> acc
             | Some r -> Set.add r acc) s2 acc
      ) s1 Set.empty


  (** {2 Lattice operators} *)
  (** ********************* *)
  let subset (a1:t) (a2:t) : bool =
    match a1,a2 with
    | In s1, In s2       -> Set.subset s1 s2
    | NotIn s1, NotIn s2 -> Set.subset s2 s1
    | In s1, NotIn s2    -> Set.inter s1 s2 |> Set.is_empty
    (* [NotIn s1] is an infinite set, so that is cannot be smaller than a finite set. *)
    | NotIn s1, In s2    -> false

  let join (a1:t) (a2:t) : t =
    match a1,a2 with
    | In s1, In s2       -> In    (Set.union s1 s2)
    | NotIn s1, NotIn s2 -> NotIn (Set.inter s1 s2)
    (* From the excluded values, we add those that are possibly included. *)
    | In s1, NotIn s2    -> NotIn (Set.diff s2 s1)
    | NotIn s1, In s2    -> NotIn (Set.diff s1 s2)

  let meet (a1:t) (a2:t) : t =
    match a1,a2 with
    | In s1, In s2       -> In (Set.inter s1 s2)
    | NotIn s1, NotIn s2 -> NotIn (Set.union s1 s2)
    (* From the known values, we remove those that are excluded. *)
    | In s1, NotIn s2    -> In (Set.diff s1 s2)
    | NotIn s1, In s2    -> In (Set.diff s2 s1)

  let widen ctx (a1:t) (a2:t) : t =
    match a1,a2 with
    | NotIn _, _ | _, NotIn _ ->
      (* If any of the two is an exclusion set, the join will result in an
         exclusion set. Once we have an exclusion set, there are no infinite
         ascending chains, as the joins will result in intersections of finite
         sets. *)
      join a1 a2
    | In s1, In s2 ->
      (* If the size is under the maximum number of finite elements in the
         union, we keep it as a set of constants. Otherwise, we go to top. This
         might be improved by selecting a finite set of elements that are not in
         the union, and returning the excluded set of those elements. *)
      if Set.cardinal s1 + Set.cardinal s2 <= !opt_max_intset
      then In (Set.union s1 s2)
      else top

  (** {2 Forward semantics} *)
  (** ********************* *)

  let constant (c: constant) (t: typ) : t =
    match c with
    | C_bool true                           -> In (Set.singleton Z.one)
    | C_bool false                          -> In (Set.singleton Z.zero)
    | C_top T_bool                          -> In (Set.of_list [Z.zero; Z.one]) |> bound_size
    | C_top T_int                           -> top
    | C_int n                               -> In (Set.singleton n)
    | C_int_interval (Finite i1, Finite i2) -> of_bounds i1 i2
    | _                                     -> top

  let unop (op: operator) (t: typ) (a: t) (type_return: typ) =
    match op with
    | O_plus    -> a
    | O_minus   -> map Z.neg a
    | O_log_not -> of_bool (contains_zero a) (contains_non_zero a)
    | _         -> top

  (** [combine_with combiner a1 a2] combines the elements of [a1] and [a2] with
      [combiner]. If [a1] and [a2] are finite sets, it applies [combiner] to the
      cartesian product of the two. If [a1] and [a2] are both exclued powersets,
      returns [top]. If one of the two is a finite set of size exactly one (and
      therefore, it is a definite value), returns an excluded set where the
      constant is combined with the excluded powerset. Otherwise, returns [top].
    *)
  let combine_with combine combiner a1 a2 =
    match a1,a2 with
    | In s1, In s2       -> In (combine combiner s1 s2) |> bound_size
    | NotIn s1, NotIn s2 -> top
    | NotIn notin_set, In in_set ->
      begin match Set.cardinal in_set with
      | 0 -> bottom
      | 1 -> NotIn (combine combiner notin_set in_set)
      | _ -> top
      end
    | In in_set, NotIn notin_set ->
      begin match Set.cardinal in_set with
      | 0 -> bottom
      | 1 -> NotIn (combine combiner in_set notin_set)
      | _ -> top
      end

  let binop (op:operator) (t1:typ) (a1:t) (t2:typ) (a2:t) (type_return:typ) =
    if is_bottom a1 || is_bottom a2 then bottom else
    if is_top a1    || is_top a2    then top    else
    let with_int f a b = f a (Z.to_int b) in
    try match op with
    | O_plus       -> combine_with combine Z.add a1 a2

    | O_minus      -> combine_with combine Z.sub a1 a2

    | O_mult       -> if is_zero a1 || is_zero a2 then zero else combine_with combine Z.mul a1 a2

    | O_div        ->
      let () = debug "%a O_div %a@." (format print) a1 (format print) a2 in
      if is_zero a1 then a1 else
      (* We remove 0 from both the in and the notin to prevent division by zero. *)
      let a2 = match a2 with
      | In s2    -> In    (Set.remove Z.zero s2)
      | NotIn s2 -> NotIn (Set.remove Z.zero s2)
      in
      combine_with combine_opt (fun x y ->
          if Z.(x mod y = zero) then Some (Z.div x y)
          else None) a1 a2

    | O_pow        ->
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine (with_int Z.pow) a1 a2
      | _            -> top
      end

    | O_bit_and    ->
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine Z.logand a1 a2
      | _            -> top
      end

    | O_bit_or     ->
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine Z.logor a1 a2
      | _            -> top
      end

    | O_bit_xor    ->
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine Z.logxor a1 a2
      | _            -> top
      end

    | O_bit_lshift ->
      if is_zero a1 then a1 else
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine (with_int Z.shift_left) a1 a2
      | _            -> top
      end

    | O_bit_rshift ->
      if is_zero a1 then a1 else
      begin match a1,a2 with
      | In s1, In s2 -> combine_with combine (with_int Z.shift_right) a1 a2
      | _            -> top
      end

    | O_mod        ->
      begin match a1,a2 with
      | In s1, In s2 -> if is_zero a1 then a1 else combine_with combine Z.rem a1 (remove_zero a2)
      | _            -> top
      end

    | O_log_or     ->
      of_bool
        (contains_non_zero a1 || contains_non_zero a2)
        (contains_zero a1 && contains_zero a2)

    | O_log_and    ->
      of_bool
        (contains_non_zero a1 && contains_non_zero a2)
        (contains_zero a1 || contains_zero a2)

    | O_log_xor    ->
      of_bool
        ((contains_non_zero a1 && contains_zero a2) ||
         (contains_zero a1     && contains_non_zero a2))
        ((contains_zero a1     && contains_zero a2) ||
         (contains_non_zero a1 && contains_non_zero a2))

    | _ -> top
    with Z.Overflow -> top

  include DefaultValueFunctions

  let avalue : type r. r avalue_kind -> t -> r option =
    fun aval a ->
    match aval with
    | Common.V_int_interval       -> Some (to_itv a)
    | Common.V_int_interval_fast  -> Some (to_itv a)
    | Common.V_int_congr_interval -> Some (to_itv a, Bot.Nb Common.C.minf_inf)
    | _                           -> None

  let rec compare (op:operator) (b:bool) (t1:typ) (a1:t) (t2:typ) (a2:t) : (t*t) =
    if is_bottom a1 || is_bottom a2 then (bottom, bottom) else
    let op = if b then op else negate_comparison_op op in
    match op with
    | O_eq ->
      begin match a1,a2 with
      | In s1, In s2               -> let a = In (Set.inter s1 s2) in a,a
      | NotIn s1, NotIn s2         -> let a = NotIn (Set.union s1 s2) in a,a
      | In in_set, NotIn notin_set -> In (Set.diff in_set notin_set), a2
      | NotIn notin_set, In in_set -> a1, In (Set.diff in_set notin_set)
      end

    | O_ne ->
      begin match a1,a2 with
      | In s1, In s2 ->
        let s1 = if Set.cardinal s2 = 1 then Set.diff s1 s2 else s1 in
        let s2 = if Set.cardinal s1 = 1 then Set.diff s2 s1 else s2 in
        In s1, In s2

      | NotIn notin_set, In in_set ->
        if Set.cardinal in_set = 1 then
          (* [x=∉{x1,x2} != y=∈{y1} ⟹ x=∉{x1,x2,y1}]*)
          let notin_set' = Set.union notin_set in_set in
          let in_set' = in_set in
          NotIn notin_set', In in_set'
        else
          a1,a2

      | In in_set, NotIn notin_set ->
        if Set.cardinal in_set = 1 then
          let notin_set' = Set.union notin_set in_set in
          let in_set' = in_set in
          In in_set', NotIn notin_set'
        else
          a1,a2

      | NotIn _, NotIn _ -> a1, a2
      end

    | O_le ->
      begin match a1,a2 with
      | In s1, In s2 ->
        let min_s1 = Set.min_elt s1 in
        let max_s2 = Set.max_elt s2 in
        In (Set.filter (fun n -> Z.leq n max_s2) s1),
        In (Set.filter (fun n -> Z.geq n min_s1) s2)
      | In s1, NotIn s2 ->
        (* To the excluded set we can add the successor of the largest set on
           the left: [x=∈{1,3} <= y=∉{2} ⟹ y=∉{0,2}], as [y] cannot have value
           [0]. This is a simple precision improvement. *)
        a1, NotIn (Set.add (Set.min_elt s1 |> Z.pred) s2)
      | NotIn s1, In s2 ->
        NotIn (Set.add (Set.max_elt s2 |> Z.succ) s1), a2
      | NotIn _, NotIn _ -> a1, a2
      end

    | O_ge ->
      begin match a1,a2 with
      | In s1, In s2 ->
        let max_s1 = Set.max_elt s1 in
        let min_s2 = Set.min_elt s2 in
        In (Set.filter (fun n -> Z.geq n min_s2) s1),
        In (Set.filter (fun n -> Z.leq n max_s1) s2)
      | In s1, NotIn s2 ->
        a1, NotIn (Set.add (Set.max_elt s1 |> Z.succ) s2)
      | NotIn s1, In s2 ->
        NotIn (Set.add (Set.min_elt s2 |> Z.pred) s1), a2
      | NotIn _, NotIn _ -> a1, a2
      end

    | O_lt ->
      begin match a1,a2 with
      | In s1, In s2 ->
        let min_s1 = Set.min_elt s1 in
        let max_s2 = Set.max_elt s2 in
        In (Set.filter (fun n -> Z.lt n max_s2) s1),
        In (Set.filter (fun n -> Z.gt n min_s1) s2)
      | In s1, NotIn s2 ->
        a1, NotIn (Set.add (Set.min_elt s1) s2)
      | NotIn s1, In s2 ->
        NotIn (Set.add (Set.max_elt s2) s1), a2
      | NotIn _, NotIn _ -> a1, a2
      end

    | O_gt ->
      begin match a1,a2 with
      | In s1, In s2 ->
        let max_s1 = Set.max_elt s1 in
        let min_s2 = Set.min_elt s2 in
        In (Set.filter (fun n -> Z.gt n min_s2) s1),
        In (Set.filter (fun n -> Z.lt n max_s1) s2)
      | In s1, NotIn s2 ->
        a1, NotIn (Set.add (Set.max_elt s1) s2)
      | NotIn s1, In s2 ->
        NotIn (Set.add (Set.min_elt s2) s1), a2
      | NotIn _, NotIn _ -> a1, a2
      end

    | _ -> default_compare op b t1 a1 t2 a2

end

let () =
  register_simplified_value_abstraction (module SimplifiedValue)
