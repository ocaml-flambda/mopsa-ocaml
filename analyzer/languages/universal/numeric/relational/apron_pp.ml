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

(**  Pretty-printer of Apron environments *)

open Mopsa
open Apron

let coeff_eq_1 (c: Coeff.t) = match c with
  | Coeff.Scalar s when Scalar.cmp_int s 1 = 0 -> true
  | Coeff.Interval i when Scalar.cmp_int i.Interval.inf 1 = 0 && Scalar.cmp_int i.Interval.sup 1 = 0 -> true
  | _ -> false

let coeff_eq_0 (c: Coeff.t) = match c with
  | Coeff.Scalar s -> Scalar.cmp_int s 0 = 0
  | Coeff.Interval i ->
    Scalar.cmp_int i.Interval.inf 0 = 0
    && Scalar.cmp_int i.Interval.sup 0 = 0

let coeff_cmp_0 (c: Coeff.t) = match c with
  | Coeff.Scalar s -> Some (Scalar.cmp_int s 0)
  | Coeff.Interval i ->
    if Scalar.cmp_int i.Interval.inf 0 > 0 then Some 1
    else if Scalar.cmp_int i.Interval.sup 0 < 0 then Some (-1)
    else None

let linexpr_to_list_pair env (x: Linexpr1.t) =
  let envi, envf = Environment.vars env in
  let process pos neg env =
  Array.fold_left (fun (pos, neg) var ->
      let c = Linexpr1.get_coeff x var in
      if coeff_eq_0 c then
        (pos, neg)
      else match coeff_cmp_0 c with
        | None -> (c, var) :: pos, neg
        | Some x when x > 0 -> (c, var) :: pos, neg
        | Some x -> pos, (c, var)::neg
      ) (pos, neg) env in
  let pos, neg = process [] [] envi in
  process pos neg envf

let pp_coef_var_list bnd fmt l =
  match l with
  | [] -> Format.fprintf fmt "0"
  | _ -> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " + ")
           (fun fmt (c, v) ->
              if coeff_eq_1 c then
                Format.fprintf fmt "%a" pp_var (Binding.apron_to_mopsa_var v bnd)
              else
                Format.fprintf fmt "%a%a" Coeff.print c pp_var (Binding.apron_to_mopsa_var v bnd)
           ) fmt l

let pp_typ fmt (x, b) = match x, b with
  | Lincons1.DISEQ, _ -> Format.fprintf fmt "!="
  | Lincons1.EQ, _ -> Format.fprintf fmt "="
  | Lincons1.SUP, false -> Format.fprintf fmt ">"
  | Lincons1.SUP, true -> Format.fprintf fmt "<"
  | Lincons1.SUPEQ, false -> Format.fprintf fmt "≥"
  | Lincons1.SUPEQ, true -> Format.fprintf fmt "≤"
  | Lincons1.EQMOD _, _ -> assert false

let neg_list l =
  List.map (fun (c, v) -> Coeff.neg c, v) l

let pp_lincons bnd fmt lc =
  let cst = Lincons1.get_cst lc in
  let typ = Lincons1.get_typ lc in
  let pos, neg = linexpr_to_list_pair (Lincons1.get_env lc) (Lincons1.get_linexpr1 lc) in
  if coeff_eq_0 (cst) then
    Format.fprintf fmt "%a %a %a" (pp_coef_var_list bnd) pos pp_typ (typ, false) (pp_coef_var_list bnd) (neg_list neg)
  else
    match coeff_cmp_0 (cst) with
    | Some x when x > 0 ->
      if pos = [] then
        Format.fprintf fmt "%a %a %a" (pp_coef_var_list bnd) (neg_list neg) pp_typ (typ, true) Coeff.print cst
      else if neg = [] then
        Format.fprintf fmt "%a %a %a" (pp_coef_var_list bnd) pos pp_typ (typ, false) Coeff.print (Coeff.neg cst)
      else
        Format.fprintf fmt "%a %a %a + %a" (pp_coef_var_list bnd) (neg_list neg) pp_typ (typ, true) (pp_coef_var_list bnd) pos Coeff.print cst
    | _ ->
      if neg = [] then
        Format.fprintf fmt "%a %a %a" (pp_coef_var_list bnd) pos pp_typ (typ, false) Coeff.print (Coeff.neg cst)
      else if pos = [] then
        Format.fprintf fmt "%a %a %a" (pp_coef_var_list bnd) (neg_list neg) pp_typ (typ, true) Coeff.print (cst)
      else
        Format.fprintf fmt "%a %a %a + %a" (pp_coef_var_list bnd) pos pp_typ (typ, false) (pp_coef_var_list bnd) (neg_list neg) Coeff.print (Coeff.neg cst)

let pp_lincons_earray bnd pr ea =
  let rec read n =
    if n < 0 then []
    else
      let x = Lincons1.array_get ea n in
      x :: (read (n-1))
  in
  let l = read (Lincons1.array_length ea -1) in
  match l with
  | [] -> pp_string pr "⊤"
  | _  ->
    let sl = List.map (fun c ->
        Format.asprintf "%a" (pp_lincons bnd) c) l in
    pp_set pp_string pr (SetExtPoly.of_list String.compare sl)

let pp_env man pr (x,bnd) =
  if Abstract1.is_bottom man x then
    pp_string pr "⊥"
  else
    try 
      let ea = Abstract1.to_lincons_array man x in
      pp_lincons_earray bnd pr ea
    with (Failure s) when String.starts_with ~prefix:"Linexpr1.get_coeff" s ->
      let () = warn "%s~>%s" s "defaulting to standard printing" in
      pp_string pr (Format.asprintf "%a" Abstract1.print x)
