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

(** Math Python library. *)
(** Currently based on types. If this is moved to values at some
   point, it should be kept for the type analysis I think *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open MapExt
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.libs.math"
      end)

    let interface = {
      iexec = { provides = []; uses = [] };
      ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [] }
    }

    type stub_signature = {in_args: string list;
                           out_type: Mopsa.typ}
    type stub_db = stub_signature StringMap.t

    let add_signature funname in_args out_type db =
      let out_type = match out_type with
        | "bool" -> T_bool
        | "int" -> T_int
        | "float" -> T_float F_DOUBLE
        | "str" -> T_string
        | "NoneType" -> T_py_none
        | "NotImplementedType" -> T_py_not_implemented
        | _ -> assert false in
      StringMap.add funname {in_args; out_type} db

    let stub_base =
      StringMap.empty |>
      (* FIXME: I guess math.ceil also works from int to int... and that kind of stuff *)
      add_signature "math.ceil"      ["float"]           "int"   |>
      add_signature "math.copysign"  ["float"; "float"]  "float" |>
      add_signature "math.erf"       ["float"]           "float" |>
      add_signature "math.erfc"      ["float"]           "float" |>
      add_signature "math.exp"       ["float"]           "float" |>
      add_signature "math.expm1"     ["float"]           "float" |>
      add_signature "math.fabs"      ["float"]           "float" |>
      add_signature "math.factorial" ["int"]             "int"   |>
      add_signature "math.floor"     ["float"]           "int"   |>
      add_signature "math.fmod"      ["float"; "float"]  "float" |>
      add_signature "math.gamma"     ["float"]           "float" |>
      add_signature "math.gcd"       ["int"; "int"]      "int"   |>
      add_signature "math.hypot"     ["float"; "float"]  "float" |>
      add_signature "math.isclose"   ["float"; "float"]  "bool"  |>
      add_signature "math.isfinite"  ["float"]           "bool"  |>
      add_signature "math.isinf"     ["float"]           "bool"  |>
      add_signature "math.isnan"     ["float"]           "bool"  |>
      add_signature "math.ldexp"     ["float"; "int"]    "float" |>
      add_signature "math.lgamma"    ["float"]           "float" |>
      add_signature "math.log"       ["float"; "float"]  "float" |>
      add_signature "math.log1p"     ["float"]           "float" |>
      add_signature "math.log10"     ["float"]           "float" |>
      add_signature "math.log2"      ["float"]           "float" |>
      add_signature "math.sqrt"      ["float"]           "float"

    let process_simple f man flow range exprs instances return =
      Utils.check_instances f man flow range exprs instances (fun _ flow -> man.eval (mk_py_top return range) flow)

    let init prog man flow =
      flow

    let exec _ _ _ _ = None

    let eval zones exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, []) when StringMap.mem f stub_base ->
        debug "function %s in stub_base, processing@\n" f;
        let {in_args; out_type} = StringMap.find f stub_base in
        process_simple f man flow range args in_args out_type
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("math.frexp" as f))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["float"]
          (fun args flow ->
             man.eval (mk_expr (E_py_tuple
                                  [mk_py_top (T_float F_DOUBLE) range; mk_py_top T_int range]
                               ) range)  flow
          )
        |> Option.return

      (* math.fsum is handled in the list abstraction *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("math.modf" as f))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["float"]
          (fun args flow ->
             man.eval (mk_expr (E_py_tuple
                                  [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]
                               ) range)  flow
          )
        |> Option.return



      | _ -> None

    let ask _ _ _ = None

  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
