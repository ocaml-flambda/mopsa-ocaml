(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Mopsa
open Ast
open Zone

module Domain(* : Framework.Domains.Stateless.S *) =
struct
  let name = "universal.stdlib.builtins"
  type _ domain += D_universal_stdlib_builtins : unit domain
  let id = D_universal_stdlib_builtins

  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_stdlib_builtins -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface =
    { export = [Z_any] ;
      import = []
    }
  let eval_interface =
    { export = [Z_any, Z_any];
      import = []
    }

  let exec (_: zone) (stmt: stmt) (man: ('a, unit) man) (flow: 'a flow) =
    match skind stmt with
    | S_assign(_, {ekind = E_call ({ekind = E_function (Builtin {name = "mopsa_assume"})}, [e])})
    | S_expression({ekind = E_call ({ekind = E_function (Builtin {name = "mopsa_assume"})}, [e])}) ->
      man.exec (mk_assume e (srange stmt)) flow |> Post.of_flow |> OptionExt.return
    | _ -> None

  let eval z expr man flow =
    None

  let init _ _ _ = None
  let ask _ _ _ = None
end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
