(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Math Python library. *)


open Mopsa
open Universal.Ast
open Ast
open Addr


module Domain =
  struct

    type _ domain += D_python_libs_math : unit domain

    let id = D_python_libs_math
    let name = "python.libs.math"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_libs_math -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = { export = []; import = [] }
    let eval_interface = { export = [Zone.Z_py, Zone.Z_py_obj]; import = [] }

    let init _ _ flow = Some flow

    let exec _ _ _ _ = None

    let eval zones exp man flow = None
      (* let range = erange exp in
       * match ekind exp with
       * | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "math.sqrt")}, _)}, [e], []) ->
       *    Exceptions.panic "blaaaaa@\n";
       *    let exp' = mk_unop O_sqrt e ~etyp:(T_float F_DOUBLE) range in
       *    Eval.singleton exp' flow
       *    |> OptionExt.return
       *
       * | _ ->
       *    None *)


    let ask _ _ _ = None

  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
