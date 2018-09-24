open Framework.Essentials
open Ast
open Universal.Ast
open MapExt
(* gérer les appels sur int + constantes *)

module Domain =
  struct
    type _ domain += D_python_types_t_int : unit domain

    let id = D_python_types_t_int
    let name = "python.types.t_int"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_types_t_int -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, any_zone]; import = []}

    let init _ _ _ = None

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_constant (C_int _) ->
         Eval.singleton (mk_expr (Typing.E_get_type_partition (Typingdomain.builtin_inst "int")) range) flow |> Option.return
      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
