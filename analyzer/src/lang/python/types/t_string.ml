open Framework.Essentials
open Ast
open Universal.Ast
open MapExt
(* au moins gérer les strings *)

module Domain =
  struct
    type _ domain += D_python_types_t_string : unit domain

    let id = D_python_types_t_string
    let name = "python.types.t_string"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_types_t_string -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Framework.Zone.Z_top]; import = []}

    let init _ _ _ = None

    let eval zs exp (man: ('a, unit) man) (flow:'a flow) : ('a, expr) evl option =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_constant (C_string _) ->
         Eval.singleton (mk_expr (Typing.E_get_type_partition (Typingdomain.builtin_inst "str")) range) flow |> Option.return
      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
