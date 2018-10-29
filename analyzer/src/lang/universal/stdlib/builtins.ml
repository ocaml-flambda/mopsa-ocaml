open Framework.Essentials
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
    { export = [Framework.Zone.Z_top] ;
      import = []
    }
  let eval_interface =
    { export = [Framework.Zone.Z_top, Framework.Zone.Z_top];
      import = []
    }

  let exec (_: Framework.Zone.zone) (stmt: Framework.Ast.stmt) (man: ('a, unit) man) (flow: 'a flow) =
    let () = debug "I was asked: %a" pp_stmt stmt in
    match skind stmt with
    | S_assign(_, {ekind = E_call ({ekind = E_function (Builtin {name = "mopsa_assume"})}, [e])}) ->
      man.exec (mk_assume e (srange stmt)) flow |> Post.of_flow |> Option.return
    | _ -> None

  let eval z expr man flow =
    None

  let init _ _ _ = None
  let ask _ _ _ = None
end

let () =
    Framework.Domains.Stateless.register_domain (module Domain)
