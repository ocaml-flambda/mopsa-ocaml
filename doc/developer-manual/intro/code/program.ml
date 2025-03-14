open Mopsa
open Sig.Abstraction.Stateless
open Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "universal.iterators.program"
    end)

  let alarms = []

  let init prog man flow = flow

  let eval exp man flow = None

  let ask query man flow = None

  let exec stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = P_universal{universal_main} }, _)
      man.exec universal_main flow |>
      OptionExt.return

    | _ -> None

end

let () =
  register_stateless_domain (module Domain)
