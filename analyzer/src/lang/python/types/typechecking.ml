open Mopsa
open Typing
open Addr_env

module Domain =
struct

  module Product = Framework.Domains.Iter.Make(Addr_env.Domain)(Typing.Domain)
  include Product

  let name = "python.types.typechecking"
  let debug fmt = Debug.debug ~channel:name fmt


end

let () =
  register_domain (module Domain)
