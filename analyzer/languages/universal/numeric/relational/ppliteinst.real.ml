open Instances
open Mopsa

module PplitePolyhedra = Domain.Make(struct
    type t = Pplite.loose Pplite.t
    let name = "universal.numeric.relational"
    let numeric_name = "pplite"
    let man = Pplite.manager_alloc_loose ()
    let () = Pplite.manager_set_kind man "F_Poly"
  end)

let () = register_instance (module PplitePolyhedra : RELATIONAL)
