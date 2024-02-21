open Instances

module ElinaOct = Domain.Make(struct
    type t = Elina_oct.t
    let name = "universal.numeric.relational"
    let numeric_name = "elina_oct"
    let man = Elina_oct.manager_alloc ()
  end)

let () = register_instance (module ElinaOct : RELATIONAL)


module ElinaPoly = Domain.Make(struct
    type t = Elina_poly.loose Elina_poly.t
    let name = "universal.numeric.relational"
    let numeric_name = "elina_poly"
    let man = Elina_poly.manager_alloc_loose ()
  end)

let () = register_instance (module ElinaPoly : RELATIONAL)

