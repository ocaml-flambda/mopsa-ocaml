module StrSigmaAlgebra =
struct
  type t = string
  let compare = compare
  let print = Format.pp_print_string
  let witness = "[]"
end

module State =
struct
  type t = int

  let cur = ref 0

  let fresh () =
    let rep = !cur in
    let () = incr cur in
    rep

  let restart () = cur := 0
  let print = Format.pp_print_int

  let compare = (-)
end
