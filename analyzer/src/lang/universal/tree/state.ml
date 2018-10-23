type t = int

let cur = ref 0

let fresh () =
  let rep = !cur in
  let () = incr cur in
  rep

let restart () = cur := 0
let print = Format.pp_print_int

let compare a b = a - b
