open Mopsa
open Sig.Abstraction.Value

module Sign : VALUE =
struct
  (* Definition of the sign domain *)
end

let () =
  register_value_abstraction (module Sign)
