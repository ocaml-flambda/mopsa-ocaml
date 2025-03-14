(** Equalities numerical domain *)
(* type t = ℘(℘(𝒱))
   𝛾(S♯) = { ρ ∈ 𝒱 → 𝕍 ∣ ∀s ∈ S♯, ∃v ∈ 𝕍, ∀x ∈ s, 𝜌(x) = v}
*)

open Mopsa
open Sig.Abstraction.Simplified

module Equalities =
struct
  (* Definition of the equalities domain *)
end

let () =
  register_simplified_domain (module Equalities)
