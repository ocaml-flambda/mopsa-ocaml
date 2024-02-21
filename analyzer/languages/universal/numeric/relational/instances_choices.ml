open Instances
open Sig.Abstraction.Simplified
open Mopsa


(* the next two open are here to force compilation of the instances before the registration of domain options, in order to be sure all domains are registered *)
open Elinainst
open Ppliteinst


let numeric_domain : (module RELATIONAL) ref = ref (module Polyhedra : RELATIONAL)

let () =
  register_domain_option "universal.numeric.relational" {
    key = "-numeric";
    category = "Numeric";
    doc = " select the relational numeric domain.";
    spec = ArgExt.Symbol (
        get_instances_names (),
        (fun name ->
           let (module M : RELATIONAL) = List.find
               (fun (module C : RELATIONAL) -> C.numeric_name = name)
               !numeric_domains in
           opt_numeric := M.numeric_name;
           numeric_domain := (module M : RELATIONAL);
           register_simplified_domain (module M)
        )
      );
    default = "polyhedra"
  }

let () =
  register_simplified_domain (module Polyhedra)
