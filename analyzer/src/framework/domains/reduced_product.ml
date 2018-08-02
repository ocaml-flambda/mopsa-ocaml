(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of abstract domains. *)

open Manager

(** A pool of abstract domains *)
module Pool =
struct

  (** A pool is encoded as GADT tuples *)
  type 'a t =
  | [] : unit t
  | (::) : 'a Domain.info * 'b t -> ('a * 'b) t

end

(** Signature for reduction rules *)
module type REDUCTION =
sig
end


(** Functor module to create a reduced product abstract domain given a
   pool of abstract domains and a reduction operator *)
module Make(Config:
            sig
              type t
              val pool : t Pool.t
              val reductions : (module REDUCTION) list
            end) =
struct

end
