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
  | (::) : (module Domain.DOMAIN with type t = 'a) * 'b t -> ('a * 'b) t

end

(** Signature for reduction rules *)
module type REDUCTION =
sig
end
