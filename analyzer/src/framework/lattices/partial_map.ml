(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Map abstraction assuming ⊥ values for non-existing keys. *)

open Top
open Lattice

let debug fmt = Debug.debug ~channel:"framework.lattices.partial_map" fmt

module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make
    (Key : KEY)
    (Value: Lattice.LATTICE)
=
struct
  module Map = MapExt.Make(Key)

  type v = Value.t Map.t
  (** Maps from variable to non-⊥ non-relational values. *)

  type t = v with_top

  (** Possibly ⊤ abstract elements. *)

  (** {2 Framework.Domain.DOMAIN functions} *)

  let bottom = Nt Map.empty

  let top = TOP

  let is_bottom  abs =
    top_dfl1 false (fun m ->
        Map.for_all (fun _ v -> Value.is_bottom v) m
      ) abs

  let is_top abs =
    top_dfl1 true (fun _ -> false) abs

  let empty = bottom

  let init = empty
  let leq  (a1:t) (a2:t) : bool =
    top_included
      (Map.for_all2zo
         (fun _ v1 -> Value.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
         (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
         (fun _ v1 v2 -> Value.leq v1 v2)
      )
      a1 a2
  (** Inclusion testing. Missing variables in one map are assimilated to ⊥. *)

  let unify op a1 a2 = assert false

  let join  (a1:t) (a2:t) : t =
    top_lift2
      (Map.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> Value.join v1 v2)
      )
      a1 a2
  (** Join. Missing variables in one map are assimilated to ⊥. *)

  let widening ctx (a1:t) (a2:t) : t =
    top_lift2
      (Map.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> Value.widening ctx v1 v2)
      )
      a1 a2
  (** Widening (naive). *)
  let meet  (a1:t) (a2:t) : t =
    top_neutral2
      (fun b1 b2 ->
          (Map.map2zo
             (fun _ v1 -> Value.bottom)
             (fun _ v2 -> Value.bottom)
             (fun _ v1 v2 -> Value.meet  v1 v2)
             b1) b2
      )
      a1 a2
  (** Meet. Missing variables in one map are assimilated to ⊤. *)

  let print  fmt (a:t) =
    let open Format in
    top_fprint (fun fmt m ->
        if Map.is_empty m then
          pp_print_string fmt "⊥ "
        else
          fprintf fmt "@[<v>%a@]"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
               (fun fmt (k, v) ->
                  fprintf fmt "%a ⇀ @[ %a@]" Key.print k Value.print v
               )
            ) (Map.bindings m)
      ) fmt a
  (** Printing. *)
  let find  (k: Key.t) (a: t) =
    try begin
      let m = top_to_exn a in
      try Map.find k m with Not_found -> Value.bottom
    end
    with
      Found_TOP -> Value.top

  let remove  (k: Key.t) (a: t) : t =
    top_lift1 (Map.remove k) a

  let add  (k: Key.t) (v: Value.t) (a: t) =
    top_lift1 (Map.add k v) a

  let singleton  k v =
    add k v empty

  let filter (f : Key.t -> Value.t -> bool) (d : t) =
    top_lift1 (Map.filter f) d

  let fold f a x =
    top_to_exn a |> (fun m -> Map.fold f m x)

  let fold_d f (a : 'a Map.t with_top) (d : 'b) (x : 'b) : 'b = match a with
    | Top.TOP -> d
    | Top.Nt m -> Map.fold f m x

  let mem x a =
    top_to_exn a |> (fun m -> Map.mem x m)

  let map f a =
    top_lift1 (fun m -> Map.map f m) a

  let map_p f a = match a with
    | TOP -> TOP
    | Nt m ->
      Nt (Map.fold (fun k v acc ->
          let k',v' = f (k,v) in Map.add k' v' acc
        ) m Map.empty)

  let bindings a =
    top_to_exn a |> Map.bindings

  let for_all f a =
    top_to_exn a |> (Map.for_all f)

  let exists f a =
    top_to_exn a |> (Map.exists f)

end
