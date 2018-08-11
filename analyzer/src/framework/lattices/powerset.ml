(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Powerset lattice with finite cardinality elements or ⊺. *)

open Top

module type ELT =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make(Elt: ELT) =
struct
  module Set = Set.Make(Elt)

  type v = Set.t

  type t = v with_top

  let bottom = Nt Set.empty

  let top = TOP

  let subset abs1 abs2 =
    top_included Set.subset abs1 abs2

  let join annot  abs1 abs2 =
    top_lift2 Set.union abs1 abs2

  let meet annot abs1 abs2 =
    top_neutral2 Set.inter abs1 abs2

  let union = join

  let inter = meet

  let diff abs1 abs2 =
    top_neutral2 Set.diff abs1 abs2

  let widen annot abs1 abs2 =
    top_absorb2
      (fun s1 s2 ->
         if Set.subset s2 s1 then
           abs2
         else
           TOP
      )
      abs1 abs2

  open Format
  let print fmt abs =
    let open Format in
    top_fprint (fun fmt s ->
        if Set.is_empty s then pp_print_string fmt "∅"
        else
          let l = Set.elements s in
          fprintf fmt "@[<h>{";
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
            Elt.print fmt l
          ;
          fprintf fmt "}@]";
          ()
      ) fmt abs

  let add v abs =
    top_lift1 (Set.add v) abs

  let fold f abs init =
    top_to_exn abs |> (fun s -> Set.fold f s init)

  let remove v abs =
    top_lift1 (Set.remove v) abs

  let mem v abs =
    top_dfl1 true (Set.mem v) abs

  let filter f abs =
    top_lift1 (Set.filter f) abs

  let exists f abs =
    top_to_exn abs |> (fun s -> Set.exists f s)

  let cardinal abs =
    top_to_exn abs |> (fun s -> Set.cardinal s)

  let find f abs =
    top_to_exn abs |> (fun s -> Set.find f s)

  let choose abs =
    top_to_exn abs |> (fun s -> Set.choose s)

  let singleton x = Nt (Set.singleton x)

  let of_list l =
    Nt (Set.of_list l)

  let is_empty abs =
    top_dfl1 false Set.is_empty abs

  let empty = bottom

  let is_bottom = is_empty

  let filter_dfl dfl f abs =
    top_dfl1 (Nt dfl) (fun s -> Nt (Set.filter f s)) abs

  let elements abs =
    top_to_exn abs |> Set.elements

  let map f abs =
    top_lift1 (Set.map f) abs

  let iter f abs =
    top_to_exn abs |> Set.iter f

end
