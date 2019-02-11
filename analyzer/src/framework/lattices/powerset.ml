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

  let bottom : t = Nt Set.empty

  let top : t = TOP

  let is_top (abs: t) = top_dfl1 true (fun x -> false) abs

  let subset (abs1:t) (abs2:t) : bool =
    top_included Set.subset abs1 abs2

  let equal (abs1:t) (abs2:t) : bool =
    top_equal Set.equal abs1 abs2

  let join annot (abs1:t) (abs2:t) : t =
    top_lift2 Set.union abs1 abs2

  let meet annot (abs1:t) (abs2:t) : t =
    top_neutral2 Set.inter abs1 abs2

  let union = join

  let inter = meet

  let diff (abs1:t) (abs2:t) : t =
    top_neutral2 Set.diff abs1 abs2

  let widen annot (abs1:t) (abs2:t) : t =
    top_absorb2
      (fun s1 s2 ->
         if Set.subset s2 s1 then
           abs2
         else
           TOP
      )
      abs1 abs2

  open Format
  let print fmt (abs:t) =
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

  let add v (abs:t) : t =
    top_lift1 (Set.add v) abs

  let fold (f:Elt.t->'a->'a) (abs:t) (init:'a) : 'a =
    top_to_exn abs |> (fun s -> Set.fold f s init)

  let remove (v:Elt.t) (abs:t) : t =
    top_lift1 (Set.remove v) abs

  let mem (v:Elt.t) (abs:t) : bool =
    top_dfl1 true (Set.mem v) abs

  let filter f (abs:t) : t =
    top_lift1 (Set.filter f) abs

  let exists f (abs:t) : bool =
    top_to_exn abs |> (fun s -> Set.exists f s)

  let for_all f (abs:t) : bool =
    top_to_exn abs |> (fun s -> Set.for_all f s)

  let cardinal (abs:t) : int =
    top_to_exn abs |> (fun s -> Set.cardinal s)

  let find f (abs:t) : Elt.t =
    top_to_exn abs |> (fun s -> Set.find f s)

  let choose (abs:t) : Elt.t =
    top_to_exn abs |> (fun s -> Set.choose s)

  let singleton (x:Elt.t) : t = Nt (Set.singleton x)

  let of_list (l:Elt.t list) =
    Nt (Set.of_list l)

  let is_empty (abs:t) =
    top_dfl1 false Set.is_empty abs

  let empty = bottom

  let is_bottom = is_empty

  let elements (abs:t) =
    top_to_exn abs |> Set.elements

  let map (f:Elt.t -> Elt.t)  (abs:t) : t =
    top_lift1 (Set.map f) abs

  let iter (f:Elt.t -> unit) (abs:t) : unit =
    top_to_exn abs |> Set.iter f

  let apply (f:Set.t -> 'a) (dfl:'a) (abs:t) : 'a = Top.top_apply f dfl abs

end






(** Powerset with lower and upper approximations *)

module MakeWithUnder(Elt: ELT) =
struct

  module Set = Set.Make(Elt)
  module USet = Make(Elt)

  (* Lower approximation *)
  type l = Set.t

  (* Upper approximation *)
  type u = USet.t

  (* Powerset with lower and upper approximation *)
  type t = l * u

  let empty : t = (Set.empty, USet.empty)

  let bottom : t = empty

  let top : t = (Set.empty, USet.top)

  let is_empty ((l,u):t) : bool =
    Set.is_empty l &&
    USet.is_empty u

  let is_bottom = is_empty

  let is_top ((l,u): t) =
    Set.is_empty l &&
    USet.is_top u

  let subset ((l1,u1): t) ((l2,u2): t) : bool =
    Set.subset l1 l2 &&
    USet.subset u1 u2

  let equal ((l1,u1): t) ((l2,u2): t) : bool =
    Set.equal l1 l2 &&
    USet.equal u1 u2

  let join annot ((l1,u1): t) ((l2,u2): t) : t =
    Set.inter l1 l2,
    USet.join annot u1 u2

  let meet annot ((l1,u1): t) ((l2,u2): t) : t =
    Set.union l1 l2,
    USet.meet annot u1 u2

  let union = join

  let inter = meet

  let widen annot ((l1,u1): t) ((l2,u2): t) : t =
    Set.inter l1 l2,
    USet.widen annot u1 u2

  open Format
  let print fmt ((l,u):t) =
    let l = Set.elements l in
    fprintf fmt "@[<h>{";
    if l = [] then fprintf fmt "∅"
    else
      fprintf fmt "@[<h>{%a}@]"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
           Elt.print
        ) l
    ;
    fprintf fmt ", %a}@]"
      USet.print u

end
