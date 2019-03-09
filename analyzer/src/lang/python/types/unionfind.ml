module type ELS = sig
  type t
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

module Unionfind(Els: ELS) =
struct
  type el = Els.t

  type node = {rank : int; elem: el}

  module MapN = MapExt.Make(Els) (* todo change to nodes? *)

  type t = node MapN.t (* maps each element to its parent *)

  let rec find (d : t) (e : el) =
    (* todo: path compression *)
    let r = MapN.find e d in
    if Els.compare r.elem e = 0 then (d, e)
    else find d r.elem

  let is_equiv (d: t) (e1: el) (e2: el) : bool =
    Els.compare (snd @@ find d e1) (snd @@ find d e2) == 0

  let to_llist (d: t) : el list list =
    let classes =
      MapN.fold (fun k _ acc ->
          let parent = snd @@ find d k in
          let old = if MapN.mem parent acc then MapN.find parent acc else [] in
          MapN.add parent (k::old) acc) d MapN.empty in
    MapN.fold (fun _ cl acc -> cl::acc) classes []

  let print_class (fmt: Format.formatter) (l: el list) : unit =
    Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Els.print) l

  let print_llist fmt llist =
    Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") print_class) llist

  let map_printer = MapExtSig.{ print_empty = "∅";
                                print_begin = "{";
                                print_arrow = ":";
                                print_sep = ";";
                                print_end = "}"; }

  let print fmt d =
    let llist = to_llist d in
    (* Format.fprintf fmt "\tgood_printer %a@\n\tbad_printer %a@\n@\n" print_llist llist
     *   (MapN.fprint map_printer (Els.print) (fun fmt x -> Els.print fmt x.elem)) d *)
    print_llist fmt llist


  let init (domain: el list) : t =
    List.fold_left (fun acc (x:el) ->
        let n = {rank = 0; elem = x} in
        MapN.add x n acc) MapN.empty domain

  let rec union (d: t) (e1 : el) (e2: el) : t =
    (* this is should force the parent of any set to be the smallest wrt Els.compare *)
    let parent1 = snd @@ find d e1 in
    let parent2 = snd @@ find d e2 in
    let res = if Pervasives.compare parent1 parent2 <= 0 then
        MapN.add parent2 {rank=0; elem=parent1} d
      else
        MapN.add parent1 {rank=0; elem=parent2} d
    in
    (* Format.printf "union %a %a %a = %a@\n" print d Els.print e1 Els.print e2 print res; *)
    res

  let keys (d: t) : el list =
    MapN.fold (fun k _ acc -> k :: acc) d []

  let compress (d: t) : t =
    let k = keys d in
    List.fold_left (fun map el -> fst @@ find map el) d k

  let intersection (d1: t) (d2: t) : t =
    let d1 = compress d1 in
    let d2 = compress d2 in
    MapN.fold2 (fun key parent1 parent2 acc ->
        (* remove these lets when find works with path compression *)
        let parent1 = snd @@ find d1 key in
        let parent2 = snd @@ find d2 key in
        if Pervasives.compare parent1 parent2 = 0 then
          MapN.add key {elem=parent1; rank=0} acc
        else
          (* I think we should be more precise here:
             {x, y} {z,t,v,u} \cap {x, y} {z,t} {v, u} will result in {x, y}, {z, t} {v} {u} rather than ... {v, u} *)
          MapN.add key {rank=0; elem=key} acc) d1 d2 MapN.empty

  let from_llist (domain: el list) (equivs: el list list) : t =
    (* Format.printf "from_llist %a\n" print_llist equivs; *)
    List.fold_left (fun uf eqclass ->
        if List.length eqclass = 1 then uf else
          let parent = List.hd eqclass in
          List.fold_left (fun uf el -> union uf parent el) uf (List.tl eqclass)
      ) (init domain) equivs
end
