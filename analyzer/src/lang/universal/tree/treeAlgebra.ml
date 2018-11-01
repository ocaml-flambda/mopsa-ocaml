module Make(A: SIG.COMPARABLE) =
struct

  type u = A.t * int

  module SA = Set.Make(
    struct
      type t = u
      let compare = ToolBox.pair_compare A.compare (-)
    end
    )

  type t = SA.t

  let compare =
    SA.compare

  let print =
    ToolBox.print_set_inline
      (ToolBox.print_pair A.print Format.pp_print_int)
      SA.elements

  let mem_symbol s sa =
    SA.exists (fun (s', _) -> A.compare s s' = 0) sa

  let max_arity sa =
    SA.fold (fun (a, i) acc -> if i > acc then i else acc) sa (-1)

  let fold f sa acc =
    SA.fold f sa acc

  let union =
    SA.union

  let add =
    SA.add

  let empty =
    SA.empty

  let iter =
    SA.iter

  let of_list =
    SA.of_list

  let to_list =
    SA.elements

  let equal =
    SA.equal
end
