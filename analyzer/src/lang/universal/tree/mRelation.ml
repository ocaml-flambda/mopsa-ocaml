module Make
    (A : SIG.COMPARABLE)
    (B : SIG.COMPARABLE)
=
struct
  module BM = Map.Make(B)
  module AM = Map.Make(A)
  type t =
    {
      a : B.t AM.t;
      b : A.t BM.t;
    }
  let fold f r acc =
    BM.fold f r acc

  let add_pair (u,v) r =
    {
      a = AM.add v u r.a;
      b = BM.add u v r.b;
    }

  let empty =
    {
      a = AM.empty;
      b = BM.empty;
    }

  let to_list r =
    fold (fun k v acc -> (k,v)::acc) r []

  let of_list l =
    List.fold_left (fun acc p -> add_pair p acc) empty l

  let mem (u,v) r =
    AM.mem v (r.a) && let y = AM.find v (r.a) in y = u

  let meml v r =
    AM.mem v (r.a)

  let memr u r =
    BM.mem u (r.b)

  let findl v r =
    AM.find v (r.a)

  let findr u r =
    BM.find u (r.b)

  let pp_sep_comma = fun fmt () -> Format.fprintf fmt ","

  let print fmt r =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:pp_sep_comma (fun fmt (x,y) -> Format.fprintf fmt "(%a~%a)" A.print x B.print y))
      (AM.bindings r.a)
end
