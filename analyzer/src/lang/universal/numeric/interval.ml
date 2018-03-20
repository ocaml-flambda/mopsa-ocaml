type t = Apron.Interval.t

let debug fmt = Debug.debug ~channel:"universal.numeric.value.interval" fmt

let bottom = Apron.Interval.bottom

let top = Apron.Interval.top

let print = Apron.Interval.print

let leq abs1 abs2 = Apron.Interval.cmp abs1 abs2 = -1

and join abs1 abs2 =
  match Apron.Interval.cmp abs1 abs2 with
  | 0 -> abs1
  | -1 -> abs2
  | 1 -> abs1
  | -2 -> Apron.Interval.of_scalar abs1.Apron.Interval.inf abs2.Apron.Interval.sup
  | 2 -> Apron.Interval.of_scalar abs2.Apron.Interval.inf abs1.Apron.Interval.sup
  | _ -> assert false

let meet abs1 abs2 =
  match Apron.Interval.cmp abs1 abs2 with
  | 0 -> abs1
  | -1 -> abs1
  | 1 -> abs2
  | -2 -> Apron.Interval.of_scalar abs2.Apron.Interval.inf abs1.Apron.Interval.sup
  | 2 -> Apron.Interval.of_scalar abs1.Apron.Interval.inf abs2.Apron.Interval.sup
  | _ -> assert false

let is_bounded abs =
  let inf = abs.Apron.Interval.inf
  and sup = abs.Apron.Interval.sup in
  Apron.Scalar.is_infty inf = 0 && Apron.Scalar.is_infty sup = 0

let get_bounds abs =
  let inf = abs.Apron.Interval.inf
  and sup = abs.Apron.Interval.sup in

  let scalar_to_float s =
    let x = Apron.Scalar.to_string s in
    float_of_string x
  in

  let min = int_of_float @@ ceil @@ scalar_to_float inf
  and max = int_of_float @@ floor @@ scalar_to_float sup in
  (min, max)
