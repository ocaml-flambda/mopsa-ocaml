open Mopsa

module Aux = Equiv.Make(
  struct
    type t = string
    let compare (u: t) (v: t) = compare u v
    let print = Format.pp_print_string
  end
  )(
  struct
    type t = var
    let compare = compare_var
    let print = pp_var
  end
  )

include Aux

let get_var (s: string) (vb: Aux.t) =
  try
    (Aux.find_l s vb, vb)
  with
  | Not_found ->
    let v = mktmp Ast.T_int () in
    (v, Aux.add (s, v) vb)

let get_var_list (s: string list) (vb: Aux.t) =
  let l, vb =
    ToolBox.fold (fun s (l, vb) ->
        let s', vb = get_var s vb in
        (s' :: l, vb)
      ) s ([], vb)
  in
  List.rev l, vb
