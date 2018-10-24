open Framework.Essentials

module StrVarBind = Equiv.Make(
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

include StrVarBind

let get_var (s: string) (vb: StrVarBind.t) =
  try
    (StrVarBind.find_l s vb, vb)
  with
  | Not_found ->
    let v = mk_tmp ~vtyp:(Ast.T_int) () in
    (v, StrVarBind.add (s, v) vb)

let get_var_list (s: string list) (vb: StrVarBind.t) =
  let l, vb =
    ToolBox.fold (fun s (l, vb) ->
        let s', vb = get_var s vb in
        (s' :: l, vb)
      ) s ([], vb)
  in
  List.rev l, vb
