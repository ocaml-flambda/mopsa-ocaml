(** Set with printing builder *)

(** Builder *)
module Make(K : ValueSig.S) =
struct
  include Set.Make(K)
  let print fmt (s : t) =
    Format.fprintf fmt "@[{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         K.print
      ) (elements s)
  let compare s s' =
    if subset s s' then
      if subset s' s then
        0
      else
        -1
    else
      1
end
