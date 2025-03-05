open Sexplib
open Sexplib.Std

type type_shape =
  | Any (** anything of C type [value]*)
  | Imm (** immediate, tagged with a one at the end*)
  | Nativeint (** block to a native word (= 64-bit) integer*)
  | Double (** block to a native double *)
  | Int64 (** block to a 64-bit integer *)
  | Int32 (** block to a 32-bit integer *)
  | String (** block to a char pointer with a size *)
  | FloatArray (** block containing native doubles *)
  | Block of (int * type_shape list) option
  (** Block below no-scan tag. If the argment is [None], then the block could have any tag
      and any elements. If the argument is [Some (t, shs)], then [t] is the tag of the
      block and [shs] contains the shapes of its fields. The length of the block is known statically.
      Otherwise, it must be an array (see below). *)
  | Array of type_shape
  (** Block with tag 0 and a fixed size (not known statically). The shape of the elements is given
      by the argument. *)
  | Closure (** Block with closure tag. *)
  | Obj (** Block with object tag. *)
  | Or of type_shape * type_shape
  (** Disjunction between two shapes for (e.g., variant types) *)
[@@deriving sexp]

type fn_type_shapes =
  { arguments : type_shape list
  ; return : type_shape
  }
[@@deriving sexp]

(** An [extfun_desc] describes the information that we know about an external function.
    To enable extensions in the future, we define it as a record of options.
    This enables adding new, optional fields in the future without breaking the serialized
    form.
*)
type extfun_desc =
  { shape : fn_type_shapes option [@sexp.option]
  (** If the shape is not present, then we fallback on the arity of the C code. *)
  }
[@@deriving sexp]

type extfun =
  { name : string (** C name of the function *)
  ; desc : extfun_desc
  }
[@@deriving sexp]

let rec pp_shape fmt (sh : type_shape) =
  match sh with
  | Any -> Format.pp_print_string fmt "*"
  | Imm -> Format.pp_print_string fmt "imm"
  | Nativeint -> Format.pp_print_string fmt "native"
  | Double -> Format.pp_print_string fmt "double"
  | Int64 -> Format.pp_print_string fmt "int64"
  | Int32 -> Format.pp_print_string fmt "int32"
  | String -> Format.pp_print_string fmt "string"
  | Block None -> Format.pp_print_string fmt "block"
  | Block (Some (tag, shapes)) ->
    Format.fprintf
      fmt
      "block[%d](%a)"
      tag
      (Format.pp_print_list pp_shape ~pp_sep:(fun fmt () ->
         Format.pp_print_string fmt "; "))
      shapes
  | FloatArray -> Format.pp_print_string fmt "float_array"
  | Obj -> Format.pp_print_string fmt "object"
  | Array s -> Format.fprintf fmt "array(%a)" pp_shape s
  | Closure -> Format.pp_print_string fmt "closure"
  | Or (s1, s2) -> Format.fprintf fmt "%a ∨ %a" pp_shape s1 pp_shape s2
;;

let pp_extfun_desc fmt desc =
  match desc with
  | { shape = Some { arguments; return }; _ } ->
    Format.fprintf
      fmt
      "args(%a) -> %a"
      (Format.pp_print_list pp_shape ~pp_sep:(fun fmt () ->
         Format.pp_print_string fmt "; "))
      arguments
      pp_shape
      return
  | { shape = None; _ } -> Format.fprintf fmt "*"
;;

let pp_ext_fun fmt ext = Format.fprintf fmt "%s: %a" ext.name pp_extfun_desc ext.desc

let serialize_extfun ext =
  let sexp = [%sexp_of: extfun] ext in
  Sexp.to_string sexp
;;

let deserialize_extfun (ef : string) =
  try
    let sexp = Sexp.of_string ef in
    Some ([%of_sexp: extfun] sexp)
  with
  | _ -> None
;;
