

open Mopsa
open Common
open Common.Base
open Common.Type_shapes
open Sig.Abstraction.Domain


module type OCamlValueShape = sig
  include LATTICE

  val is_top : t -> bool

  (** [any] covers any possible OCaml value,
      it is the same as [Top] *)
  val any: unit -> t
  val immediate : unit -> t

  (** [any_block] covers any allocated runtime block, including strings, custom blocks, abstract blocks, etc. *)
  val any_block : unit -> t

  (** [any_standard_block] covers tuples, records, arrays, and variant type constructors; excludes strings, custom blocks, abstract blocks, etc.  *)
  val any_standard_block: unit -> t

  (** [any_non_array_standard_block] covers tuples, records, and variant type constructors; it does not cover arrays *)
  val any_non_array_standard_block: unit -> t

  val fixed_size_standard_block:
    tag: int (** tag of the block, if known, should be a standard variant tag *)
    -> fields: t list (** shapes of the fields of the block, if known *)
    -> t

  (* all of the shapes below are blocks *)
  val array : t -> t

  val int64: unit -> t
  val int32: unit -> t
  val double: unit -> t
  val string: unit -> t
  val bigarray: unit -> t
  val abstract: unit -> t
  val nativeint: unit -> t
  val floatarray : unit -> t
  val closure: unit -> t
  val obj : unit -> t
  val custom_block : unit -> t


  (* utility functions *)
  val pp_shapes : Format.formatter -> t -> unit
  val to_string : t -> string


  val field_shape_at_index : t -> int -> t

end


module OCamlValueExt (OCamlValue : OCamlValueShape) =
struct


  let rec type_shape_to_shapes (sh: type_shape) =
    match sh with
    | Any -> OCamlValue.any ()
    | Imm -> OCamlValue.immediate ()
    | Block None -> OCamlValue.any_non_array_standard_block ()
    | Block (Some (t, shs)) -> OCamlValue.fixed_size_standard_block ~tag:t ~fields: (List.map type_shape_to_shapes shs)
    | String -> OCamlValue.string ()
    | Array sh -> OCamlValue.array (type_shape_to_shapes sh)
    | Int64 -> OCamlValue.int64 ()
    | Int32 -> OCamlValue.int32 ()
    | Double -> OCamlValue.double ()
    | Nativeint -> OCamlValue.nativeint ()
    | Closure -> OCamlValue.closure ()
    | Obj -> OCamlValue.obj ()
    | FloatArray -> OCamlValue.floatarray ()
    | Or (s1, s2) -> OCamlValue.join (type_shape_to_shapes s1) (type_shape_to_shapes s2)



  (* Shapes that can be set/asserted from C using primitives *)
  (* FIXME: distinguish between any block and standard block *)
  type runtime_shape_enum =
    | FFI_IMMEDIATE
    | FFI_BLOCK
    | FFI_DOUBLE
    | FFI_INT64
    | FFI_INT32
    | FFI_NATIVEINT
    | FFI_STRING
    | FFI_VARIANT
    | FFI_BIGARRAY
    | FFI_ABSTRACT
    | FFI_ANY


  let runtime_shape_to_integer (s: runtime_shape_enum) =
    match s with
    | FFI_IMMEDIATE -> 1
    | FFI_BLOCK     -> 2
    | FFI_DOUBLE    -> 3
    | FFI_INT64     -> 4
    | FFI_INT32     -> 5
    | FFI_NATIVEINT -> 6
    | FFI_STRING    -> 7
    | FFI_VARIANT   -> 8
    | FFI_BIGARRAY  -> 9
    | FFI_ABSTRACT  -> 10
    | FFI_ANY       -> 11


  let runtime_shape_of_integer (z: int) =
    List.find_opt (fun sh -> runtime_shape_to_integer sh = z) [
      FFI_IMMEDIATE;
      FFI_BLOCK;
      FFI_DOUBLE;
      FFI_INT64;
      FFI_INT32;
      FFI_NATIVEINT;
      FFI_STRING;
      FFI_VARIANT;
      FFI_BIGARRAY;
      FFI_ABSTRACT;
      FFI_ANY;
    ]

  let runtime_shape_to_shapes (s: runtime_shape_enum) =
    match s with
    | FFI_IMMEDIATE -> OCamlValue.immediate ()
    | FFI_BLOCK     -> OCamlValue.any_standard_block ()
    | FFI_DOUBLE    -> OCamlValue.double ()
    | FFI_INT64     -> OCamlValue.int64 ()
    | FFI_INT32     -> OCamlValue.int32 ()
    | FFI_NATIVEINT -> OCamlValue.nativeint ()
    | FFI_STRING    -> OCamlValue.string ()
    | FFI_VARIANT   -> OCamlValue.join (OCamlValue.any_standard_block ()) (OCamlValue.immediate ())
    | FFI_BIGARRAY  -> OCamlValue.bigarray ()
    | FFI_ABSTRACT  -> OCamlValue.abstract ()
    | FFI_ANY       -> OCamlValue.any ()

  let shape_of_runtime_shape (id: int) : OCamlValue.t option =
    match runtime_shape_of_integer id with
    | None -> None
    | Some sh -> Some (runtime_shape_to_shapes sh)


  type runtime_block_tag =
  | BLOCK_ORDINARY of int
  | BLOCK_OBJECT
  | BLOCK_CLOSURE
  | BLOCK_STRING
  | BLOCK_DOUBLE
  | BLOCK_DOUBLE_ARRAY
  | BLOCK_ABSTRACT
  | BLOCK_CUSTOM
  (* unsupported: *)
  | BLOCK_INFIX
  | BLOCK_LAZY
  | BLOCK_FORWARD

  let runtime_block_tag_to_int (rbt: runtime_block_tag) =
    match rbt with
    | BLOCK_ORDINARY n    -> n
    | BLOCK_LAZY          -> 246
    | BLOCK_CLOSURE       -> 247
    | BLOCK_OBJECT        -> 248
    | BLOCK_INFIX         -> 249
    | BLOCK_FORWARD       -> 250
    | BLOCK_ABSTRACT      -> 251
    | BLOCK_STRING        -> 252
    | BLOCK_DOUBLE        -> 253
    | BLOCK_DOUBLE_ARRAY  -> 254
    | BLOCK_CUSTOM        -> 255

  let runtime_block_tag_of_int (z: int) =
    if 0 <= z && z < runtime_block_tag_to_int BLOCK_LAZY then
      Some (BLOCK_ORDINARY z)
    else
      List.find_opt (fun sh -> runtime_block_tag_to_int sh = z) [
        BLOCK_OBJECT;
        BLOCK_CLOSURE;
        BLOCK_STRING;
        BLOCK_DOUBLE;
        BLOCK_DOUBLE_ARRAY;
        BLOCK_ABSTRACT;
        BLOCK_CUSTOM;
        (* unsupported: *)
        BLOCK_INFIX;
        BLOCK_LAZY;
        BLOCK_FORWARD;
      ]

  let runtime_block_tag_to_shapes (s: runtime_block_tag) =
    match s with
    | BLOCK_OBJECT -> OCamlValue.obj ()
    | BLOCK_CLOSURE -> OCamlValue.closure ()
    | BLOCK_STRING -> OCamlValue.string ()
    | BLOCK_DOUBLE -> OCamlValue.double ()
    | BLOCK_DOUBLE_ARRAY -> OCamlValue.floatarray ()
    | BLOCK_ABSTRACT -> OCamlValue.abstract ()
    | BLOCK_CUSTOM -> OCamlValue.custom_block ()
    | BLOCK_INFIX -> OCamlValue.any ()
    | BLOCK_LAZY -> OCamlValue.any ()
    | BLOCK_FORWARD -> OCamlValue.any ()
    | BLOCK_ORDINARY n -> OCamlValue.any_standard_block ()

  let shape_of_runtime_block_tag_int (id: Z.t) : OCamlValue.t option =
    match runtime_block_tag_of_int (Z.to_int id) with
    | exception Z.Overflow -> None
    | None -> None
    | Some sh -> Some (runtime_block_tag_to_shapes sh)


  (* Compatibility of OCaml values *)
  let compat ~value ~constr = not (OCamlValue.is_bottom (OCamlValue.meet value constr))


  (* Value Check *)
  type value_kind = Pointer | Immediate

  let value_kind (s: OCamlValue.t) flow =
    let imm = OCamlValue.immediate () in
    let blk = OCamlValue.any_block () in
    let imm_case =
      if compat ~value:s ~constr:imm
        then Cases.singleton (Some (OCamlValue.meet s imm, Immediate)) flow
        else Cases.empty flow
    in let ptr_case =
      if compat ~value:s ~constr:blk
        then Cases.singleton (Some (OCamlValue.meet s blk, Pointer)) flow
        else Cases.empty flow
    in
    (Cases.join imm_case ptr_case)

end






module Singleton =
struct

  type t = unit

  let compare a b = 0
  let print printer a = Print.pp_string printer "()"
  let to_string a = "()"

end



module RuntimeShape (OCamlValue: OCamlValueShape) =
struct

  module OCamlValueExt = OCamlValueExt(OCamlValue)

  (** Runtime Value Shape Domain *)
  module NonValue = Framework.Lattices.Const.Make(Singleton)
  module Self = Framework.Lattices.Disjoint.Make(OCamlValue)(NonValue)
  include Self

  let non_ocaml_value : t = Nbt (R (Nbt ()))

  let ocaml_value (v: OCamlValue.t) : t =
    Nbt (L v)


  let compat_runtime_shape ~value ~constr =
    not (is_bottom (meet value constr))


  let print printer (s: t) =
    match s with
    | TOP -> Print.pp_string printer Top.top_string
    | BOT -> Print.pp_string printer Bot.bot_string
    | Nbt (L ss) when OCamlValue.is_top ss ->
      Print.pp_string printer "OCaml Value"
    | Nbt (L ss) -> OCamlValue.print printer ss
    | Nbt (R _) ->
      Print.pp_string printer "Non-OCaml Value"

  let pp_shapes fmt s =
    Print.format print fmt s

  let to_string s : string =
    Format.asprintf "%a" pp_shapes s

  let value_kind_runtime_shape (s: t) flow =
    match s with
    | TOP ->
      Cases.join
        (Cases.singleton None flow)
        (OCamlValueExt.value_kind (OCamlValue.top) flow)
    | _ when is_bottom s -> Cases.singleton None flow
    | Nbt (L s) -> OCamlValueExt.value_kind s flow
    | Nbt (R _) -> Cases.singleton None flow
    | _ -> Cases.singleton None flow

end


(** Simple, Flat Runtime Shapes *)
module FlatShapes : OCamlValueShape =
struct


  module Shape =
  struct

    type t =
      Immediate
    | OrdinaryBlock
    | String
    | Int64
    | Int32
    | Nativeint
    | Double
    | Bigarray
    | Abstract
    | Obj
    | FloatArray
    | Closure
    | Custom

    let rec compare a b =
      match a, b with
      | Immediate, Immediate
      | OrdinaryBlock, OrdinaryBlock
      | String, String
      | Int64, Int64
      | Int32, Int32
      | Nativeint, Nativeint
      | Double, Double
      | Bigarray, Bigarray
      | Abstract, Abstract
      | Obj, Obj
      | FloatArray, FloatArray
      | Closure, Closure
      | Custom, Custom -> 0
      | _, _ ->  Stdlib.compare a b


    let rec pp_shape fmt s =
      match s with
      | Immediate -> Format.pp_print_string fmt "imm"
      | OrdinaryBlock -> Format.pp_print_string fmt "standard block"
      | String -> Format.pp_print_string fmt "string"
      | Int64 -> Format.pp_print_string fmt "int64"
      | Int32 -> Format.pp_print_string fmt "int32"
      | Nativeint -> Format.pp_print_string fmt "nativeint"
      | Double -> Format.pp_print_string fmt "double"
      | Bigarray -> Format.pp_print_string fmt "bigarray"
      | Abstract -> Format.pp_print_string fmt "abstract block"
      | Obj -> Format.pp_print_string fmt "object"
      | FloatArray -> Format.pp_print_string fmt "float array"
      | Closure -> Format.pp_print_string fmt "closure"
      | Custom -> Format.pp_print_string fmt "custom block"

    let to_string s = Format.asprintf "%a" pp_shape s

    let print printer b = pp_string printer (to_string b)

  end

  module ValueShapes = Framework.Lattices.Powerset.Make(Shape)
  include ValueShapes


  let ocaml_value (s: Shape.t) : t =
    ValueShapes.singleton s

  let immediate () : t =
    ocaml_value Immediate

  let int64 () : t =
    ocaml_value Int64

  let int32 () : t =
    ocaml_value Int32

  let double () : t =
    ocaml_value Double

  let any () : t =
    (ValueShapes.top)

  let string () : t =
    ocaml_value String

  let nativeint () : t =
    ocaml_value Nativeint

  let bigarray () : t =
    ocaml_value Bigarray

  let abstract () : t =
    ocaml_value Abstract

  let obj () : t =
    ocaml_value Obj

  let floatarray () : t =
    ocaml_value FloatArray

  let closure () : t =
    ocaml_value Closure

  let custom_block () : t =
    ocaml_value Custom

  let array _ : t =
    ocaml_value OrdinaryBlock

  let any_standard_block () : t =
    ocaml_value OrdinaryBlock

  let fixed_size_standard_block ~tag ~fields : t =
    ocaml_value OrdinaryBlock

  let any_non_array_standard_block () : t =
    ocaml_value OrdinaryBlock


  let any_block () =
    ocaml_value OrdinaryBlock |>
    join (int64 ())  |>
    join (int32 ()) |>
    join (string ()) |>
    join (double ()) |>
    join (nativeint ()) |>
    join (bigarray ()) |>
    join (abstract ()) |>
    join (obj ()) |>
    join (floatarray ()) |>
    join (closure ()) |>
    join (custom_block ())

  let print printer (s: t) =
    if is_top s
    then Print.pp_string printer "OCaml Value"
    else  ValueShapes.print printer s

  let pp_shapes fmt s =
    Print.format print fmt s

  let to_string s : string = Format.asprintf "%a" pp_shapes s

  let field_shape_at_index _ _ = any ()

end


(* Advanced, Nested Runtime Shapes *)
module NestedShapes : OCamlValueShape  =
struct


  (** Auxillary Operations *)
  (** ==================== *)

  module IntMap = Map.Make(Int)
  let join_bool b1 b2 = b1 || b2
  let meet_bool b1 b2 = b1 && b2
  let subset_bool b1 b2 = if b1 then b2 else true


  let join_opt join o1 o2 =
    match o1, o2 with
    | None, _ -> o2
    | _, None -> o1
    | Some x, Some y -> Some (join x y)

  let join_maps join m1 m2 =
    IntMap.merge (fun _ o1 o2 -> join_opt join o1 o2) m1 m2


  let join_with_top join (t1: 'a Top.with_top) (t2: 'a Top.with_top) : 'a Top.with_top =
    match t1, t2 with
    | TOP, _ -> TOP
    | _, TOP -> TOP
    | Nt t1, Nt t2 -> Nt (join t1 t2)

  let meet_opt meet o1 o2 =
    match o1, o2 with
    | None, _ -> None
    | _, None -> None
    | Some x, Some y -> Some (meet x y)

  let meet_maps meet m1 m2 =
    IntMap.merge (fun _ o1 o2 -> meet_opt meet o1 o2) m1 m2

  let meet_with_top meet (t1: 'a Top.with_top) (t2: 'a Top.with_top) : 'a Top.with_top =
    match t1, t2 with
    | TOP, _ -> t2
    | _, TOP -> t1
    | Nt t1, Nt t2 -> Nt (meet t1 t2)


  let subset_opt subset o1 o2 =
    match o1, o2 with
    | None, _ -> true
    | _, None -> false
    | Some s1, Some s2 -> subset s1 s2

  let subset_map subset b1 b2 =
    IntMap.for_all (fun k s -> subset_opt subset (Some s) (IntMap.find_opt k b2)) b1

  let subset_with_top subset (t1: 'a Top.with_top) (t2: 'a Top.with_top)  =
    match t1, t2 with
    | _, TOP -> true
    | TOP, _ -> false
    | Nt t1, Nt t2 -> subset t1 t2




  type value_shape =
  {
    immediate: bool;

    (* block types*)
    ordinary_block: bool;
    int64: bool;
    int32: bool;
    double: bool;
    string: bool;
    nativeint: bool;
    bigarray: bool;
    abstract: bool;
    custom: bool;
    floatarray: bool;
    closure: bool;
    obj : bool;
  }

  type t = {
    shape: value_shape;
    variant_fields: t IntMap.t Top.with_top IntMap.t Top.with_top; (* includes tuples and records, where tag = 0 *)
    array_fields: t option Top.with_top;
  }


  let bot_value_shape : value_shape =
    {
      immediate = false;
      ordinary_block = false;
      int64 = false;
      int32 = false;
      double = false;
      string = false;
      nativeint = false;
      bigarray = false;
      floatarray = false;
      closure = false;
      abstract = false;
      custom = false;
      obj = false;
    }

  let top_value_shape : value_shape =
    {
      immediate = true;
      ordinary_block = true;
      int64 = true;
      int32 = true;
      double = true;
      string = true;
      nativeint = true;
      bigarray = true;
      floatarray = true;
      closure = true;
      abstract = true;
      custom = true;
      obj = true;
    }

  let flat_shape (vs: value_shape) : t =
    {
      shape = vs;
      variant_fields = Nt IntMap.empty;
      array_fields = Nt None;
    }

  let variant_fields_of_shapes tag (ls: t list) : t IntMap.t Top.with_top IntMap.t =
    let (_, field_map) = List.fold_left (fun (i, map) sh -> (i + 1, IntMap.add i sh map)) (0, IntMap.empty) ls in
    (IntMap.add tag (Top.Nt field_map) IntMap.empty)



  let immediate () : t =
    flat_shape { bot_value_shape with immediate = true }

  let int64 () : t =
    flat_shape { bot_value_shape with int64 = true }

  let int32 () : t =
    flat_shape { bot_value_shape with int32 = true }

  let double () : t =
    flat_shape { bot_value_shape with double = true }

  let any () : t =
    { shape = top_value_shape; array_fields = TOP; variant_fields = TOP }

  let string () : t =
    flat_shape { bot_value_shape with string = true }

  let nativeint () : t =
    flat_shape { bot_value_shape with nativeint = true }

  let bigarray () : t =
    flat_shape { bot_value_shape with bigarray = true }

  let abstract () : t =
    flat_shape { bot_value_shape with abstract = true }

  let obj () : t =
    flat_shape { bot_value_shape with obj = true }

  let floatarray () : t =
    flat_shape { bot_value_shape with floatarray = true }

  let closure () : t =
    flat_shape { bot_value_shape with closure = true }

  let custom_block () : t =
    flat_shape { bot_value_shape with custom = true }

  let array sh : t =
    let vs = { bot_value_shape with ordinary_block = true } in
    { shape = vs; variant_fields = Nt IntMap.empty; array_fields = Nt (Some sh) }

  let any_block () =
    let vs = { top_value_shape with immediate = false } in
    { shape = vs; variant_fields = TOP; array_fields = TOP }

  let any_standard_block () =
    let vs = { bot_value_shape with ordinary_block = true } in
    { shape = vs; array_fields = TOP; variant_fields = TOP }

  let any_non_array_standard_block () =
    let vs = { bot_value_shape with ordinary_block = true } in
    { shape = vs; array_fields = Nt None; variant_fields = TOP }

  let fixed_size_standard_block ~tag ~fields : t =
    let vs = { bot_value_shape with ordinary_block = true } in
    { shape = vs; array_fields = Nt None; variant_fields = Nt (variant_fields_of_shapes tag fields) }


  let join_value_shape sh1 sh2 =
    {
      immediate = join_bool sh1.immediate sh2.immediate;
      ordinary_block = join_bool sh1.ordinary_block sh2.ordinary_block;
      int64 = join_bool sh1.int64 sh2.int64;
      int32 = join_bool sh1.int32 sh2.int32;
      double = join_bool sh1.double sh2.double;
      string = join_bool sh1.string sh2.string;
      nativeint = join_bool sh1.nativeint sh2.nativeint;
      bigarray = join_bool sh1.bigarray sh2.bigarray;
      floatarray = join_bool sh1.floatarray sh2.floatarray;
      closure = join_bool sh1.closure sh2.closure;
      abstract = join_bool sh1.abstract sh2.abstract;
      custom = join_bool sh1.custom sh2.custom;
      obj = join_bool sh1.obj sh2.obj;
    }


  let meet_value_shape sh1 sh2 =
    {
      immediate = meet_bool sh1.immediate sh2.immediate;
      ordinary_block = meet_bool sh1.ordinary_block sh2.ordinary_block;
      int64 = meet_bool sh1.int64 sh2.int64;
      int32 = meet_bool sh1.int32 sh2.int32;
      double = meet_bool sh1.double sh2.double;
      string = meet_bool sh1.string sh2.string;
      nativeint = meet_bool sh1.nativeint sh2.nativeint;
      bigarray = meet_bool sh1.bigarray sh2.bigarray;
      floatarray = meet_bool sh1.floatarray sh2.floatarray;
      closure = meet_bool sh1.closure sh2.closure;
      abstract = meet_bool sh1.abstract sh2.abstract;
      custom = meet_bool sh1.custom sh2.custom;
      obj = meet_bool sh1.obj sh2.obj;
    }

  let subset_value_shape sh1 sh2 =
    subset_bool sh1.immediate sh2.immediate &&
    subset_bool sh1.ordinary_block sh2.ordinary_block &&
    subset_bool sh1.int64 sh2.int64 &&
    subset_bool sh1.int32 sh2.int32 &&
    subset_bool sh1.double sh2.double &&
    subset_bool sh1.string sh2.string &&
    subset_bool sh1.nativeint sh2.nativeint &&
    subset_bool sh1.bigarray sh2.bigarray &&
    subset_bool sh1.floatarray sh2.floatarray &&
    subset_bool sh1.closure sh2.closure &&
    subset_bool sh1.abstract sh2.abstract &&
    subset_bool sh1.custom sh2.custom &&
    subset_bool sh1.obj sh2.obj

  let is_bottom_value_shape sh =
    subset_value_shape sh bot_value_shape

  let rec join s1 s2 =
    {
      shape = join_value_shape s1.shape s2.shape;
      variant_fields = join_with_top (join_maps (join_with_top (join_maps join))) s1.variant_fields s2.variant_fields;
      array_fields = join_with_top (join_opt join) s1.array_fields s2.array_fields
    }

  let rec meet s1 s2 =
    {
      shape = meet_value_shape s1.shape s2.shape;
      variant_fields = meet_with_top (meet_maps (meet_with_top (meet_maps meet))) s1.variant_fields s2.variant_fields;
      array_fields = meet_with_top (meet_opt join) s1.array_fields s2.array_fields
    }

  let rec subset s1 s2 =
    subset_value_shape s1.shape s2.shape &&
    subset_with_top (subset_map (subset_with_top (subset_map subset))) s1.variant_fields s2.variant_fields &&
    subset_with_top (subset_opt subset) s1.array_fields s2.array_fields

  let top =
    {
      shape = top_value_shape;
      variant_fields = TOP;
      array_fields = TOP;
    }

  let bottom =
    {
      shape = bot_value_shape;
      variant_fields = Nt IntMap.empty;
      array_fields = Nt None;
    }

  let is_bottom s =
    is_bottom_value_shape s.shape

  let is_top s =
    subset (any ()) s


  let truncate_with_top trunc (t: 'a Top.with_top) : 'a Top.with_top =
    match t with
    | TOP -> TOP
    | Nt t -> Nt (trunc t)

  let truncate_opt trunc x =
    match x with
    | None -> None
    | Some x -> Some (trunc x)

  let truncat_map_with_top trunc ~breadth (m: 'a IntMap.t Top.with_top) : 'a IntMap.t Top.with_top =
    match m with
    | TOP -> TOP
    | Nt m -> if IntMap.cardinal m > breadth then TOP else Nt (IntMap.map trunc m)

  let rec truncate ~depth ~breadth t =
    if depth < 0 then top else
    {
      shape = t.shape;
      variant_fields = truncat_map_with_top (truncat_map_with_top (truncate ~depth:(depth - 1) ~breadth) ~breadth) ~breadth t.variant_fields;
      array_fields = truncate_with_top (truncate_opt (truncate ~depth:(depth - 1) ~breadth)) t.array_fields;
    }

  let widen ctx a b =
    truncate ~depth:5 ~breadth: 20 (join a b)


  (* Printing shapes *)
  let field_map_to_list (m: t IntMap.t) =
    if IntMap.is_empty m then []
    else
      let max = IntMap.fold (fun key _ acc -> Int.max key acc) m 0 in
      List.init (max + 1) (fun i ->
        match IntMap.find_opt i m with
        | None -> top
        | Some x -> x
      )


  let rec to_string (s: t) =
    if is_top s then "*"
    else
      let {shape;variant_fields;array_fields} = s in
      let immediate = if shape.immediate then ["imm"] else [] in
      let int64 = if shape.int64 then ["int64"] else [] in
      let int32 = if shape.int32 then ["int32"] else [] in
      let double = if shape.double then ["double"] else [] in
      let string = if shape.string then ["string"] else [] in
      let nativeint = if shape.nativeint then ["nativeint"] else [] in
      let bigarray = if shape.bigarray then ["bigarray"] else [] in
      let abstract = if shape.abstract then ["abstract"] else [] in
      let custom = if shape.custom then ["custom"] else [] in
      let floatarray = if shape.custom then ["floatarray"] else [] in
      let closure = if shape.closure then ["closure"] else [] in
      let obj = if shape.obj then ["object"] else [] in
      let block = if not shape.ordinary_block then [] else to_strings_blocks variant_fields in
      let array = if not shape.ordinary_block then [] else to_strings_arrays array_fields in
      let strings = List.concat [immediate; int64; int32; nativeint; floatarray; string; bigarray; double; abstract; custom; obj; closure; block; array] in
      strings_to_string strings

  and strings_to_string strings =
    match strings with
    | [s] -> s
    | strs ->
      Format.asprintf "{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") Format.pp_print_string) strs

  and to_strings_field_map (t: t IntMap.t Top.with_top) =
    match t with
    | TOP -> [Top.top_string]
    | Nt m ->
      (List.map to_string (field_map_to_list m))

  and to_strings_blocks (t: t IntMap.t Top.with_top IntMap.t Top.with_top) =
    match t with
    | TOP -> ["block"]
    | Nt m ->
      IntMap.fold (fun tag field_map acc -> Format.asprintf "block(%d, [%s])" tag (String.concat "; " (to_strings_field_map field_map)) :: acc) m []

  and to_strings_arrays (t: t option Top.with_top) =
    match t with
    | TOP -> ["array"]
    | Nt None -> []
    | Nt (Some t) -> [Format.asprintf "array(%s)" (to_string t)]

  let pp_shapes fmt x = Format.pp_print_string fmt (to_string x)

  let print printer x =
    Print.unformat pp_shapes printer x



  let variant_shapes_at_index (sh: t IntMap.t Top.with_top IntMap.t Top.with_top) i : t option =
    let shape_at_index_fields (fields: t IntMap.t Top.with_top) i =
      match fields with
      | TOP -> top
      | Nt m ->
        begin match IntMap.find_opt i m with
        | None -> top
        | Some t -> t
        end
    in
    match sh with
    | TOP -> Some top
    | Nt m when IntMap.is_empty m -> None
    | Nt m ->
      Some (IntMap.fold (fun tag fields acc -> join (shape_at_index_fields fields i) acc) m bottom)

  let array_shapes_at_index (sh: t option Top.with_top) i : t option =
    match sh with
    | TOP -> Some top
    | Nt None -> None
    | Nt (Some t) -> Some t


  let field_shape_at_index (sh: t) i =
    match variant_shapes_at_index sh.variant_fields i, array_shapes_at_index sh.array_fields i with
    | Some t1, Some t2 -> join t1 t2
    | Some t, None -> t
    | None, Some t -> t
    (* FIXME: not ideal, but okay *)
    | None, None -> top


end
