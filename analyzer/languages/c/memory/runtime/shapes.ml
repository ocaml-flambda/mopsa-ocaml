

open Mopsa
open Common
open Common.Base
open Sig.Abstraction.Domain


type value_kind = Pointer | Immediate

module type RuntimeShapes = sig 
  include LATTICE

  val non_value_shape : t

  val immediate : unit -> t
  val block: unit -> t 
  val int64: unit -> t
  val int32: unit -> t
  val double: unit -> t
  val any: unit -> t
  val string: unit -> t
  val bigarray: unit -> t
  val abstract: unit -> t
  val nativeint: unit -> t


  val pp_shapes : Format.formatter -> t -> unit 
  val to_string : t -> string

  val compat: value:t -> constr:t -> bool 
  val value_kind: t -> 'a flow -> ('a, (t * value_kind) option) cases
end 



(** Simple Runtime SShapes *)

module Singleton (Descr: sig val descr: string end) = 
struct 

  type t = unit 

  let compare a b = 0
  let print printer a = Print.pp_string printer Descr.descr
  let to_string a = Descr.descr

end


module SimpleShapes : RuntimeShapes = 
struct 


  module Shape =
  struct 

    type t = 
      Immediate
    | Block
    | String 
    | Int64
    | Int32 
    | Nativeint
    | Double  
    | Bigarray 
    | Abstract 



    let rec compare a b = 
      match a, b with 
      | Immediate, Immediate -> 0 
      | Block, Block 
      | String, String  
      | Int64, Int64
      | Int32, Int32 
      | Nativeint, Nativeint
      | Double, Double  
      | Bigarray, Bigarray 
      | Abstract, Abstract 
      | _, _ ->  Stdlib.compare a b


    let rec pp_shape fmt s = 
      match s with 
      | Immediate -> Format.pp_print_string fmt "imm"
      | Block -> Format.pp_print_string fmt "block"
      | String -> Format.pp_print_string fmt "string"
      | Int64 -> Format.pp_print_string fmt "int64"
      | Int32 -> Format.pp_print_string fmt "int32"
      | Nativeint -> Format.pp_print_string fmt "nativeint"
      | Double -> Format.pp_print_string fmt "double"
      | Bigarray -> Format.pp_print_string fmt "bigarray"
      | Abstract -> Format.pp_print_string fmt "abstract"
   
    let to_string s = Format.asprintf "%a" pp_shape s

    let print printer b = pp_string printer (to_string b)

  end

  module ValueShapes = Framework.Lattices.Powerset.Make(Shape)
  module NonValue = Framework.Lattices.Const.Make(Singleton(struct let descr = "nonval" end))
  module Self = Framework.Lattices.Disjoint.Make(ValueShapes)(NonValue)
  include Self 


  let non_value_shape : t = Nbt (R (Nbt ()))

  let immediate () : t = 
    Nbt (L (ValueShapes.singleton Immediate))

  let block () : t = 
    Nbt (L (ValueShapes.singleton Block))

  let int64 () : t = 
    Nbt (L (ValueShapes.singleton Int64))

  let int32 () : t = 
    Nbt (L (ValueShapes.singleton Int32))

  let double () : t = 
    Nbt (L (ValueShapes.singleton Double))

  let any () : t = 
    Nbt (L (ValueShapes.top))

  let string () : t = 
    Nbt (L (ValueShapes.singleton String))

  let nativeint () : t = 
    Nbt (L (ValueShapes.singleton Nativeint))

  let bigarray () : t = 
    Nbt (L (ValueShapes.singleton Bigarray))
  
  let abstract () : t = 
    Nbt (L (ValueShapes.singleton Abstract))

  let pp_shapes fmt s = 
    Print.format print fmt s

  let to_string s : string = Format.asprintf "%a" pp_shapes s

  let compat ~value ~constr = is_bottom (meet value constr)


  let value_kind (s: t) flow = 
    match s with 
    | BOT -> Cases.singleton None flow  
    | TOP -> Cases.singleton None flow 
    | Nbt (R _) -> Cases.singleton None flow  
    | Nbt (L ss) -> 
      let imm = if ValueShapes.is_top ss || ValueShapes.mem Immediate ss then Cases.singleton (Some (s, Immediate)) flow else Cases.empty flow in
      let ptr_shapes: Shape.t list = [Block; Int64; Int32; Double; String; Nativeint; Bigarray; Abstract] in
      let ptr = if ValueShapes.is_top ss || List.exists (fun s -> ValueShapes.mem s ss) ptr_shapes then Cases.singleton (Some (s, Pointer)) flow else Cases.empty flow in 
      Cases.join imm ptr

  let print printer (s: t) = 
    match s with 
    | TOP -> Print.pp_string printer Top.top_string
    | BOT -> Print.pp_string printer Bot.bot_string 
    | Nbt (L ss) when ValueShapes.is_top ss -> Print.pp_string printer "*"
    | Nbt (L ss) -> ValueShapes.print printer ss
    | Nbt (R _) -> Print.pp_string printer "none"

end


(* Advanced Runtime Shapes *)
module AdvancedShapes : RuntimeShapes = 
struct 

  module IntMap = Map.Make(Int)

  type shape_body = 
  { 
    immediate: bool;
    int64: bool;
    int32: bool; 
    double: bool;
    string: bool;
    nativeint: bool;
    bigarray: bool;
    array: t option;
    abstract: bool;
    block: (t IntMap.t IntMap.t) option
  }
  and shape = 
    NonVal 
  | Shape of shape_body
  | Any
  and t = shape Bot_top.with_bot_top


  let bottom : t = BOT
  let top : t = TOP


  let is_bottom (a: t) = 
    match a with 
    | BOT -> true 
    | _ -> false

  let join_opt join o1 o2 = 
    match o1, o2 with 
    | None, _ -> o2 
    | _, None -> o1 
    | Some x, Some y -> Some (join x y) 

  let join_maps join m1 m2 = 
    IntMap.merge (fun _ o1 o2 -> join_opt join o1 o2) m1 m2

  let meet_opt meet o1 o2 = 
      match o1, o2 with 
      | None, _ -> None
      | _, None -> None
      | Some x, Some y -> Some (meet x y) 
  
  let meet_maps meet m1 m2 = 
      IntMap.merge (fun _ o1 o2 -> meet_opt meet o1 o2) m1 m2
  
  let rec join_shape_bodies s1 s2 : shape_body = 
    let immediate = s1.immediate || s2.immediate in 
    let double = s1.double || s2.double in 
    let int64 = s1.int64 || s2.int64 in 
    let int32 = s1.int32 || s2.int32 in 
    let string = s1.string || s2.string in 
    let nativeint = s1.nativeint || s2.nativeint in
    let bigarray = s1.bigarray || s2.bigarray in 
    let abstract = s1.abstract || s2.abstract in 
    let array = join_opt join s1.array s2.array in 
    let block = join_opt (join_maps (join_maps join)) s1.block s2.block in 
    { immediate; int64; int32; string; nativeint; bigarray; double; abstract; array; block }
  
  and join a b = 
    match a, b with
    | BOT, _ -> b
    | _, BOT -> a
    | TOP, _ -> TOP 
    | _, TOP -> TOP 
    | Nbt NonVal, Nbt NonVal -> Nbt NonVal
    | Nbt NonVal, _ -> TOP 
    | Nbt _, Nbt NonVal -> TOP
    | Nbt Any, Nbt _ -> b (* shape and any *) 
    | Nbt _, Nbt Any -> a (* shape and any *) 
    | Nbt (Shape s1), Nbt (Shape s2) -> Nbt (Shape (join_shape_bodies s1 s2))

  let rec meet_shape_bodies s1 s2 : shape_body = 
    let immediate = s1.immediate && s2.immediate in 
    let double = s1.double && s2.double in 
    let int64 = s1.int64 && s2.int64 in 
    let int32 = s1.int32 && s2.int32 in 
    let string = s1.string && s2.string in 
    let nativeint = s1.nativeint && s2.nativeint in
    let bigarray = s1.bigarray && s2.bigarray in 
    let abstract = s1.abstract && s2.abstract in 
    let array = meet_opt meet s1.array s2.array in 
    let block = meet_opt (meet_maps (meet_maps join)) s1.block s2.block in 
    { immediate; int64; int32; string; nativeint; bigarray; double; abstract; array; block }
  
  and meet a b = 
    match a, b with
    | BOT, _ -> BOT
    | _, BOT -> BOT
    | TOP, _ -> b
    | _, TOP -> a 
    | Nbt NonVal, Nbt NonVal -> Nbt NonVal
    | Nbt NonVal, _ -> BOT 
    | Nbt _, Nbt NonVal -> BOT
    | Nbt Any, Nbt _ -> Nbt Any (* shape and any *) 
    | Nbt _, Nbt Any -> Nbt Any (* shape and any *) 
    | Nbt (Shape s1), Nbt (Shape s2) -> Nbt (Shape (meet_shape_bodies s1 s2))


  let subset_bool b1 b2 =
    if b1 then b2 else true

  let subset_opt subset b1 b2 =
    match b1, b2 with 
    | None, _ -> true 
    | Some _, None -> false  
    | Some x, Some y -> subset x y  

  let subset_map subset b1 b2 = 
    IntMap.for_all (fun k s -> subset_opt subset (Some s) (IntMap.find_opt k b2)) b1

  let rec subset_shape s1 s2 = 
    subset_bool s1.immediate s2.immediate &&
    subset_bool s1.int64 s2.int64 &&
    subset_bool s1.int32 s2.int32 &&
    subset_bool s1.string s2.string &&
    subset_bool s1.nativeint s2.nativeint &&
    subset_bool s1.double s2.double &&
    subset_bool s1.bigarray s2.bigarray &&
    subset_bool s1.abstract s2.abstract &&
    subset_opt subset s1.array s2.array &&
    subset_opt (subset_map (subset_map subset)) s1.block s2.block
  and subset a b = 
    match a, b with 
    | BOT, _ -> true 
    | _, BOT -> false 
    | _, TOP -> true 
    | TOP, _ -> false 
    | Nbt NonVal, Nbt NonVal -> true 
    | Nbt NonVal, Nbt _ -> false 
    | Nbt _, Nbt NonVal -> false 
    | Nbt Any, _ -> true
    | Nbt _, Nbt Any -> false 
    | Nbt (Shape s1), Nbt (Shape s2) -> subset_shape s1 s2


  (* FIXME: for now we diverge in loops *)
  let widen ctx = join


  let pp_map pp_elem fmt map = 
    Format.pp_print_string fmt "{"; 
    IntMap.fold (fun k v () -> Format.fprintf fmt "%d ↦ %a" k pp_elem v) map ();
    Format.pp_print_string fmt "}"


  
  let rec pp_shapes fmt (s: t) = 
    match s with 
    | BOT -> Format.pp_print_string fmt Bot.bot_string
    | TOP -> Format.pp_print_string fmt Top.top_string
    | Nbt NonVal -> Format.pp_print_string fmt "none"
    | Nbt Any -> Format.pp_print_string fmt "*"
    | Nbt (Shape s) -> Format.fprintf fmt "{%a}" pp_shape s 
  and pp_shape fmt s = 
    let immediate = if s.immediate then ["imm"] else [] in 
    let int64 = if s.int64 then ["int64"] else [] in 
    let int32 = if s.int32 then ["int32"] else [] in 
    let nativeint = if s.nativeint then ["nativeint"] else [] in 
    let string = if s.string then ["string"] else [] in 
    let bigarray = if s.bigarray then ["bigarray"] else [] in 
    let double = if s.double then ["double"] else [] in 
    let abstract = if s.abstract then ["abstract"] else [] in 
    let array = match s.array with None -> [] | Some s -> [Format.asprintf "array(%a)" pp_shapes s] in 
    let block = match s.block with None -> [] | Some m -> [Format.asprintf "block(%a)" (pp_map (pp_map pp_shapes)) m] in 
    let strings = List.concat [immediate; int64; int32; nativeint; string; double; bigarray; abstract; array; block] in 
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") Format.pp_print_string fmt strings 

  let print (printer: Print.printer) (x: t) : unit =
    Print.unformat pp_shapes printer x
    
  let to_string (x: t) : string =
    Format.asprintf "%a" pp_shapes x



    
  (* simple constructors *)
  let non_value_shape : t = Nbt NonVal
  
  let any () : t = Nbt Any 

  let nonvalue () : t = Nbt NonVal
  
  let immediate () : t = 
    Nbt (Shape { immediate = true; double = false; int32 = false; int64 = false; nativeint = false; string = false; bigarray = false; abstract = false;  array = None; block = None})

  let double () : t =
    Nbt (Shape { immediate = false; double = true; int32 = false; int64 = false; nativeint = false; string = false; bigarray = false; abstract = false;  array = None; block = None})
  
  let int32 () : t =
    Nbt (Shape { immediate = false; double = false; int32 = true; int64 = false; nativeint = false; string = false; bigarray = false; abstract = false; array = None; block = None})

  let int64 () : t =
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = true; nativeint = false; string = false; bigarray = false; abstract = false; array = None; block = None})
  
  let nativeint () : t =
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = true; string = false; bigarray = false; abstract = false; array = None; block = None})
  
  let string () : t =
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = false; string = true; bigarray = false; abstract = false; array = None; block = None})
  
  let bigarray () : t =
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = false; string = false; bigarray = true; abstract = false;  array = None; block = None})

  let abstract () : t =
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = false; string = false; bigarray = false; abstract = true;  array = None; block = None})

  let array (s: t) : t = 
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = false; string = false; bigarray = false; abstract = false; array = (Some s); block = None})

  let block () : t = 
    Nbt (Shape { immediate = false; double = false; int32 = false; int64 = false; nativeint = false; string = false; bigarray = false; abstract = false; array = None; block = Some (IntMap.empty)})

  let value_kind (s: t) flow =
    match s with 
    | BOT 
    | TOP 
    | Nbt NonVal -> Cases.singleton None flow 
    | Nbt Any -> 
      let imm_case = Cases.singleton (Some (any (), Immediate)) flow in 
      let ptr_case = Cases.singleton (Some (any (), Pointer)) flow in 
      Cases.join imm_case ptr_case
    | Nbt (Shape ({immediate=imm;double;int32;int64;nativeint;string;bigarray;abstract;array;block} as sh)) ->
      let contains_ptr = (double || int64 || int32 || nativeint || string || bigarray || abstract || Option.is_some array || Option.is_some block) in
      let impossible_case = if not contains_ptr && not imm then Cases.singleton None flow else Cases.empty flow in 
      let imm_case = if imm then Cases.singleton (Some (immediate (), Immediate)) flow else Cases.empty flow in 
      let ptr_case = if contains_ptr then Cases.singleton (Some (Bot_top.Nbt (Shape { sh with immediate = false }), Pointer)) flow else Cases.empty flow in 
        impossible_case |> Cases.join imm_case |> Cases.join ptr_case  


 
  let compat ~value ~constr = 
    subset value constr
        
end 