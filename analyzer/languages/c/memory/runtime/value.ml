(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)


(** Non-relational abstraction of pointer values *)


open Mopsa
open Sig.Abstraction.Value
open Universal.Ast
open Ast
open Common.Points_to
open Common.Base
open Top

open Framework.Lattices.Partial_inversible_map


module Roots =
struct
  type t = 
    Rooted | NotRooted

  let compare a b =
    match a, b with 
    | NotRooted, NotRooted -> 0
    | Rooted, Rooted -> 0
    | _, _ -> Stdlib.compare a b


  let to_string s = 
    match s with
    | Rooted-> "root"
    | NotRooted -> "not rooted"
    
  let print printer b = pp_string printer (to_string b)
end


module RuntimeLock =
struct
  type t = 
    Locked | Unlocked 

  let compare a b =
    match a, b with 
    | Locked, Locked -> 0
    | Unlocked, Unlocked -> 0
    | _, _ -> Stdlib.compare a b


  let to_string s = 
    match s with
    | Locked -> "locked"
    | Unlocked-> "unlocked"
    
  let print printer b = pp_string printer (to_string b)
end


module Shape =
struct 

  type t = 
    NonValue
  | Immediate
  | Block
  | String 
  | Int64
  | Int32 
  | Nativeint
  | Double  
  | Bigarray 
  | Abstract 
  | Any  
  | Tuple of t list 
  | Array of t 


  let rec compare a b = 
    match a, b with 
    | Immediate, Immediate -> 0 
    | Tuple ts, Tuple us -> List.compare compare ts us 
    | Array t, Array u -> compare t u
    | Block, Block 
    | String, String  
    | Int64, Int64
    | Int32, Int32 
    | Nativeint, Nativeint
    | Double, Double  
    | Bigarray, Bigarray 
    | Abstract, Abstract 
    | Any, Any
    | NonValue, NonValue -> 0
    | _, _ ->  Stdlib.compare a b


  let rec pp_shape fmt s = 
    match s with 
    | NonValue -> Format.pp_print_string fmt "none"
    | Immediate -> Format.pp_print_string fmt "imm"
    | Block -> Format.pp_print_string fmt "block"
    | String -> Format.pp_print_string fmt "string"
    | Int64 -> Format.pp_print_string fmt "int64"
    | Int32 -> Format.pp_print_string fmt "int32"
    | Nativeint -> Format.pp_print_string fmt "nativeint"
    | Double -> Format.pp_print_string fmt "double"
    | Bigarray -> Format.pp_print_string fmt "bigarray"
    | Abstract -> Format.pp_print_string fmt "abstract"
    | Any -> Format.pp_print_string fmt "*"
    | Array t -> Format.fprintf fmt "array[%a]" pp_shape t
    | Tuple ts -> Format.pp_print_list pp_shape ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " × ") fmt ts 

  let to_string s = Format.asprintf "%a" pp_shape s

  let print printer b = pp_string printer (to_string b)

end



module Shapes
(* 
: LATTICE *)
= 
struct 

  module IntMap = Map.Make(Int)

  type shape_body = 
  { 
    immediate: bool;
    int64: bool;
    int32: bool; 
    string: bool;
    nativeint: bool;
    bigarray: bool;
    array: t option;
    block: t IntMap.t IntMap.t
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
    let int64 = s1.int64 || s2.int64 in 
    let int32 = s1.int32 || s2.int32 in 
    let string = s1.string || s2.string in 
    let nativeint = s1.nativeint || s2.nativeint in
    let bigarray = s1.bigarray || s2.bigarray in 
    let array = join_opt join s1.array s2.array in 
    let block = join_maps (join_maps join) s1.block s2.block in 
    { immediate; int64; int32; string; nativeint; bigarray; array; block }
  
  and join a b = 
    match a, b with
    | BOT, _ -> b
    | _, BOT -> a
    | TOP, _ -> TOP 
    | _, TOP -> TOP 
    | Nbt NonVal, Nbt NonVal -> Nbt NonVal
    | Nbt NonVal, _ -> TOP 
    | Nbt _, Nbt NonVal -> TOP
    | Nbt Any, Nbt _ -> Nbt Any (* shape and any *) 
    | Nbt _, Nbt Any -> Nbt Any (* shape and any *) 
    | Nbt (Shape s1), Nbt (Shape s2) -> Nbt (Shape (join_shape_bodies s1 s2))

  let rec meet_shape_bodies s1 s2 : shape_body = 
    let immediate = s1.immediate && s2.immediate in 
    let int64 = s1.int64 && s2.int64 in 
    let int32 = s1.int32 && s2.int32 in 
    let string = s1.string && s2.string in 
    let nativeint = s1.nativeint && s2.nativeint in
    let bigarray = s1.bigarray && s2.bigarray in 
    let array = meet_opt meet s1.array s2.array in 
    let block = meet_maps (meet_maps join) s1.block s2.block in 
    { immediate; int64; int32; string; nativeint; bigarray; array; block }
  
  and meet a b = 
    match a, b with
    | BOT, _ -> BOT
    | _, BOT -> BOT
    | TOP, _ -> b
    | _, TOP -> a 
    | Nbt NonVal, Nbt NonVal -> Nbt NonVal
    | Nbt NonVal, _ -> BOT 
    | Nbt _, Nbt NonVal -> BOT
    | Nbt Any, Nbt _ -> b (* shape and any *) 
    | Nbt _, Nbt Any -> a (* shape and any *) 
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
    subset_bool s1.bigarray s2.bigarray &&
    subset_opt subset s1.array s2.array &&
    subset_map (subset_map subset) s1.block s2.block
  and subset a b = 
    match a, b with 
    | BOT, _ -> true 
    | _, BOT -> false 
    | _, TOP -> true 
    | TOP, _ -> false 
    | Nbt NonVal, Nbt NonVal -> true 
    | Nbt NonVal, Nbt _ -> false 
    | Nbt _, Nbt NonVal -> false 
    | Nbt _, Nbt Any -> true 
    | Nbt Any, _ -> false 
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
    let array = match s.array with None -> [] | Some s -> [Format.asprintf "array(%a)" pp_shapes s] in 
    let block = if IntMap.cardinal s.block > 0 then [Format.asprintf "block(%a)" (pp_map (pp_map pp_shapes)) s.block] else [] in 
    let strings = List.concat [immediate;int64;int32;nativeint;string;bigarray;array;block] in 
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") Format.pp_print_string fmt strings 

  let print (printer: Print.printer) (x: t) : unit =
    Print.unformat pp_shapes printer x
    
  let to_string (x: t) : string =
    Format.asprintf "%a" pp_shapes x


end 