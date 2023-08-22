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