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

(** Program variables. *)


open Typ



(*========================================================================*)
(**                     {2 Definition of variables}                       *)
(*========================================================================*)



(** Languages can extend this type to add new kinds of variables *)
type var_kind = ..

(** Access mode of a variable *)
type mode =
  | WEAK   (** Weak variables represent multiple concrete variables *)
  | STRONG (** Strong variables represent a single concrete variable *)
(* /!\ declaration order is important: it allows strong objects to be
   iterated on before weak objects, especially useful in the recency
   with garbage collection *)

(** Program variables *)
type var = {
  vname : string;     (** unique variable name *)
  vkind : var_kind;   (** language-dependent info on the variable *)
  vtyp  : typ;        (** type of the variable *)
  vmode : mode;       (** access mode of the variable *)
}


(** Accessor functions *)
let vname v = v.vname
let vkind v = v.vkind
let vtyp  v = v.vtyp
let vmode v = v.vmode

(** Create a variable with a name, a kind and a type *)
let mkv name kind ?(mode=STRONG) typ =
  {vname = name; vkind = kind; vtyp = typ; vmode = mode }


(** Internal pretty printer chain over variable kinds *)
let var_pp_chain = TypeExt.mk_print_chain (fun fmt v ->
    Format.pp_print_string fmt v.vname
  )


let pp_mode fmt = function
  | STRONG -> Format.fprintf fmt "STRONG"
  | WEAK   -> Format.fprintf fmt "WEAK"


(** Pretty printer of variables *)
let pp_var fmt v = TypeExt.print var_pp_chain fmt v


(** Internal compare chain over variable kinds *)
let var_compare_chain = TypeExt.mk_compare_chain (fun v1 v2 ->
    Stdlib.compare v1.vkind v2.vkind
  )

let compare_mode (m1:mode) (m2:mode) = compare m1 m2

(** Total order between variables *)
let compare_var v1 v2 =
  if v1 == v2 then 0
  else Compare.compose [
      (fun () -> TypeExt.compare var_compare_chain v1 v2);
      (fun () -> Stdlib.compare v1.vname v2.vname);
      (fun () -> compare_typ v1.vtyp v2.vtyp);
      (fun () -> compare_mode v1.vmode v2.vmode);
    ]


(** Register a new kind of variables *)
let register_var info =
  TypeExt.register info var_compare_chain var_pp_chain


(** Variables as ordered elements, useful for maps *)
module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end



(*========================================================================*)
(**                  {2 Generation of fresh variables}                    *)
(*========================================================================*)


(** Internal counter for fresh variables *)
let vcounter = ref 0


(** Create a fresh variable. Function [f] is given a fresh and unique
    identifier. It should return a unique name and a variable kind
*)
let mkfresh (f:int -> string * var_kind) typ () =
  incr vcounter;
  let name, kind = f !vcounter in
  mkv name kind typ


(** Temporary fixes for frontends *)
let start_vcounter_at (d:int) : unit =
  assert (!vcounter <= d);
  vcounter := d

let get_vcounter_val () = !vcounter



(*========================================================================*)
(**                     {2 Common kinds of variables}                     *)
(*========================================================================*)

type var_kind +=

  (** Variables identified by their unique id *)
  | V_uniq of string (** Original name *) *
              int    (** Unique ID *)

  (** Temporary variables *)
  | V_tmp of int (** Unique ID *)

  (** Attribute attached to a variable *)
  | V_var_attr of var    (** Attach variable *) *
                  string (** Attribute *)

  (** Attribute attached to a range of program location *)
  | V_range_attr of Location.range (** Attach range *) *
                    string (** Attribute *)


(** Create a fresh temporary variable *)
let mk_uniq_var orig uid typ =
  let name = orig ^ ":" ^ (string_of_int uid) in
  mkv name (V_uniq (orig, uid)) typ


(** Create a fresh variable with a unique ID *)
let mk_fresh_uniq_var orig typ () =
  mkfresh (fun uid ->
      let name = orig ^ ":" ^ (string_of_int uid) in
      name, (V_uniq (orig, uid))
    ) typ ()


(** Create a fresh temporary variable *)
let mktmp ?(typ=T_any) () =
  mkfresh (fun uid ->
      let name = "$tmp" ^ (string_of_int uid) in
      name, V_tmp uid
    ) typ ()


(** Create a variable attribute *)
let mk_attr_var v attr typ =
  let name = v.vname ^ "." ^ attr in
  mkv name (V_var_attr (v, attr)) typ


(** Create a program range attribute *)
let mk_range_attr_var range attr typ =
  let name = Format.asprintf "%a.%s" Location.pp_range range attr in
  mkv name (V_range_attr (range, attr)) typ


(** Return the original name of variables with UIDs *)
let get_orig_vname ?(warn=true) v =
  match v.vkind with
  | V_uniq (orig,_) -> orig
  | _ ->
    if warn then Exceptions.warn "variable %a does not have an original name" pp_var v;
    v.vname

(** Change the original name of variables with UIDs *)
let set_orig_vname name v =
  let uid = match v.vkind with V_uniq (_,uid) -> uid | _ -> assert false in
  mk_uniq_var name uid v.vtyp


(** Return the weakest mode between m1 and m2 *)
let weakest_mode m1 m2 =
  match m1, m2 with
  | STRONG, STRONG -> STRONG
  | _              -> WEAK

(** Registration of the common variable kinds *)
let () =
  register_var {
    compare = (fun next v1 v2 ->
        match vkind v1, vkind v2 with
        | V_uniq (orig,uid), V_uniq (orig',uid') ->
          Stdlib.compare uid uid'

        | V_tmp (uid), V_tmp (uid') ->
          Stdlib.compare uid uid'

        | V_var_attr (v,attr), V_var_attr (v',attr') ->
          Compare.compose [
            (fun () -> compare attr attr');
            (fun () -> compare_var v v');
          ]


        | V_range_attr (r,attr), V_range_attr (r',attr') ->
          Compare.compose [
            (fun () -> compare attr attr');
            (fun () -> Location.compare_range r r');
          ]

        | _ -> next v1 v2
      );
    print = (fun next fmt v ->
        match vkind v with
        | V_uniq (orig,_) -> Format.fprintf fmt "%s" orig

        | V_tmp _ | V_var_attr _ | V_range_attr _ -> Format.pp_print_string fmt v.vname

        | _ -> next fmt v
      )
  }


(*========================================================================*)
(**                         {2 Utility modules}                           *)
(*========================================================================*)

module VarSet = SetExt.Make(struct type t = var let compare = compare_var end)
module VarMap = MapExt.Make(struct type t = var let compare = compare_var end)
