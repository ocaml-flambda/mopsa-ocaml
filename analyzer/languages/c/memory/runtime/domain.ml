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

(** Non-relational abstraction of C pointers *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open Ast
open Common
open Common.Points_to
open Common.Base
open Common.Alarms
open Value
open Stubs.Ast (* for the printing functions *)



module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Val = Framework.Lattices.Const.Make(Value)
  module Map = Framework.Lattices.Partial_map.Make(Var)(Val)

  type t = Map.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.runtime"
    end)

  let bottom = Map.bottom

  let top = Map.top

  let checks = [ ]

  (** {2 Lattice operators} *)
  (** ===================== *)

  let is_bottom = Map.is_bottom

  let subset = Map.subset

  let join = Map.join

  let meet = Map.meet

  let widen ctx = Map.join

  let merge pre (a,e) (a',e') =
    let aa,aa' =
      generic_merge (a,e) (a',e')
        ~add:Map.add ~find:Map.find ~remove:Map.remove
        ~custom:(fun stmt ->
            match skind stmt with
            | S_c_declaration (var,init,scope) ->
              Some Effect.{ modified = VarSet.singleton var; removed = VarSet.empty }

            | S_assign ({ekind = E_c_deref _},_)
            | S_forget ({ekind = E_c_deref _}) ->
              (* We can ignore theses statements because we transform then into statements
                 with variables, and we use the manager to execute them. Therefore, we are
                 sure that the statements with variables are logged in the effects. *)
              Some Effect.{ modified = VarSet.empty; removed = VarSet.empty }

            | _ -> None
          ) in
    Map.meet aa aa'


  let set p v a = Map.add p (Val.embed v) a
  let remove p a = Map.remove p a

  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur Map.empty man flow



  (** {2 Offset variables} *)
  (** ==================== *)


  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  let exec_add var man flow =
    let m = get_env T_cur man flow in
    let m' = set var Untracked m in
    let flow = set_env T_cur m' man flow in
    let () = Debug.debug ~channel:"runtime" "added %a" pp_var var in
    Some (Post.return flow) 

  let exec_remove var man flow =
    let m = get_env T_cur man flow in
    let m' = remove var m in
    let flow = set_env T_cur m' man flow in
    let () = Debug.debug ~channel:"runtime" "removed %a" pp_var var in
    Some (Post.return flow) 

  let exec_garbage_collect man flow =
    let m  = get_env T_cur man flow in 
    let upd_set s = if Val.is_const s Alive then Val.embed Collected else s in 
    let m' = Map.map (fun s -> upd_set s) m in
    let flow = set_env T_cur m' man flow in
    let () = Debug.debug ~channel:"runtime" "garbage collect" in
    Some (Post.return flow)



  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  (** Entry point of abstract transformers *)
  let exec stmt man flow = 
    match skind stmt with 
    | S_add { ekind = E_var (var, _) } -> exec_add var man flow      
    | S_remove { ekind = E_var (var, _) } -> exec_remove var man flow
    | S_forget { ekind = E_var (var, _) } -> exec_remove var man flow
    | S_rename ({ ekind = E_var (from, _) }, { ekind = E_var (into, _) }) -> (Debug.debug ~channel:"runtime" "attempt rename %a into %a" pp_var from pp_var into; None)
    | S_c_garbage_collect -> exec_garbage_collect man flow
    | S_assign ({ ekind= E_var (var, mode) }, e) ->
      let () = Debug.debug ~channel:"runtime" "assigning %a = %a" pp_var var pp_expr e in 
      None
    | _ -> None


  (** {2 Pointer evaluation} *)
  (** ====================== *)


  (** Entry point of abstraction evaluations *)
  let eval exp man flow = None

  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask : type a r. (a,r) query -> (a,t) man -> a flow -> r option = fun query man flow -> None


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_state printer a =
    pprint ~path:[Key "runtime"] printer (pbox Map.print a)



  let print_expr man flow printer exp =
    match ekind (remove_casts exp) with
    | E_var (var,_) when is_c_pointer_type var.vtyp
                      && not (is_c_array_type var.vtyp) ->
      let a = get_env T_cur man flow in
      let v = Map.find var a in 
      let po : print_object = String (Val.to_string v) in
        pprint printer ~path:[ Key "runtime"; fkey "%a" pp_var var ] po
    | _ -> ()

end

let () =
  register_standard_domain (module Domain)
