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

(** Simple packing strategy based on static scoping of C variables.

    The idea is simple: global variables are kept in one pack and each function
    has its own pack for its local variables. To preserve relations between 
    the call arguments and the returned value, the packs may overlap: 
    the formal parameters and the return variable are kept in the caller and
    callee packs.
*)

open Mopsa
open Sig.Domain.Simplified
open Universal.Packing.Static
open Universal.Ast
open Ast
open Common.Base



module Strategy =
struct

  (** Command-line option to define custom packs *)
  let opt_user_packs : (string option*string) list list ref = ref []

  (** Parse a command-line pack definition using the syntax [f1.]<v1>,[f2.]<v2>,... *)
  let parse_user_pack (s:string) : unit =
    (* Split using delimiter ',' *)
    let parts = Str.(split (regexp_string ",") s) in
    (* Split each part using delimiter '.' *)
    let parts = List.map (fun part ->
        match Str.(split (regexp_string ".") part) with
        | [v] -> (None,v)
        | [f;v] -> (Some f,v)
        | _ -> panic "incorrect argument for option -pack"
      ) parts
    in
    if parts = [] then () else
    opt_user_packs := parts :: !opt_user_packs
    
  let () = register_domain_option "c.memory.packing.static_scope" {
      key      = "-c-pack";
      category = "Numeric";
      doc      = " create a pack of variables (syntax: [f1.]v1,[f2.]v2,...)";
      spec     = ArgExt.String parse_user_pack;
      default  = "";
    }
      

  (** Packing key *)
  type pack =
    | Globals             (** Pack of global variables *)
    | Locals of string    (** Pack of local variables of a function *)
    | User   of ( string option (** Function *) *
                  string        (** Variable*)
                ) list (** User defined custom packs *)


  (** Generate a unique ID for the strategy *)
  include GenId(struct
      type t = pack
      let name = "c.memory.packing.static_scope"
    end)


  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1 f2
    | User vl1, User vl2 -> Compare.list (Compare.pair (Compare.option compare) compare) vl1 vl2
    | _ -> compare k1 k2


  let pp_user_pack_vars fmt vl =
    Format.(pp_print_list
              ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
              (fun fmt -> function
                 | (None,v)   -> pp_print_string fmt v
                 | (Some f,v) -> fprintf fmt "%s.%s" f v
              )
           ) fmt vl


  (** Pretty printer of packing keys *)
  let print fmt = function
    | Globals  -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f
    | User vl  -> Format.fprintf fmt "(%a)" pp_user_pack_vars vl


  (** Initialization *)
  let init prog = ()


  (** Return the list of user-defined packs of a given base *)
  let rec user_packs_of_base ctx b =
    match b.base_kind with
    | Var {vkind = V_cvar {cvar_orig_name; cvar_scope}} ->
      let packs = List.filter
          (fun pack ->
             List.exists (function
                 | (None,v)   -> v = cvar_orig_name
                 | (Some f,v) ->
                   v = cvar_orig_name &&
                   ( match cvar_scope with
                     | Variable_local ff -> ff.c_func_org_name = f
                     | _ -> false )
               ) pack
          ) !opt_user_packs
      in
      List.map (fun vl -> User vl) packs

    | Var { vkind = Cstubs.Aux_vars.V_c_primed_base b } ->
      user_packs_of_base ctx b
 
    | _ -> []


  (** Return the list of fixed packs (Global of Local) of a given base *)
  let rec fixed_packs_of_base ctx b =
    match b.base_kind with
    (* Local temporary variables are not packed *)
    | Var { vkind = V_cvar {cvar_scope = Variable_local f; cvar_orig_name}; vtyp }
    | Var { vkind = V_cvar {cvar_scope = Variable_func_static f; cvar_orig_name}; vtyp }
      when cvar_orig_name = "__SAST_tmp" ->
      []

    (* Local scalar variables are packed in the function's pack *)
    | Var { vkind = V_cvar {cvar_scope = Variable_local f}; vtyp }
    | Var { vkind = V_cvar {cvar_scope = Variable_func_static f}; vtyp }
      ->
      [Locals f.c_func_unique_name]

    (* Formal scalar parameters are part of the caller and the callee packs *)
    | Var { vkind = V_cvar {cvar_scope = Variable_parameter f}; vtyp }
      when is_c_scalar_type vtyp
      ->
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs
      then [Locals f.c_func_unique_name]
      else
        let _, cs' = Callstack.pop cs in
        if Callstack.is_empty cs'
        then [Locals f.c_func_unique_name]
        else
          let caller, _ = Callstack.pop cs' in
          [Locals f.c_func_unique_name; Locals caller.call_fun_uniq_name]

    (* Return variables are also part of the caller and the callee packs *)
    | Var { vkind = Universal.Iterators.Interproc.Common.V_return call } ->
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs
      then []
      else
        (* Note that the top of the callstack is not always the callee
           function, because the return variable is used after the function
           returns
        *)
        let f1, cs' = Callstack.pop cs in
        let fname = match ekind call with
          | E_call ({ekind = E_function (User_defined f)},_) -> f.fun_uniq_name
          | Stubs.Ast.E_stub_call(f,_) -> f.stub_func_name
          | _ -> assert false
        in
        if Callstack.is_empty cs'
        then [Locals f1.call_fun_uniq_name]
        else if f1.call_fun_uniq_name <> fname
        then [Locals f1.call_fun_uniq_name]
        else
          let f2, _ = Callstack.pop cs' in
          [Locals f1.call_fun_uniq_name; Locals f2.call_fun_uniq_name]

    (* Primed bases are in the same pack as the original ones *)
    | Var { vkind = Cstubs.Aux_vars.V_c_primed_base b } ->
      fixed_packs_of_base ctx b

    | _ -> []



  (** Packs of a base memory block *)
  let rec packs_of_base ctx b =
    (* Invalid bases are not packed *)
    if b.base_valid = false then []
    else
      let user_packs = user_packs_of_base ctx b in
      let fixed_packs = fixed_packs_of_base ctx b in
      user_packs @ fixed_packs



  (** Packing function returning packs of a variable *)
  let rec packs_of_var ctx v =
    match v.vkind with
    | V_cvar _ -> packs_of_base ctx (mk_var_base v)
    | Memory.Lowlevel.Cells.Domain.V_c_cell ({base = { base_kind = Var v; base_valid = true}} as c) ->
      if not (is_c_scalar_type v.vtyp) then user_packs_of_base ctx c.base else packs_of_base ctx c.base
    | Memory.Lowlevel.String_length.Domain.V_c_string_length (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_sentinel (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_at_sentinel (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_before_sentinel (base) -> packs_of_base ctx base
    | Memory.Scalars.Pointers.Domain.Domain.V_c_ptr_offset vv -> packs_of_var ctx vv
    | Memory.Scalars.Machine_numbers.Domain.V_c_num vv -> packs_of_var ctx vv
    | Cstubs.Aux_vars.V_c_bytes a -> packs_of_base ctx (mk_addr_base a)
    | Cstubs.Aux_vars.V_c_primed_base b -> packs_of_base ctx b
    | _ -> []

end

(** Registration *)
let () =
  Universal.Packing.Static.register_strategy (module Strategy)
