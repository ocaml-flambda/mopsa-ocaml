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
open Universal.Packing.Static
open Universal.Ast
open Ast
open Common.Base



module Strategy =
struct

  (** {2 User-defined packs} *)
  (** ********************** *)

  type user_pack = user_pack_elm list

  and user_pack_elm =
    | Var of string
    | Function of string
    | FunctionVar of string * string
    | Resource of string

  let pp_user_pack_elm fmt = function
    | Var v -> Format.pp_print_string fmt v
    | Function f -> Format.fprintf fmt "$%s" f
    | FunctionVar (f,v) -> Format.fprintf fmt "$%s.%s" f v
    | Resource r -> Format.fprintf fmt "@@%s" r

  let pp_user_pack fmt pack =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") pp_user_pack_elm fmt pack

  let compare_user_pack_elem e1 e2 =
    match e1,e2 with
    | Var v1, Var v2 -> compare v1 v2
    | Function f1,Function f2 -> compare f1 f2
    | FunctionVar (f1,v1), FunctionVar (f2,v2) -> Compare.pair compare compare (f1,v1) (f2,v2)
    | Resource r1, Resource r2 -> compare r1 r2
    | _ -> compare e1 e2

  let compare_user_pack p1 p2 =
    Compare.list compare_user_pack_elem p1 p2

  (** Command-line option to define custom packs *)
  let opt_user_packs : user_pack list ref = ref []

  (** Parse a command-line pack definition as a list with elements of the form:
      - var
      - %function
      - %function.var
      - @resource 
  *)
  let parse_user_pack (s:string) : unit =
    (* Utility function to parse an identifier *)
    let ensure_is_id s =
      if Str.string_match (Str.regexp "^[A-Za-z].*$") s 0 then
        s
      else
        panic "incorrect argument '%s' for option -c-pack" s
    in
    (* Split using delimiter ',' *)
    let parts = Str.(split (regexp_string ",") s) in
    (* Look at first character *)
    let parts = List.map (fun part ->
        let n = String.length part in
        if n = 0 then panic "empty argument for option -c-pack";
        match part.[0] with
        | '@' -> Resource (String.sub part 1 (n-1) |> ensure_is_id)
        | '%' ->
          begin match Str.(split (regexp_string ",") (String.sub part 1 (n-1))) with
            | [f] -> Function (ensure_is_id f)
            | [f;v] -> FunctionVar (ensure_is_id f, ensure_is_id v)
            | _ -> panic "incorrect argument '%s' for option -c-pack" part
          end 
        | _ -> Var (ensure_is_id part)
      ) parts
    in
    if parts = [] then () else
    opt_user_packs := parts :: !opt_user_packs
    
  let () = register_domain_option "c.memory.packing.static_scope" {
      key      = "-c-pack";
      category = "Numeric";
      doc      = " create a pack of variables (syntax: var,%function,%function.var,@resource)";
      spec     = ArgExt.String parse_user_pack;
      default  = "";
    }


  (** {2 Packs} *)
  (** ********* *)

  (** Packing key *)
  type pack =
    | Globals             (** Pack of global variables *)
    | Locals of string    (** Pack of local variables of a function *)
    | User   of user_pack (** User-defined packs *)


  (** Generate a unique ID for the strategy *)
  include GenDomainId(struct
      type t = pack
      let name = "c.memory.packing.static_scope"
    end)


  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1 f2
    | User u1, User u2 -> compare_user_pack u1 u2
    | _ -> compare k1 k2


  (** Pretty printer of packing keys *)
  let print fmt = function
    | Globals  -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f
    | User u  -> Format.fprintf fmt "(%a)" pp_user_pack u


  (** Initialization *)
  let init prog = ()


  (** Return the list of user-defined packs of a given base *)
  let rec user_packs_of_base ctx b =
    match b.base_kind with
    | Var {vkind = V_cvar {cvar_orig_name; cvar_scope}} ->
      let packs = List.filter
          (fun pack ->
             List.exists (function
                 | Var v   -> v = cvar_orig_name
                 | FunctionVar(f,v) ->
                   v = cvar_orig_name &&
                   ( match cvar_scope with
                     | Variable_local ff -> ff.c_func_org_name = f
                     | Variable_parameter ff -> ff.c_func_org_name = f
                     | _ -> false )
                 | Function f ->
                   ( match cvar_scope with
                     | Variable_local ff -> ff.c_func_org_name = f
                     | Variable_parameter ff -> ff.c_func_org_name = f
                     | _ -> false )
                 | Resource _ -> false
               ) pack
          ) !opt_user_packs
      in
      List.map (fun u -> User u) packs

    | Addr { addr_kind = Stubs.Ast.A_stub_resource r } ->
      let packs = List.filter
          (fun pack ->
             List.exists (function
                 | Resource rr -> r = rr
                 | _ -> false
               ) pack
          ) !opt_user_packs
      in
      List.map (fun u -> User u) packs

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
      let cs = Context.ufind Context.callstack_ctx_key ctx in
      if is_empty_callstack cs
      then [Locals f.c_func_unique_name]
      else
        let _, cs' = pop_callstack cs in
        if is_empty_callstack cs'
        then [Locals f.c_func_unique_name]
        else
          let caller, _ = pop_callstack cs' in
          [Locals f.c_func_unique_name; Locals caller.call_fun_uniq_name]

    (* Return variables are also part of the caller and the callee packs *)
    | Var { vkind = Universal.Iterators.Interproc.Common.V_return call } ->
      let cs = Context.ufind Context.callstack_ctx_key ctx in
      if is_empty_callstack cs
      then []
      else
        (* Note that the top of the callstack is not always the callee
           function, because the return variable is used after the function
           returns
        *)
        let f1, cs' = pop_callstack cs in
        let fname = match ekind call with
          | E_call ({ekind = E_function (User_defined f)},_) -> f.fun_uniq_name
          | Stubs.Ast.E_stub_call(f,_) -> f.stub_func_name
          | _ -> assert false
        in
        if is_empty_callstack cs'
        then [Locals f1.call_fun_uniq_name]
        else if f1.call_fun_uniq_name <> fname
        then [Locals f1.call_fun_uniq_name]
        else
          let f2, _ = pop_callstack cs' in
          [Locals f1.call_fun_uniq_name; Locals f2.call_fun_uniq_name]

    (* Primed bases are in the same pack as the original ones *)
    | Var { vkind = Cstubs.Aux_vars.V_c_primed_base b } ->
      fixed_packs_of_base ctx b

    | _ -> []



  (** Packs of a base memory block *)
  let packs_of_base ctx b =
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
    | Memory.Lowlevel.Cells.Domain.V_c_cell ({base = {base_valid = true}} as c) -> user_packs_of_base ctx c.base
    | Memory.Lowlevel.String_length.Domain.V_c_string_length (base,_) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_sentinel (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_sentinel_pos (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Pointer_sentinel.Domain.V_c_before_sentinel (base) -> packs_of_base ctx base
    | Memory.Lowlevel.Smashing.Domain.V_c_uninit (base) -> packs_of_base ctx base
    | Memory.Scalars.Pointers.Domain.Domain.V_c_ptr_offset vv -> packs_of_var ctx vv
    | Memory.Scalars.Machine_numbers.Domain.V_c_num vv -> packs_of_var ctx vv
    | Cstubs.Aux_vars.V_c_bytes a -> packs_of_base ctx (mk_addr_base a)
    | Cstubs.Aux_vars.V_c_primed_base b -> packs_of_base ctx b
    | _ -> []

end

(** Registration *)
let () =
  Universal.Packing.Static.register_strategy (module Strategy)
