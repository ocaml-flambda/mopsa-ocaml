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

  let pp_user_pack_elm printer = function
    | Var v -> pp_string printer v
    | Function f -> pprint printer (fbox "%%%s" f)
    | FunctionVar (f,v) -> pprint printer (fbox "%%%s.%s" f v)
    | Resource r -> pprint printer (fbox "@@%s" r)

  let pp_user_pack printer pack =
    pp_list pp_user_pack_elm printer pack

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
      if Str.string_match (Str.regexp "^[A-Za-z_-].*$") s 0 then
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

  let symargs_pack_string = "@argv,@arg,argc,%main,%getopt_long,optind,%execvp,@arg#0,@arg#+1"

  let add_symargs_pack () =
    parse_user_pack symargs_pack_string

  let () = register_domain_option "c.memory.packing.static_scope"  {
      key      = "-c-pack-symargs";
      category = "Numeric";
      doc      = " create a user pack for variables related to symbolic arguments. This is equivalent to -c-pack=" ^ symargs_pack_string;
      spec    = ArgExt.Unit add_symargs_pack;
      default = "";

    }

  let opt_pack_only_stubs = ref false

  let () = register_domain_option "c.memory.packing.static_scope" {
      key      = "-c-pack-only-stub-initialization";
      category = "Numeric";
      doc      = " pack only during Mopsa's stub initialization";
      spec     = ArgExt.Set opt_pack_only_stubs;
      default  = string_of_bool !opt_pack_only_stubs;
    }

  let opt_pack_resources = ref false

  let () = register_domain_option "c.memory.packing.static_scope" {
      key      = "-c-pack-resources";
      category = "Numeric";
      doc      = " pack variables based on resources (such as dynamically allocated blocks)";
      spec     = ArgExt.Set opt_pack_resources;
      default  = string_of_bool !opt_pack_resources;
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
  let print printer = function
    | Globals  -> pp_string printer "[globals]"
    | Locals f -> pp_string printer f
    | User u  -> pp_user_pack printer u


  (** Initialization *)
  let init prog = ()


  (** Get the packs of a function *)
  let packs_of_function ?(user_only=false) f =
    let user =
      !opt_user_packs |>
      List.filter
        (fun pack ->
           List.exists (function
               | Var _ -> false
               | FunctionVar(ff,_) -> ff = f
               | Function ff -> ff = f
               | Resource _ -> false
             ) pack
        ) |>
      List.map (fun u -> User u) in
    if user_only then user else Locals f :: user

  (** Get the user-defined packs of a C variable *)
  let user_packs_of_cvar cvar =
    !opt_user_packs |>
    List.filter
      (fun pack ->
         List.exists (function
             | Var v -> v = cvar.cvar_orig_name
             | FunctionVar(f,v) ->
                 v = cvar.cvar_orig_name &&
                 ( match cvar.cvar_scope with
                   | Variable_local ff -> ff.c_func_org_name = f
                   | Variable_parameter ff -> ff.c_func_org_name = f
                   | _ -> false )
               | Function f ->
                 ( match cvar.cvar_scope with
                   | Variable_local ff -> ff.c_func_org_name = f
                   | Variable_parameter ff -> ff.c_func_org_name = f
                   | _ -> false )
               | Resource _ -> false
             ) pack
      ) |>
    List.map (fun u -> User u)


  (** Get the user-defined packs of a resource *)
  let user_packs_of_resource r =
    !opt_user_packs |>
    List.filter
      (fun pack ->
         List.exists (function
             | Resource rr -> r = rr
             | _ -> false
           ) pack
      ) |>
    List.map (fun u -> User u)


  let is_outside_stub_init cs =
    let is_range_in_mopsa_stubs range =
      let file = get_range_file range in
      try let _ = Str.search_forward (Str.regexp_string "share/mopsa/stubs/c/") file 0 in true
      with Not_found -> false in 
    (
      Callstack.callstack_length cs > 0 &&
      not (is_range_in_mopsa_stubs @@ (ListExt.last cs).call_range)
      (* not (is_range_in_mopsa_stubs range) *)
    )
  (** Get the packs of a base *)
  let rec packs_of_base ?(user_only=false) ctx b =
    (* Invalid bases are not packed *)
    if b.base_valid = false then [] else
    match b.base_kind with
    (* Global variables *)
    | Var ({ vkind = V_cvar ({cvar_scope = Variable_global} as cvar); vtyp })
      ->
      user_packs_of_cvar cvar @
      (if !opt_pack_only_stubs then [Globals] else [])

    (* Local temporary variables are not packed *)
    | Var { vkind = V_cvar {cvar_scope = Variable_local f; cvar_orig_name}; vtyp }
    | Var { vkind = V_cvar {cvar_scope = Variable_func_static f; cvar_orig_name}; vtyp }
      when cvar_orig_name = "__SAST_tmp" ->
      []

    (* Local scalar variables are packed in the function's pack *)
    | Var ({ vkind = V_cvar ({cvar_scope = Variable_local f} as cvar); vtyp })
    | Var ({ vkind = V_cvar ({cvar_scope = Variable_func_static f} as cvar); vtyp })
      ->
      packs_of_function ~user_only f.c_func_unique_name @
      user_packs_of_cvar cvar

    (* Formal scalar parameters are part of the caller and the callee packs *)
    | Var { vkind = V_cvar {cvar_scope = Variable_parameter f}; vtyp }
      when is_c_scalar_type vtyp
      ->
      let cs = find_ctx Context.callstack_ctx_key ctx in
      if is_empty_callstack cs
      then packs_of_function ~user_only f.c_func_unique_name
      else
        let _, cs' = pop_callstack cs in
        if is_empty_callstack cs'
        then packs_of_function ~user_only f.c_func_unique_name
        else
          let caller, _ = pop_callstack cs' in
          packs_of_function ~user_only f.c_func_unique_name @
          packs_of_function ~user_only caller.call_fun_uniq_name

    (* Parameters are part of the caller and the callee packs *)
    (* Subcase to avoid many distinctions, cf c/iterators/interproc.ml, function *)
    (*    eval_calls_in_args a *)
    | Var { vkind = V_range_attr(range, "arg"); vtyp }
      when is_c_scalar_type vtyp ->
      let prog = find_ctx Ast.c_program_ctx ctx in
      let o_caller = List.find_opt (fun f -> subset_range range f.c_func_range) prog.c_functions in
      OptionExt.apply (fun caller ->
          let o_callee = OptionExt.none_to_exn @@
            OptionExt.lift (Visitor.fold_stmt
                              (fun acc expr ->
                                 match ekind expr with
                                 | E_call (f, args) ->
                                   begin match ekind (remove_casts f) with
                                   | E_c_function f ->
                                     if subset_range range expr.erange then
                                       VisitParts (Some f)
                                     else
                                       VisitParts acc
                                   | _ -> VisitParts acc
                                   end
                                 | _ -> VisitParts acc)
                              (fun acc stmt -> VisitParts acc)
                              None
                           ) caller.c_func_body in
          (OptionExt.apply (fun callee ->
               packs_of_function ~user_only callee.c_func_unique_name) [] o_callee) @
          packs_of_function ~user_only caller.c_func_unique_name
        ) [] o_caller

    (* Return variables are also part of the caller and the callee packs *)
    | Var { vkind = Universal.Iterators.Interproc.Common.V_return (call, _) } ->
      let cs = find_ctx Context.callstack_ctx_key ctx in
      let r = if is_empty_callstack cs
      then []
      else
        (* Note that the top of the callstack is not always the callee
           function, because the return variable is used after the function
           returns
        *)
        let fname = match ekind call with
          | E_call ({ekind = E_function (User_defined f)},_) -> f.fun_uniq_name
          | Stubs.Ast.E_stub_call(f,_) -> f.stub_func_name
          | _ -> assert false
        in
        let f1, f2 =
          let rec process cs = match cs with
            | f :: f' :: tl -> if f'.call_fun_uniq_name = fname then Some f, Some f' else process (f'::tl)
            | [f] -> Some f, None 
            | [] -> assert false 
          in
          process (List.rev cs) in
        let () = debug "cs = %a@.fname = %s@.f1 = %a@.f2 = %a" Callstack.pp_callstack_short cs fname  (OptionExt.print (fun fmt x -> Format.pp_print_string fmt x.call_fun_uniq_name)) f1 (OptionExt.print (fun fmt x -> Format.pp_print_string fmt x.call_fun_uniq_name)) f2 in
        match f2, f1 with
        | None, None -> []
        | Some _, None -> assert false
        | None, Some f1 -> packs_of_function ~user_only f1.call_fun_uniq_name
        | Some f2, Some f1 ->
          packs_of_function ~user_only f1.call_fun_uniq_name @
          packs_of_function ~user_only f2.call_fun_uniq_name in
      r

    (* Primed bases are in the same pack as the original ones *)
    | Var { vkind = Cstubs.Aux_vars.V_c_primed_base b } ->
      packs_of_base ~user_only ctx b

    | Var ({ vkind = V_c_stack_var(_, v)}) ->
      packs_of_base ~user_only ctx {b with base_kind = Var v}

    (* Resource are put in the function scope corresponding to their allocation area *)
    (* | Addr { addr_kind = Stubs.Ast.A_stub_resource r; *)
    (*          addr_partitioning = Universal.Heap.Policies.G_range range } -> *)
    (*   let prog = find_ctx Ast.c_program_ctx ctx in *)
    (*   let pack = match List.find_opt (fun f -> subset_range range f.c_func_range) prog.c_functions with *)
    (*     | None -> *)
    (*       (\* let () = debug "putting %a %a in globals" pp_base b pp_range range in *\) *)
    (*       Globals *)
    (*     | Some f -> *)
    (*       (\* let () = debug "putting %a in %s" pp_base b f.c_func_unique_name in *\) *)
    (*       Locals f.c_func_unique_name *)
    (*   in *)
    (*   pack :: user_packs_of_resource r *)

    (* | Addr { addr_kind = Stubs.Ast.A_stub_resource r; *)
    (*          addr_partitioning = Universal.Heap.Policies.G_stack_range (cs, range) } -> *)
    (*   let prog = find_ctx Ast.c_program_ctx ctx in *)
    (*   let is_c_stub func = *)
    (*     not (List.exists (fun c -> c.c_func_unique_name = func && c.c_func_stub = None) prog.c_functions) in *)
    (*   let path_contains p1 p2 = *)
    (*     let l1 = String.split_on_char '/' p1 in *)
    (*     let l2 = String.split_on_char '/' p2 in *)
    (*     List.for_all (fun el1 -> *)
    (*         List.mem el1 l2 *)
    (*       ) l1 *)
    (*   in *)
    (*   let packs = match List.find_opt (fun f -> subset_range range f.c_func_range) prog.c_functions with *)
    (*     | None -> *)
    (*       if List.length cs > 0 && not @@ List.for_all (fun c -> is_c_stub c.call_fun_orig_name) cs then  *)
    (*         List.map (fun c -> Locals c.call_fun_uniq_name) cs *)
    (*       else [Globals] *)
    (*       (\* begin match List.find_opt (fun callsite -> *\) *)
    (*       (\*     not (path_contains "share/mopsa/stubs/c/" (Location.get_range_file callsite.call_range)) && *\) *)
    (*       (\*     not (is_c_stub callsite.call_fun_uniq_name)) cs with *\) *)
    (*       (\*   | None -> let () = debug "puntting %a in Globals" pp_base b in Globals *\) *)
    (*       (\*   | Some callsite -> *\) *)
    (*       (\*     let () = debug "puntting %a in %s" pp_base b callsite.call_fun_uniq_name in  *\) *)
    (*       (\*     Locals callsite.call_fun_uniq_name *\) *)
    (*       (\* end *\) *)
    (*     | Some f -> *)
    (*       if List.for_all (fun c -> is_c_stub c.call_fun_orig_name) cs then [] *)
    (*       else *)
    (*       [Locals f.c_func_unique_name] *)
    (*   in *)
    (*   packs @ user_packs_of_resource r  *)
        
    | Addr { addr_kind = Stubs.Ast.A_stub_resource r;
             addr_partitioning = Universal.Heap.Policies.G_stack_range (cs, range) } when !opt_pack_resources ->
      let prog = find_ctx Ast.c_program_ctx ctx in
      let is_c_stub func =
        not (List.exists (fun c -> c.c_func_unique_name = func && c.c_func_stub = None) prog.c_functions) in
      let _path_contains p1 p2 =
        let l1 = String.split_on_char '/' p1 in
        let l2 = String.split_on_char '/' p2 in
        List.for_all (fun el1 ->
            List.mem el1 l2
          ) l1
      in
      let packs = match List.find_opt (fun f -> subset_range range f.c_func_range) prog.c_functions with
        | None ->
          if List.length cs > 0 && not @@ List.for_all (fun c -> is_c_stub c.call_fun_orig_name) cs then
            List.map (fun c -> Locals c.call_fun_uniq_name) cs
          else [Globals]
        | Some f ->
          if List.for_all (fun c -> is_c_stub c.call_fun_orig_name) cs then []
          else
          [Locals f.c_func_unique_name]
      in
      packs @ user_packs_of_resource r

    | Addr { addr_kind = Stubs.Ast.A_stub_resource r; } ->
      user_packs_of_resource r

    | _ ->
      []

  (** Packing function returning packs of a variable *)
  let rec packs_of_var ctx v =
    if !opt_pack_only_stubs && is_outside_stub_init (find_ctx callstack_ctx_key ctx) then
      let () = debug "packs_of_var %a, globals due to pack_only_stubs" pp_var v in [Globals]
    else
    let () = debug "packs_of_var %a" pp_var v in
    match v.vkind with
    | Memory.Cells.Domain.Domain.V_c_cell ({base = { base_kind = Var v; base_valid = true}} as c) ->
      let user_only = not (is_c_scalar_type v.vtyp) in
      packs_of_base ~user_only ctx c.base

    | Memory.String_length.Domain.V_c_string_length (base,_) -> packs_of_base ctx base
    | Memory.Pointer_sentinel.Domain.V_c_sentinel (base) -> packs_of_base ctx base
    | Memory.Pointer_sentinel.Domain.V_c_sentinel_pos (base) -> packs_of_base ctx base
    | Memory.Pointer_sentinel.Domain.V_c_before_sentinel (base) -> packs_of_base ctx base
    | Memory.Smashing.Domain.V_c_uninit (base) -> packs_of_base ctx base
    | Memory.Pointers.Domain.Domain.V_c_ptr_offset vv -> packs_of_var ctx vv
    | Memory.Machine_numbers.Domain.V_c_num vv -> packs_of_var ctx vv
    | Memory.Variable_length_array.Domain.V_c_variable_length vv -> packs_of_var ctx vv
    | Cstubs.Aux_vars.V_c_bytes a -> packs_of_base ctx (mk_addr_base a)
    | Cstubs.Aux_vars.V_c_primed_base b -> packs_of_base ctx b
    | V_c_stack_var(_, vv) -> packs_of_var ctx vv
    | _ -> packs_of_base ctx (mk_var_base v)

end

(** Registration *)
let () =
  Universal.Packing.Static.register_strategy (module Strategy);
  Universal.Packing.Intervals_static_scope.register_itv_packing_reduction (module Strategy)
