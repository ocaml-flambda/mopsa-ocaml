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

(** Smashing abstraction of memory blocks *)

open Mopsa
open Core.Sig.Stacked.Intermediate
open Universal.Ast
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to


module Domain =
struct

  (** {2 Smashed blocks} *)
  (** ****************** *)

  (** A smashed block is described by a scalar type and a base memory
      block (e.g.  a program variable, an address of a heap block,
      etc.)  
  *)
  type smash = {
    base : base;
    typ  : typ;
  }

  (** Total order of smashed blocks *)
  let compare_smash s1 s2 =
    Compare.compose [
      (fun () -> compare_base s1.base s2.base);
      (fun () -> compare_typ s1.typ s2.typ);
    ]


  (** Pretty printer of smashed blocks *)
  let pp_smash fmt s =
    Format.fprintf fmt "[%a:*:%a]"
      pp_base s.base
      Pp.pp_c_type_short (remove_qual s.typ)


  (** Create a smash from a base and a type *)
  let mk_smash base typ =
    { base; typ = remove_typedef_qual typ }



  (** {2 Smash variables} *)
  (** ******************* *)

  type var_kind +=
    | V_c_smash of smash

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_smash s -> pp_smash fmt s
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_smash s1, V_c_smash s2 -> compare_smash s1 s2
          | _ -> next v1 v2
        );
    }


  (** Create a scalar variable from a smash *)
  let mk_smash_var (s:smash) : var =
    let name =
      let () = pp_smash Format.str_formatter s in
      Format.flush_str_formatter ()
    in
    mkv name (V_c_smash s) s.typ


  
  (** {2 Domain header} *)
  (** ***************** *)

  (** Set of memory smashes *)
  module SmashSet = Framework.Lattices.Powerset.Make(struct
      type t = smash
      let compare = compare_smash
      let print = pp_smash
    end)


  (** Set of declared bases. Needed during unification to determine whether a
      missing cell belongs to an optional base 
  *)
  module BaseSet = Framework.Lattices.Powerset.Make(Base)


  (** Abstract state *)
  type t = {
    smashes: SmashSet.t;
    bases: BaseSet.t;
  }

  let bottom = {
    smashes = SmashSet.bottom;
    bases = BaseSet.bottom;
  }

  let top = {
    smashes = SmashSet.top;
    bases = BaseSet.top;
  }

  let print fmt a =
    Format.fprintf fmt "smashes: @[%a@]@\n"
      SmashSet.print a.smashes


  (** Domain identifier *)
  include GenDomainId(struct
      type typ = t
      let name = "c.memory.lowlevel.smashing"
    end)


  (** Zone interface *)
  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [Z_c_scalar];
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        (Z_c_low_level, Z_c_scalar);
        (Z_c_low_level, Z_c_points_to);
      ];
    }
  }


  (** {2 Unification} *)
  (** *************** *)

  (** [phi c a range] returns a constraint expression over cell [c] found in [a] *)
  let phi (c:smash) (a:t) range : expr option =
    if is_c_int_type c.typ then
      let a,b = rangeof c.typ in
      Some (mk_z_interval a b range)

    else
    if is_c_float_type c.typ then
      let prec = get_c_float_precision c.typ in
      Some (mk_top (T_float prec) range)

    else
      None


  (** Add a smashed cell to the underlying scalar domain *)
  let add_smash c a range man ctx s =
    (* Do nothing if c is already known or if it is an optional cell
       (i.e. its base is not declared) 
    *) 
    if SmashSet.mem c a.smashes || not (BaseSet.mem c.base a.bases)
    then s

    (* Otherwise add numeric constraints about the cell value if
       overlapping cells already exist 
    *)
    else
      let v = mk_smash_var c in
      let s' = man.sexec ~zone:Z_c_scalar (mk_add (mk_var v range) range) ctx s in
      if is_c_pointer_type c.typ then s'
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
          man.sexec ~zone:Z_c_scalar stmt ctx s'

        | None ->
          s'


  (** Unify the support of two abstract elements by adding missing cells *)
  let unify man ctx (a,s) (a',s') =
    if a == a' then s, s' else
    if SmashSet.is_empty a.smashes  then s, s' else
    if SmashSet.is_empty a'.smashes then s, s'
    else
      try
        let range = mk_fresh_range () in
        let diff' = SmashSet.diff a.smashes a'.smashes in
        let diff = SmashSet.diff a'.smashes a.smashes in
        SmashSet.fold (fun c s ->
            add_smash c a range man ctx s
          ) diff s
        ,
        SmashSet.fold (fun c s' ->
            add_smash c a' range man ctx s'
          ) diff' s'
      with Top.Found_TOP ->
        s, s'



  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom a = false


  let subset man ctx (a,s) (a',s') =
    let s, s' = unify man ctx (a, s) (a', s') in
    (true, s, s')


  let join man ctx (a,s) (a',s') =
    let s, s' = unify man ctx (a,s) (a',s') in
    let a = {
      smashes = SmashSet.join a.smashes a'.smashes;
      bases = BaseSet.join a.bases a'.bases;
    }
    in
    (a, s, s')


  let meet man ctx (a,s) (a',s') =
    join man ctx (a,s) (a',s')


  let widen man ctx (a,s) (a',s') =
    let (a, s, s') = join man ctx (a,s) (a',s') in
    (a, s, s', true)


  let merge ctx pre (a,log) (a',log') =
    assert false



  (** {2 Initial state} *)
  (** ***************** *)

  let init prog man flow =
    set_domain_env T_cur {
      smashes = SmashSet.empty;
      bases = BaseSet.empty
    } man flow



  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Add a base to the set of declared blocks *)
  let add_base base range man flow =
    let a = get_domain_env T_cur man flow in
    let aa = { a with bases = BaseSet.add base a.bases } in
    set_domain_env T_cur aa man flow



  (** Declaration of a C variable *)
  let declare_variable v init scope range man flow =
    (* Add the variable as a declared base *)
    let flow = add_base (V v) range man flow in

    (* Get the flat initialization generated by the structured domain *)
    let flat_init = match init with
      | Some (C_init_flat l) -> l
      | _ -> assert false
    in

    (* For the moment, we support only initialization with the same scalar type *)
    let rec collect_homogenous_init = function
      | [] -> Some (T_any, [])

      | C_flat_expr(e, typ) :: tl
      | C_flat_fill(e,typ,_) :: tl ->
        collect_homogenous_init tl |>
        Option.bind (fun (typ',acc) ->
            if typ' = T_any ||
               compare_typ typ typ' = 0
            then Some (typ, (Some (C_init_expr(e)) :: acc))
            else None
          )

      | C_flat_none _ :: tl ->
        collect_homogenous_init tl |>
        Option.bind (fun (typ',acc) ->
            if typ' = T_any ||
               compare_typ u8 typ' = 0
            then Some (u8, (None :: acc))
            else None
          )
    in

    let init_list = collect_homogenous_init flat_init in

    match init_list with
    | None -> Post.return flow

    | Some (typ, el) ->
      let c = mk_smash (V v) typ in
      let flow = map_domain_env T_cur (fun a ->
          { a with smashes = SmashSet.add c a.smashes }
        ) man flow
      in

      let cv = mk_smash_var c in
      (* Iterate over initialization expressions.
      *)
      let rec aux = function
        | [] -> Post.return flow

        | init :: tl ->
          let stmt = mk_c_declaration cv init scope range in
          man.exec_sub stmt flow |>
          Post.join (aux tl)
      in
      aux el

    


  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    assert false


  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ type v = init; ⟧ *)      
    | S_c_declaration (v,init,scope) when not (is_c_scalar_type v.vtyp) ->
      declare_variable v init scope stmt.srange man flow |>
      Option.return

    (* 𝕊⟦ add var; ⟧ *)      
    | S_add { ekind = E_var (v, _) } when not (is_c_scalar_type v.vtyp) ->
      add_base (V v) stmt.srange man flow |>
      Post.return |>
      Option.return

    (* 𝕊⟦ add @addr; ⟧ *)      
    | S_add { ekind = E_addr addr } ->
      add_base (A addr) stmt.srange man flow |>
      Post.return |>
      Option.return

    (* 𝕊⟦ *p = rval; ⟧ *)      
    | S_assign({ ekind = E_c_deref p}, rval) ->
      assign_deref p rval stmt.srange man flow |>
      Option.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Abstract evaluation of a dereference *)
  let eval_deref p range man flow =
    assert false

  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ *p ⟧ *)      
    | E_c_deref p when is_c_num_type exp.etyp ->
      eval_deref p exp.erange man flow |>
      Option.return

    | _ -> None



  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask query man flow = None

  let refine channel man flow = Channel.return flow

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
