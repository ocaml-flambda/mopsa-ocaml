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
    base   : base;
    typ    : typ;
    primed : bool;
  }

  (** Total order of smashed blocks *)
  let compare_smash s1 s2 =
    Compare.compose [
      (fun () -> compare_base s1.base s2.base);
      (fun () -> compare_typ s1.typ s2.typ);
      (fun () -> compare s1.primed s2.primed);
    ]


  (** Pretty printer of smashed blocks *)
  let pp_smash fmt s =
    Format.fprintf fmt "[%a:*:%a]%s"
      pp_base s.base
      Pp.pp_c_type_short (remove_qual s.typ)
      (if s.primed then "'" else "")


  (** Create a smash from a base and a type *)
  let mk_smash base ?(primed=false) typ =
    { base; typ = remove_typedef_qual typ; primed }



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
  let add_smash_sub c a range man ctx s =
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

      if is_c_pointer_type c.typ
      then s'

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
            add_smash_sub c a range man ctx s
          ) diff s
        ,
        SmashSet.fold (fun c s' ->
            add_smash_sub c a' range man ctx s'
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




  (** {2 Smashing of pointer dereferences} *)
  (** ************************************ *)

  type deref =
    | Smash of smash
    | Top of typ


  (** Evaluate a pointer deref into a smashed cell *)
  let deref p range man flow : (deref,'a) eval =
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_alarm Alarms.ANullDeref p.erange ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to P_invalid ->
      raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man.lattice flow |>
      Eval.empty_singleton

    | E_c_points_to (P_block (base, offset)) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->

      eval_base_size base range man flow |>
      Eval.bind @@ fun size flow ->

      man.eval ~zone:(Z_c_scalar, Z_u_num) size flow |>
      Eval.bind @@ fun size flow ->

      let typ = under_type p.etyp in
      let elm_size = sizeof_type typ in

      (* Check that offset ∈ [0, size - elm_size] *)
      assume_eval (mk_in offset (mk_zero range) (sub size (mk_z elm_size range) range) range)
        ~fthen:(fun flow ->
            (* Do not handle scalar bases *)
            match base with
            | S _ ->
              Eval.singleton (Top typ) flow

            | V v when is_c_scalar_type v.vtyp ->
              Eval.singleton (Top typ) flow

            | _ ->
              let c = mk_smash base typ in
              Eval.singleton (Smash c) flow
          )
        ~felse:(fun flow ->
            (* Unsafe case *)
            raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow |>
            Eval.empty_singleton
          )
        ~zone:Z_u_num man flow


    | _ -> assert false


  

  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Add a base to the set of declared blocks *)
  let add_base base range man flow =
    let flow = map_domain_env T_cur (fun a ->
        { a with bases = BaseSet.add base a.bases }
      ) man flow
    in
    
    let c = mk_smash base u8 |>
            mk_smash_var
    in
    man.exec_sub ~zone:Z_u_num (mk_add_var c range) flow


  let add_smash c range man flow =
    let a = get_domain_env T_cur man flow in

    if SmashSet.mem c a.smashes || not (is_c_scalar_type c.typ)
    then Post.return flow
    else
      let v = mk_smash_var c in
      man.exec_sub ~zone:Z_c_scalar (mk_add (mk_var v range) range) flow |>
      Post.bind @@ fun flow ->

      if is_c_pointer_type c.typ
      then
        set_domain_env T_cur { a with smashes = SmashSet.add c a.smashes } man flow |>
        Post.return
      else
        begin
          match phi c a range with
          | Some e ->
            let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
            man.exec_sub ~zone:Z_c_scalar stmt flow

          | None -> Post.return flow
        end
        |>
        Post.bind @@ fun flow ->
        set_domain_env T_cur { a with smashes = SmashSet.add c a.smashes } man flow |>
        Post.return


  (** Declaration of a C variable *)
  let declare_variable v init scope range man flow =
    (* Add the variable as a declared base *)
    let flow = map_domain_env T_cur (fun a ->
        { a with bases = BaseSet.add (V v) a.bases }
      ) man flow
    in

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

      | C_flat_none (n,typ) :: tl ->
        collect_homogenous_init tl |>
        Option.bind (fun (typ',acc) ->
            if typ' = T_any ||
               compare_typ typ typ' = 0
            then Some (typ, (None :: acc))
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

      (* Iterate over initialization expressions *)
      let cv = mk_smash_var c in
      let rec aux = function
        | [] -> Post.return flow

        | init :: tl ->
          let evl =
            match init with
            | Some (C_init_expr e) ->
              man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow |>
              Eval.bind @@ fun e flow ->

              Eval.singleton (Some (C_init_expr e)) flow

            | _ ->
              Eval.singleton init flow
          in
          
          evl |> post_eval man @@ fun init flow ->
            
          let stmt = mk_c_declaration cv init scope range in
          man.exec_sub ~zone:Z_c_scalar stmt flow |>
          Post.join (aux tl)
      in
      aux el

  (* Remove a cell and its associated scalar variable *)
  let remove_smash c range man flow =
    let flow = map_domain_env T_cur (fun a ->
        { a with smashes = SmashSet.remove c a.smashes }
      ) man flow
    in
    let v = mk_smash_var c in
    let stmt = mk_remove_var v range in
    man.exec_sub ~zone:Z_c_scalar stmt flow


  let remove_overlappings c range man flow =
    let a = get_domain_env T_cur man flow in
    SmashSet.fold (fun c' acc ->
        if compare_base c.base c'.base = 0 &&
           compare_typ c.typ c'.typ <> 0
        then
          Post.bind (remove_smash c' range man) acc
        else
          acc
      ) a.smashes (Post.return flow)

  
  let assign_smash c rval range man flow =
    let cv = mk_smash_var c in
    add_smash c range man flow |>
    Post.bind @@ fun flow ->

    let stmt = mk_assign (mk_var cv ~mode:WEAK range) rval range in
    man.exec_sub ~zone:Z_c_scalar stmt flow |>
    Post.bind @@ fun flow ->

    remove_overlappings c range man flow



  (** Assignment abstract transformer for 𝕊⟦ *p = rval; ⟧ *)
  let assign_deref p rval range man flow =
    deref p range man flow |>
    post_eval man @@ fun deref flow ->

    man.eval rval ~zone:(Z_c_low_level,Z_c_scalar) flow |>
    post_eval man @@ fun rval flow ->

    match deref with
    | Top _ -> Post.return flow

    | Smash c ->
      match c.base with
      | S _ ->
        Post.return flow

      | V v when is_c_scalar_type v.vtyp ->
        Post.return flow

      | _ ->
        assign_smash c rval range man flow


  let rename_smash oldc newc range man flow =
    (* Remove the old cell and add the new one *)
    let flow =
      map_domain_env T_cur (fun a ->
          { a with smashes = SmashSet.remove oldc a.smashes |>
                             SmashSet.add newc
          }
        ) man flow
    in

    let oldv = mk_smash_var oldc in
    let newv = mk_smash_var newc in
    let stmt = mk_rename_var oldv newv range in
    man.exec_sub ~zone:Z_c_scalar stmt flow


  (** Rename bases and their smash *)
  let rename_base base1 base2 range man flow =
    let a = get_domain_env T_cur man flow in
    (* Smashes of base1 *)
    let smashes1 = SmashSet.filter (fun c ->
        compare_base c.base base1 = 0
      ) a.smashes
    in

    (* Cell renaming *)
    let to_base2 c = { c with base = base2 } in

    (* Content copy, depends on the presence of base2 *)
    let copy =
      if not (BaseSet.mem base2 a.bases) then
        (* If base2 is not already present => rename the cells *)
        fun c flow ->
          rename_smash c (to_base2 c) range man flow
      else
        (* Otherwise, assign with weak update *)
        fun c flow ->
          let v = mk_smash_var c in
          assign_smash (to_base2 c) (mk_var v range) range man flow |>
          Post.bind @@ remove_smash c range man
    in

    (* Apply copy function *)
    SmashSet.fold (fun c acc -> Post.bind (copy c) acc) smashes1 (Post.return flow) |>
    Post.bind @@ fun flow ->

    (* Remove base1 and add base2 *)
    map_domain_env T_cur (fun a ->
        {
          a with
          bases = BaseSet.remove base1 a.bases |>
                  BaseSet.add base2;
        }
      ) man flow
    |>
    Post.return



  let remove_base base range man flow =
    let a = get_domain_env T_cur man flow in
    let flow = set_domain_env T_cur { a with bases = BaseSet.remove base a.bases } man flow in
    try SmashSet.fold (fun c acc ->
        if compare_base c.base base = 0 then
          Post.bind (remove_smash c range man) acc
        else
          acc
      ) a.smashes (Post.return flow)
    with Top.Found_TOP -> Post.return flow



  (** Rename primed smashes into unprimed ones *)
  let rename_primed target offsets range man flow =
    (* target is pointer, so resolve it and compute the affected offsets *)
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) target flow |>
    post_eval man @@ fun pt flow ->
    match ekind pt with
    | E_c_points_to P_top ->
      Post.return flow

    | E_c_points_to (P_block(base, offset)) ->

      (* Get cells with the same base *)
      let a = get_domain_env T_cur man flow in
      let same_base_cells = SmashSet.filter (fun c ->
          compare_base base c.base = 0
        ) a.smashes
      in

      SmashSet.fold (fun c acc ->
          if c.primed then
            (* Weak assign primed cells to their unprimed version before removing them *)
            let cc = { c with primed = false } in
            Post.bind (assign_smash cc (mk_var (mk_smash_var c) ~mode:WEAK range) range man) acc |>
            Post.bind (remove_smash c range man)

          else
          if not (SmashSet.mem { c with primed = true } same_base_cells) then
            (* Remove unprimed cells that have no primed version *)
            Post.bind (remove_smash c range man) acc
          else
            acc
        ) same_base_cells (Post.return flow)

    | _ -> assert false


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
      Option.return

    (* 𝕊⟦ add @addr; ⟧ *)      
    | S_add { ekind = E_addr addr } ->
      add_base (A addr) stmt.srange man flow |>
      Option.return

    (* 𝕊⟦ *p = rval; ⟧ *)      
    | S_assign({ ekind = E_c_deref p}, rval) ->
      assign_deref p rval stmt.srange man flow |>
      Option.return

    | S_remove { ekind = E_var (v, _) } ->
      remove_base (V v) stmt.srange man flow |>
      Option.return

    | S_rename({ ekind = E_var (v1, _) }, { ekind = E_var (v2, _) }) ->
      rename_base (V v1) (V v2) stmt.srange man flow |>
      Option.return

    | S_rename({ ekind = E_addr addr1 }, { ekind = E_addr addr2 }) ->
      rename_base (A addr1) (A addr2) stmt.srange man flow |>
      Post.bind (man.exec_sub ~zone:Z_c_scalar stmt) |>
      Option.return


    | S_stub_rename_primed(p, []) ->
      None

    | S_stub_rename_primed(p,offsets) ->
      rename_primed p offsets stmt.srange man flow |>
      Option.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)



  (** Abstract evaluation of a dereference *)
  let eval_deref p range man flow =
    deref p range man flow |>
    Eval.bind @@ fun deref flow ->

    match deref with
    | Smash c ->
      let v = mk_smash_var c in
      let flow = add_smash c range man flow |>
                 Post.to_flow man.lattice
      in
      Eval.singleton (mk_var v ~mode:WEAK range) flow

    | Top typ ->
      Eval.singleton (mk_top typ range) flow



  (** Abstract evaluation of primed expressions in stubs *)
  let eval_primed lval range man flow =
    deref (mk_c_address_of lval range) range man flow |>
    Eval.bind @@ fun deref flow ->

    match deref with
    | Smash c ->
      let cc = { c with primed = true } in
      let v = mk_smash_var cc in
      let flow = add_smash cc range man flow |>
                 Post.to_flow man.lattice
      in
      Eval.singleton (mk_var v  ~mode:WEAK range) flow

    | Top typ ->
      Eval.singleton (mk_top typ range) flow
  


  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ *p ⟧ *)      
    | E_c_deref p when is_c_scalar_type exp.etyp ->
      eval_deref p exp.erange man flow |>
      Option.return

    | E_stub_primed lval ->
      eval_primed lval exp.erange man flow |>
      Option.return

    | _ -> None



  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask query man flow = None

  let refine channel man flow = Channel.return flow

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
