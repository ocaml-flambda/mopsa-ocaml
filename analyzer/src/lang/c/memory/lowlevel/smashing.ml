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

(** Abstraction of arrays by smashing. *)

open Mopsa
open Core.Sig.Stacked.Intermediate
open Universal.Ast
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to
open Common.Alarms
open Format
open Universal.Numeric.Common

module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  let name = "c.memory.lowlevel.smashing"

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [
        Z_c_scalar;
        Z_u_num
      ];
    };
    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c_low_level, Z_u_num;
        Z_c_scalar, Z_u_num;
        Z_c_low_level, Z_c_scalar;
        Z_c_low_level, Z_c_points_to;
      ];
    }
  }

  let alarms = []


  (** {2 Smashes} *)
  (** *********** *)

  (** Types of smashed elements *)
  type styp =
    | Int   of c_integer_type
    | Float of c_float_type
    | Ptr

  (** Smashes *)
  type smash = {
    base: base;         (** smashed memory container *)
    styp: styp;         (** type of the smashed elements *)
  }

  let pp_styp fmt = function
    | Int C_unsigned_char    -> pp_print_string fmt "u8"
    | Int C_signed_char      -> pp_print_string fmt "s8"
    | Int C_unsigned_short   -> pp_print_string fmt "u16"
    | Int C_signed_short     -> pp_print_string fmt "s16"
    | Int C_unsigned_int     -> pp_print_string fmt "u32"
    | Int C_signed_int       -> pp_print_string fmt "s32"
    | Int C_unsigned_long    -> pp_print_string fmt "ul"
    | Int C_signed_long      -> pp_print_string fmt "sl"
    | Int C_unsigned_long_long -> pp_print_string fmt "ull"
    | Int C_signed_long_long -> pp_print_string fmt "sll"
    | Int C_unsigned_int128  -> pp_print_string fmt "u128"
    | Int C_signed_int128    -> pp_print_string fmt "u128"
    | Float C_float          -> pp_print_string fmt "f"
    | Float C_double         -> pp_print_string fmt "d"
    | Float C_long_double    -> pp_print_string fmt "ld"
    | Ptr                    -> pp_print_string fmt "ptr"

  let pp_smash fmt s =
    Format.fprintf fmt "smash(%a:%a)"
      pp_base s.base
      pp_styp s.styp

  let compare_styp st1 st2 =
    match st1, st2 with
    | Int i1, Int i2 -> compare i1 i2
    | Float f1, Float f2 -> compare f1 f2
    | Ptr, Ptr -> 0
    | _ -> compare st1 st2

  let compare_smash s1 s2 =
    Compare.pair compare_base compare_styp
      (s1.base,s1.styp)
      (s2.base,s2.styp)

  let styp_of_typ t =
    match remove_typedef_qual t with
    | T_c_integer i -> Int i
    | T_c_bool -> Int C_unsigned_char
    | T_c_enum e -> Int e.c_enum_integer_type
    | T_c_float f   -> Float f
    | T_c_pointer _ -> Ptr
    | _ -> panic "smash: unsupported type %a" pp_typ t

  let typ_of_styp = function
    | Int i   -> T_c_integer i
    | Float f -> T_c_float f
    | Ptr     -> T_c_pointer T_c_void
  
  let mk_smash base typ =
    { base;
      styp = styp_of_typ typ; }


  (** {2 Abstract state} *)
  (** ****************** *)

  module STypeSet = Framework.Lattices.Powerset.Make
      (struct type t = styp let compare = compare_styp let print = pp_styp end)

  module Init =
  struct
    type t =
      | Bot
      | None
      | Partial of STypeSet.t
      | Full    of STypeSet.t

    let bottom = Bot

    let top = Partial STypeSet.top

    let print fmt x =
      match x with
      | Bot        -> fprintf fmt "⊥"
      | None       -> fprintf fmt "none"
      | Full ts    -> fprintf fmt "full(%a)" STypeSet.print ts
      | Partial ts -> fprintf fmt "partial(%a)" STypeSet.print ts

    let is_bottom x = (x = Bot)

    let subset x y =
      if x == y then true else
      match x, y with
      | Bot, _ -> true
      | _, Bot -> false
      | None, None -> true
      | None, Full _ -> false
      | None, Partial _ -> true
      | Full ts1, Full ts2 -> STypeSet.subset ts1 ts2
      | Full _, None -> false
      | Full ts1, Partial ts2 -> STypeSet.subset ts1 ts2
      | Partial ts1, Partial ts2 -> STypeSet.subset ts1 ts2
      | Partial _, _ -> false

    let join x y =
      if x == y then x else
      match x, y with
      | Bot, a | a, Bot -> a
      | None, None -> None
      | None, Full ts | Full ts, None -> Partial ts
      | None, (Partial _ as a) | (Partial _ as a), None -> a
      | Full ts1, Full ts2 -> Full (STypeSet.join ts1 ts2)
      | Full ts1, Partial ts2 | Partial ts1, Full ts2 -> Partial (STypeSet.join ts1 ts2)
      | Partial ts1, Partial ts2 -> Partial (STypeSet.join ts1 ts2)

    let meet x y =
      if x == y then x else
      match x, y with
      | Bot, _ | _, Bot -> Bot
      | None, None -> None
      | None, Full _ | Full _, None -> Bot
      | None, Partial _ | Partial _, None -> None
      | Full ts1, Full ts2 -> Full (STypeSet.meet ts1 ts2)
      | Full ts1, Partial ts2 | Partial ts1, Full ts2 -> Full (STypeSet.meet ts1 ts2)
      | Partial ts1, Partial ts2 -> Partial (STypeSet.meet ts1 ts2)

    let widen ctx = join

    let apply f = function
      | Bot        -> Bot
      | None       -> None
      | Full ts    -> Full (f ts)
      | Partial ts -> Partial (f ts)

    let add st = apply (STypeSet.add st)

    let remove st = apply (STypeSet.remove st)
  end

  module State = Framework.Lattices.Pointwise.Make(Base)(Init)

  type t = State.t

  include GenDomainId(struct
      type nonrec t = t
      let name = name
    end)

  let print fmt (a:t) =
    fprintf fmt "smashes: @[%a@]@\n" State.print a


  (** {2 Smash variables} *)
  (** ******************* *)

  type var_kind += V_c_smash of smash

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_smash s -> pp_smash fmt s
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_smash(s1), V_c_smash(s2) -> compare_smash s1 s2
          | _ -> next v1 v2
        );
    }


  (** Create a variable from a smash *)
  let mk_smash_var s : var =
    let uname =
      asprintf "smash(%s:%a)"
        (base_uniq_name s.base)
        pp_styp s.styp
    in
    mkv uname (V_c_smash (s)) (typ_of_styp s.styp) ~mode:WEAK


  let mk_smash_expr s ?(typ=None) ?(mode=None) range =
    let v = mk_var (mk_smash_var s) ~mode range in
    match s.styp with
    | Int _   -> v
    | Float _ -> v
    | Ptr ->
      match typ with
      | None -> v
      | Some t ->
        match remove_typedef_qual t with
        | T_c_pointer T_c_void -> v
        | (T_c_pointer _) as t -> mk_c_cast v t range
        | _ -> assert false


  (** {2 Offset of the first uninitialized byte} *)
  (** ****************************************** *)

  type var_kind += V_c_uninit of base

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_uninit b -> fprintf fmt "uninit(%a)" pp_base b
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_uninit(b1), V_c_uninit(b2) -> compare_base b1 b2
          | _ -> next v1 v2
        );
    }


  let mk_uninit_var b : var =
    let uname = asprintf "uninit(%s)" (base_uniq_name b) in
    mkv uname (V_c_uninit b) ul ~mode:(base_mode b)


  let mk_uninit_expr b ?(mode=None) range =
    mk_var (mk_uninit_var b) ~mode range

  
  (** {2 Unification} *)
  (** *************** *)

  let unify man ctx ((a,s):t*'s) ((a',s'):t*'s) : t * 's * t * 's =
    a, s, a', s'


  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom = State.bottom

  let top = State.top

  let is_bottom _ = false

  let subset man ctx (a,s) (a',s') =
    true, s, s'

  let join man ctx (a,s) (a',s') =
    let a, s, a', s' = unify man ctx (a,s) (a',s') in
    State.join a a', s, s'

  let meet = join

  let widen man ctx (a,s) (a',s') =
    let (a, s, s') = join man ctx (a,s) (a',s') in
    (a, s, s', true)

  let merge _ _ _  = assert false


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow =
    set_env T_cur State.empty man flow


  (** {2 Utility functions} *)
  (** ********************* *)

  (** Get the base and offset pointed by ptr *)
  let eval_pointed_base_offset ptr range man flow =
    man.eval ptr ~zone:(Zone.Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    (* Ignore invalid accesses as the domain does not report such alarms *)
    | E_c_points_to P_null
    | E_c_points_to P_invalid
    | E_c_points_to (P_block ({ base_valid = false }, _, _))
    | E_c_points_to P_top ->
      Cases.empty_singleton flow

    | E_c_points_to (P_block (base, offset, mode)) ->
      Cases.singleton (base, offset, mode) flow

    | _ -> assert false


  (** Predicate defining interesting bases to smash *)
  let is_interesting_base base =
    match base with
    | { base_valid = false } -> false
    | { base_kind = Var v } when is_c_array_type v.vtyp ->
      let rec aux t =
        match remove_typedef_qual t with
        | T_c_array(tt,_) -> aux tt
        | T_c_record{c_record_kind = C_struct; c_record_fields = fields} ->
          let rec aux2 = function
            | [] -> None
            | [f] -> Some f.c_field_type
            | f::tl ->
              match aux2 tl with
              | None -> None
              | Some t -> if compare_typ t f.c_field_type = 0 then Some t else None
          in
          if aux2 fields = None then false else true
        | x -> is_c_scalar_type x
      in
      aux v.vtyp
    | { base_kind = Addr _ } -> true
    | _ -> false


  let smashes_of_base base a =
    match State.find base a with
    | Bot | None -> []
    | Full ts | Partial ts ->
      STypeSet.fold
        (fun styp acc -> { base; styp } :: acc)
        ts []


  (** [is_aligned o n man flow] checks whether the value of an
      expression [o] is aligned w.r.t. type [t] *)
  let is_aligned e t man flow =
    let s = sizeof_type t in
    if is_c_expr_equals_z e Z.zero then true else
    if Z.(s = one) then true
    else
      man.eval e ~zone:(Zone.Z_c_low_level,Universal.Zone.Z_u_num) flow |>
      Cases.for_all_some (fun ee flow ->
          (* Compute the step-interval of ee *)
          let _, c = man.ask (Universal.Numeric.Common.Q_int_congr_interval ee) flow in
          let c' = (s,Z.zero) in
          Universal.Numeric.Common.C.included c c'
        )

  let add_smash base styp range man flow =
    man.post
      (mk_add_var (mk_smash_var {base;styp}) range)
      ~zone:Z_c_scalar flow

  let remove_smash base styp range man flow =
    man.post
      (mk_remove_var (mk_smash_var {base;styp}) range)
      ~zone:Z_c_scalar flow

  let rename_smash base1 base2 styp range man flow =
    man.post
      (mk_rename_var (mk_smash_var {base=base1;styp}) (mk_smash_var {base=base2;styp}) range)
      ~zone:Z_c_scalar flow

  let forget_smash base styp range man flow =
    man.post
      (mk_forget_var (mk_smash_var {base;styp}) range)
      ~zone:Z_c_scalar flow

  let expand_smash base basel styp range man flow =
    man.post
      (mk_expand_var (mk_smash_var {base;styp}) (List.map (fun b -> mk_smash_var {base=b;styp}) basel) range)
      ~zone:Z_c_scalar flow

  let fold_smash base basel styp range man flow =
    man.post
      (mk_fold_var (mk_smash_var {base;styp}) (List.map (fun b -> mk_smash_var {base=b;styp}) basel) range)
      ~zone:Z_c_scalar flow

  let add_uninit base range man flow =
    man.post
      (mk_add_var (mk_uninit_var base) range)
      ~zone:Z_c_scalar flow

  let remove_uninit base range man flow =
    man.post
      (mk_remove_var (mk_uninit_var base) range)
      ~zone:Z_c_scalar flow

  let rename_uninit base1 base2 range man flow =
    man.post
      (mk_rename_var (mk_uninit_var base1) (mk_uninit_var base2) range)
      ~zone:Z_c_scalar flow

  let forget_uninit base range man flow =
    man.post
      (mk_forget_var (mk_uninit_var base) range)
      ~zone:Z_c_scalar flow

  let expand_uninit base basel range man flow =
    man.post
      (mk_expand_var (mk_uninit_var base) (List.map mk_uninit_var basel) range)
      ~zone:Z_c_scalar flow

  let fold_uninit base basel range man flow =
    man.post
      (mk_fold_var (mk_uninit_var base) (List.map mk_uninit_var basel) range)
      ~zone:Z_c_scalar flow

  let fold_stypes (f:styp -> range -> _ man -> 'a flow -> 'a post) ts range man flow =
    STypeSet.fold
        (fun styp acc -> Post.bind (f styp range man) acc)
        ts (Post.return flow)

  let exec_smashes (f:styp -> range -> _ man -> 'a flow -> 'a post) base a range man flow =
    match State.find base a with
    | Init.Bot        -> Post.return flow
    | Init.None       -> Post.return flow
    | Init.Full ts
    | Init.Partial ts -> fold_stypes f ts range man flow

  let exec_uninit (f:range -> _ man -> 'a flow -> 'a post) base a range man flow =
    match State.find base a with
    | Init.Partial ts -> f range man flow
    | _ -> Post.return flow


  let assume_optim cond ~fthen ~felse ~zone man flow =
    man.eval cond ~zone:(zone, Z_u_num) flow >>$ fun cond flow ->
    let op,e1,e2 = match ekind cond with
      | E_binop(op,e1,e2) -> op,e1,e2
      | _ -> assert false
    in
    (* Get the intervals of e1 and e2 *)
    let interval e : int_itv =
      man.ask (mk_int_interval_query ~fast:true e) flow
    in
    let i1 = interval e1 in
    let i2 = interval e2 in
    let cmp = function
      | O_eq -> Bot.bot_dfl2 false I.is_log_eq
      | O_ne -> Bot.bot_dfl2 false I.is_log_neq
      | O_lt -> Bot.bot_dfl2 false I.is_log_lt
      | O_le -> Bot.bot_dfl2 false I.is_log_leq
      | O_gt -> Bot.bot_dfl2 false I.is_log_gt
      | O_ge -> Bot.bot_dfl2 false I.is_log_geq
      | _    -> assert false
    in
    match cmp op i1 i2, cmp (negate_comparison_op op) i1 i2 with
    | true, false -> fthen flow
    | false, true -> felse flow
    | _ -> assume cond ~fthen ~felse ~zone:Z_u_num man flow
          
    
  

  (** {2 Execution of statements} *)
  (** *************************** *)

  (** 𝕊⟦ add(base); ⟧ *)
  let exec_add_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      map_env T_cur (State.add base Init.None) man flow |>
      Post.return


  (** 𝕊⟦ type v; ⟧ *)
  let exec_declare_variable v scope range man flow =
    exec_add_base (mk_var_base v) range man flow


  (** 𝕊⟦ remove(base); ⟧ *)
  let exec_remove_base base range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else      
      let a = get_env T_cur man flow in
      (* Remove the base from the map *)
      let flow = set_env T_cur (State.remove base a) man flow in
      (* Remove associated vars *)
      exec_smashes (remove_smash base) base a range man flow >>$ fun () flow ->
      exec_uninit
        (remove_uninit base) base a range man flow


  (** 𝕊⟦ rename(base1,base2); *)
  let exec_rename_base base1 base2 range man flow =
    if not (is_interesting_base base1) then Post.return flow else
    if not (is_interesting_base base2) then exec_remove_base base1 range man flow
    else
      (* Remove base2 before doing renaming *)
      exec_remove_base base2 range man flow >>$ fun () flow ->
      (* Rename the base in the map *)
      let a = get_env T_cur man flow in
      let flow = set_env T_cur (State.rename base1 base2 a) man flow in
      (* Rename associated vars *)
      exec_smashes (rename_smash base1 base2) base1 a range man flow >>$ fun () flow ->
      exec_uninit (rename_uninit base1 base2) base1 a range man flow


  (** 𝕊⟦ expand(base, bases); ⟧ *)
  let exec_expand_base base bases range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      (* Expand the base in the map *)
      let a = get_env T_cur man flow in
      let init = State.find base a in
      let a' = List.fold_left (fun acc b ->
          State.add b init acc
        ) a bases in
      let flow = set_env T_cur a' man flow in
      (* Expand associated variables *)
      exec_smashes (expand_smash base bases) base a range man flow >>$ fun () flow ->
      exec_uninit (expand_uninit base bases) base a range man flow


  (** 𝕊⟦ fold(base, bases); ⟧ *)
  let exec_fold_bases base bases range man flow =
    if not (is_interesting_base base) then Post.return flow else
    if List.exists (fun b -> not (is_interesting_base b)) bases then panic_at range "fold %a not supported" pp_base base
    else
      match bases with
      | [] -> Post.return flow
      | x::y::z -> panic_at range "smash: folding of multiple bases not supported"
      | [base'] ->
        (* Fold the base in the map *)
        let a = get_env T_cur man flow in
        let init = State.find base a in
        let init' = State.find base' a in
        let init'' = if base_mode base = STRONG then init' else Init.join init init' in
        let a' = State.add base init'' a |>
                 State.remove base'
        in
        let flow = set_env T_cur a' man flow in
        (* Fold associate variables *)
        exec_smashes (fold_smash base bases) base a range man flow >>$ fun () flow ->
        exec_uninit (fold_uninit base bases) base a range man flow


  (** 𝕊⟦ forget(lval) ⟧ *)
  let exec_forget lval range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun (base,offset,mode) flow ->
    if not (is_interesting_base base) then
      Post.return flow
    else
      let a = get_env T_cur man flow in
      let init = State.find base a in
      let s = mk_smash base lval.etyp in
      let vsmash = mk_smash_var s in
      let uninit = mk_uninit_expr base ~mode range in

      match init with
      | Init.Bot -> Post.return flow

      | Init.Full ts ->
        (* When base is fully initialized, we remove smashes other
           than the one with the type of lval, which will be put to
           top after *)
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        if STypeSet.mem s.styp ts then
          man.post (mk_forget_var vsmash range) ~zone:Z_c_scalar flow
        else
          let flow = set_env T_cur (State.add base (Init.Full (STypeSet.singleton s.styp)) a) man flow in
          man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow

      | Init.None ->
        (* When base is not initialized at all, we switch its state to
           partially initialized if the offset is not universally
           quantified *)
        if not (is_aligned offset lval.etyp man flow) || not (is_expr_forall_quantified offset) then
          let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
          man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow  >>$ fun () flow ->
          man.post (mk_add uninit range) ~zone:Z_c_scalar flow >>$ fun () flow ->
          man.post (mk_assign uninit zero range) ~zone:Z_c_scalar flow
        else
          (* In the case of universally quantified offset, we check three cases: *)
          let min, max = Common.Quantified_offset.bound offset in
          let elm = mk_z (sizeof_type lval.etyp) range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume_optim (eq min zero range)
            ~fthen:(fun flow ->
                assume_optim (eq max (sub size elm range) range)
                  ~fthen:(fun flow ->
                      (* Case #1: fully initialized
                            0            size - |elm|
                            |----------------|------>
                           min              max
                      *)
                      let init' = Init.Full (STypeSet.singleton s.styp) in
                      set_env T_cur (State.add base init' a) man flow |>
                      man.post (mk_add_var vsmash range) ~zone:Z_c_scalar
                    )
                  ~felse:(fun flow ->
                      (* Case #2: partially initialized until max + |elm|
                            0            size - |elm|
                            |---------x------|------>
                           min       max
                      *)
                      let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
                      man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow  >>$ fun () flow ->
                      man.post (mk_add uninit range) ~zone:Z_c_scalar flow >>$ fun () flow ->
                      man.post (mk_assign uninit (add max elm range) range) ~zone:Z_c_scalar flow
                    ) ~zone:Z_c_scalar man flow
              )
            ~felse:(fun flow ->
                (* Case #3: nop
                     0            size - |elm|
                     |----x-----------|------>
                         min
                *)
                Post.return flow
              ) ~zone:Z_c_scalar man flow

      | Init.Partial ts ->
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        begin if STypeSet.mem s.styp ts then
            man.post (mk_forget_var vsmash range) ~zone:Z_c_scalar flow
          else
            let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
            man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow
        end >>$ fun () flow ->
        if not (is_aligned offset lval.etyp man flow) || not (is_expr_forall_quantified offset) then
          Post.return flow
        else
          let min, max = Common.Quantified_offset.bound offset in
          let elm = mk_z (sizeof_type lval.etyp) range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume_optim (le min (add uninit elm range) range)
            ~fthen:(fun flow ->
                assume_optim (eq max (sub size elm range) range)
                  ~fthen:(fun flow ->
                      (* Case #1: fully initialized
                            0      uninit     size - |elm|
                            |---x----0----------|------>
                               min             max
                      *)
                      let init' = Init.Full (STypeSet.singleton s.styp) in
                      set_env T_cur (State.add base init' a) man flow |>
                      Post.return
                    )
                  ~felse:(fun flow ->
                      (* Case #2: partially initialized until max + |elm|
                            0      uninit     size - |elm|
                            |---x----0------x---|------>
                               min         max
                      *)
                      man.post (mk_assign uninit (add max elm range) range) ~zone:Z_c_scalar flow
                    ) ~zone:Z_c_scalar man flow
              )
            ~felse:(fun flow ->
                (* Case #3: nop
                      0  uninit      size - |elm|
                      |----0----x---------|------>
                               min       
                *)
                Post.return flow
              )  ~zone:Z_c_scalar man flow



  (** 𝕊⟦ *p = rval; ⟧ *)
  let exec_assign p rval range man flow =
    man.eval rval ~zone:(Z_c_low_level,Z_c_scalar) flow >>$ fun rval flow ->
    eval_pointed_base_offset p range man flow >>$ fun (base,offset,mode) flow ->
    if not (is_interesting_base base) then
      Post.return flow
    else
      let a = get_env T_cur man flow in
      if not (is_aligned offset (under_type p.etyp) man flow) then
        exec_smashes (forget_smash base) base a range man flow
      else
        let s = mk_smash base (under_type p.etyp) in
        let vsmash = mk_smash_var s in
        let esmash = mk_smash_expr s ~mode range in
        let elm = mk_z (sizeof_type (under_type p.etyp)) range in
        match State.find base a with
        | Init.Bot -> Post.return flow

        | Init.None ->
          let init = Init.Partial (STypeSet.singleton s.styp) in
          let flow = set_env T_cur (State.add base init a) man flow in
          let strong_smash = mk_smash_expr s ~mode:(Some STRONG) range in
          let uninit = mk_uninit_expr base ~mode range in
          man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow >>$ fun () flow ->
          man.post (mk_assign strong_smash rval range) ~zone:Z_c_scalar flow >>$ fun () flow ->
          man.post (mk_add uninit range) ~zone:Z_c_scalar flow >>$ fun () flow ->
          assume_optim (eq offset zero range)
            ~fthen:(fun flow ->
                man.post (mk_assign uninit elm range) ~zone:Z_c_scalar flow
              )
            ~felse:(fun flow ->
                man.post (mk_assign uninit zero range) ~zone:Z_c_scalar flow
              ) ~zone:Z_c_scalar man flow

        | Init.Full ts ->
          fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
          if not (STypeSet.mem s.styp ts) then
            man.post (mk_forget_var vsmash range) ~zone:Z_c_scalar flow
          else
            man.post (mk_assign esmash rval range) ~zone:Z_c_scalar flow

        | Init.Partial ts ->
          fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
          if not (STypeSet.mem s.styp ts) then
            let init = Init.Partial (STypeSet.singleton s.styp) in
            let flow = set_env T_cur (State.add base init a) man flow in
            man.post (mk_add_var vsmash range) ~zone:Z_c_scalar flow
          else
            man.post (mk_assign esmash rval range) ~zone:Z_c_scalar flow >>$ fun () flow ->
            let uninit = mk_uninit_expr base ~mode range in
            eval_base_size base range man flow >>$ fun size flow ->
            assume_optim (eq uninit offset range)
              ~fthen:(fun flow ->
                  assume_optim (eq offset (sub size elm range) range)
                    ~fthen:(fun flow ->
                        let init = Init.Full (STypeSet.singleton s.styp) in
                        let flow = set_env T_cur (State.add base init a) man flow in
                        man.post (mk_remove uninit range) ~zone:Z_c_scalar flow
                      )
                    ~felse:(fun flow ->
                        let init = Init.Partial (STypeSet.singleton s.styp) in
                        let flow = set_env T_cur (State.add base init a) man flow in
                        man.post (mk_assign uninit (add uninit elm range) range) ~zone:Z_c_scalar flow
                      ) ~zone:Z_c_scalar man flow
                )
              ~felse:(fun flow ->
                  let init = Init.Partial (STypeSet.singleton s.styp) in
                  let flow = set_env T_cur (State.add base init a) man flow in
                  Post.return flow
                ) ~zone:Z_c_scalar man flow




  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when is_interesting_base (mk_var_base v) ->
      exec_declare_variable v scope stmt.srange man flow |>
      OptionExt.return

    | S_add e when is_base_expr e ->
      exec_add_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_rename (e1,e2) when is_base_expr e1 && is_base_expr e2 ->
      exec_rename_base (expr_to_base e1) (expr_to_base e2) stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_base_expr e && List.for_all is_base_expr el ->
      exec_expand_base (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_fold(e,el) when is_base_expr e && List.for_all is_base_expr el ->
      exec_fold_bases (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) ->
      exec_forget e stmt.srange man flow |>
      OptionExt.return

    | S_remove(e) when is_base_expr e ->
      exec_remove_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_assign({ ekind = E_c_deref p}, rval) when is_c_scalar_type (under_type p.etyp) ->
      exec_assign p rval stmt.srange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Abstract evaluation of a dereference *)
  let eval_deref p range man flow =
    eval_pointed_base_offset p range man flow >>$ fun (base,offset,mode) flow ->
    if not (is_interesting_base base) || not (is_aligned offset (under_type p.etyp) man flow) then
      Eval.singleton (mk_top (under_type p.etyp) range) flow
    else
      let a = get_env T_cur man flow in
      let s = mk_smash base (under_type p.etyp) in
      let esmash = mk_smash_expr s ~typ:(Some  (under_type p.etyp)) ~mode range in
      match State.find base a with
      | Init.Bot ->  Eval.singleton (mk_top (under_type p.etyp) range) flow

      | Init.None -> Eval.singleton (mk_top (under_type p.etyp) range) flow

      | Init.Full ts ->
        if not (STypeSet.mem s.styp ts) then
          Eval.singleton (mk_top (under_type p.etyp) range) flow
        else
          Eval.singleton esmash flow

      | Init.Partial ts ->
        if not (STypeSet.mem s.styp ts) then
          Eval.singleton (mk_top (under_type p.etyp) range) flow
        else
          let uninit = mk_uninit_expr base ~mode range in
          assume_optim (ge offset uninit range)
            ~fthen:(fun flow ->
                Eval.singleton (mk_top (under_type p.etyp) range) flow
              )
            ~felse:(fun flow ->
                Eval.singleton esmash flow
              ) ~zone:Z_c_scalar man flow



  (** Evaluations entry point *)
  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p when is_c_scalar_type exp.etyp ->
      eval_deref p exp.erange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None

  let refine _ _ _ = assert false

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
