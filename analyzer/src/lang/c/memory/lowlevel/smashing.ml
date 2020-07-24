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

(** Abstraction of arrays by smashing.

    This domain summarizes the initialized values of a base using single smash
    variable `smash(base)`.

    In order to determine whether the accessed element has been initialized,
    the domain uses a numeric variable `uninit(base)` that tracks the offset
    of the first unintilialized element of memory block `base`.

    The concretization of this domain is therefore:
     
       ∀ i ∈ [0, uninit(base) - 1]: base[i] ∈ γ(smash(base))

    For efficiency reason, some specific values of `uninit(base)` are directly
    encoded in the abstract state. More particularly, the domain uses two
    shortcut states:

    - Init.None : used to denote that the base has not been initialized yet.
    This corresponds to `uninit(base) = 0`.

    - Init.Full : used to denote that the base has been fully initialized.
    This corresponds to `uninit(base) = size(base)`.


    Limitations:
    - Considers only (multi-)arrays of scalars.
    - Supports only sequential initialization starting from offset 0.
    - memcpy-like formulas limited to numeric arrays where the destination
      is fully covered.
*)

open Mopsa
open Sig.Abstraction.Stacked
open Universal.Ast
open Stubs.Ast
open Ast
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

  let scalar = Semantic "C/Scalar"

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

  let sizeof_styp st = sizeof_type (typ_of_styp st)

  let rangeof_styp st = rangeof (typ_of_styp st)

  let mk_smash base typ =
    { base;
      styp = styp_of_typ typ; }


  (** {2 Abstract state} *)
  (** ****************** *)

  module STypeSet = Framework.Lattices.Powerset.Make
      (struct type t = styp let compare = compare_styp let print = pp_styp end)

  (** Initialization state with shortcut states to avoid useless tests on `uninit` *)
  module Init =
  struct
    type t =
      | Bot
      (* Empty state, useful only for the lattice signature *)

      | None
      (* Not yet initialized base *)

      | Full of STypeSet.t
      (* Fully initialized base with the types of initialized elements *)

      | Partial of STypeSet.t
      (* Partially initialized base. Note that [Full ts] and [None] are special case of [Partial ts] *)
 

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
      | Full ts1, Full ts2 -> STypeSet.subset ts1 ts2
      | None, None -> true
      | Partial ts1, Partial ts2 -> STypeSet.subset ts1 ts2
      | Full ts1, Partial ts2 -> STypeSet.subset ts1 ts2 (* since [Full ts] is a special case of [Partial ts] *)
      | None, Partial _ -> true
      | _ -> false

    let join x y =
      if x == y then x else
      match x, y with
      | Bot, a | a, Bot -> a
      | None, None -> None
      | Full ts1, Full ts2 -> Full (STypeSet.join ts1 ts2)
      | Partial ts1, Partial ts2 -> Partial (STypeSet.join ts1 ts2)
      | Full ts1, Partial ts2 | Partial ts1, Full ts2 -> Partial (STypeSet.join ts1 ts2)
      | None, Partial ts | Partial ts, None -> Partial ts
      | None, Full ts | Full ts, None -> Partial ts
           

    let meet x y =
      if x == y then x else
      match x, y with
      | Bot, a | a, Bot -> a
      | None, None -> None
      | Full ts1, Full ts2 -> Full (STypeSet.meet ts1 ts2)
      | Partial ts1, Partial ts2 -> Partial (STypeSet.meet ts1 ts2)
      | Full ts1, Partial ts2 | Partial ts1, Full ts2 -> Full (STypeSet.meet ts1 ts2)
      | None, Partial _ | Partial _, None -> None
      | None, Full ts | Full ts, None -> Bot

    let widen = join

    let apply f = function
      | Bot        -> Bot
      | None       -> None
      | Full ts    -> Full (f ts)
      | Partial ts -> Partial (f ts)

    let add st = apply (STypeSet.add st)

    let remove st = apply (STypeSet.remove st)

    let is_partial = function
      | Partial _ -> true
      | _         -> false

    let is_full = function
      | Full _ -> true
      | _      -> false

    let is_none = function
      | None -> true
      | _    -> false

    let types = function
      | Bot -> STypeSet.bottom
      | None -> STypeSet.empty
      | Full ts | Partial ts -> ts
  end

  (* The abstract state is a map from bases to initialization state *)
  module State = Framework.Lattices.Partial_map.Make(Base)(Init)

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
    mkv uname (V_c_smash (s)) (typ_of_styp s.styp) ~mode:WEAK ~semantic:"C/Scalar"


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


  (** Functions for managing smash variables *)
  let add_smash base styp range man flow =
    man.post
      (mk_add_var (mk_smash_var {base;styp}) range)
      ~route:scalar flow

  let remove_smash base styp range man flow =
    man.post
      (mk_remove_var (mk_smash_var {base;styp}) range)
      ~route:scalar flow

  let rename_smash base1 base2 styp range man flow =
    man.post
      (mk_rename_var (mk_smash_var {base=base1;styp}) (mk_smash_var {base=base2;styp}) range)
      ~route:scalar flow

  let forget_smash base styp range man flow =
    man.post
      (mk_forget_var (mk_smash_var {base;styp}) range)
      ~route:scalar flow

  let expand_smash base basel styp range man flow =
    man.post
      (mk_expand_var (mk_smash_var {base;styp}) (List.map (fun b -> mk_smash_var {base=b;styp}) basel) range)
      ~route:scalar flow

  let fold_smash base basel styp range man flow =
    man.post
      (mk_fold_var (mk_smash_var {base;styp}) (List.map (fun b -> mk_smash_var {base=b;styp}) basel) range)
      ~route:scalar flow


  (** {2 Offset of the first uninitialized element} *)
  (** ********************************************* *)

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


  (** Functions for managing uninit variables *)
  let add_uninit base range man flow =
    man.post
      (mk_add_var (mk_uninit_var base) range)
      ~route:scalar flow

  let set_uninit_to_base_size base range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    man.post (mk_assign (mk_uninit_expr base range) size range) ~route:scalar flow

  let remove_uninit base range man flow =
    man.post
      (mk_remove_var (mk_uninit_var base) range)
      ~route:scalar flow

  let rename_uninit base1 base2 range man flow =
    man.post
      (mk_rename_var (mk_uninit_var base1) (mk_uninit_var base2) range)
      ~route:scalar flow

  let forget_uninit base range man flow =
    man.post
      (mk_forget_var (mk_uninit_var base) range)
      ~route:scalar flow

  let expand_uninit base basel range man flow =
    man.post
      (mk_expand_var (mk_uninit_var base) (List.map mk_uninit_var basel) range)
      ~route:scalar flow

  let fold_uninit base basel range man flow =
    man.post
      (mk_fold_var (mk_uninit_var base) (List.map mk_uninit_var basel) range)
      ~route:scalar flow

  
  (** {2 Unification} *)
  (** *************** *)

  
  (** [is_aligned o n man flow] checks whether the value of an
      expression [o] is aligned w.r.t. type [t] *)
  let is_aligned ?(route=toplevel) e t man flow =
    let s = sizeof_type t in
    if is_c_expr_equals_z e Z.zero then true else
    if Z.(s = one) then true
    else
      man.eval ~route e flow |>
      Cases.for_all_some (fun ee flow ->
          (* Compute the step-interval of ee *)
          let _, c = man.ask (Universal.Numeric.Common.Q_int_congr_interval ee) flow in
          let c' = (s,Z.zero) in
          Universal.Numeric.Common.C.included c c'
        )

  (** Synthesis function of integer smashes *)
  let phi_int s init range man flow =
    (* Synthesis is not supported for partially initialized
       bases where the uninit offset is not aligned w.r.t. to
       the type of the smash *)
    if Init.is_partial init
    && not (is_aligned (mk_uninit_expr s.base range) (typ_of_styp s.styp) ~route:scalar man flow)
    then
      Post.return flow
    else
      (* Two cases are considered (inspired synthesis function of the Cells domain):

         Case #1: ∃ s' : sizeof(s) = sizeof(s')
               ⇒ s = wrap(s', rangeof(s))

         Case #2: ∃ s' : type(s') = unsigned char
                ⇒ s = wrap(Σ_{i = 0}^{size(s) - 1}{256^i * s'}, rangeof(s))

         Case #3: typeof(s) = unsigned char
                ⇒   s = s' / 2^(8*0) mod 256
                  ∨ s = s' / 2^(8*1) mod 256
                  ∨ ...
                  ∨ s = s' / 2^(8*(sizeof(s') - 1)) mod 256
      *)
      let int_type = match s.styp with Int t -> t | _ -> assert false in
      let esmash_strong = mk_smash_expr s ~mode:(Some STRONG) range in
      STypeSet.fold
        (fun styp' acc ->
           let s' = {base = s.base; styp = styp'} in
           let esmash' = mk_smash_expr s' range in
           match styp' with
           (* Case #1 *)
           | Int _ when Z.(sizeof_styp s.styp = sizeof_styp styp') ->
             let e = wrap_expr esmash' (rangeof_styp s.styp) range in
             let cond = eq esmash_strong e range in
             Post.bind (man.post (mk_assume cond range) ~route:scalar) acc

           (* Case #2 *)
           | Int C_unsigned_char ->
             let n = sizeof_styp s.styp |> Z.to_int in
             let rec sum i =
               if i = 0 then esmash' else
                 let m = Z.(of_int 256 ** i) in
                 add (sum (i-1)) (mul (mk_z m range) esmash' range) range
             in
             let e = wrap_expr (sum (n-1)) (rangeof_styp s.styp) range in
             let cond = eq esmash_strong e range in
             Post.bind (man.post (mk_assume cond range) ~route:scalar) acc

           (* Case #3 *)
           | Int _ when int_type = C_unsigned_char ->
             acc >>$ fun () flow ->
             let n = sizeof_styp styp' |> Z.to_int in
             let rec aux i =
               if i = n then [] else
                 let e = _mod_ (div esmash' (mk_z Z.(of_int 2 ** Stdlib.(8 * i)) range) range) (mk_int 256 range) range in
                 let cond = eq esmash_strong e range in
                 man.post (mk_assume cond range) ~route:scalar flow :: aux (i+1)
             in
             aux 0 |>
             Post.join_list ~empty:(fun () -> Post.return flow)


           | _ -> acc
        ) (Init.types init) (Post.return flow)


  (** [phi s r m f] synthesizes the value of smash [s] when existing
      smashes in the same base exist *)
  let phi (s:smash) range man flow =
    let a = get_env T_cur man flow in
    let init = State.find s.base a in
    match init with
    (* Only fully initialized bases are supported for the moment *)
    | Init.Bot -> Post.return flow
    | Init.None -> Post.return flow

    | Init.Partial ts
    | Init.Full ts ->
      if STypeSet.mem s.styp ts then
        Post.return flow
      else
        let flow = set_env T_cur (State.add s.base (Init.add s.styp init) a) man flow in
        man.post (mk_add_var (mk_smash_var s) range) ~route:scalar flow >>$ fun () flow ->
        if STypeSet.is_empty ts then
          Post.return flow
        else
        match s.styp with
        (* No synthesis for floats for the moment *)
        | Float _ -> Post.return flow

        (* Synthesis of integer values *)
        | Int int_type -> phi_int s init range man flow

        (* No synthesis for pointer values *)
        | Ptr -> Post.return flow


  (** Singleton unification range *)
  let unify_range = tag_range (mk_fresh_range ()) "smashing-unification"

  let unify man sman ctx ((a,s):t*'s) ((a',s'):t*'s) : t * 's * t * 's =
    State.fold2zo
      (* No unification for bases present in one branch only *)
      (fun b init acc -> acc)
      (fun b' init' acc -> acc)
      (fun base init init' (a,s,a',s') ->
         let doit a init s other_init =
           match init, other_init with
           | Init.Bot, _ | _, Init.Bot -> a,s

           | Init.None, Init.None -> a,s

           | Init.Partial _, Init.None -> a,s

           (* The base becomes partially initialized from offset 0 *)
           | Init.None, Init.Partial _
           | Init.None, Init.Full _ ->
             (* Add the uninit variable and initialize it to 0 *)
             (* Note that there is no need to add a smashes in order
                to keep the values of the other flow *)
             let a,s = sub_env_exec (add_uninit base unify_range man) ctx man sman a s in
             sub_env_exec (man.post (mk_assign (mk_uninit_expr base unify_range) zero unify_range) ~route:scalar) ctx man sman a s

           (* The base becomes partially initialized from offset size(base) *)
           | Init.Full _, Init.None ->
             (* Add the uninit variable and initialize it to size *)
             let a,s = sub_env_exec (add_uninit base unify_range man) ctx man sman a s in
             sub_env_exec (set_uninit_to_base_size base unify_range man) ctx man sman a s

           (* The base becomes partially initialized from offset size(base) with possible synthesis of missing smashes *)
           | Init.Full ts1, Init.Partial ts2 ->
             (* Add the uninit variable and initialize it to size(base) *)
             let a,s = sub_env_exec (add_uninit base unify_range man) ctx man sman a s in
             let a,s = sub_env_exec (set_uninit_to_base_size base unify_range man) ctx man sman a s in
             (* Synthesize missing smashes *)
             STypeSet.fold
               (fun styp (a,s) ->
                  sub_env_exec (phi {base; styp} unify_range man) ctx man sman a s
               ) (STypeSet.diff ts2 ts1) (a,s)

           (* Initialization state will not change, but possible synthesis of missing smashes *)
           | Init.Partial ts1, Init.Full ts2
           | Init.Partial ts1, Init.Partial ts2
           | Init.Full ts1, Init.Full ts2 ->
             STypeSet.fold
               (fun styp (a,s) ->
                  sub_env_exec (phi {base; styp} unify_range man) ctx man sman a s
               ) (STypeSet.diff ts2 ts1) (a,s)

         in
         let a,s = doit a init s init' in
         let a',s' = doit a' init' s' init in
         (a,s,a',s')
      )
      a a' (a,s,a',s')



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom = State.bottom

  let top = State.top

  let is_bottom _ = false

  let subset man sman ctx (a,s) (a',s') =
    let a, s, a', s' = unify man sman ctx (a,s) (a',s') in
    true, s, s'

  let join man sman ctx (a,s) (a',s') =
    let a, s, a', s' = unify man sman ctx (a,s) (a',s') in
    State.join a a', s, s'

  let meet man sman ctx (a,s) (a',s') =
    let a, s, a', s' = unify man sman ctx (a,s) (a',s') in
    State.join a a', s, s'

  let widen man sman ctx (a,s) (a',s') =
    let (a, s, s') = join man sman ctx (a,s) (a',s') in
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
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    (* Ignore invalid accesses as the domain does not report such alarms *)
    | P_null
    | P_invalid
    | P_block ({ base_valid = false }, _, _)
    | P_top ->
      Cases.empty_singleton flow

    | P_block (base, offset, mode) ->
      Cases.singleton (base, offset, mode) flow

    | P_fun _ -> assert false


  (** Predicate defining interesting bases to smash *)
  let rec is_interesting_base base =
    match base with
    | { base_valid = false } -> false

    | { base_kind = Var {vkind = Cstubs.Aux_vars.V_c_primed_base base}; base_valid = true } ->
       is_interesting_base base

    | { base_kind = Var v } when is_c_array_type v.vtyp ->
      (* Keep arrays of scalars or records with fields having the same scalar type *)
      let rec aux t =
        match remove_typedef_qual t with
        | T_c_array(tt,_) -> aux tt
        | T_c_record{c_record_kind = C_struct; c_record_fields = fields} ->
          let rec aux2 = function
            | [] -> None
            | [f] -> if is_c_scalar_type f.c_field_type then Some f.c_field_type else None
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


  (** Fold an exec transfer function over a set of stypes *)
  let fold_stypes (f:styp -> range -> _ man -> 'a flow -> 'a post) ts range man flow =
    STypeSet.fold
        (fun styp acc -> Post.bind (f styp range man) acc)
        ts (Post.return flow)

  (** Fold an exec transfer function smashes of a base *)
  let exec_smashes (f:styp -> range -> _ man -> 'a flow -> 'a post) base a range man flow =
    if not (State.mem base a) then Post.return flow else
    match State.find base a with
      | Init.Bot     | Init.None       -> Post.return flow
      | Init.Full ts | Init.Partial ts -> fold_stypes f ts range man flow

  (** Execute a transfer when a uninit variable exists *)
  let exec_uninit (f:range -> _ man -> 'a flow -> 'a post) base a range man flow =
    if not (State.mem base a) then Post.return flow else
    match State.find base a with
    | Init.Partial ts -> f range man flow
    | _ -> Post.return flow



  (** {2 Environment transfer functions} *)
  (** ********************************** *)

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
    if not (is_interesting_base base) then
      Post.return flow
    else
      match bases with
      | [] -> Post.return flow

      | x::y::z -> panic_at range "smash: folding of multiple bases not supported"

      | [base'] when not (is_interesting_base base') ->
        let a = get_env T_cur man flow in
        let flow = set_env T_cur (State.add base Init.None a) man flow in
        exec_smashes (remove_smash base) base a range man flow >>$ fun () flow ->
        exec_uninit (remove_uninit base) base a range man flow

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
        (* Fold associated variables *)
        exec_smashes (fold_smash base [base']) base' a range man flow >>$ fun () flow ->
        exec_uninit (fold_uninit base [base']) base' a range man flow


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

      match init with
      | Init.Bot -> Post.return flow

      | Init.None -> Post.return flow
        
      | Init.Full ts ->
        (* When base is fully initialized, we remove smashes other
           than the one with the type of lval, which will be put to
           top after *)
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        let flow = set_env T_cur (State.add base (Init.Full (STypeSet.singleton s.styp)) a) man flow in
        if STypeSet.mem s.styp ts then
          man.post (mk_forget_var vsmash range) ~route:scalar flow
        else
          man.post (mk_add_var vsmash range) ~route:scalar flow

      | Init.Partial ts ->
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
        if STypeSet.mem s.styp ts then
          man.post (mk_forget_var vsmash range) ~route:scalar flow
        else
          let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
          man.post (mk_add_var vsmash range) ~route:scalar flow


  (** 𝕊⟦ ∀i ∈ [lo,hi]: forget( *(p + i) ) ⟧ *)
  let exec_forget_quant quants lval range man flow =
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

      | Init.None ->
        if not (is_aligned offset lval.etyp man flow) then
          Post.return flow
        else
          (* In case of unintialized base, three cases are possible:

             Case #1: fully initialized
                0              size - |elm|
                |-------------------|------>
               min                 max

             Case #2: partially initialized
                0              size - |elm|
                |-----------x-------|------>
               min         max

             Case #3: nop
                0              size - |elm|
                |---------x---------|------>
                         min
          *)
          let min, max = Common.Quantified_offset.bound offset quants in
          let elm = mk_z (sizeof_type lval.etyp) range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume_num (eq min zero range)
            ~fthen:(fun flow ->
                man.post (mk_add_var vsmash range) ~route:scalar flow >>$ fun () flow ->
                assume_num (eq max (sub size elm range) range)
                  ~fthen:(fun flow ->
                      (* Case #1 *)
                      let init' = Init.Full (STypeSet.singleton s.styp) in
                      set_env T_cur (State.add base init' a) man flow |>
                      Post.return
                    )
                  ~felse:(fun flow ->
                      (* Case #2 *)
                      let init' = Init.Partial (STypeSet.singleton s.styp) in
                      set_env T_cur (State.add base init' a) man flow |>
                      man.post (mk_add uninit range) ~route:scalar >>$ fun () flow ->
                      man.post (mk_assign uninit (add max elm range) range) ~route:scalar flow
                    ) ~route:scalar man flow
              )
            ~felse:(fun flow ->
                (* Case #3 *)
                Post.return flow
              )  ~route:scalar man flow
        

      | Init.Full ts ->
        (* When base is fully initialized, we remove smashes other
           than the one with the type of lval, which will be put to
           top after *)
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        let flow = set_env T_cur (State.add base (Init.Full (STypeSet.singleton s.styp)) a) man flow in
        if STypeSet.mem s.styp ts then
          man.post (mk_forget_var vsmash range) ~route:scalar flow
        else
          man.post (mk_add_var vsmash range) ~route:scalar flow

      | Init.Partial ts ->
        fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
        let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
        begin if STypeSet.mem s.styp ts then
            man.post (mk_forget_var vsmash range) ~route:scalar flow
          else
            let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
            man.post (mk_add_var vsmash range) ~route:scalar flow
        end >>$ fun () flow ->
        if not (is_aligned offset lval.etyp man flow) then
          Post.return flow
        else
          (* In case of partially intialized base, three cases are possible:

             Case #1: fully initialized
                0      uninit     size - |elm|
                |---x----0----------|------>
                   min             max

             Case #2: partially initialized until max + |elm|
                0      uninit     size - |elm|
                |---x----0------x---|------>
                   min         max

             Case #3: nop
                0  uninit      size - |elm|
                |----0----x---------|------>
                         min
          *)
          let min, max = Common.Quantified_offset.bound offset quants in
          let elm = mk_z (sizeof_type lval.etyp) range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume_num (le min (add uninit elm range) range)
            ~fthen:(fun flow ->
                assume_num (eq max (sub size elm range) range)
                  ~fthen:(fun flow ->
                      (* Case #1 *)
                      let init' = Init.Full (STypeSet.singleton s.styp) in
                      set_env T_cur (State.add base init' a) man flow |>
                      Post.return
                    )
                  ~felse:(fun flow ->
                      (* Case #2 *)
                      man.post (mk_assign uninit (add max elm range) range) ~route:scalar flow
                    ) ~route:scalar man flow
              )
            ~felse:(fun flow ->
                (* Case #3 *)
                Post.return flow
              )  ~route:scalar man flow



  (** {2 Assignment transfer function} *)
  (** ******************************** *)

  (** 𝕊⟦ lval = rval; ⟧ *)
  let exec_assign lval rval range man flow =
    man.eval rval flow >>$ fun rval flow ->
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun (base,offset,mode) flow ->
    if not (is_interesting_base base) then
      Post.return flow
    else
      let a = get_env T_cur man flow in
      (* Non-aligned offset destroys all existing smashes *)
      if not (is_aligned offset lval.etyp man flow) then
        exec_smashes (forget_smash base) base a range man flow
      else
        let s = mk_smash base lval.etyp in
        let vsmash = mk_smash_var s in
        let esmash = mk_smash_expr s ~mode range in
        let elm = mk_z (sizeof_type lval.etyp) range in
        match State.find base a with
        | Init.Bot -> Post.return flow

        | Init.None ->
          man.post (mk_add_var vsmash range) ~route:scalar flow >>$ fun () flow ->
          (* If this is the first time we initialize the base, we look at two cases:

             Case #1: begin intialization
                0             size - |elm|
                |------------------|------>
              offset

             Case #2: nop
                0              size - |elm|
                |--------x----------|------>
                       offset
          *)
          let uninit = mk_uninit_expr base ~mode range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume_num (eq offset zero range)
            ~fthen:(fun flow ->
                (* Case #1 *)
                let init = Init.Partial (STypeSet.singleton s.styp) in
                let flow = set_env T_cur (State.add base init a) man flow in
                man.post (mk_add_var vsmash range) ~route:scalar flow >>$ fun () flow ->
                man.post (mk_add uninit range) ~route:scalar flow >>$ fun () flow ->
                man.post (mk_assign (strongify_var_expr esmash) rval range) ~route:scalar flow >>$ fun () flow ->
                man.post (mk_assign uninit elm range) ~route:scalar flow
              )
            ~felse:(fun flow ->
                (* Case #2 *)
                Post.return flow
              ) ~route:scalar man flow


        | Init.Full ts ->
          (* Destroy existing incompatible smashes *)
          fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
          let flow = set_env T_cur (State.add base (Init.Full (STypeSet.singleton s.styp)) a) man flow in
          if not (STypeSet.mem s.styp ts) then
            (* If this is the first time we access this type, we add
               the corresponding smash *)
            man.post (mk_add_var vsmash range) ~route:scalar flow
          else
            (* If we have already a smash for this type, we update it
               with a weak update *)
            man.post (mk_assign esmash rval range) ~route:scalar flow

        | Init.Partial ts ->
          (* Destroy existing incompatible smashes *)
          fold_stypes (remove_smash base) (STypeSet.remove s.styp ts) range man flow >>$ fun () flow ->
          let flow = set_env T_cur (State.add base (Init.Partial (STypeSet.singleton s.styp)) a) man flow in
          if not (STypeSet.mem s.styp ts) then
            (* If this is the first time we access this type, we add
               the corresponding smash *)
            man.post (mk_add_var vsmash range) ~route:scalar flow
          else
            (* If we have already a smash for this type, we update it
               with a weak update *)
            man.post (mk_assign esmash rval range) ~route:scalar flow >>$ fun () flow ->
            (* To update where we are in the initialization, three cases are considered:

               Case #1: fully initialized
                  0           uninit = size - |elm|
                  |------------------|------>
                                  offset

               Case #2: advance initialization
                  0     uninit    size - |elm|
                  |--------x----------|------>
                         offset

               Case #3: nop
                  0  uninit      size - |elm|
                  |----x------x------|------>
                          offset
            *)
            let uninit = mk_uninit_expr base ~mode range in
            eval_base_size base range man flow >>$ fun size flow ->
            assume_num (eq uninit offset range)
              ~fthen:(fun flow ->
                  assume_num (eq offset (sub size elm range) range)
                    ~fthen:(fun flow ->
                        (* Case #1 *)
                        let init = Init.Full (STypeSet.singleton s.styp) in
                        let flow = set_env T_cur (State.add base init a) man flow in
                        man.post (mk_remove uninit range) ~route:scalar flow
                      )
                    ~felse:(fun flow ->
                        (* Case #2 *)
                        let init = Init.Partial (STypeSet.singleton s.styp) in
                        let flow = set_env T_cur (State.add base init a) man flow in
                        man.post (mk_assign uninit (add uninit elm range) range) ~route:scalar flow
                      ) ~route:scalar man flow
                )
              ~felse:(fun flow ->
                  (* Case #3 *)
                  let init = Init.Partial (STypeSet.singleton s.styp) in
                  let flow = set_env T_cur (State.add base init a) man flow in
                  Post.return flow
                ) ~route:scalar man flow


  (** {2 Quantified tests transfer functions} *)
  (** *************************************** *)

  (** 𝕊⟦ ∀i ∈ [lo,hi] : *(p + i) ? e ⟧ *)
  let exec_assume_quant i lo hi op qe e range man flow =
    man.eval e flow >>$ fun e flow ->
    eval_pointed_base_offset (mk_c_address_of qe range) range man flow >>$ fun (base,offset,mode) flow ->
    let t = get_c_deref_type qe in
    if not (is_interesting_base base) || not (is_aligned offset t man flow) then
      Post.return flow
    else
      let a = get_env T_cur man flow in
      match State.find base a with
      | Init.Bot | Init.None -> Post.return flow

      | Init.Partial ts ->
        (* When base is partially initialized, check these cases:

           Case #1: predicate on the entire initialized part
              0           uninit - |elm|
              |----------------|------>
             min              max

           Case #2: otherwise nop
        *)
        let s = mk_smash base t in
        phi s range man flow >>$ fun () flow ->
        (* Check valuation range of the offset *)
        let min, max = Common.Quantified_offset.bound offset [FORALL,i,S_interval(lo,hi)] in
        let elm = mk_z (sizeof_type t) range in
        let uninit = mk_uninit_expr base ~mode range in
        eval_base_size base range man flow >>$ fun size flow ->
        assume (log_and
                  (eq min zero range)
                  (eq max (sub uninit elm range) range)
                  range)
          ~fthen:(fun flow ->
              (* Case #1 *)
              let smash = mk_smash_expr s ~typ:(Some t) ~mode:(Some STRONG) range in
              (* Add a cast if the type of the lvalue is different than the type of expression qe *)
              let smash = if compare_typ t qe.etyp = 0 then smash else mk_c_cast smash qe.etyp range in
              man.post (mk_assume (mk_binop smash op e ~etyp:T_bool range) range) ~route:scalar flow
            )
          ~felse:(fun flow ->
              (* Case #2 *)
              Post.return flow
            ) ~route:scalar man flow
        

      | Init.Full ts ->
        (* When base is fully initialized, check these cases:

           Case #1: predicate on the entire memory block
              0            size - |elm|
              |----------------|------>
             min              max

           Case #2: nop
              0            size - |elm|
              |--x-------x------|------>
                min     max
        *)
        let s = mk_smash base t in
        phi s range man flow >>$ fun () flow ->
        (* Check valuation range of the offset *)
        let min, max = Common.Quantified_offset.bound offset [FORALL,i,S_interval(lo,hi)] in
        let elm = mk_z (sizeof_type t) range in
        eval_base_size base range man flow >>$ fun size flow ->
        assume (log_and
                  (eq min zero range)
                  (eq max (sub size elm range) range)
                  range)
          ~fthen:(fun flow ->
              (* Case #1 *)
              let smash = mk_smash_expr s ~typ:(Some t) ~mode:(Some STRONG) range in
              (* Add a cast if the type of the lvalue is different than the type of expression qe *)
              let smash = if compare_typ t qe.etyp = 0 then smash else mk_c_cast smash qe.etyp range in
              man.post (mk_assume (mk_binop smash op e ~etyp:T_bool range) range) ~route:scalar flow
            )
          ~felse:(fun flow ->
              (* Case #2 *)
              Post.return flow
            ) ~route:scalar man flow


  (** 𝕊⟦ ∀i ∈ [lo,hi] : *(p + i) == *(q + i) ⟧ *)
  let exec_assume_quant2_num_eq i lo hi qe1 qe2 range man flow =
    eval_pointed_base_offset (mk_c_address_of qe1 range) range man flow >>$ fun (base1,offset1,mode1) flow ->
    eval_pointed_base_offset (mk_c_address_of qe2 range) range man flow >>$ fun (base2,offset2,mode2) flow ->
    let t1 = get_c_deref_type qe1 in
    let t2 = get_c_deref_type qe2 in
    if not (is_interesting_base base1) || not (is_interesting_base base2) ||
       not (is_aligned offset1 t1 man flow) || not (is_aligned offset2 t2 man flow)
    then
      Post.return flow
    else
      let a = get_env T_cur man flow in
      match State.find base1 a, State.find base2 a with
      | Init.Full ts1, Init.Full ts2 ->
        (* Add the smashes if not existent *)
        let s1 = mk_smash base1 t1 in
        let s2 = mk_smash base2 t2 in
        phi s1 range man flow >>$ fun () flow ->
        phi s2 range man flow >>$ fun () flow ->
        (* Ensure that elements of a block that is entirely covered have values in the other block *)
        let ensure_included base offset t qet s other_base other_offset other_t other_qet other_s range man flow =
          let min, max = Common.Quantified_offset.bound offset [FORALL,i,S_interval(lo,hi)] in
          let elm = mk_z (sizeof_type t) range in
          eval_base_size base range man flow >>$ fun size flow ->
          assume (log_and
                    (eq min zero range)
                    (eq max (sub size elm range) range)
                    range)
          ~fthen:(fun flow ->
                let smash = mk_smash_expr s ~typ:(Some t) ~mode:(Some STRONG) range in
                let smash = if compare_typ t qet = 0 then smash else mk_c_cast smash qet range in
                let other_smash = mk_smash_expr other_s ~typ:(Some other_t) ~mode:(Some WEAK) range in
                let other_smash = if compare_typ other_t other_qet = 0 then other_smash else mk_c_cast other_smash other_qet range in
                man.post (mk_assume (eq smash other_smash range) range) ~route:scalar flow
              )
          ~felse:(fun flow ->
              Post.return flow
            ) ~route:scalar man flow
        in
        ensure_included base1 offset1 t1 qe1.etyp s1 base2 offset2 t2 qe2.etyp s2 range man flow >>$ fun () flow ->
        ensure_included base2 offset2 t2 qe2.etyp s2 base1 offset1 t1 qe1.etyp s1 range man flow

      | _ -> Post.return flow


  (** {2 Exec entry point} *)
  (** ******************** *)

  let exec stmt man flow =
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

    | S_forget(e) when is_c_deref e ->
      exec_forget e stmt.srange man flow |>
      OptionExt.return

    | S_forget({ ekind = E_stub_quantified_formula(quants, e) })
      when is_c_deref e ->
      exec_forget_quant quants e stmt.srange man flow |>
      OptionExt.return

    | S_remove(e) when is_base_expr e ->
      exec_remove_base (expr_to_base e) stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      exec_assign lval rval stmt.srange man flow |>
      OptionExt.return

    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_binop(op, e1, e2)}) })
      when is_comparison_op op &&
           is_c_deref e1 &&
           is_c_int_type i.vtyp &&
           is_var_in_expr i e1 &&
           not (is_var_in_expr i e2) ->
      exec_assume_quant i a b op e1 e2 stmt.srange man flow |>
      OptionExt.return

    | S_assume({ ekind = E_stub_quantified_formula([FORALL,i,S_interval(a,b)], { ekind = E_binop(O_eq, e1, e2)}) })
      when is_c_deref e1 &&
           is_c_deref e2 &&
           is_c_int_type e1.etyp &&
           is_c_int_type e2.etyp &&
           is_c_int_type i.vtyp &&
           is_var_in_expr i e1 &&
           is_var_in_expr i e2 ->
      exec_assume_quant2_num_eq i a b e1 e2 stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Abstract evaluation of a dereference *)
  let eval_deref p range man flow =
    eval_pointed_base_offset p range man flow >>$ fun (base,offset,mode) flow ->
    if not (is_interesting_base base) || not (is_aligned offset (under_type p.etyp) man flow) then
      man.eval (mk_top (under_type p.etyp) range) ~route:scalar flow
    else
      let a = get_env T_cur man flow in
      let s = mk_smash base (under_type p.etyp) in
      let init = State.find base a in
      (* Synthesize the smash if it is new and there are some existing constraints *)
      phi s range man flow >>$ fun () flow ->
      let esmash = mk_smash_expr s ~typ:(Some (under_type p.etyp)) ~mode range in
      match init with
      | Init.Bot | Init.None ->  man.eval (mk_top (under_type p.etyp) range) ~route:scalar flow

      | Init.Full ts -> man.eval esmash ~route:scalar flow

      | Init.Partial ts ->
        (* When accessing a partially initialized base, two cases are possible

           Case #1: before the uninitialized part
              0                uninit
              |------x------------|------>
                  offset

           Case #2: after the uninitialized part
              0     uninit
              |--------|----------x------>
                                 offset
        *)
        let uninit = mk_uninit_expr base ~mode range in
        assume_num (ge offset uninit range)
          ~fthen:(fun flow ->
              (* Case #2 *)
              man.eval (mk_top (under_type p.etyp) range) ~route:scalar flow
            )
          ~felse:(fun flow ->
              (* Case #1 *)
              man.eval esmash ~route:scalar flow
            ) ~route:scalar man flow



  (** Evaluations entry point *)
  let eval exp man flow =
    match ekind exp with
    | E_c_deref p when is_c_scalar_type exp.etyp ->
      eval_deref p exp.erange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  register_stacked_domain (module Domain)
