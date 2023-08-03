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

(** [Nonrel ∈ 𝒱 → 𝒟] lifts a non-relational value abstraction into an
    abstract domain of partial environments from variables to values.
*)

open Core.All
open Sig.Abstraction.Value
open Mopsa_utils


(** {2 Identifier for the non-relation domain} *)
(** ****************************************** *)

type _ id += D_nonrel : (module VALUE with type t = 'v) -> (var,'v) Lattices.Partial_map.map id

let () =
  let open Eq in
  register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a, b) eq option = fun next id1 id2 ->
        match id1, id2 with
        | D_nonrel v1, D_nonrel v2 ->
          begin
            let module V1 = (val v1) in
            let module V2 = (val v2) in
            match equal_id V1.id V2.id with
            | Some Eq -> Some Eq
            | None -> None
          end
        | _ -> next.eq id1 id2
      in
      f
    );
  }


(** {2 Variable's context} *)
(** ********************** *)

(** The context of a variable keeps (flow-insensitive) information about the
   variable that can pushed by external domains and consumed by the value
   abstraction.

   This is useful to implement a widening with thresholds: external
   heuristics discover the theresholds and put them in the context of the
   variable. When [widen] is called on a the value of a variable, it is enriched
   with its context. *)

module K = GenContextKey(struct
    type 'a t = 'a ctx VarMap.t
    let print pp fmt m =
      Format.fprintf fmt "variables contexts:@, @[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt (v,c) -> Format.fprintf fmt "%a:@, @[<v>%a@]" pp_var v (pp_ctx pp) c)
        ) (VarMap.bindings m)
  end)

let var_ctx_key = K.key

let add_var_ctx var k v ctx =
  let map = try find_ctx var_ctx_key ctx with Not_found -> VarMap.empty in
  let vctx = try VarMap.find var map with Not_found -> empty_ctx in
  add_ctx var_ctx_key (VarMap.add var (add_ctx k v vctx) map) ctx

let find_var_ctx_opt var k ctx =
  match find_ctx_opt var_ctx_key ctx with
  | None     -> None
  | Some map ->
    match VarMap.find_opt var map with
    | None      -> None
    | Some vctx -> find_ctx_opt k vctx

let find_var_ctx var k ctx =
  match find_var_ctx_opt var k ctx with
  | None   -> raise Not_found
  | Some v -> v

let remove_var_ctx var k ctx =
  try
    let map = find_ctx var_ctx_key ctx in
    let vctx = VarMap.find var map in
    add_ctx var_ctx_key (VarMap.add var (remove_ctx k vctx) map) ctx
  with Not_found ->
    ctx


(** {2 Variable bounds} *)
(** ******************* *)

(** The bounds of a variable is an invariant about its value that is always valid.
    It is put in the context of the variable and is used to refine its value whenever it
    changes. *)

module VarBoundsKey = GenContextKey(struct
    type 'a t = constant
    let print pp fmt c =
      Format.fprintf fmt "@[<h>bounds: %a@]" pp_constant c
  end)

(** Context for saving the bounds of a variable *)
let var_bounds_ctx = VarBoundsKey.key

(** Add the bounds of a variable to context *)
let add_var_bounds_ctx v b ctx =
  add_var_ctx v var_bounds_ctx b ctx

(** Add the bounds of a variable to flow *)
let add_var_bounds_flow v b flow =
  let ctx = add_var_bounds_ctx v b (Flow.get_ctx flow) in
  Flow.set_ctx ctx flow

(** Remove the bounds of a variable from context *)
let remove_var_bounds_ctx v ctx =
  remove_var_ctx v var_bounds_ctx ctx

(** Remove the bounds of a variable from flow *)
let remove_var_bounds_flow v flow =
  let ctx = remove_var_bounds_ctx v (Flow.get_ctx flow) in
  Flow.set_ctx ctx flow


(** Find the bounds of a variable in context *)
let find_var_bounds_ctx_opt v ctx =
  find_var_ctx_opt v var_bounds_ctx ctx


(** {2 Non-relational domain} *)
(** ************************* *)

module Make(Value: VALUE) :
  Sig.Abstraction.Simplified.SIMPLIFIED
  with type t = (var,Value.t) Lattices.Partial_map.map =
struct


  (** {2 Domain header} *)
  (** ***************** *)

  (** Map with variables as keys. *)
  module VarMap =
    Lattices.Partial_map.Make
      (Var)
      (Value)

  include VarMap

  let id = D_nonrel (module Value)

  let name = "framework.abstraction.combiners.value.nonrel"

  let debug fmt = Debug.debug ~channel:name fmt

  let merge pre (a1, e1) (a2, e2) =
    let a1', a2' = generic_merge ~add ~remove ~find (a1, e1) (a2, e2) in
    try VarMap.map2zo
      (fun _ v1 -> v1)
      (fun _ v2 -> v2)
      (fun _ v1 v2 ->
         let v = Value.meet v1 v2 in
         if Value.is_bottom v then raise Bot.Found_BOT else v
      ) a1' a2'
    with Bot.Found_BOT -> VarMap.bottom

  (* This value manager isn't able to perform sub-evaluations.
     It is used only to compute values of constants used as bounds of variables. *)
  let imprecise_value_man = {
    bottom = Value.bottom;
    top    = Value.top;
    is_bottom = Value.is_bottom;
    subset = Value.subset;
    join = Value.join;
    meet = Value.meet;
    print = Value.print;
    get = (fun v -> v);
    set = (fun v _ -> v);
    eval = (fun e -> Value.top);
    avalue = (fun avk v ->
        match Value.avalue avk v with
        | Some r -> r
        | None -> top_avalue avk);
    ask = (fun q -> Exceptions.panic "Queries not accessible");
  }

  let bound_range = Location.mk_fresh_range ()

  (* Constrain the value of a variable with its bounds *)
  let meet_with_bound_constraints ctx var v =
    match find_var_bounds_ctx_opt var ctx with
    | None        -> v
    | Some bounds ->
      let vv = Value.eval imprecise_value_man (mk_constant bounds bound_range ~etyp:var.vtyp) in
      Value.meet v vv

  let widen ctx a1 a2 =
    let open Bot_top in
    if a1 == a2 then a1 else
      match a1, a2 with
      | BOT, x | x, BOT -> x
      | TOP, x | x, TOP -> TOP
      | Nbt m1, Nbt m2 ->
        Nbt (
          MapExtPoly.map2zo
            (fun _ v1 -> v1)
            (fun _ v2 -> v2)
            (fun var v1 v2 ->
               let vctx =
                 match find_ctx_opt var_ctx_key ctx with
                 | None   -> empty_ctx
                 | Some map ->
                   match Core.Ast.Var.VarMap.find_opt var map with
                   | None   -> empty_ctx
                   | Some c -> c
               in
               let w = Value.widen vctx v1 v2 in
               (* Apply the bounds constraints*)
               meet_with_bound_constraints ctx var w
            )
            m1 m2
        )

  let top_of_typ typ range =
    Value.eval imprecise_value_man (mk_top typ range)

  let add ctx var v a =
    let vv = meet_with_bound_constraints ctx var v in
    VarMap.add var vv a


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Value manager *)
  let rec value_man (cache: Value.t vexpr) (map:t) : (Value.t,Value.t) value_man = {
    imprecise_value_man with
    eval = (fun e ->
        match find_vexpr_opt e cache with
        | Some (v,_) -> v
        | None ->
          match eval e map with
          | Some (v,_) -> v
          | None -> Value.top
      );
    ask = (fun query ->
        match Value.ask (value_man cache map) query with
        | Some r -> r
        | None   -> raise Not_found
      );
  }


  (** Forward evaluation returns the abstract value of the expression,
     but also a tree annotated by the intermediate abstract
     values for each sub-expression *)
  and eval (e:expr) (a:t) : (Value.t * Value.t vexpr) option =
    if not (Value.accept_type e.etyp) then None
    else
      match ekind e with
      | E_var(var, mode) ->
        (* Get the value of the variable from the map *)
        let v = find var a in
        (v, empty_vexpr) |>
        OptionExt.return

      | _
        when for_all_expr
            (fun ee -> Value.accept_type ee.etyp)
            (fun s -> false) e
        ->
        (* Before asking the domain to evaluate the expression, evaluate each sub-expression
           their evaluations in the manager. This will speedup the evaluation when the domain
           [Value.eval] will request these values.  *)
        let parts,build = structure_of_expr e in
        let rec iter = function
          | [] -> Some empty_vexpr
          | ee::tl ->
            eval ee a |> OptionExt.bind @@ fun (vv,vee) ->
            iter tl |> OptionExt.lift @@ fun tl ->
            add_vexpr ee vv vee tl
        in
        iter parts.exprs |> OptionExt.lift @@ fun ve ->
        let v = Value.eval (value_man ve a) e in
        (v,ve)

      | _ -> None


  (** Backward refinement of expressions; given an annotated tree, and
      a target value, refine the environment using the variables in the
      expression *)
  let rec refine ctx (e:expr) (ve:Value.t vexpr) (r:Value.t) (a:t) : t =
    if Value.is_bottom r then bottom else
    match e.ekind with
      | E_var(var,mode) ->
        (* Refine the value of the variable in the map *)
      if var_mode var mode = WEAK then a
      else add ctx var (Value.meet (find var a) r) a

    | _ ->
      (* Refine the sub-expressions by calling [Value.backward].
       * Note that we need to apply this function to the root sub-expressions only *)
      let veroot = root_vexpr ve in
      let veroot' = Value.backward (value_man ve a) e veroot r in
      (* Go back to the whole value expression by merging [veroot'] with [ve].
         Missing sub-expressions in [veroot'] will be copied from [ve]. *)
      let ve' = merge_vexpr Value.meet ve veroot' in
      fold_root_vexpr
        (fun acc ee vv eev -> refine ctx ee eev vv acc)
        a ve'


  (* utility function to reduce the complexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
  *)
  let rec filter ctx (e:expr) (b:bool) (a:t) : t option =
    match ekind e with
    | E_unop (O_log_not, e) ->
      filter ctx e (not b) a

    | E_binop (O_log_and, e1, e2) ->
      filter ctx e1 b a |> OptionExt.bind @@ fun a1 ->
      filter ctx e2 b a |> OptionExt.bind @@ fun a2 ->
      (if b then meet else join) a1 a2 |>
      OptionExt.return

    | E_binop (O_log_or, e1, e2) ->
      filter ctx e1 b a |> OptionExt.bind @@ fun a1 ->
      filter ctx e2 b a |> OptionExt.bind @@ fun a2 ->
      (if b then join else meet) a1 a2 |>
      OptionExt.return

    | E_binop (O_log_xor, e1, e2) ->
      filter ctx e1 b a |> OptionExt.bind @@ fun a1 ->
      filter ctx e2 b a |> OptionExt.bind @@ fun a2 ->
      filter ctx e1 (not b) a |> OptionExt.bind @@ fun na1 ->
      filter ctx e2 (not b) a |> OptionExt.bind @@ fun na2 ->
      (if b
       then join (meet a1 na2) (meet na1 a2)
       else join (meet a1 a2) (meet na1 na2)
      ) |>
      OptionExt.return

    (* arithmetic comparison part, handled by Value *)
    | E_binop (op, e1, e2) when is_comparison_op op ->
      (* evaluate forward each argument expression *)
      eval e1 a |> OptionExt.bind @@ fun (v1,ve1) ->
      eval e2 a |> OptionExt.bind @@ fun (v2,ve2) ->

      (* apply comparison *)
      let r1,r2 = Value.compare (value_man empty_vexpr a) op b e1 v1 e2 v2 in
      (* propagate backward on both argument expressions *)
      refine ctx e1 ve1 (Value.meet v1 r1) a |>
      refine ctx e2 ve2 (Value.meet v2 r2) |>
      OptionExt.return

    | _ ->
      (* Filter on arbitrary expressions (variables, predicates, etc.).
         First, evaluate the expression *)
      eval e a |> OptionExt.lift @@ fun (v,ve) ->
      (* Then filter the obtained value to match the truth value [b] *)
      let w = Value.filter b e.etyp v in
      (* Now refine the sub-expresions w.r.t. the filtered value *)
      refine ctx e ve (Value.meet v w) a


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog = empty

  let pp_list pp sep fmt l =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt sep) pp fmt l
  
  let pp_pair pp_fst pp_snd fmt (a, b) = Format.fprintf fmt "(%a, %a)" pp_fst a pp_snd b
  
  let pp_bracketed_list pp_elem fmt xs = Format.fprintf fmt "[%a]" (pp_list pp_elem ", ") xs
  
  
  let exec stmt man ctx (map:t) : t option =
    match skind stmt with
    | S_remove { ekind = E_var (v, _) } when Value.accept_type v.vtyp ->
      VarMap.remove v map |>
      OptionExt.return

    | S_add { ekind = E_var (v, _) } when Value.accept_type v.vtyp ->
      (* Check if the variable is already present *)
      if VarMap.mem v map
      then OptionExt.return map
      else
        add ctx v (top_of_typ v.vtyp stmt.srange) map |>
        OptionExt.return


    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars ->
      let vars = List.map (function
            | { ekind = E_var (v, _) } -> v
            | _ -> assert false
          ) vars
      in
      List.fold_left
        (fun acc v ->
           add ctx v (find v map) acc
        ) empty vars
      |> OptionExt.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) when Value.accept_type var1.vtyp ->
      let v = find var1 map in
      remove var1 map |>
      add ctx var2 v |>
      OptionExt.return

    | S_forget { ekind = E_var (var, _) } when Value.accept_type var.vtyp ->
      add ctx var (top_of_typ var.vtyp stmt.srange) map |>
      OptionExt.return

    | S_assign ({ ekind= E_var (var, mode) }, e) when Value.accept_type var.vtyp ->
      eval e map |> OptionExt.lift @@ fun (v,_) ->
      let map' = add ctx var v map in
      begin
        match var_mode var mode with
        | STRONG -> map'
        | WEAK -> join map map'
      end

    | S_expand ({ekind = E_var (v, _)}, vl)
      when Value.accept_type v.vtyp &&
           List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let value = find v map in
      List.fold_left (fun acc v' ->
          add ctx v' value acc
        ) map vl |>
      OptionExt.return

    | S_fold ({ekind = E_var (v, mode)}, vl)
      when Value.accept_type v.vtyp &&
           List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      (* Collect values of variables vl before removing them from the map *)
      let value,map' = List.fold_left
          (fun (accv,accm) -> function
             | { ekind = E_var (vv, _) } ->
               let accv' = find vv map |>
                           Value.join accv
               in
               let accm' = remove vv accm in
               accv',accm'
             | _ -> assert false
        ) (Value.bottom,map) vl
      in
      let value' =
        if mem v map then
          Value.join value (find v map)
        else
          value
      in
      add ctx v value' map' |>
      OptionExt.return

    | S_assume e ->
      filter ctx e true map

    | S_havoc ->
      let dom = VarMap.fold (fun v a c -> v :: c) map [] in
      let () = Debug.debug ~channel:"havoc" "non-rel havoc for %a" (pp_bracketed_list pp_var) dom in
      Some map

    | _ -> None

  let ask : type r.
    ('a,r) query ->
    ('a,t) Sig.Abstraction.Simplified.simplified_man ->
    'a ctx -> t -> r option =
    fun query man ctx map ->
    match query with
    | Q_avalue(e,av) when Value.accept_type e.etyp ->
      eval e map |> OptionExt.bind @@ fun (v,ve) ->
      Value.avalue av v

    | _ -> Value.ask (value_man empty_vexpr map) query

  let print_state printer a =
    Print.pprint printer ~path:[Key Value.display]
      (pbox VarMap.print a)

  let print_expr man ctx a printer exp =
    match eval exp a with
    | None -> ()
    | Some (v,_) ->
      Print.pprint printer
        ~path:[ Key Value.display;
                fkey "%a" pp_expr exp ]
        (pbox Value.print v)
end
