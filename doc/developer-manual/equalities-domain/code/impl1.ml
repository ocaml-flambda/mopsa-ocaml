(** Equalities numerical domain *)
(* type t = ℘(℘(𝒱))
   𝛾(S♯) = { ρ ∈ 𝒱 → 𝕍 ∣ ∀s ∈ S♯, ∃v ∈ 𝕍, ∀x ∈ s, 𝜌(x) = v}
*)
(* This is a simplified version, no evaluation is performed *)

open Mopsa
open Sig.Abstraction.Simplified

module SimplifiedDomain =
struct

  (* Lattice structure *)

  module VarSetSet = SetExt.Make(VarSet)
  type t = VarSetSet.t Bot.with_bot

  let print =
    Bot.bot_fprint (
        VarSetSet.fprint SetExt.printer_default
          (VarSet.fprint (SetExt.({print_empty = "" ; print_begin = "" ;
                                   print_sep = " = " ; print_end = ""}))
             pp_var
          )
      )

  let top = Bot.Nb VarSetSet.empty
  let bottom = Bot.BOT
  let is_bottom (x: t) = match x with | Bot.BOT -> true | Bot.Nb _ -> false

  (* [wf ss] ensures the well-formedness of ss, meaning that no two
     partitions intersects, and no partition is of size less than 1 *)
  let wf (ss: t): t =
    Bot.bot_lift1 (fun ss -> 
        let l = VarSetSet.elements ss in
        let rec aux l res = match l with
          | p::q ->
            begin
              if VarSet.cardinal p <= 1 then aux q res
              else 
                match find_inter p q [] with
                | None ->
                  aux q (p :: res)
                | Some (h, rest) ->
                  let newl = ((VarSet.union p h)::rest) in
                  aux newl res
            end
          | [] -> res
        and find_inter p l start =
          match l with
          | h::r ->
            if not (VarSet.is_empty (VarSet.inter p h)) then Some (h, List.rev_append start r)
            else find_inter p r (h :: start)
          | [] -> None
        in
        aux l [] |> VarSetSet.of_list
      ) ss

  (* ⊔ *)
  let join ss ss' =
    Bot.bot_neutral2 (fun ss ss' -> 
        VarSetSet.fold (fun s acc ->
            VarSetSet.fold (fun s' acc ->
                let inter = VarSet.inter s s' in
                if not (VarSet.cardinal inter <= 1) then
                  VarSetSet.add inter acc
                else acc
              ) ss' acc
          ) ss VarSetSet.empty
      ) ss ss'

  (* ⊓ *)
  let meet (ss: t) (ss': t) : t =
    Bot.bot_absorb2 (fun ss ss' -> 
        Bot.Nb (VarSetSet.union ss ss')
      ) ss ss'
    |> wf

  (* ▿ *)
  let widen _ = join

  (* ⊑ *)
  let subset (ss: t) (ss': t) =
    let ss, ss' = wf ss, wf ss' in
    Bot.bot_included (fun ss ss' -> 
        VarSetSet.for_all (fun s' ->
            VarSetSet.exists (fun s ->
                VarSet.subset s' s
              ) ss
          ) ss'
      ) ss ss'

  (* Domain identification *)

  let name = "universal.simple_equalities"

  include GenDomainId(struct
      type nonrec t = t
      let name = name
    end
    )

  (* Transfer functions *)
      
  let init prog = top

    (** [remove_var v ss] removes variable [v] from abstract state [ss] *)
  let remove_var (v: var) (ss: t) =
    Bot.bot_lift1 (VarSetSet.map (VarSet.remove v)) ss
  let merge (pre: t) ((post1, s1): t * block) ((post2, s2): t * block): t =
    let x1,x2 =
      Log.generic_domain_merge
        ~add:(fun v () x -> x)
        ~find:(fun v x -> ())
        ~remove:(fun v x -> remove_var v x)
        (post1, s1) (post2, s2)
    in
    meet x1 x2

  (** [rename_var v v' ss] rename variable [v] to [v'] in abstract state [ss] *)
  let rename_var (v: var) (v': var) (ss: t) =
    Bot.bot_lift1 (fun ss ->
        VarSetSet.map (fun s ->
            VarSet.map (fun v'' -> if compare_var v'' v = 0 then v' else v'') s
          ) ss
      ) ss

  (* [is_eq v v' ss] tests whether [ss] implies that [v] is equal to [v']*)
  let is_eq (v: var) (v': var) (ss: t) =
    Bot.bot_dfl1 (true) (fun ss ->
        VarSetSet.exists (fun s -> VarSet.mem v s && VarSet.mem v' s) ss
      ) ss

  (* [assume_eq v v' ss] filters [ss] to add the assumption that [v] is equal to [v'] *)
  let assume_eq (v: var) (v': var) (ss: t) =
    Bot.bot_lift1 (fun env -> VarSetSet.add (VarSet.of_list [v; v'])
                      env) ss
    |> wf

  (* [assume_neq v v' ss] filters [ss] to add the assumption that [v] is different from [v'] *)
  let assume_neq (v: var) (v': var) (ss: t) =
    if is_eq v v' ss then
      Bot.BOT
    else ss

  let exec (stmt: stmt) _ _ (env: t) =
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      env
      |> OptionExt.return

    | S_remove { ekind = E_var (var, _) } ->
      remove_var var env |>
      OptionExt.return

    | S_forget { ekind = E_var (var, _) } ->
      remove_var var env |>
      OptionExt.return


    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) })
      ->
      rename_var var1 var2 env
      |> OptionExt.return

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars
      ->
      let vars = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vars
      in
      List.fold_left (fun acc v ->
          remove_var v acc
        ) env vars
      |> OptionExt.return

    | S_assign({ ekind = E_var (v, mode) }, { ekind = E_var (v', _) }) -> 
      let strong_assign = assume_eq v v' env in
      begin
        match var_mode v mode with
        | STRONG -> OptionExt.return strong_assign
        | WEAK   -> OptionExt.return (join strong_assign env)
      end

    | S_assign({ ekind = E_var (v, mode) }, _) ->
      remove_var v env
      |> OptionExt.return

    | S_expand({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let envl = List.map (fun x -> rename_var v x env) vl in
      List.fold_left join bottom envl
      |> OptionExt.return

    | S_fold({ekind = E_var (v, _)}, vl)
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let envl = List.map (fun x -> rename_var x v env) vl in
      List.fold_left meet bottom envl
      |> OptionExt.return

    | S_assume({ekind = E_binop(O_eq, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})})})
      ->
      assume_eq v v' env
      |> OptionExt.return

    | S_assume({ekind = E_binop(O_ne, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})})
    | S_assume({ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})})})
      ->
      assume_neq v v' env
      |> OptionExt.return

    | S_assume _ ->
      env |> OptionExt.return

    | _ ->
      None

  (** [related_vars v ss] returns the list of variables, related to [v] in [ss] *)
  let related_vars (v: var) (ss: t) =
    Bot.bot_dfl1 [] (fun ss ->
        try
          VarSetSet.find_first (fun s -> VarSet.mem v s) ss |>
          VarSet.remove v |>
          VarSet.elements
        with
        | Not_found -> []
      ) ss

  let ask : type r. ('a,r) query -> ('a,t) simplified_man -> 'a ctx -> t -> r option =
    fun query man ctx env ->
    match query with
    | Numeric.Common.Q_int_interval({ekind = E_binop(Ast.O_minus, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})}) ->
      Bot.Nb (ItvUtils.IntItv.of_int 0 0)
      |> OptionExt.return
    | _ -> None

end

let () =
  register_simplified_domain (module SimplifiedDomain)
