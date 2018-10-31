let debug fmt = ToolBox.debug "regular automaton" fmt

module Make(S : SIG.STATE)(A : SIG.COMPARABLE) =
struct

  type state = S.t
  let state_generator () =
    let () = S.restart () in
    fun () -> S.fresh ()

  let var_of_state x=
    Printf.sprintf "v_%s" (ToolBox.format_to_string S.print x)

  let print_state = S.print

  (** Transition *)
  module DetTrans = Map.Make(struct type t = A.t * state let compare = ToolBox.pair_compare A.compare S.compare end )
  type trans = S.t DetTrans.t
  let print_dettrans = ToolBox.print_map
      (ToolBox.print_pair A.print print_state)
      print_state
      DetTrans.bindings

  (** Set of states *)
  module SS    = Set.Make(struct type t = state let compare = S.compare end)
  type states = SS.t
  let print_states = ToolBox.print_set_inline S.print SS.elements

  (** algebra *)
  module Algebra = RegularAlgebra.Make(A)

  let pp_sep_comma = fun fmt () -> Format.fprintf fmt ","

  let print_state_list fmt l =
    Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:(pp_sep_comma) print_state) l

  let print_transition_list fmt l =
    Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:pp_sep_comma (fun fmt (s,c,s') -> Format.fprintf fmt "(%a,%a,%a)" print_state s A.print c print_state s')) l

  type t =
    {
      states       : states ;
      algebra      : Algebra.t ;
      start        : state ;
      final        : states  ;
      trans        : trans ;
      hole         : state ;
      hole_coreach : bool;
    }

  let print fmt (x : t) =
    Format.fprintf fmt "@[<v 2>{@,@[<v>states: %a@,algebra: %a@,start: %a@,final: %a@,trans: %a@,hole: %a;@,hole_coreach: %a@]@,}@]"
      print_states x.states
      Algebra.print x.algebra
      print_state x.start
      print_states x.final
      print_dettrans x.trans
      print_state x.hole
      Format.pp_print_bool x.hole_coreach

  let fold_trans_nh f u acc =
    DetTrans.fold (fun (letter, q) q' acc ->
        f (q, letter, q') acc
      ) u.trans acc

  let delta (q: state) (a: A.t) (u: t): state =
    try
      DetTrans.find (a, q) u.trans
    with
    | Not_found -> u.hole

  let get_states x = x.states
  let rec create_fresh_state (s: SS.t) (f: unit -> S.t) =
    let n = f () in
    if SS.mem n s then
      create_fresh_state s f
    else n

  let get_algebra x = x.algebra

  module RSS = Rewrite.Make(S)(S)

  let make_non_coreachable_hole (u: t) =
    if u.hole_coreach then
      begin
        let all_states = get_states u in
        let a = get_algebra u in
        let () = S.restart () in
        let new_hole = S.fresh () in
        let r = RSS.empty in
        let n_states_minus_hole, r = SS.fold (fun q (n_states', r) ->
            let r, q' = RSS.get q S.fresh r in
            (SS.add q' n_states', r)
          ) u.states (SS.empty, r)
        in
        let n_states = SS.add new_hole n_states_minus_hole in
        let n_start = RSS.apply u.start r in
        let n_final = SS.map (fun x -> RSS.apply x r) u.final in
        let n_trans =
          Algebra.fold (fun letter trans ->
              SS.fold (fun q trans ->
                  let q' = delta q letter u in
                  let nq = RSS.apply q  r in
                  let nq'= RSS.apply q' r in
                  DetTrans.add (letter, nq) nq' trans
                ) all_states trans
            ) a DetTrans.empty
        in
        {
          states  = n_states;
          algebra = a;
          start   = n_start;
          final   = n_final;
          trans   = n_trans;
          hole    = new_hole;
          hole_coreach = false;
        }
      end
    else u

  (** Non deterministic automaton *)
  type nd_trans = SS.t DetTrans.t
  let print_nd_trans = ToolBox.print_map
      (ToolBox.print_pair A.print print_state)
      print_states
      DetTrans.bindings

  module PartS = (struct type t = states let print = print_states let compare = SS.compare end)
  module RPart = Rewrite.Make(PartS)(S)
  type nfa =
    {
      nd_states  : SS.t;
      nd_algebra : Algebra.t;
      nd_init    : S.t;
      nd_trans   : nd_trans;
      nd_finals  : SS.t;
    }

  let print_nfa fmt (x : nfa) =
    Format.fprintf fmt "@[<v 2>{@,@[<v>nd_states: %a@,nd_algebra: %a@,nd_init: %a@,nd_finals: %a@,nd_trans: %a@]@,}@]"
      print_states x.nd_states
      Algebra.print x.nd_algebra
      print_state x.nd_init
      print_states x.nd_finals
      print_nd_trans x.nd_trans

  let delta_nfa q a (u: nfa) =
    try
      DetTrans.find (a, q) u.nd_trans
    with
    | Not_found -> SS.empty

  let nd_add_trans (s,c,s') tr =
    if DetTrans.mem (c,s) tr then
      let t = DetTrans.find (c,s) tr in
      tr |> DetTrans.add (c,s) (SS.add s' t)
    else
      tr |> DetTrans.add (c,s) (SS.singleton s')

  let nd_fold_trans f x acc =
    DetTrans.fold (fun (a, q) v acc ->
        SS.fold (fun q' acc ->
            f (q, a, q') acc
          ) v acc
      ) x acc

  let determinize (u: nfa) : t =
    let ng = state_generator () in
    let a  = u.nd_algebra in
    let rec go (todo : (states * state) list) (seen: SS.t) (finals: SS.t) trans r = match todo with
      | (ps, p)::q ->
        let todo, seen, trans, finals, r=
          Algebra.fold (fun letter (todo, seen, trans, finals, r) ->
              let dirs =
                SS.fold (fun st acc ->
                    SS.union (delta_nfa st letter u) acc
                  ) ps SS.empty
              in
              let r, dir = RPart.get dirs ng r in

              let ntrans =
                if not (SS.is_empty dirs) then
                  DetTrans.add (letter, p) dir trans
                else trans
              in
              if SS.mem dir seen then
                (todo, seen, ntrans, finals, r)
              else
                begin
                  if not (SS.is_empty (SS.inter dirs u.nd_finals))
                  then ((dirs, dir):: todo, SS.add dir seen, ntrans, SS.add dir finals, r )
                  else ((dirs, dir):: todo, SS.add dir seen, ntrans, finals, r )
                end
            ) a (q, seen, trans, finals, r)
        in
        go todo seen finals trans r
      | [] -> (seen, trans, finals, r)
    in
    let inits = SS.singleton u.nd_init in
    let r, init  = RPart.get inits ng (RPart.empty) in
    let r, hole  = RPart.get (SS.empty) ng r in
    let finals =
      if not (SS.is_empty (SS.inter inits u.nd_finals))
      then SS.singleton init
      else SS.empty in
    let seen, trans, finals, r = go [(inits, init)] (SS.of_list [init; hole]) finals DetTrans.empty r in
    {
      states = seen;
      algebra = u.nd_algebra;
      start = init;
      final = finals;
      trans = trans;
      hole  = hole;
      hole_coreach = false;
    }

  let non_determinize u =
    let u = make_non_coreachable_hole u in
    {
      nd_states = u.states;
      nd_algebra = u.algebra;
      nd_init = u.start;
      nd_finals = u.final;
      nd_trans = DetTrans.map SS.singleton u.trans
    }

  (** automata renaming *)
  module Rename = Map.Make(S)
  type rename = S.t Rename.t
  let print_rename = ToolBox.print_map S.print S.print Rename.bindings

  let apply_rename (r: rename) (u: t) =
    let map x =
      try
        Rename.find x r
      with
      | Not_found -> x
    in
    let nhole = map u.hole in
    let is_hole x = S.compare nhole x = 0 in
    {
      u with
      states = SS.map map u.states;
      start  = map u.start;
      final  = SS.map map u.final;
      trans  = fold_trans_nh (fun (q, a, q') acc ->
          let qq' = map q' in
          if is_hole qq' then
            acc
          else
            let qq = map q in
            DetTrans.add (a, qq) qq' acc
        ) u DetTrans.empty;
    }

  let apply_rename_det_to_non_det (r: rename) (u: t) =
    let b = u.hole_coreach in
    let map x =
      try
        Rename.find x r
      with
      | Not_found -> x
    in
    (** over approximation: *)
    let will_hole_be_coreachable =
      b ||
      (
        let hd = map u.hole in
        Rename.exists (fun k a -> S.compare k u.hole <> 0 && S.compare a hd = 0) r
      )
    in
    let nhole = map u.hole in
    let is_hole x = S.compare nhole x = 0 in
    {
      nd_states = SS.map map u.states;
      nd_init   = map u.start;
      nd_finals = SS.map map u.final;
      nd_algebra = u.algebra;
      nd_trans =
        if not will_hole_be_coreachable then
          fold_trans_nh ( fun (q, a, q') acc ->
              let qq' = map q' in
              if is_hole qq' then
                acc
              else
                let qq = map q in
                nd_add_trans (qq, a, qq') acc
            ) u DetTrans.empty
        else
          SS.fold (fun q acc ->
              Algebra.fold (fun a acc ->
                  let q' = delta q a u in
                  let qq' = map q' in
                  let qq  = map q  in
                  nd_add_trans (qq, a, qq') acc
                ) u.algebra acc
            ) (get_states u) DetTrans.empty
    }



  let mini (u: t) max_split =
    let alg = get_algebra u in
    let module UF = UnionFind.Make(S) in
    let module TransInv = Map.Make(S) in

    let add_trans_inv q q' tinv =
      try
        let s = TransInv.find q' tinv in
        let news = SS.add q s in
        TransInv.add q' news tinv
      with
      | Not_found -> TransInv.add q' (SS.singleton q) tinv
    in

    let split_one_class_on_letter (letter: A.t) (cl: states) (uf: UF.t) max_split =
      let tinv, uf =
        SS.fold (fun q (tinv, uf) ->
            let q'  = delta q letter u in
            let rq', uf = UF.find q' uf in
            let tinv = add_trans_inv q q' tinv in
            (tinv, uf)
          ) cl (TransInv.empty, uf)
      in

      let nb, ncl_list, reste =
        TransInv.fold (fun _ ncl (nb, ncl_list, reste) ->
            if nb >= (max_split - 1) then
              (nb, ncl_list, SS.union ncl reste)
            else
              (nb+1, ncl :: ncl_list, reste)
          ) tinv (0, [], SS.empty)
      in
      if SS.is_empty reste then
        (ncl_list, uf, nb)
      else
        (reste :: ncl_list, uf, nb+1)
    in
    let exception Found of states list * UF.t * int in
    let split_one_classe (cl: states) (uf: UF.t) max_split =
      try
        let uf = Algebra.fold (fun letter uf ->
            let (sp, uf, nb) = split_one_class_on_letter letter cl uf max_split in
            if nb > 1 then
              raise (Found(sp, uf, nb))
            else
              uf
          ) alg uf
        in
        ([cl], uf, 1)
      with
      | Found(sp, uf, nb) -> sp, uf, nb
    in
    let aux (l : states list) (nb_class: int) =
      let uf = UF.from_list (List.map SS.elements l) in
      List.fold_left (fun (ncl_all, nb_class, uf, one_split) cl ->
          let cl', uf, nb= split_one_classe cl uf (max_split - nb_class + 1) in
          (cl' @ ncl_all, nb_class -1 + nb, uf, one_split || nb > 1)
        ) ([], nb_class, uf, false) l
    in

    let rec fix l nb =
      let l, nb, _, b = aux l nb in
      if b && nb < max_split then
        fix l nb
      else
        l, not b, nb
    in

    let build_rewrite l = List.fold_left (fun acc cl ->
        match cl with
        | p::q -> List.fold_left (fun acc q -> Rename.add q p acc) acc q
        | [] -> acc
      ) Rename.empty l
    in

    let term, nterm = SS.fold (fun q (term, nterm) ->
        if SS.mem q u.final then
          (SS.add q term, nterm)
        else
          (term, SS.add q nterm)
      ) (SS.remove u.hole u.states) (SS.empty, SS.empty)
    in

    let part, stable, _ = fix [SS.singleton u.hole; term; nterm] 3 in
    let r = build_rewrite (List.map SS.elements part) in
    if stable then
      apply_rename r u
    else
      let nd_u = apply_rename_det_to_non_det r u in
      nd_u |> determinize

  let minimization (u: t) =
    let n = SS.cardinal (get_states u) in
    mini u n


  let change_algebra (u: t) (a: Algebra.t) =
    let u = make_non_coreachable_hole u in
    let u_algebra = get_algebra u in
    let to_remove = Algebra.diff u_algebra a in
    if Algebra.is_empty to_remove then
      {u with algebra = a}
    else
      {
        u with
        algebra = a;
        trans   = DetTrans.filter (fun (a, q) q' -> not (Algebra.mem_symbol a to_remove)) u.trans
      }

  module S_square = struct
    type t = state * state
    let compare = ToolBox.pair_compare S.compare S.compare
    let print =
      ToolBox.print_pair S.print S.print
  end

  module S_S_square =
    Set.Make(S_square)
  let print_s_sq = ToolBox.print_set S_square.print S_S_square.elements

  module RP = Rewrite.Make(S_square)(S)

  let unify_algebra u u' =
    let a  = get_algebra u  in
    let a' = get_algebra u' in
    let na = Algebra.union a a' in

    let u  = change_algebra u  na in
    let u' = change_algebra u' na in
    (u, u')

  let product (u: t) (u': t) (hole_filter: bool-> bool-> bool) (final_filter: bool -> bool -> bool) =
    let () = S.restart () in
    let r  = RP.empty in

    let u  = make_non_coreachable_hole u  in
    let u' = make_non_coreachable_hole u' in

    let is_hole u q =
      S.compare q u.hole = 0
    in

    let u, u' = unify_algebra u u' in
    let na = get_algebra u' in

    let rec aux trans r (todo: (S_square.t * S.t) list) (seen: states) = match todo with
      | ((q, q'), qq) :: rl ->
        begin
          let trans, r, todo, seen =
            Algebra.fold (fun a ((trans, r, todo, seen) as acc) ->
                let qdir  = delta q  a u  in
                let qdir' = delta q' a u' in
                if hole_filter (not (is_hole u qdir)) (not (is_hole u' qdir')) then
                  let r, dir = RP.get (qdir, qdir') S.fresh r in
                  let todo, seen =
                    if SS.mem dir seen then
                      todo, seen
                    else
                      ((qdir, qdir'), dir) :: todo, SS.add dir seen
                  in
                  let trans = DetTrans.add (a, qq) dir trans in
                  (trans, r, todo, seen)
                else
                  acc
              ) na (trans, r, rl, seen)
          in
          aux trans r todo seen
        end
      | [] -> (trans, r, seen)
    in

    let r, n_start = RP.get (u.start, u'.start) S.fresh r in
    let r, n_hole = RP.get (u.hole, u'.hole) S.fresh r in
    let trans, r, seen = aux DetTrans.empty r [(u.start, u'.start), n_start] (SS.singleton n_start) in

    {
      states = SS.add n_hole seen;
      algebra = na;
      start = n_start;
      final = RP.M.fold (fun (q, q') nq finals ->
          if final_filter (SS.mem q u.final) (SS.mem q' u'.final) then
            SS.add nq finals
          else finals
        ) r SS.empty;
      trans = trans;
      hole = n_hole;
      hole_coreach = false;
    }

  let top (a: Algebra.t) =
    let () = S.restart () in
    let s0 = S.fresh () in
    let s1 = S.fresh () in
    {
      states = SS.of_list [s0; s1];
      algebra= a;
      start  = s0;
      final  = SS.singleton s0;
      trans  = Algebra.fold (fun a acc -> DetTrans.add (a, s0) s0 acc)
           a DetTrans.empty;
      hole   = s1;
      hole_coreach = false;
    }

  let bottom (a: Algebra.t) =
    let () = S.restart () in
    let s0 = S.fresh () in
    let s1 = S.fresh () in
    {
      states = SS.of_list [s0; s1];
      algebra= a;
      start  = s0;
      final  = SS.singleton s1;
      trans  = DetTrans.empty;
      hole   = s0;
      hole_coreach = false;
    }

  let join u u' =
    product u u' (||) (||)
    |> minimization

  let widening p u u' =
    let pdt = product u u' (||) (||) in
    let () = debug "pdt: %a" print pdt in
    mini pdt p

  let meet u u' =
    product u u' (&&) (&&)
    |> minimization

  let compl u =
    let () = debug "in: %a" print u in
    let st = get_states u in
    let rep =
      {
        u with
        final = SS.diff st u.final;
        hole_coreach = not u.hole_coreach;
      }
    in
    let () = debug "out: %a" print rep in
    rep

  let diff u v =
    meet u (compl v)

  let is_bottom u =
    let u = make_non_coreachable_hole u in
    let a = get_algebra u in
    let exception NB in

    let next todo q seen =
      Algebra.fold (fun x ((todo_next, seen) as acc) ->
          try
            let q' = DetTrans.find (x, q) u.trans in
            if SS.mem q' u.final then
              raise NB
            else
              begin
                if SS.mem q' seen then
                  acc
                else
                  (q' :: todo_next, SS.add q' seen)
              end
          with
          | Not_found -> acc
        ) a (todo, seen)
    in
    let rec reach todo seen = match todo with
      | q::todo_r ->
        let todo, seen = next todo_r q seen in
        reach todo seen
      | [] -> true
    in
    try
      if SS.mem u.start u.final then raise NB
      else reach [u.start] (SS.singleton u.start)
    with
    | NB -> false

  let leq u u' =
    is_bottom (diff u u')

  let from_word (sa: Algebra.t) (l: A.t list) =
    let ng = state_generator () in
    let a = ng () in
    let t, l, alpha, states =
      List.fold_left (fun (acc, last, alpha, states) x ->
          let s' = ng () in
          (DetTrans.add (x, last) s' acc, s', Algebra.add x alpha, SS.add s' states)) (DetTrans.empty,a, Algebra.empty, SS.singleton a) l
    in
    let hole = ng () in
    if not (Algebra.subset alpha sa) then
      failwith "[abstractRegularAutomaton.from_word] some letters are \
                not contained in the objective alphabet"
    else
      {
        states = SS.add hole states;
        algebra = sa;
        start = a;
        final = SS.singleton l;
        trans = t;
        hole = hole;
        hole_coreach = false
      }

  let bi_rename (u: t) (u': t) =
    let ng = state_generator () in
    let s = get_states u in
    let s' = get_states u' in
    let r = SS.fold (fun x acc ->
        let x' = ng () in
        Rename.add x x' acc
      ) s Rename.empty
    in
    let r' = SS.fold (fun x acc ->
        let x' = ng () in
        Rename.add x x' acc
      ) s' Rename.empty
    in
    (apply_rename r u, apply_rename r' u')


  let concat (u: t) (u': t) =
    let u, u' = unify_algebra u u' in
    let u, u' = bi_rename u u' in
    let u  = non_determinize u  in
    let u' = non_determinize u' in

    let t' =
      nd_fold_trans (fun (s,c,s') acc ->
          if S.compare s u'.nd_init = 0 then
            begin
              if S.compare s' u'.nd_init = 0 then
                SS.fold (fun f acc ->
                    SS.fold (fun f' acc ->
                        nd_add_trans (f,c,f') acc
                      ) u.nd_finals acc
                  ) u.nd_finals acc
              else
                SS.fold (fun f acc ->
                    nd_add_trans (f,c,s') acc
                  ) u.nd_finals acc
            end
          else
            begin
              if S.compare s' u'.nd_init = 0 then
                SS.fold (fun f' acc ->
                    nd_add_trans (s,c,f') acc
                  ) u.nd_finals acc
              else
                nd_add_trans (s,c,s') acc
            end
        )
        (DetTrans.merge
           (fun k v v' ->
              match v,v' with
              | _ ,None -> v
              | None, _ -> v'
              | _ -> assert false
           ) u.nd_trans u'.nd_trans
        ) DetTrans.empty
    in
    {
      nd_states = SS.union u.nd_states u'.nd_states;
      nd_init = u.nd_init ;
      nd_algebra = u.nd_algebra;
      nd_trans = t';
      nd_finals =
        if SS.mem u'.nd_init u'.nd_finals
        then SS.union u.nd_finals u'.nd_finals
        else u'.nd_finals
    } |> determinize

  let star (u: t) =
    let u = non_determinize u in
    let t' = nd_fold_trans (fun (s,c,s') acc ->
        if SS.mem s' u.nd_finals
        then nd_add_trans (s, c, u.nd_init) acc
        else acc
      ) u.nd_trans u.nd_trans
    in
    { u with
      nd_finals = SS.add u.nd_init u.nd_finals;
      nd_trans = t';
    } |> determinize

  type var = string
  let print_var = Format.pp_print_string

  module VMap = Map.Make(struct type t = var let compare = compare end)
  exception NA
  type u =
    | L of A.t
    | S of u
    | C of (u * u)
    | A of (u * u)
    | V of var
    | E
    | N

  let rewrite (u : u) = match u with
    | C ( A ( a, b) , c) -> Some (A ( C (a,c) , C (b,c)))
    | A ( C ( a, S b) , E) when a = b -> Some (S b)
    | C ( c , A ( a, b)) -> Some (A ( C (c,a) , C (c,b)))
    | C ( x , E ) | C ( E , x ) | A ( x , N ) | A ( N , x ) -> Some x
    | S (S x) -> Some (S x)
    | _ -> None

  let visitor (u : u ) = match u with
    | S x -> ([x],fun l -> S (List.hd l))
    | C(a,b) -> ([a;b], fun l -> C(List.nth l 0,List.nth l 1))
    | A(a,b) -> ([a;b], fun l -> A(List.nth l 0,List.nth l 1))
    | _ -> ([],fun l -> u)

  let rec apply_one_rewrite (u : u) =
    match rewrite u with
    | None ->
      let (l,f) = visitor u in
      let l,b = List.fold_left (fun (accl,accb) x ->
          let y,b = (apply_one_rewrite x) in
          (y::accl,(accb || b))) ([],false)l in
      (f (List.rev l),b)
    | Some x -> x,true

  type dnf =
    u list

  let make_conjunction u v =
    List.fold_left (fun acc x ->
        List.fold_left (fun acc y ->
            C(x,y) :: acc
          ) acc v
      ) [] u

  let rec make_dnf_regexp (u: u) = match u with
    | N -> []
    | E -> [E]
    | C(v, w) ->
      make_conjunction (make_dnf_regexp v) (make_dnf_regexp w)
    | A(v, w) ->
      (make_dnf_regexp v) @ (make_dnf_regexp w)
    | V v -> [V v]
    | L x -> [L x]
    | S u ->
      let u' = make_dnf_regexp u in
      let regopt =
        List.fold_left (fun acc x -> match acc with
            | Some acc -> Some (C(acc, S x))
            | None -> Some (S x)
          ) None u'
      in
      match regopt with
      | None -> [E]
      | Some x -> [S x]

  let norm (u : u) =
    let x = make_dnf_regexp u in
    let x = ToolBox.rm_doub compare x in
    let u =
      let uopt =
        List.fold_left (fun acc x -> match acc with
            | Some y -> Some (A(y, x))
            | None -> Some x
          ) None x in
      match uopt with
      | None -> N
      | Some x -> x
    in
    let rec aux u =
      let u,b = apply_one_rewrite u in
      if b then aux u
      else u
    in
    aux u

  let expr_precedence = function
    | S(_ )   -> 99
    | C(_, _) -> 6
    | A(_, _) -> 5
    | _ -> 100

  let rec print_u fmt (u : u) = match u with
    | L x      -> Format.fprintf fmt "%a" A.print x
    | S v      ->
      Format.fprintf fmt "%a˟"
        (ToolBox.print_paren
           (expr_precedence u > expr_precedence v)
           print_u
        ) v
    | C (v, w) ->
      Format.fprintf fmt "%a·%a"
        (ToolBox.print_paren
           (expr_precedence u > expr_precedence v)
           print_u
        ) v
        (ToolBox.print_paren
           (expr_precedence u > expr_precedence w)
           print_u
        ) w
    | A (v, w) ->
      Format.fprintf fmt "%a+%a"
        (ToolBox.print_paren
           (expr_precedence u > expr_precedence v)
           print_u
        ) v
        (ToolBox.print_paren
           (expr_precedence u > expr_precedence w)
           print_u
        ) w
    | V v      -> Format.fprintf fmt "%a" print_var v
    | E        -> Format.fprintf fmt "ε"
    | N        -> Format.fprintf fmt "∅"

  let pp_print_u fmt u =
    let u = norm u in
    Format.fprintf fmt "%a" print_u u

  let to_string u =
    let u = norm u in
    let rec aux u = match u with
      | L x      -> ToolBox.format_to_string A.print x
      | S v      ->
        let vs = aux v in
        Printf.sprintf "(%s)*" vs
      | C (v, w) ->
        let vs = aux v in
        let ws = aux w in
        Format.sprintf "(%s.%s)" vs ws
      | A (v, w) ->
        let vs = aux v in
        let ws = aux w in
        Format.sprintf "(%s+%s)" vs ws
      | V v      -> ToolBox.format_to_string print_var v
      | E        -> "eps"
      | N        -> "nul"
    in
    aux u

  type system = ( var * u ) list

  type systemvm = (var * (u option * (u VMap.t))) list
  type lr = Left | Right

  let print_systemvm x =
    ToolBox.print_list
      (fun fmt (v, (uo, m)) ->
         match uo with
         | Some u -> Format.fprintf fmt "(%s -> (%a + %a))" v print_u u (ToolBox.print_map_inline Format.pp_print_string print_u VMap.bindings) m
         | None -> Format.fprintf fmt "(%s -> %a)" v (ToolBox.print_map_inline Format.pp_print_string print_u VMap.bindings) m
      ) x

  let print_system fmt (s : system) : unit =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (v,u) -> Format.fprintf fmt "%a ?= %a" print_var v print_u u)
      ) s

  let add_vm (c,v) (c',v') =
    let c'' = match c,c' with
      | None, _        -> c'
      | _, None        -> c
      | Some a, Some b -> Some (A(a,b))
    in
    (c'',VMap.merge (fun k g d -> match g,d with
         | Some g, Some d -> Some (A(g,d))
         | None  , _      -> d
         | _     , None   -> g
       ) v v')

  let mul_c_vm e (c,v) =
    let c =
      match c with
      | None -> None
      | Some st -> Some (C (e,st))
    in
    let v = VMap.map (fun x -> C(e,x)) v in
    (c,v)

  let mul_c_vm_right e (c,v) =
    let c =
      match c with
      | None -> None
      | Some st -> Some (C (st, e))
    in
    let v = VMap.map (fun x -> C(x, e)) v in
    (c,v)


  let rec get_coeff (u : u) = match u with
    | N -> (Some N,VMap.empty)
    | E -> (Some E,VMap.empty)
    | L a -> (Some (L a),VMap.empty)
    | S v -> (Some (S v),VMap.empty)
    | V v -> (None, VMap.singleton v E)
    | C (v, w) ->
      begin
        let (c,v),(c',v') =
          get_coeff v,
          get_coeff w
        in
        let vc =
          if VMap.is_empty v then
            v'
          else if VMap.is_empty v' then
            v
          else raise NA
        in
        match (c,c') with
        | None, None ->
          None,vc
        | Some e, None ->
          None,(VMap.map (fun e' -> C(e,e')) vc)
        | None, Some e ->
          raise NA
        | Some e, Some e'' ->
          Some (C(e,e'')),(VMap.map (fun e' -> C(e,e')) vc)
      end
    | A (v, w) ->
      begin
        let (c,v),(c',v') =
          get_coeff v,
          get_coeff w
        in
        add_vm (c,v) (c',v')
      end

  let rec subst x e u = match u with
    | N   -> u
    | V v when x = v -> e
    | V v -> u
    | L a -> u
    | E   -> u
    | S u -> S (subst x e u)
    | C (v, w) -> C (subst x e v, subst x e w)
    | A (v, w) -> A (subst x e v, subst x e w)

  let add_coeff c c' =
    match c,c' with
    | None  , _      -> c'
    | _     , None   -> c
    | Some x, Some y -> Some (A(x,y))

  let product_lr lr a c =
    match lr with
    | Left -> C(a, c)
    | Right -> C(c, a)

  let rec subst_ne lr x (c',vm') (c,vm) =
    try
      let cx  = VMap.find x vm in
      let vm' = VMap.map (fun e' -> product_lr lr cx e') vm' in
      let c'  =
        match c' with
        | None -> None
        | Some cc -> Some (product_lr lr cx cc)
      in
      add_vm (c',vm') (c,VMap.remove x vm)
    with
    | Not_found -> (c,vm)

  let vm_to_regexp lr (c,vm) =
    match c with
    | Some x -> VMap.fold (fun v k acc -> A(product_lr lr k (V v),acc)) vm x
    | None ->
      try
        let v,k = VMap.choose vm in
        let vm  = VMap.remove v vm in
        VMap.fold (fun v k acc -> A(product_lr lr k (V v),acc)) vm (product_lr lr k (V v))
      with
      | Not_found -> N



  let print_systemm lr fmt l =
    let l' = List.map (fun (a,b) -> (a,vm_to_regexp lr b)) l in
    print_system fmt l'

  let system_simplify (l: systemvm) : systemvm =
    List.map (fun (q, (c, vm)) ->
        (q, (
            ToolBox.map_if_not_none norm c,
            VMap.map norm vm
          ))
      ) l

  let arden_solve lr l =
    let rec aux1 l b = match l with
      | [] -> b
      | (v,(l',vm))::q ->
        let to_subst =
          if VMap.mem v vm then
            let k = VMap.find v vm in
            let vm' = VMap.remove v vm in
            mul_c_vm (S(k)) (l',vm')
          else
            (l',vm)
        in
        let q = List.map (fun (y,z) -> (y,subst_ne lr v to_subst z)) q in
        aux1 q ((v,to_subst)::b)
    in
    let rec aux2 l b = match l with
      | (v,e) :: q ->
        let q = List.map (fun (y,z) -> (y,subst_ne lr v e z)) q in
        aux2 q ((v,e)::b)
      | [] -> b
    in
    let l = aux1 l [] in
    let l = aux2 l [] in
    l

  let regexp_of_automata (a : t) =
    let a = make_non_coreachable_hole a in
    let vos = var_of_state in
    let sys = fold_trans_nh (fun (s,c,s') acc ->
        if VMap.mem (vos s) acc then
          let y = VMap.find (vos s) acc in
          let r = add_vm y (None,VMap.singleton  (vos s') (L c)) in
          acc |> VMap.remove (vos s) |> VMap.add (vos s) r
        else
          let r = (None,VMap.singleton (vos s') (L c)) in
          acc |> VMap.add (vos s) r
      ) a VMap.empty
    in
    let sys = SS.fold (fun s acc ->
        if VMap.mem (vos s) acc then
          let (c,y) = VMap.find (vos s) acc in
          let r = (add_coeff c (Some E)),y  in
          acc |> VMap.remove (vos s) |> VMap.add (vos s) r
        else
          let r = (Some E, VMap.empty) in
          acc |> VMap.add (vos s) r
      ) a.final sys
    in
    let s = arden_solve Left (VMap.bindings sys) in
    try
      let (x, a) = List.find (fun (x,_) -> x = vos (a.start)) s in
      norm (vm_to_regexp Left a)
    with
    | Not_found -> N

  type word = A.t list
  let compare_word =
    ToolBox.list_compare A.compare
  let print_word fmt w =
    Format.fprintf fmt "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "·")
         A.print
      ) w

  type card =
    | Infinite
    | Finite of word list

  type accepts =
    | AI
    | AF of word
    | AN

  let cardinality (u: t) =
    let v = regexp_of_automata u in
    let v = norm v in
    let rec disj_discovery (v: u) = match v with
      | N | L _ | C _ | S _ | E -> [v]
      | V _ -> failwith "[cardinality] regexp with variables"
      | A(e, e') -> (disj_discovery e) @ (disj_discovery e')
    in
    let vl = disj_discovery v in
    let rec accepts_one (v: u) = match v with
      | A _ | V _ -> failwith "[cardinality] add or var occured"
      | N -> AN
      | E -> AF []
      | L x -> AF [x]
      | C(e, f) ->
        begin
          match accepts_one e, accepts_one f with
          | AN, _ -> AN
          | _, AN -> AN
          | AI, _ -> AI
          | _, AI -> AI
          | AF l, AF l' -> AF (l@l')
        end
      | S e ->
        begin
          match accepts_one e with
          | AN -> AN
          | AI -> AI
          | AF [] -> AF []
          | AF _ -> AI
        end
    in
    let rep =
      List.fold_left (fun acc x -> match acc, (accepts_one x) with
          | Infinite, _ -> Infinite
          | _, AI -> Infinite
          | _, AN -> acc
          | Finite wl, AF w ->
            if ToolBox.mem_compare compare_word w wl then
              Finite wl
            else
              Finite (w :: wl)
        ) (Finite []) vl
    in
    match rep with
    | Infinite -> None
    | Finite wl -> Some (List.length wl)

  let is_cardinality_one (u: t) =
    match cardinality u with
    | None -> false
    | Some x when x <> 1 -> false
    | _ -> true

  let rec automata_of_regexp_no_mini (sa: Algebra.t) (r : u) =
    (* let () = debug "automata_of_regexp_no_mini: %a" print_u r in *)
    match r with
    | L a ->
      from_word sa [a]
    | S (r') ->
      let a = automata_of_regexp_no_mini sa r' in
      star a
    | C (a,b) ->
      let a = automata_of_regexp_no_mini sa a in
      let b = automata_of_regexp_no_mini sa b in
      concat a b
    | A (a,b) ->
      let a = automata_of_regexp_no_mini sa a in
      let b = automata_of_regexp_no_mini sa b in
      join a b
    | V _ -> failwith "[automata_of_regexp_no_mini] variable in regexp"
    | E ->
      from_word sa []
    | N ->
      bottom Algebra.empty

  let automata_of_regexp (aa : Algebra.t) x =
    x
    |> automata_of_regexp_no_mini aa
    |> (fun y -> change_algebra y aa)
    |> minimization


  let equ a b =
    leq a b && leq b a

  let default_eq a b =
    let aa = get_algebra a
    and bb = get_algebra b in
    Algebra.compare aa bb = 0 &&
    equ a b

  let derivative (u: t) (a: A.t) =
    try
      let q = DetTrans.find (a, u.start) u.trans in
      {u with start = q}
    with
    | Not_found ->
      bottom u.algebra

  let head (u: t) =
    let u = make_non_coreachable_hole u in
    let a = u.algebra in
    Algebra.fold (fun x acc ->
        (x, derivative u x) :: acc
      ) a []

  let fresh_rename (u: t) (f: unit -> S.t)=
    let module MS = Map.Make(S) in
    let memo = MS.empty in
    let get_name x memo =
      try
        (MS.find x memo, memo)
      with
      | Not_found ->
        let nx = f () in
        (nx, MS.add x nx memo)
    in
    let nstates, memo = SS.fold (fun x (res, memo) ->
        let x, memo = get_name x memo in
        (SS.add x res, memo)
      ) u.states (SS.empty, memo)
    in
    let nstart, memo = get_name u.start memo in
    let nfinal, memo = SS.fold (fun x (res, memo) ->
        let x, memo = get_name x memo in
        (SS.add x res, memo)
      ) u.final (SS.empty, memo)
    in
    let ntrans, memo = DetTrans.fold (fun (s, q) q' (res, memo) ->
        let q , memo = get_name q  memo in
        let q', memo = get_name q' memo in
        (DetTrans.add (s, q) q' res, memo)
      ) u.trans (DetTrans.empty, memo)
    in
    let nhole, memo = get_name u.hole memo in
    {
      u with
      states = nstates;
      start = nstart;
      final = nfinal;
      trans = ntrans;
      hole = nhole;
    }

  let integrate (u: t) (a: A.t) =
    let u = make_non_coreachable_hole u in
    let () = S.restart () in
    let new_start = create_fresh_state u.states S.fresh in
    { u with
      states = SS.add new_start u.states;
      start = new_start;
      trans = DetTrans.add (a, new_start) u.start u.trans;
    }
end
