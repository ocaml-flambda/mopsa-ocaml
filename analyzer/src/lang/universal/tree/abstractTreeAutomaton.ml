(** Tree automata creating functor *)

(**/**)
let debug fmt = ToolBox.debug "tree automata" fmt
(**/**)

module Make(S : SIG.STATE)(A : SIG.COMPARABLE) =
struct

  let var_of_state x =
    Printf.sprintf "v_%s" (ToolBox.format_to_string S.print x)

  (** {2 Types and printers} *)
  (* type color = Color.t
   * type symbol = Symbol.t *)
  module SA = TreeAlgebra.Make(A)
  module SS = Set.Make(S)
  module MS = Map.Make(S)

  type algebra = A.t
  type sigma_algebra = SA.t

  (** [print_state_set fmt ss] printer for SS.t *)
  let print_state_set fmt (ss : SS.t) : unit =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") S.print) (SS.elements ss)

  (** [print_state_list fmt ss] printer for S.t list *)
  let print_state_list fmt (sl : S.t list) : unit =
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") S.print) (sl)

  type transition = algebra * S.t list * S.t

  let print_transition fmt ((s,l,p) : transition) : unit =
    match l with
    | [] ->
      Format.fprintf fmt "%a -> %a"
        A.print s
        S.print p
    | _ ->
      Format.fprintf fmt "%a(%a) -> %a"
        A.print s
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") S.print) l
        S.print p

  let compare_transition =
    ToolBox.triplet_compare A.compare (ToolBox.list_compare S.compare) S.compare

  module ST = Set.Make(
    struct
      type t = transition
      let compare = compare_transition
    end
    )

  let print_transition_set fmt (st : ST.t) : unit =
    Format.fprintf fmt "{@[<v>%a@]}"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") print_transition) (ST.elements st)

  module SAM = Map.Make(
    struct
      type t = (algebra * S.t list)
      let compare = ToolBox.pair_compare A.compare (ToolBox.list_compare S.compare)
    end
    )



  let print_sam_key fmt (s,l) =
    Format.fprintf fmt "%a(%a)"
      A.print s
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") S.print) l

  let print_transition_map =
    ToolBox.print_map print_sam_key S.print SAM.bindings

  type t =
    {
      (* set of all states *)
      q : SS.t;
      (* set of final states *)
      f : SS.t;
      (* set of transition *)
      d : ST.t;
    }

  module DetTrans = Mapper.Make(
    struct
      type t = algebra
      let compare = A.compare
      let print = A.print
    end)(S)

  type dfta =
    {
      (* set of all states *)
      dfta_q  : SS.t;
      (* set of final states *)
      dfta_f  : SS.t;
      (* set of transition *)
      dfta_d  : DetTrans.t;
      (* hole state *)
      dfta_h  : S.t;
      (* hole coreachable *)
      dfta_hc : bool;
      (* symbol environment *)
      dfta_a  : sigma_algebra;
    }

  type ext_tree =
    | Sta of S.t
    | Sym of S.t * (A.t * ext_tree list) list
  let transform (u: dfta) =
    let init =
      DetTrans.fold (fun (sym, ql, q) (xt_tree_map: ext_tree MS.t) ->
          try
            let t = MS.find q xt_tree_map in
            match t with
            | Sta _ -> failwith "should not be here"
            | Sym(q, l) ->
              let n = Sym(q, (sym, List.map (fun q -> Sta q) ql) :: l) in
              MS.add q n xt_tree_map
          with
          | Not_found ->
            let n = Sym(q, (sym, List.map (fun q -> Sta q) ql) :: []) in
            MS.add q n xt_tree_map
        ) u.dfta_d MS.empty
    in
    let rec go xtree init (seen: SS.t) = match xtree with
      | Sta q ->
        begin
          if SS.mem q seen then
            Some (Sta q, SS.singleton q)
          else
            try
              let xt = MS.find q init in
              go xt init (SS.add q seen)
            with
            | Not_found -> None
        end
      | Sym(q, l) ->
        let l, loops =
          ToolBox.fold (fun (sym, xlist) (accl, loops) ->
              let exception BadPath in
              try
                let l, loops' =
                  ToolBox.fold (fun (xtree) (l, loops) ->
                      match go xtree init (SS.add q seen) with
                      | Some (xt, loops') -> (xt :: l, SS.union loops loops')
                      | None -> raise BadPath
                    ) xlist ([], SS.empty)
                in
                ((sym, List.rev l) :: accl, SS.union loops' loops)
              with
              | BadPath -> accl, loops
            ) (l) ([], SS.empty)
        in Some (Sym(q, l), loops)
    in
    let l = SS.fold (fun q acc ->
        match MS.find_opt q init with
        | Some xt ->
          begin
            match go xt init (SS.singleton q) with
            | Some (x, s) -> (x, s) :: acc
            | None -> acc
          end
        | None -> acc
      ) u.dfta_f []
    in
    l

  let print_transform fmt l =
    let rec print fmt (xt, ss) = match xt with
      | Sta q -> S.print fmt q
      | Sym (q, l) ->
        let print_l fmt (l: (A.t * ext_tree list) list) =
          Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " || ") (
            fun fmt (sym, xl) ->
              if List.length xl = 0 then
                Format.fprintf fmt "%a" A.print sym
              else
                Format.fprintf fmt "%a(%a)" A.print sym
                  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
                     (fun fmt x -> print fmt (x, ss))
                  ) xl
          ) fmt l
        in
        if SS.mem q ss then
          Format.fprintf fmt "%a-%a" S.print q print_l l
        else
          print_l fmt l
    in
    ToolBox.print_list_inline_no_border
      ( fun fmt (xt, ss) ->
          print fmt (xt, ss)
      ) fmt l

  let print_dfta2 fmt d =
    let l = transform d in
    Format.fprintf fmt "%a" print_transform l

    let get_sigma_algebra (u: dfta) : sigma_algebra =
    u.dfta_a

  let delta (u : t) (ql : S.t list) (a : algebra) =
    ST.fold (fun (a', ql', q') acc ->
        if A.compare a a' = 0 && (ToolBox.list_compare S.compare) ql ql' = 0 then
          q' :: acc
        else acc
      ) u.d []

  let delta_dfta (u : dfta) (ql : S.t list) (a : algebra) =
    try
      DetTrans.find_transition a ql u.dfta_d
    with
    | Not_found -> u.dfta_h

  let get_sigma_algebra_from_t (u : t) : sigma_algebra =
    ST.fold (fun (s, l, _) acc ->
        let n = List.length l in
        SA.add (s, n) acc
      ) u.d SA.empty

  let print fmt (u : t) : unit =
    Format.fprintf fmt "@[<v 2>{@,q : %a@,f : %a@,d : %a}@]"
      print_state_set u.q
      print_state_set u.f
      print_transition_set u.d

  let print_dfta fmt (u : dfta) : unit =
    Format.fprintf fmt "@[<v 2>{@,q : %a@,f : %a@,d : %a@,h: %a@,a: %a@,}@]"
      print_state_set u.dfta_q
      print_state_set u.dfta_f
      DetTrans.print u.dfta_d
      S.print u.dfta_h
      SA.print u.dfta_a

  type 'a tree =
    (* | Hole *)
    | Node of 'a * ('a tree) list

  type stree = algebra tree
  type rtree = S.t tree

  let rec print_tree printer fmt t = match t with
    (* | Hole ->
     *   Format.fprintf fmt "☐" *)
    | Node(s,l) -> if List.length l > 0 then
        Format.fprintf fmt "@[<v 1>%a(@,%a@,@])" printer s
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") (print_tree printer)) l
      else Format.fprintf fmt "%a" printer s

  let print_stree = print_tree A.print
  let print_rtree = print_tree S.print

  let print_runpartial fmt (r : (S.t * rtree) list) =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (x,y) -> Format.fprintf fmt "%a : %a" S.print x
             print_rtree y)) r

  let mem (t : stree) (u : t) =
    let tree_product p l : (S.t * rtree) list=
      let rec aux (l : (S.t * rtree) list list) = match l with
        | p::[] -> List.map (fun x -> [x]) p
        | p::q ->
          let ll = aux q in
          List.fold_left (fun acc x -> (List.map (fun y -> (x :: y)) ll) @ acc) [] p
        | [] -> failwith "I should not be here"
      in
      let ppp = aux l in
      List.map (fun (x : (S.t * rtree) list) -> (p,Node(p,List.map snd x))) (ppp)
    in

    let rec aux t : (S.t * rtree) list = match t with
      (* | Hole ->
       *   delta u [] H *)
        (* |> List.map (fun x -> (x, Hole)) *)
      | Node(s,l) ->
        let n = List.length l in
        let ts = ST.filter (fun (s',l,p) -> s = s' && List.length l = n) u.d in
        let possible_state = List.map (fun t ->
            aux t
          ) l in
        let ret =
          if n > 0 then
            ST.fold (fun ((_,l,p)) acc ->
                let partial = List.fold_left2 ( fun acc (x : S.t) (y : (S.t * rtree) list) ->
                    let possibles = List.filter (fun (yy,_) -> x = yy) y in
                    possibles :: acc
                  ) [] l possible_state
                in
                let rep = tree_product p partial in
                rep @ acc
              ) ts []
          else
            ST.fold (fun (_,_,p) acc -> (p,Node(p,[]))::acc) ts []
        in
        ret
    in
    let runs = aux t in
    runs

  let states_in_transition d =
    ST.fold (fun (_,l,p) acc ->
        List.fold_left (fun acc x ->
            SS.add x acc) acc l |> SS.add p
      ) d SS.empty

  let state (u : t) =
    u.q

  let state_dfta (u : dfta) =
    SS.add u.dfta_h u.dfta_q

  module R = Rewrite.Make(S)(S)

  let apply_rewrite (ru : R.t) (u : t) =
    let f = fun x -> R.apply x ru in
    {
      q = SS.map f u.q;
      f = SS.map f u.f;
      d = ST.map (fun (s,l,p) -> (s,List.map f l,f p)) u.d
    }

  let apply_map (ru : S.t MS.t) (u : t) (* (widening : bool) *) =
    (* let () = debug "applying map : %a" (ToolBox.print_map S.print S.print MS.bindings) ru in *)
    let f x = if MS.mem x ru then MS.find x ru else x in
    let redo q = SS.fold (fun x acc -> SS.add x acc) q SS.empty in
    {f = SS.map f u.f |> redo;
     d = ST.map (fun (s,l,p) -> (s,List.map f l,f p)) u.d;
     q = SS.map f u.q |> redo ;
    }

  let apply_map_dfta (ru : S.t MS.t) (u : dfta) =
    let f x = if MS.mem x ru then MS.find x ru else x in
    let new_hole = f u.dfta_h in
    let redo q = SS.fold (fun x acc -> SS.add x acc) q SS.empty in
    {u with
     dfta_f = SS.map f u.dfta_f |> redo;
     dfta_d = DetTrans.fold (fun (s, l, q) acc ->
         (* let () = debug "%a" print_transition (s, l, q) in *)
         let q' = f q in
         if S.compare q' new_hole = 0 then
           acc
         else
           let l' = List.map f l in
           (* let () = debug "new_transition: %a" print_transition (s, l', q') in *)
           DetTrans.add_transition (s, l', q') acc
       ) u.dfta_d DetTrans.empty;
     dfta_q = SS.map f u.dfta_q |> redo ;
     dfta_h = new_hole;
     (* dfta_hc  *)
    }

  let unify u u' =
    let () = S.restart () in
    let ru  = SS.fold (fun k acc -> R.add k S.fresh acc) (state u ) R.empty in
    let ru' = SS.fold (fun k acc -> R.add k S.fresh acc) (state u') R.empty in
    apply_rewrite ru u,apply_rewrite ru' u'

  module RP = Rewrite.Make(
    struct
      type t = S.t * S.t
      let compare (a,b) (c,d) = if S.compare a c = 0 then S.compare b d else S.compare a c
      let print fmt (x,y) = Format.fprintf fmt "(%a,%a)" S.print x S.print y
    end
    )(S)

  let product u u' =
    let () = S.restart () in
    let rp = RP.empty in
    let qq,d,rp =
      ST.fold ( fun (s,l,p) (qq, acc, rp) ->
          ST.fold ( fun (s',l',p') (qq, acc,rp) ->
              if s = s' && List.length l = List.length l' then
                let repl,rp = List.fold_left2 (fun (accl,rp) a b ->
                    let rp,c = RP.get (a,b) S.fresh rp in
                    (c :: accl , rp)
                  ) ([],rp) l l'
                in
                let rp,c = RP.get (p,p') S.fresh rp in
                (SS.add c qq, ST.add (s,List.rev repl,c) acc, rp)
              else
                (qq, acc, rp)
            ) u'.d (qq, acc,rp)
        ) u.d (SS.empty, ST.empty, rp)
    in
    let rp =
      SS.fold (fun s rp ->
          SS.fold (fun s' rp ->
              let rp,_ = RP.get (s,s') S.fresh rp in
              rp
            ) u'.f rp
        ) (state u) rp
    in
    let rp =
      SS.fold (fun s rp ->
          SS.fold (fun s' rp ->
              let rp,_ = RP.get (s,s') S.fresh rp in
              rp
            ) (state u') rp
        ) u.f rp
    in
    qq,d,rp

  type sstate = SS.t
  module SSS = Set.Make(struct type t = sstate let compare = SS.compare end)

  type ssquare = S.t * S.t
  let print_ssquare = ToolBox.print_pair S.print S.print
  let compare_ssquare = ToolBox.pair_compare S.compare S.compare
  module SSquare = Set.Make(struct type t = ssquare let compare = compare_ssquare end)
  let print_ssqare_m = ToolBox.print_set print_ssquare (SSquare.elements)

  module PartR = Rewrite.Make(struct
      type t = sstate
      let print = print_state_set
      let compare = SS.compare
    end
    )(S)

  module ListGenerator = struct
    exception NoMore
    type 'a t = 'a list list * 'a list
    let init l n = let rec aux i rep = if i = n then rep else aux (i+1) (l::rep) in
      (aux 0 [], l)
    let non_empty u = snd u <> []
    let read u =
      let a,_ = u in
      List.map (fun a -> List.hd a) a
    let next u =
      let a,b = u in
      let rec aux l = match l with
        | p::q ->
          begin
            match p with
            | t::[] -> b :: (aux q)
            | t::r  -> r :: q
            | []    -> failwith "Should not be here"
          end
        | [] -> raise NoMore
      in
      aux a,b
    let read_and_next u =
      let rep = read u in
      let nex =
        try Some (next u)
        with
        | NoMore -> None
      in
      (rep,nex)
  end

  let inverse_sigma_algebra (sa : sigma_algebra) =
    SA.fold
      (fun (s, n) acc -> ToolBox.intm_replace n (function | None -> [s] | Some x -> s :: x) acc)
      sa
      ToolBox.IntM.empty

  let reord (u, u') b =
    if b then u, u'
    else u', u

  let product_dfta_improved (sa: sigma_algebra) (u: dfta) (u': dfta) =
    let () = S.restart () in
    let delta_square sl sym =
      let slx = List.map fst sl in
      let sly = List.map snd sl  in
      delta_dfta u slx sym, delta_dfta u' sly sym
    in
    let is_hole (x, y) =
      S.compare x u.dfta_h = 0
      && S.compare y u'.dfta_h = 0
    in
    let rp = RP.empty in
    let rp, pdt_h = RP.get (u.dfta_h, u'.dfta_h) S.fresh rp in
    let rp_i, todos_i, reachable_i, delta_i =
      SA.fold (fun (sym, arite) ((rp, todos, reachable, delta) as acc) ->
          if arite = 0 then
            let ((x, y) as news) = delta_square [] sym in
            if is_hole news
            then
              acc
            else
              let rp, dir = RP.get news S.fresh rp in
              let delta = DetTrans.add_transition (sym, [], dir) delta in
              if SSquare.mem news reachable then
                (rp, todos, reachable, delta)
              else
                (rp, news :: todos, SSquare.add news reachable, delta)
          else
            acc
        ) sa (rp, [], SSquare.empty, DetTrans.empty)
    in
    let module TMap = Map.Make(struct type t = transition let compare = compare_transition end) in
    let u_access_transition =
      DetTrans.fold (fun ((sym, sl, dir) as trans) ->
          TMap.add trans (SS.of_list sl)
        ) u.dfta_d TMap.empty
    in
    let v_access_transition =
      DetTrans.fold (fun ((sym, sl, dir) as trans) ->
          TMap.add trans (SS.of_list sl)
        ) u'.dfta_d TMap.empty
    in
    (* let print_access_transition =
     *   ToolBox.print_map print_transition (ToolBox.print_set S.print SS.elements) TMap.bindings
     * in *)
    let rec aux rp (todo: ssquare list) (reachable: SSquare.t) (delta : DetTrans.t) (u_access_transition : SS.t TMap.t) (v_access_transition : SS.t TMap.t) =
      match todo with
      | (x, y)::reste ->
        let do_with (x, y) rpa builder getter u todo reachable delta access_transition =
          let access_transition, tofire =
            TMap.fold (fun t v (access_transition, to_fire) ->
                let newv = SS.remove x v in
                let access_transition = TMap.add t newv access_transition in
                if SS.is_empty newv && not (SS.is_empty v) then
                  (access_transition, t::to_fire)
                else
                  (access_transition, to_fire)
              ) access_transition (TMap.empty, [])
          in
          if tofire = [] then
            (rpa, reste, reachable, delta, access_transition)
          else
            let get_reachable_pair x reachable =
              SSquare.fold (fun (a, b) l ->
                  let a, b = builder (a, b) in
                  if S.compare x a = 0 then
                    b :: l
                  else l
                ) reachable []
            in
            (* let snd_reachable = SSquare.elements reachable |> List.map getter in *)
            let rpb, todos, reachable, delta =
              List.fold_left ( fun (rp0, todos, reachable, delta) (sym, slx, dirx) ->
                  let slyl = List.map (fun q -> get_reachable_pair q reachable) slx in
                  Enumerator.fold (fun sly ((rp1, todos, reachable, delta) as acc) ->
                      let diry = delta_dfta u sly sym in
                      let pdir = builder (dirx, diry) in
                      if is_hole (builder pdir) then
                        acc
                      else
                        begin
                          let rp2, dir = RP.get pdir S.fresh rp1 in
                          let rp2, slt = List.fold_left2 (fun (rp3, l) x y ->
                              let rp4, e = RP.get (builder (x, y)) S.fresh rp3 in
                              (rp4, e::l)
                            ) (rp2, []) slx sly
                          in
                          let delta = DetTrans.add_transition (sym, (List.rev slt), dir) delta in
                          if SSquare.mem pdir reachable then
                            (rp2, todos, reachable, delta)
                          else
                            (rp2, pdir :: todos, SSquare.add pdir reachable, delta)
                        end
                    ) slyl (rp0, todos, reachable, delta)
                ) (rpa, todo, reachable, delta) tofire
            in
            (rpb, todos, reachable, delta, access_transition)
        in
        let rp, todos, reachable, delta, u_access_transition =
          do_with (x, y) rp (fun (x,y) -> (x,y)) snd u' reste reachable delta u_access_transition in
        let rp, todos, reachable, delta, v_access_transition =
          do_with (y, x) rp (fun (x,y) -> (y,x)) fst u todos reachable delta v_access_transition in
        aux rp todos reachable delta u_access_transition v_access_transition
      | [] -> (rp, todo, reachable, delta)
    in
    let rp, _, _, delta = aux rp_i todos_i reachable_i delta_i u_access_transition v_access_transition in
    (pdt_h, RP.M.bindings rp |> List.map snd |> SS.of_list, delta, rp)

  let product_dfta_intermediate (sa : sigma_algebra) (u : dfta) (u' : dfta) =
    let () = S.restart () in
    let delta_square sl sym =
      let slx = List.map fst sl in
      let sly = List.map snd sl  in
      delta_dfta u slx sym, delta_dfta u' sly sym
    in
    let is_hole (x, y) =
      S.compare x u.dfta_h = 0
      && S.compare y u'.dfta_h = 0
    in
    let rp = RP.empty in
    let rp, pdt_h = RP.get (u.dfta_h, u'.dfta_h) S.fresh rp in
    let rp_i, todos_i, reachable_i, delta_i =
      SA.fold (fun (sym, arite) ((rp, todos, reachable, delta) as acc) ->
          if arite = 0 then
            let ((x, y) as news) = delta_square [] sym in
            if is_hole news
            then
              acc
            else
              let rp, dir = RP.get news S.fresh rp in
              let delta = DetTrans.add_transition (sym, [], dir) delta in
              if SSquare.mem news reachable then
                (rp, todos, reachable, delta)
              else
                (rp, news :: todos, SSquare.add news reachable, delta)
          else
            acc
        ) sa (rp, [], SSquare.empty, DetTrans.empty)
    in
    let rec aux rp (todo: ssquare list) (reachable: SSquare.t) (delta : DetTrans.t) =
      match todo with
      | ((x, y) as s)::reste ->
        let rp, todos, reachable, delta =
          SA.fold (fun (sym, arite) ((rp, todos, reachable, delta) as acc) ->
              if arite = 0 then acc
              else
                EnumeratorOneFixed.fold (fun sl ((rp, todos, reachable, delta) as acc) ->
                    let ((x, y) as news) = delta_square sl sym in
                    if (S.compare x u.dfta_h = 0 && S.compare y u'.dfta_h = 0)
                    then
                      acc
                    else
                      let rp, dir = RP.get news S.fresh rp in
                      let rp, sql' = List.fold_left (fun (rp, acc) x -> let rp, y = (RP.get x S.fresh rp) in rp, y :: acc) (rp, []) sl in
                      let delta = DetTrans.add_transition (sym, (List.rev sql'), dir) delta in
                      if SSquare.mem news reachable then
                        (rp, todos, reachable, delta)
                      else
                        (rp, news :: todos, SSquare.add news reachable, delta)
                  ) (SSquare.elements reachable) s arite (rp, todos, reachable, delta)
            ) sa (rp, reste, reachable, delta)
        in
        aux rp todos reachable delta
      | [] -> (rp, todo, reachable, delta)
    in
    let rp, _, _, delta = aux rp_i todos_i reachable_i delta_i in
    (pdt_h, RP.M.bindings rp |> List.map snd |> SS.of_list, delta, rp)

  let product_dfta (sa : sigma_algebra) (u : dfta) (u' : dfta) =
    let im = inverse_sigma_algebra sa in
    let () = S.restart () in
    let rp = RP.empty in
    let all_states_u  = SS.elements (state_dfta u ) in
    let all_states_u' = SS.elements (state_dfta u') in
    let rp, new_hole = RP.get (u.dfta_h, u'.dfta_h) S.fresh rp in
    let allq = SS.singleton new_hole in
    let delta = DetTrans.empty in
    let (allq, rp, delta) =
      ToolBox.IntM.fold (fun n sl (allq, rp, delta) ->
          List.fold_left (fun (allq, rp, delta) s ->
              let product_intern trans all_states_other allq rp delta h u mixed_product =
                DetTrans.fold (fun (s, ql, q) (allq, rp, delta) ->
                    let () = debug "%a" print_transition (s, ql, q) in
                    Enumerator.fold_id (fun ql' (allq, rp, delta) ->
                        let q' = delta_dfta u ql' s in
                        if not mixed_product && S.compare q' h <> 0 then
                          allq, rp, delta
                        else
                          begin
                            let allq, rp, args =
                              List.fold_left2 (fun (allq, rp, args) q q' ->
                                  let rp, qq' = RP.get (reord (q, q') mixed_product) S.fresh rp in
                                  (SS.add qq' allq, rp, qq' :: args)
                                ) (allq, rp, []) ql ql'
                            in
                            let lol = (reord (q, q') mixed_product) in
                            let rp, dir = RP.get lol S.fresh rp in
                            (SS.add dir allq, rp, DetTrans.add_transition (s, (List.rev args), dir) delta)
                          end
                      ) all_states_other n (allq, rp, delta)
                  ) trans (allq, rp, delta)
              in
              let trans  = DetTrans.filter_symbol s u.dfta_d in
              let trans' = DetTrans.filter_symbol s u'.dfta_d in
              let allq, rp, delta = product_intern trans  all_states_u' allq rp delta u.dfta_h  u' true  in
              let allq, rp, delta = product_intern trans' all_states_u  allq rp delta u'.dfta_h u  false in
              allq, rp, delta
            ) (allq, rp, delta) sl
        ) im (allq, rp, delta)
    in
    (* let () = debug "%a" RP.print rp in *)
    let rp =
      SS.fold (fun s rp ->
          SS.fold (fun s' rp ->
              let rp,_ = RP.get (s,s') S.fresh rp in
              rp
            ) u'.dfta_f rp
        ) (state_dfta u) rp
    in
    let rp =
      SS.fold (fun s rp ->
          SS.fold (fun s' rp ->
              let rp,_ = RP.get (s,s') S.fresh rp in
              rp
            ) (state_dfta u') rp
        ) u.dfta_f rp
    in
    new_hole, allq, delta, rp


  let is_hole_correachable (u : dfta) =
    u.dfta_hc
    (* let exception Found in
     * let one_step (reach : SS.t) (activated : ST.t) =
     *   DetTrans.fold ( fun (s, l, d) (accr, acca) ->
     *       let trans = (s, l, d) in
     *       if (not (ST.mem trans activated) && SS.mem d reach) then
     *         List.fold_left (fun acc x ->
     *             if S.compare x u.dfta_h = 0 then
     *               raise Found
     *             else
     *               SS.add x acc
     *           ) accr l, ST.add trans acca
     *       else (accr,acca)
     *     ) u.dfta_d (reach,activated)
     * in
     * let rec fix seen activated =
     *   let next, activated = one_step seen activated in
     *   if SS.compare seen next = 0 then next
     *   else fix next activated
     * in
     * try
     *   let _ = fix u.dfta_f ST.empty in
     *   false
     * with
     * | Found -> true *)

  let product_dispatcher (sa: sigma_algebra) (u: dfta) (u': dfta) =
    if (is_hole_correachable u) || (is_hole_correachable u') then
      product_dfta sa u u'
    else product_dfta_improved sa u u'

  let algebras (u : t) =
    ST.fold (fun (s, l, q) acc ->
        let toadd =  (s,List.length l) in
        SA.add toadd acc) u.d SA.empty

  let toto fmt x =
    let s,ssl = x in
    Format.fprintf fmt "%a->%a" A.print s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") print_state_set) ssl

  let determinization_algebra (u : t) (sym : sigma_algebra) =
    let module TF = Map.Make(struct type t = algebra * S.t let compare= ToolBox.pair_compare compare S.compare end) in
    let module Seen = Set.Make(struct type t = algebra * sstate list let compare = ToolBox.pair_compare compare (ToolBox.list_compare SS.compare) end) in
    let () = S.restart () in
    let tf = ST.fold (fun (s,l,q) acc ->
        if TF.mem (s,q) acc then
          let y = TF.find (s,q) acc in
          acc |>
          TF.remove(s,q) |>
          TF.add (s,q) (l::y)
        else TF.add (s,q) [l] acc
      ) u.d TF.empty in
    let f = S.fresh in
    let su = state u in
    let exception Found in
    let compute_image (s : algebra) (l : sstate list) =
      SS.filter (fun q ->
          TF.mem (s,q) tf &&
          (let xl = TF.find (s,q) tf in
           try
             let () =
               List.iter (fun x ->
                   if List.length x = List.length l then
                     (
                       try
                         let () = List.iter2 (fun a b -> if not (SS.mem a b) then raise Not_found) x l in
                         raise Found
                       with
                       | Not_found -> ()
                     )
                 ) xl
             in
             false
           with
           | Found -> true
          )
        ) su
    in
    let exception NEXT of algebra * sstate list * sstate * Seen.t in
    let next (seen : Seen.t) (qd : sstate list) =
      SA.iter (fun (s,n) ->
          if n = 0 then
            if not (Seen.mem (s,[]) seen) then
              let rep = compute_image s [] in
              raise (NEXT(s,[],rep,Seen.add (s,[]) seen))
            else
              ()
          else
            begin
              if List.length qd = 0
              then ()
              else
                begin
                  let gene = ListGenerator.init qd n in
                  let rec aux gene =
                    match gene with
                    | None -> ()
                    | Some gene ->
                      begin
                        let ss, next = ListGenerator.read_and_next gene in
                        if (not (Seen.mem (s,ss) seen)) then
                          let rep = compute_image s ss in
                          raise (NEXT(s,ss,rep,Seen.add (s,ss) seen))
                        else aux next
                      end
                  in
                  aux (Some gene)
                end
            end
        ) sym
    in
    let rec fix seen qdold qnew trans nr =
      try
        let () = next seen qdold in
        qdold,trans,nr
      with
      | NEXT(s,ss,q,nseen) ->
        let nr = List.fold_left (fun nr s -> let nr = PartR.add s f nr in nr) nr ss in
        let ss' = List.map (fun x -> PartR.apply x nr) ss in
        let nr,q' = PartR.get q f nr in
        fix nseen (q::qdold) (SS.add q' qnew) (ST.add (s,ss',q') trans) nr
    in
    let nr = PartR.empty in

    let nstate, trans, nr = fix Seen.empty [] SS.empty ST.empty nr in
    let term = List.filter (fun x -> not (SS.is_empty (SS.inter x u.f))) nstate in
    let term = term |> List.map (fun x -> PartR.apply x nr) |> SS.of_list in
    {
      q = nstate |> List.map (fun x -> PartR.apply x nr) |> SS.of_list;
      d = trans;
      f = term ;
    }

  let determinization_algebra_to_dfta (sym : sigma_algebra) (u : t): dfta =
    let module TF = Map.Make(
      struct
        type t = algebra * S.t
        let compare= ToolBox.pair_compare compare S.compare
      end
      ) in
    let module Seen = Set.Make(
      struct
        type t = algebra * sstate list
        let compare = ToolBox.pair_compare compare (ToolBox.list_compare SS.compare)
      end
      ) in

    let () = S.restart () in

    let tf = ST.fold (fun (s,l,q) acc ->
        if TF.mem (s,q) acc then
          let y = TF.find (s,q) acc in
          acc |>
          TF.remove(s,q) |>
          TF.add (s,q) (l::y)
        else TF.add (s,q) [l] acc
      ) u.d TF.empty in

    let f = S.fresh in
    let su = state u in

    let exception Found in

    let compute_image (s : algebra) (l : sstate list) =
      SS.filter (fun q ->
          TF.mem (s,q) tf &&
          (let xl = TF.find (s,q) tf in
           try
             let () =
               List.iter (fun x ->
                   if List.length x = List.length l then
                     (
                       try
                         let () = List.iter2 (fun a b -> if not (SS.mem a b) then raise Not_found) x l in
                         raise Found
                       with
                       | Not_found -> ()
                     )
                 ) xl
             in
             false
           with
           | Found -> true
          )
        ) su
    in

    let exception NEXT of algebra * sstate list * sstate * Seen.t in
    let next (seen : Seen.t) (qd : sstate list) =
      SA.iter (fun (s, n) ->
          if n = 0 then
            if not (Seen.mem (s, []) seen) then
              let rep = compute_image s [] in
              raise (NEXT(s, [], rep, Seen.add (s, []) seen))
            else
              ()
          else
            begin
              if List.length qd = 0
              then ()
              else
                begin
                  let gene = ListGenerator.init qd n in
                  let rec aux gene =
                    match gene with
                    | None -> ()
                    | Some gene ->
                      begin
                        let ss, next = ListGenerator.read_and_next gene in
                        if (not (Seen.mem (s,ss) seen)) then
                          let rep = compute_image s ss in
                          raise (NEXT(s,ss,rep,Seen.add (s,ss) seen))
                        else aux next
                      end
                  in
                  aux (Some gene)
                end
            end
        ) sym
    in

    let rec fix seen qdold qnew trans nr =
      try
        let () = next seen qdold in
        (qdold, trans, nr)
      with
      | NEXT(s, ss, q, nseen) ->
        let nr = List.fold_left (fun nr s -> let nr = PartR.add s f nr in nr) nr ss in
        let ss' = List.map (fun x -> PartR.apply x nr) ss in
        let nr, q' = PartR.get q f nr in
        if SS.is_empty q then
          fix nseen (q::qdold) (SS.add q' qnew) (trans) nr
        else
          fix nseen (q::qdold) (SS.add q' qnew) (DetTrans.add_transition (s, ss', q') trans) nr
    in
    let nr = PartR.empty in

    let nstate, trans, nr = fix Seen.empty [] SS.empty DetTrans.empty nr in
    let term = List.filter (fun x -> not (SS.is_empty (SS.inter x u.f))) nstate in
    let term = term |> List.map (fun x -> PartR.apply x nr) |> SS.of_list in
    let _, hole = PartR.get SS.empty f nr in
    let allq = nstate |> List.map (fun x -> PartR.apply x nr) |> SS.of_list in
    let allq = SS.add hole allq in
    {
      dfta_q  = allq ;
      dfta_d  = trans;
      dfta_f  = term ;
      dfta_h  = hole ;
      dfta_hc = false;
      dfta_a  = sym  ;
    }

  let dfta_of_t (sa: sigma_algebra) (u: t) =
    determinization_algebra_to_dfta sa u



  let t_of_dfta (u: dfta) =
    let sa = u.dfta_a in
    let allstate_u = u |> state_dfta |> SS.elements in
    let trans =
      if u.dfta_hc then
        SA.fold (fun (s, n) trans ->
            Enumerator.fold_id (fun ql trans ->
                let q = delta_dfta u ql s in
                ST.add (s, ql, q) trans
              ) allstate_u n trans
          ) sa ST.empty
      else
        DetTrans.fold (fun (s, ql, q) trans ->
            ST.add (s, ql, q) trans
          ) u.dfta_d ST.empty
    in
    {
      q = SS.add u.dfta_h u.dfta_q;
      f = u.dfta_f;
      d = trans
    }

  let apply_map_dfta_break_det (ru : S.t MS.t) (u : dfta) =
    let u_t = t_of_dfta u in
    let u_t = apply_map ru u_t in
    dfta_of_t u.dfta_a u_t

  let determinization u = determinization_algebra u (algebras u)

  let from_tree (st : stree) =
    let rec aux (st : stree) : SS.t * S.t * ST.t = match st with
      (* | Hole ->
       *   let ns = S.fresh () in
       *   (SS.singleton ns, ns, ST.singleton (H, [], ns)) *)
      | Node(s,l) ->
        let ll = List.map aux l in
        let ns = S.fresh () in
        let nt = (s,List.map (fun (_, x, _) -> x) ll,ns) in
        let allstate, trans = List.fold_left (
            fun (at, ay) (t, _, y) -> (
                SS.union at t,
                ST.union ay y)
          ) (SS.singleton ns, ST.singleton nt) ll in
        (allstate, ns, trans)
    in
    let hs = S.fresh () in
    let all_state, final_state, trans = aux st in
    {
      q = SS.add hs all_state;
      d = trans;
      f = SS.singleton final_state
    }

  let from_tree_to_dfta (sa: sigma_algebra) (st : stree): dfta =
    let x = from_tree st in
    let rep = determinization_algebra_to_dfta sa x in
    rep

  let reachable_states (u : t) =
    let one_step (seen : SS.t) =
      ST.fold ( fun (s,l,d) acc ->
          if not (SS.mem d acc) && List.for_all (fun x -> SS.mem x acc) l then
            SS.add d acc
          else acc
        ) u.d seen
    in
    let rec fix seen =
      let next = one_step seen in
      if SS.compare seen next = 0 then next
      else fix next
    in
    fix SS.empty

  let make_hole_non_coreachable (u: dfta) =
    if is_hole_correachable u then
      let sa = get_sigma_algebra u in
      dfta_of_t sa (t_of_dfta u)
    else u


  let reachable_states_dfta (u : dfta) =
    (* let sa = u.dfta_a in *)
    reachable_states (t_of_dfta u)

  let rm_non_reachable_dfta (u : dfta) =
    let ss = reachable_states_dfta u in
    let ss = SS.add u.dfta_h ss in
    let f x = SS.mem x ss in
    {
      u with
      dfta_q = SS.filter f u.dfta_q;
      dfta_f = SS.filter f u.dfta_f;
      dfta_d = DetTrans.fold (fun (s, ql, q) acc ->
          if List.for_all f ql && f q then
            DetTrans.add_transition (s, ql, q) acc
          else acc
        ) u.dfta_d DetTrans.empty
    }

  let coreachable_states (u: t) =
    let one_step (reach : SS.t) (activated : ST.t) =
      ST.fold ( fun ((s,l,d) as trans) (accr,acca) ->
          if (not (ST.mem trans activated) && SS.mem d reach) then
            List.fold_left (fun acc x -> SS.add x acc) accr l, ST.add trans acca
          else (accr,acca)
        ) u.d (reach,activated)
    in
    let rec fix seen activated =
      let next, activated = one_step seen activated in
      if SS.compare seen next = 0 then next
      else fix next activated
    in
    let rep = fix u.f ST.empty in
    rep

  let change_sigma_algebra (sa: sigma_algebra) (u: dfta): dfta =
    let u = make_hole_non_coreachable u in
    {
      u with
      dfta_d = DetTrans.fold (fun (s, l, q) acc ->
          if SA.mem_symbol s sa then
            DetTrans.add_transition (s, l, q) acc
          else acc
        ) u.dfta_d DetTrans.empty;
      dfta_a = sa;
    }

  let remove_states (u : t) (rm : SS.t) =
    {
      q = SS.diff u.q rm ;
      f = SS.diff u.f rm ;
      d = ST.filter (fun (s,l,q) -> (not (SS.mem q rm)) && SS.is_empty (SS.inter (SS.of_list l) rm)) u.d;
    }

  let print_m fmt u =
    let u = (remove_states u (SS.diff (state u) (coreachable_states u))) in
    print fmt u

  type mtree =
    | MSta of S.t
    | MNod of algebra * (mtree list)
  let rec compare_mtree a b = match a,b with
    | MSta s, MSta s' -> S.compare s s'
    | MNod(s,l),MNod(s',l') when s = s' -> (ToolBox.list_compare compare_mtree) l l'
    | _ -> 1

  let rec print_mtree fmt = function
    | MSta s -> S.print fmt s
    | MNod(s,l) -> Format.fprintf fmt "%a(%a)" A.print s (ToolBox.print_list_inline_no_border print_mtree) l
  type ex_rule =
    mtree * S.t
  let compare_ex_rule = ToolBox.pair_compare compare_mtree S.compare

  let print_r (fmt : Format.formatter) (l : ex_rule list) : unit =
    ToolBox.print_list (ToolBox.print_pair print_mtree S.print) fmt l

  let only_one (s : S.t) (r : ex_rule list) =
    let exception Not_possible in
    try
      List.fold_left (fun acc (a,s') ->
          if S.compare s s' = 0  then
            match acc with
            | None -> Some a
            | Some _ -> raise Not_possible
          else
            acc
        ) None r
    with
    | Not_possible -> None

  let rec apply_tree s tr mt = match mt with
    | MSta s' when S.compare s s' = 0 -> tr
    | MNod(sy,l) -> MNod(sy,List.map (apply_tree s tr) l)
    | _ -> mt

  let apply s tr r =
    List.map (fun (x,y) -> (apply_tree s tr x,y)) r

  let rec lstate_tree r = match r with
    | MSta s -> SS.singleton s
    | MNod(s,l) -> List.fold_left (fun acc x -> SS.union (lstate_tree x) acc) SS.empty l
  let lstate r = List.fold_left (fun acc (x,y) -> SS.union (lstate_tree x) acc) SS.empty r
  let find_new_one r =
    let ls = lstate r in
    let exception Found of S.t * mtree in
    try
      let () =
        SS.iter (fun x -> match only_one x r with | None -> () | Some t -> raise (Found(x,t))) ls
      in None
    with
    | Found(x,y) -> Some(x,y)

  let rec fix (r : ex_rule list)=
    match find_new_one r with
    | None -> r
    | Some(x,t) ->
      let r = apply x t r in
      let r = ToolBox.rm_doub compare_ex_rule r in
      fix r

  let of_usual (r : ST.t) =
    r |> ST.elements |> List.map (fun (s,l,q) -> (MNod(s,List.map (fun a -> MSta a) l),q))

  let print_more fmt u =
    let u = (remove_states u (SS.diff (state u) (coreachable_states u))) in
    let r = of_usual u.d in
    let r = fix r in
    Format.fprintf fmt "@[<v>q : %a@,h : %a@,f : %a@,d : %a@,i : %a@]"
      print_state_set u.q
      print_state_set u.f
      print_r r

  let top (sa: sigma_algebra) : t=
    let ns = S.fresh () in
    {
      q = SS.singleton ns;
      d = SA.fold (fun (s, n) acc ->
          ST.add (s, ToolBox.fold_int (fun i acc -> ns :: acc) n [], ns) acc
        ) sa ST.empty;
      f = SS.singleton ns
    }

  let bottom =
    let ns = S.fresh () in
    { q = SS.singleton ns;
      d = ST.empty ;
      f = SS.empty ;
    }

  let top_dfta (sa: sigma_algebra): dfta=
    let ns = S.fresh () in
    let hole = S.fresh () in
    {
      dfta_q  = SS.of_list [ns; hole];
      dfta_d  =
        SA.fold (fun (s, n) acc  ->
            DetTrans.add_transition (s, (ToolBox.fold_int (fun _ acc -> ns :: acc) n []), ns) acc
          ) sa DetTrans.empty;
      dfta_f  = SS.singleton ns;
      dfta_h  = hole;
      dfta_hc = false;
      dfta_a  = sa;
    }

  let bottom_dfta (sa: sigma_algebra): dfta =
    let ns = S.fresh () in
    let hole = S.fresh () in
    {
      dfta_q  = SS.of_list [ns; hole];
      dfta_d  = DetTrans.empty;
      dfta_f  = SS.empty;
      dfta_h  = hole;
      dfta_hc = false;
      dfta_a  = sa;
    }

  (** [is_bottom u] tests whether [u] recognizes the empty language *)
  let is_bottom (u : t) =
    SS.is_empty (SS.inter (reachable_states u) (u.f))

  let is_bottom_dfta (u : dfta) =
    SS.is_empty (SS.inter (reachable_states_dfta u) (u.dfta_f))
  (* let is_bottom_dfta (u: t) = *)


  (** [complementary u] computes au automaton recognizing the
      complementary of [u] *)
  let complementary (u : t) =
    let u = determinization u in
    let s = state u in
    {
      u with
      f = SS.diff s u.f
    }

  let complementary_dfta (u : dfta) =
    let s = state_dfta u in
    {
      u with
      dfta_f = SS.diff s u.dfta_f;
      dfta_hc = not u.dfta_hc
    }

  (** [minimization_p u p b] computes an over-approximation of the
      minimum automaton recognizing [u] and working on the
      sigma-algebra [sym], widening is used for integers if [widening]
      and the maximum number of state of the produced automata is
      paramater if not None or is however big needed. Note : this
      function only works on complete, deterministic automaton *)
  let minimization_p (u : t) (parameter : int option) (widening : bool) =
    (* Tool function to build [l_0, ... l_{i-1},q,l_{i+1},...] *)
    let gene_arguments l q i =
      let rec aux l j =
        match l with
        | p::r ->
          if j = i then q :: p :: r
          else p :: (aux r (j+1))
        | [] -> q :: []
      in
      aux l 0
    in
    let module UF = UnionFind.Make(S) in
    let module Delta = Map.Make(
      struct
        type t = algebra * S.t list
        let compare = ToolBox.pair_compare compare (ToolBox.list_compare S.compare)
      end
      )
    in
    let trans = ST.fold (fun (s,l,q) acc -> Delta.add (s,l) q acc) u.d Delta.empty in
    let find_trans x =
      if Delta.mem x trans then
        Delta.find x trans
      else
        failwith "minimization needs to be called on a complete DFTA"
    in
    let sym = algebras u
    and sta = state u in
    let stal = SS.elements sta in
    let exception NE of UF.t in
    let equiv_e q q' (old : UF.t) =
      let b,old = UF.is_equiv q q' old in
      if not b then (* let () = debug "splitting %a and %a : due to not_previously equivalent" S.print q S.print q' in *) raise (NE old)
      else
        begin
          SA.fold (fun (f, n) (old : UF.t) ->
              if n = 0 then
                old
              else
                begin
                  if n = 1 then
                    begin
                      let iq  = find_trans (f,[q ]) in
                      let iq' = find_trans (f,[q']) in
                      let b,old = UF.is_equiv iq iq' old in
                      if not b then
                        (* let () = debug "splitting %a and %a : due to %a(%a)->%a <> %a(%a)->%a"
                         *     S.print q S.print q'
                         *     A.print f S.print q  S.print iq
                         *     A.print f S.print q' S.print iq'
                         * in *) raise (NE old)
                      else old
                    end
                  else
                    begin
                      let gene = ListGenerator.init stal (n-1) in
                      let rec aux gene old = match gene with
                        | None -> old
                        | Some gene ->
                          begin
                            let args, next = ListGenerator.read_and_next gene in
                            let old = ToolBox.fold_int ( fun i old ->
                                let arg1 = gene_arguments args q  i in
                                (* let () = debug "%a" print_state_list args in *)
                                let arg2 = gene_arguments args q' i in
                                let iq  = find_trans(f,arg1) in
                                let iq' = find_trans(f,arg2) in
                                let b,old = UF.is_equiv iq iq' old in
                                if (not b) then
                                  (* let () = debug "splitting %a and %a : due to %a(%a)->%a <> %a(%a)->%a"
                                   *     S.print q S.print q'
                                   *     A.print f print_state_list arg1 S.print iq
                                   *     A.print f print_state_list arg2 S.print iq'
                                   * in *) raise (NE old)
                                else old
                              ) n old
                            in
                            aux next old
                          end
                      in
                      aux (Some gene) old
                    end
                end
            ) sym old
        end
    in
    let equiv q q' old =
      try
        let old = equiv_e q q' old in
        (true,old)
      with
      | NE old -> (false,old)
    in
    let rec split_one_class cl old neweq = match cl with
      | p :: q ->
        let eq,neq,old =
          List.fold_left (fun (acceq,accneq,old) x ->
              let b,old = equiv p x old in
              (* let () = debug "EQ : %a %a %a" S.print p S.print x Format.pp_print_bool b in *)
              if b then
                (x :: acceq, accneq, old)
              else
                (acceq, x :: accneq, old)
            ) ([],[],old) q
        in
        split_one_class neq old ((p::eq) :: neweq)
      | [] -> neweq,old
    in
    let next_partition (old : UF.t) p =
      let eq,old = UF.to_list old in
      let nb_part = List.length eq in
      let neq,old,nb_new_part = List.fold_left (fun (nc,old,nb_class) cl ->
          let cl',old = split_one_class cl old [] in
          let nb_new_class = List.length cl' in
          if (nb_new_class - 1 + nb_class) <= p then
            (cl'@nc,old,nb_new_class - 1 + nb_class)
          else
            (cl::nc,old,nb_class)
        ) ([],old,nb_part) eq
      in
      UF.from_list neq,(nb_part = nb_new_part)
    in
    let rewrite_of_eq_classes (e : UF.t) =
      let all = UF.all e in
      List.fold_left (fun acc x -> MS.add x (fst (UF.find x e)) acc) MS.empty all
    in
    let rec fix old =
      let ne,b = next_partition old (match parameter with | None -> SS.cardinal u.q | Some p -> p) in
      match parameter with
      | None ->
        begin
          if b then old
          else fix ne
        end
      | Some a ->
        let size_n = UF.nb_class ne in
        if size_n > a || b then
          old
        else
          fix ne
    in
    let starting =
      let term = u.f in
      let non_term = SS.diff sta term in
      (* let term_no_hole = SS.remove u.h term in
       * let non_term_no_hole = SS.remove u.h non_term in *)
      (* let hole = [u.h] in *)
      let rep = UF.from_list [SS.elements (term);SS.elements (non_term)] in
      rep
    in
    let eq_final = fix starting in
    apply_map (rewrite_of_eq_classes eq_final) u (* widening *)

  let minimization_dfta_p (u : dfta) (parameter : int option) =
    let sym = u.dfta_a in
    let gene_arguments l q i =
      let rec aux l j =
        match l with
        | p::r ->
          if j = i then q :: p :: r
          else p :: (aux r (j+1))
        | [] -> q :: []
      in
      aux l 0
    in
    let module UF = UnionFind.Make(S) in
    let find_trans (q, l) =
      delta_dfta u l q
    in
    (* let () = debug "sigma_algebra: %a" print_sigma_algebra sym in *)
    let sta = state_dfta u in
    let size = SS.cardinal sta in
    let stal = SS.elements sta in
    let exception NE of UF.t in
    let equiv_e q q' (old : UF.t) =
      let b, old = UF.is_equiv q q' old in
      if not b then (* let () = debug "splitting %a and %a : due to not_previously equivalent" S.print q S.print q' in *) raise (NE old)
      else
        begin
          SA.fold (fun (f, n) (old : UF.t) ->
              if n = 0 then
                old
              else
                begin
                  if n = 1 then
                    begin
                      let iq  = find_trans (f,[q ]) in
                      let iq' = find_trans (f,[q']) in
                      let b,old = UF.is_equiv iq iq' old in
                      if not b then
                        raise (NE old)
                      else old
                    end
                  else
                    begin
                      let gene = ListGenerator.init stal (n-1) in
                      let rec aux gene old = match gene with
                        | None -> old
                        | Some gene ->
                          begin
                            let args, next = ListGenerator.read_and_next gene in
                            let old = ToolBox.fold_int ( fun i old ->
                                let arg1 = gene_arguments args q  i in
                                let arg2 = gene_arguments args q' i in
                                let iq  = find_trans(f,arg1) in
                                let iq' = find_trans(f,arg2) in
                                let b, old = UF.is_equiv iq iq' old in
                                if (not b) then
                                  raise (NE old)
                                else old
                              ) n old
                            in
                            aux next old
                          end
                      in
                      let rep = aux (Some gene) old in
                      rep
                    end
                end
            ) sym old
        end
    in
    let equiv q q' old =
      try
        let old = equiv_e q q' old in
        (true,old)
      with
      | NE old -> (false,old)
    in
    let rec split_one_class cl old neweq = match cl with
      | p :: q ->
        let eq, neq, old =
          List.fold_left (fun (acceq, accneq, old) x ->
              let b, old = equiv p x old in
              if b then
                (x :: acceq, accneq, old)
              else
                (acceq, x :: accneq, old)
            ) ([],[],old) q
        in
        split_one_class neq old ((p::eq) :: neweq)
      | [] -> neweq,old
    in
    let next_partition (old : UF.t) p =
      let eq,old = UF.to_list old in
      let nb_part = List.length eq in
      let neq, old, nb_new_part = List.fold_left (fun (nc, old, nb_class) cl ->
          let cl',old = split_one_class cl old [] in
          let nb_new_class = List.length cl' in
          if (nb_new_class - 1 + nb_class) <= p then
            (cl'@nc,old,nb_new_class - 1 + nb_class)
          else
            (cl::nc,old,nb_class)
        ) ([],old,nb_part) eq
      in
      UF.from_list neq,(nb_part = nb_new_part)
    in
    let rewrite_of_eq_classes (e : UF.t) =
      let all = UF.all e in
      List.fold_left (fun acc x -> MS.add x (fst (UF.find x e)) acc) MS.empty all
    in
    let rec fix old =
      let ne, b = next_partition old (match parameter with | None -> size | Some p -> p) in
      match parameter with
      | None ->
        begin
          if b then old
          else fix ne
        end
      | Some a ->
        let size_n = UF.nb_class ne in
        if size_n > a || b then
          old
        else
          fix ne
    in
    let starting =
      let term = u.dfta_f in
      let non_term = SS.diff sta term in
      let rep = UF.from_list [SS.elements (term);SS.elements (non_term)] in
      rep
    in
    (* let () = debug "starting there" in *)
    let eq_final = fix starting in
    (* let () = debug "eq_class: %a" UF.print eq_final in *)
    let rew = rewrite_of_eq_classes eq_final in
    (* let () = debug "rew: %a" (ToolBox.print_map
     *     S.print
     *     S.print
     *     MS.bindings) rew
     * in *)
    apply_map_dfta_break_det rew u

  (** [minimization_algebra u sym b] computes an over-approximation of
      the minimum automaton recognizing [u] and working on the
      sigma-algebra [sym], widening is used for integers if [b]*)
  let minimization_algebra_w u sym b =
    let u = determinization_algebra u sym in
    minimization_p u None b

  (** [minimization_algebra u sym b] computes an over-approximation of
      the minimum automaton recognizing [u] and working on the
      sigma-algebra [sym], widening is not used for integers *)
  let minimization_algebra u sym =
    minimization_algebra_w u sym false

  (** [minimization u] computes an over-approximation of the minimum
      automaton recognizing [u], widening is not used for integers *)
  let minimization u =
    minimization_algebra_w u (algebras u) false

  (** [minimization_w u] computes an over-approximation of the minimum
      automaton recognizing [u], widening is used for integers *)
  let minimization_w u =
    minimization_algebra_w u (algebras u) true

  (** [join_when_mini u u'] computes a tree automaton recognizing an
      over-approximation of the union of [u] and [u'] when [u] and [u']
      are deterministic, complete, minimum automaton *)
  let join_when_mini u u' =
    let q, d, rp = product u u' in
    let before_mini =
      {
        q;
        d;
        f =
          SS.union
            (SS.fold (fun x acc ->
                 SS.fold (fun y acc ->
                     SS.add (RP.apply (x,y) rp) acc
                   ) u'.f acc
               ) (state u) SS.empty
            )
            (SS.fold (fun x acc ->
                 SS.fold (fun y acc ->
                     SS.add (RP.apply (x,y) rp) acc
                   ) (state u') acc
               ) (u.f) SS.empty
            )
      } in
    before_mini

  (** [join u u'] computes a tree automaton recognizing an
      over-approximation of the union of [u] and [u'] *)
  let join u u' =
    let aa = algebras u
    and bb = algebras u' in
    let sym = SA.union aa bb in
    let u = minimization_algebra u sym in
    let u' = minimization_algebra u' sym in
    (join_when_mini u u') |> minimization

  let join_dfta (u: dfta) (u': dfta): dfta =
    if not (SA.compare u.dfta_a u'.dfta_a = 0) then
      failwith "[treeAutomataBetter.join_dfta] called on TAs on \
                incompatible sigma algebra"
    else
      begin
        let u  = make_hole_non_coreachable u  in
        let u' = make_hole_non_coreachable u' in
        let sa = u.dfta_a in
        let hole, allq, delta, rp = product_dfta_improved sa u u' in
        (* let () = debug "%a" RP.print rp in *)
        let no_mini =
          {
            dfta_q = allq;
            dfta_d = delta;
            dfta_f =
              SS.union
                (SS.fold (fun x acc ->
                     SS.fold (fun y acc ->
                         try
                           let res = RP.apply (x, y) rp in
                           SS.add res acc
                         with
                         | Not_found -> acc
                       ) u'.dfta_f acc
                   ) (state_dfta u) SS.empty
                )
                (SS.fold (fun x acc ->
                     SS.fold (fun y acc ->
                         try
                           let res = RP.apply (x, y) rp in
                           SS.add res acc
                         with
                         | Not_found -> acc
                       ) (state_dfta u') acc
                   ) u.dfta_f SS.empty
                );
            dfta_h = hole;
            dfta_hc = false;
            dfta_a = sa
          }
        in
        (* let () = debug "no mini: %a%!" print_dfta no_mini in *)
        (* let no_mini = rm_non_reachable_dfta sa no_mini in
         * let () = debug "no mini non reach: %a" print_dfta no_mini in *)
        let rep = minimization_dfta_p no_mini None in
        (* let () = debug "over: %a" print_dfta rep in *)
        rep
      end

  let meet_dfta u u' =
    if not (SA.compare u.dfta_a u'.dfta_a = 0) then
      failwith "[treeAutomataBetter.join_dfta] called on TAs on \
                incompatible sigma algebra"
    else
      begin
        let u  = make_hole_non_coreachable u  in
        let u' = make_hole_non_coreachable u' in
        let sa = u.dfta_a in
        let hole, allq, delta, rp = product_dfta_improved sa u u' in
        let no_mini =
          {
            dfta_q = allq;
            dfta_d = delta;
            dfta_f =
              SS.fold (fun x acc ->
                  SS.fold (fun y acc ->
                      try
                        let res = RP.apply (x, y) rp in
                        SS.add res acc
                      with
                      | Not_found -> acc
                    ) u'.dfta_f acc
                ) u.dfta_f SS.empty;
            dfta_h = hole;
            dfta_hc = false;
            dfta_a = sa
          }
        in
        minimization_dfta_p no_mini None
      end

  (** [meet u u'] computes a tree automaton recognizing an
      over-approximation of the intersection of [u] and [u'] *)
  let meet u u' =
    let aa = algebras u
    and bb = algebras u' in
    let sym = SA.union aa bb in
    let u = minimization_algebra u sym in
    let u' = minimization_algebra u' sym in
    let q, d, rp = product u u' in
    let before_mini = {
      q;
      d;
      f =
        SS.fold (fun x acc ->
            SS.fold (fun y acc ->
                SS.add (RP.apply (x,y) rp) acc
              ) u'.f acc
          ) u.f SS.empty
    } in
    before_mini |> minimization

  (** [leq u u'] tests the inclusion of [u] in [u']*)
  let leq u u' =
    meet u (complementary u') |> is_bottom

  let leq_dfta u u' =
    meet_dfta u (complementary_dfta u') |> is_bottom_dfta

  module Int = struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end


  let add_rule q q' i (system : RegExp.systemvm) =
    let vq  = var_of_state q  in
    let vq' = var_of_state q' in
    try
      let (cst, vm) = List.assoc vq system in
      try
        let coef = RegExp.VMap.find vq' vm in
        vm
        |> RegExp.VMap.remove vq'
        |> RegExp.VMap.add vq' (RegExp.A(coef,RegExp.L i))
        |> fun x -> ToolBox.set_assoc vq (cst, x) system
      with
      | Not_found ->
        ToolBox.set_assoc vq (cst, RegExp.VMap.add vq' (RegExp.L i) vm) system
    with
    | Not_found ->
      ToolBox.set_assoc vq (None, RegExp.VMap.singleton vq' (RegExp.L i)) system

  let add_epsilon q (system : RegExp.systemvm) =
    let vq = var_of_state q  in
    try
      let (cst, vm) = List.assoc vq system in
      match cst with
      | None -> ToolBox.set_assoc vq (Some RegExp.E, vm) system
      | Some c -> ToolBox.set_assoc vq (Some (RegExp.A(c, RegExp.E)), vm) system
    with
    | Not_found ->
      ToolBox.set_assoc vq (Some RegExp.E,RegExp.VMap.empty) system

  module SVar = MRelation.Make(S)
      (struct type t = string let compare = compare let print = Format.pp_print_string end)

  let state_position (u : t) : RegExp.u MS.t =
    let ss = state u in
    let rel =
      SS.fold (fun x acc -> SVar.add_pair (var_of_state x, x) acc)
        ss
        SVar.empty
    in
    (* let () = debug "just before: %a" print u in *)
    let system =
      ST.fold (fun (f, ql, q) acc ->
          (* let () = debug "trans: %a" print_transition (f, ql, q) in *)
          List.fold_left (fun (acc, i) q' -> (add_rule q' q i acc, i+1)) (acc, 0) ql
          |> fst
        ) u.d [] |>
      SS.fold add_epsilon u.f
      |> RegExp.system_simplify
    in
    (* let () = debug "system: %a" RegExp.print_systemvm system in *)
    (* let () = debug "@[<v>System : %a@,@]" (RegExp.print_systemm RegExp.Right) system in *)
    let system = SS.fold (fun q system ->
        let vq = var_of_state q in
        if List.mem_assoc vq system then
          system
        else (vq, (None, RegExp.VMap.empty)) :: system
      ) u.q system in
    (* let () = debug "@[<v>System : %a@,@]" (RegExp.print_systemm RegExp.Right) system in *)
    let sol = RegExp.arden_solve RegExp.Right system in
    let sol = RegExp.system_simplify sol in
    let state_of_string v = SVar.findr v rel in
    List.fold_left
      (fun acc (qv, vm) -> MS.add (state_of_string qv) (RegExp.vm_to_regexp RegExp.Right vm) acc)
      MS.empty sol

  let state_position_dfta (u: dfta): RegExp.u MS.t =
    (* let sa = u.dfta_a in *)
    let u' = t_of_dfta u in
    let () = debug "u': %a" print u' in
    state_position u'

  let find_position (a: A.t) (u : t) (ms : RegExp.u MS.t) =
    let ss =
      ST.fold (fun (a', _, q) acc ->
          if A.compare a a' = 0 then
            SS.add q acc
          else acc
        ) u.d SS.empty
    in
    SS.fold (fun q acc ->
        let e = MS.find q ms in
        RegExp.A(acc, e)
      ) ss RegExp.N

  let find_position_dfta (a: A.t) (u : dfta) (ms : RegExp.u MS.t) =
    let s = delta_dfta u [] a in
    try
      MS.find s ms
    with
    | Not_found -> RegExp.N

  let print_sp =
    ToolBox.print_map S.print RegExp.print_u MS.bindings

  let widening_dfta_p u u' p =
    let u = join_dfta u u' in
    (* let () = debug "joined: %a" print_dfta u in *)
    minimization_dfta_p u (Some p)

  let widening_dfta u u' =
    let u = join_dfta u u' in
    let p = SS.cardinal (state_dfta u) in
    minimization_dfta_p u (Some p)

  (** [widening_p u u' p] widens [u] and [u'] yielding an automata of
      maximum size [p] *)
  let widening_p u u' p =
    let aa = algebras u
    and bb = algebras u' in
    let sym = SA.union aa bb in
    let u  = minimization_algebra u sym in
    let u' = minimization_algebra u' sym in
    let u = join_when_mini u u' in
    let u = minimization_w u in
    minimization_p u (Some p) true

  (** [widening_p u u' p] widens [u] and [u'] yielding an automata of
      maximum size max(|[u]|,|[u']|) *)
  let widening u u' =
    let aa = algebras u
    and bb = algebras u' in
    let sym = SA.union aa bb in
    let u  = minimization_algebra u sym in
    let u' = minimization_algebra u' sym in
    let n  = max (SS.cardinal (u.q)) (SS.cardinal (u'.q)) in
    let u = join_when_mini u u' in
    let u = minimization_w u in
    minimization_p u (Some n) true

  let head (u: dfta) =
    let module MM = Map.Make(struct type t = algebra * int let compare = ToolBox.pair_compare A.compare (-) end) in
    let addl (s, n) (l: S.t list) (cur: SS.t list MM.t) =
      try
        let l' = MM.find (s, n) cur in
        let l'' = List.map2 SS.add l l' in
        MM.remove (s, n) cur
        |> MM.add (s, n) l''
      with
      | Not_found ->
        MM.add (s, n) (List.map SS.singleton l) cur
    in
    let u = make_hole_non_coreachable u in
    let m = DetTrans.fold (fun (s, l, q') acc ->
        if SS.mem q' u.dfta_f then
          addl (s, List.length l) l acc
        else acc
      ) u.dfta_d MM.empty
    in
    MM.map (fun l ->
        List.map ( fun qq ->
            {u with
             dfta_f = qq
            }
          ) l
      ) m
    |> MM.bindings

  (** no composition is needed *)
  (* let fold_map_ *)

  let rename_all_states (u: dfta) f =
    let get_name q memo =
      try
        (MS.find q memo, memo)
      with
      | Not_found ->
        let q' = f () in
        (q', MS.add q q' memo)
    in
    let memo = MS.empty in
    let new_dfta_q, memo =
      SS.fold (fun q (dfta_q, memo) ->
          let q', memo = get_name q memo in
          (SS.add q' dfta_q , memo)
        ) u.dfta_q (SS.empty, memo)
    in
    let new_dfta_f, memo =
      SS.fold (fun q (dfta_f, memo) ->
          let q', memo = get_name q memo in
          (SS.add q' dfta_f , memo)
        ) u.dfta_f (SS.empty, memo)
    in
    let new_dfta_d, memo =
      DetTrans.fold (fun (s, l, q) (acc, memo) ->
          let l', memo =
            List.fold_left (fun (l', memo) x ->
                let q', memo = get_name x memo in
                (q'::l' , memo)
              ) ([], memo) l
          in
          let q', memo = get_name q memo in
          (DetTrans.add_transition (s, (List.rev l'), q') acc, memo)
        ) u.dfta_d (DetTrans.empty, memo)
    in
    let new_dfta_h, memo =
      get_name u.dfta_h memo
    in
    {
      u with
      dfta_q = new_dfta_q;
      dfta_f = new_dfta_f;
      dfta_d = new_dfta_d;
      dfta_h = new_dfta_h;
    }

  let not_final_constant (s: A.t) (u: dfta): dfta =
    let sa = u.dfta_a in
    let () = S.restart () in
    let u = rename_all_states u S.fresh in
    let from_s = DetTrans.fold (fun (s', l, q) ss ->
        if A.compare s s' = 0 then SS.add q ss
        else ss
      ) u.dfta_d SS.empty in
    let final_from_s = SS.inter u.dfta_f from_s in
    let renaming, newstates, renamingopp = SS.fold (fun q (renaming, newstates, renamingopp) ->
        let q' = S.fresh () in
        (MS.add q q'  renaming, SS.add q' newstates, MS.add q' q renamingopp)
      ) final_from_s (MS.empty, SS.empty, MS.empty)
    in
    let u = t_of_dfta u in
    let r = apply_map renaming u in
    let r = { r with
      f = SS.union final_from_s r.f;
      q = SS.union final_from_s r.q
            } in
    let trans =
      ST.fold (fun (s, ql, q) trans ->
          if SS.mem q newstates then
            trans
            |> ST.add (s, ql, q)
            |> ST.add (s, ql, MS.find q renamingopp)
          else trans |> ST.add (s, ql, q)
        ) r.d ST.empty
    in
    {r with d = trans} |> dfta_of_t (sa)

  let final_constant (s: A.t) (u: dfta): dfta =
    {u with dfta_d = DetTrans.filter_symbol s u.dfta_d}

  let remove_state_dfta (s: SS.t) (u: dfta) =
    {u with
     dfta_q = SS.diff u.dfta_q s;
     dfta_f = SS.diff u.dfta_f s;
     dfta_d = DetTrans.fold (fun (s', l, q) acc ->
         if List.exists (fun x -> SS.mem x s) l || SS.mem q s then acc
         else DetTrans.add_transition (s', l, q) acc
       ) u.dfta_d DetTrans.empty;
    }

  let build_tree (s: algebra) (ul: dfta list): dfta =
    let sa =
      SA.empty
      |> SA.add (s, List.length ul)
      |> ToolBox.fold (fun x acc -> x.dfta_a |> SA.union acc) ul
    in
    let ul = List.map make_hole_non_coreachable ul in
    let () = S.restart () in
    let ul = List.map (fun u -> rename_all_states u S.fresh) ul in
    let new_hole = S.fresh () in
    let new_end = S.fresh () in
    let finals = ToolBox.fold (fun x acc -> x.dfta_f :: acc) ul [] |> List.rev in
    let merge =
      {
        q = ToolBox.fold (fun x acc -> SS.union acc x.dfta_q) ul (SS.of_list [new_end; new_hole]);
        f = SS.singleton new_end;
        d = ToolBox.fold (fun x acc ->
            DetTrans.fold (fun (s, l, q) acc ->
                ST.add (s, l, q) acc
              ) x.dfta_d acc
          ) ul ST.empty;
      }
    in
    let newd = Enumerator.fold (fun l acc ->
        ST.add (s, l, new_end) acc
      ) (List.map SS.elements finals) merge.d
    in
    let merge = {merge with d = newd} in
    let rewriter = ToolBox.fold (fun x -> MS.add x.dfta_h new_hole) ul MS.empty in
    merge
    |> apply_map rewriter
    |> dfta_of_t sa

end

  (* type t =
   *   {
   *     (\* set of all states *\)
   *     q : SS.t;
   *     (\* set of final states *\)
   *     f : SS.t;
   *     (\* set of transition *\)
   *     d : ST.t;
   *   } *)
