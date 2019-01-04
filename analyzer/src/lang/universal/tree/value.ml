open Mopsa
open Numerical

module Uid =
  struct
    let i = ref 0
    let fresh () =
      let j = !i in
      incr i;
      j
  end

let debug fmt = Debug.debug ~channel:"numerical.tree.value" fmt

module Make(S: SIG.STATE)(A: SIG.COMPARABLE_WITNESS) (* : Framework.Domains.Stacked.S *) =
  struct
    let hole_tree = A.witness

    module SA = TreeAlgebra.Make(A)

    let var_of_automata a =
      "[" ^ (a |> RegExp.regexp_of_automata |> RegExp.to_string) ^ "]"

    let var_of_regexp r =
      "[" ^ (r |> RegExp.to_string) ^ "]"

    module TA = AbstractTreeAutomaton.Make(S)(A)
    type t =
      {
        shape   : TA.dfta;
        support : RegExp.t;
        classes : RegexpPartition.t;
        env     : RegexpPartition.t;
        varbind : StrVarBind.t;
        height    : var;
        size    : var;
      }

    let fresh_var = mktmp ~typ:Ast.T_int

    let holify_sigma_algebra (sa: TA.sigma_algebra) =
      if SA.mem_symbol hole_tree sa then
        sa
      else SA.add (hole_tree, 0) sa

    type 'a tree =
      | Var of string
      | Node of 'a * ('a tree) list

    type input_tree = TA.algebra tree

    let size_it (u: 'a tree) =
      let rec aux u cont =
        match u with
        | Var s -> cont 1
        | Node(_, l) -> aux_list l (fun x -> cont (succ x))
      and aux_list l cont = match l with
        | p::q -> aux p (fun v -> aux_list q (fun v' -> cont (v + v')))
        | [] -> 0
      in
      aux u (fun x -> x)

    let size_it (u: 'a tree) =
      let rec aux u cont =
        match u with
        | Var s -> cont 0
        | Node(_, l) -> aux_list l (fun x -> (cont (succ x)))
      and aux_list l cont = match l with
        | p::q -> aux p (fun v -> aux_list q (fun v' -> cont (max v v')))
        | [] -> 0
      in
      aux u (fun x -> x)

    let rec print_tree printer fmt t = match t with
      | Var s ->
         Format.fprintf fmt "%s" s
      | Node(s,l) -> if List.length l > 0 then
                       Format.fprintf fmt "@[<v 1>%a(@,%a@,@])" printer s
                         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") (print_tree printer)) l
                     else Format.fprintf fmt "%a" printer s

    let print_input_tree = print_tree A.print

    let print2 fmt (u: t) =
      Format.fprintf fmt "@[<v>⯈ shape: @[<v 2>%a@]@,⯈ varbind: @[<v 2>%a@]@,@]"
        TA.print_dfta2 u.shape
        StrVarBind.print u.varbind
    (* Numerical.print u.numeric *)

    let print fmt (u: t) =
      Format.fprintf fmt "@[<v>⯈ shape: @[<v 2>@,%a@]@,⯈ support: @[<v \
                          2>@,%a@]@,⯈ classes: @[<v 2>@,%a@]@,⯈ env: \
                          @[<v 2>@,%a@]@,⯈ varbind: @[<v 2>@,%a@]@,⯈ \
                          height: @[<v 2>@,%a@]@,⯈ size: @[<v 2>@,%a@]@,@]"
        TA.print_dfta u.shape
        RegExp.pp_print_u (RegExp.regexp_of_automata u.support)
        RegexpPartition.print_left u.classes
        RegexpPartition.print u.env
        StrVarBind.print u.varbind
        Var.print u.height
        Var.print u.size
    (* Numerical.print u.numeric *)

    let automata_algebra_on_n (sa: TA.sigma_algebra) =
      let n = SA.max_arity sa in
      ToolBox.fold_int (fun i x -> i::x) n [] |>
        RegExp.Algebra.of_list


    let print_injection =
      ToolBox.print_list_inline (ToolBox.print_pair Format.pp_print_string Format.pp_print_string)

    (* let from_tree (st: input_tree) =
     *   let open TA in
     *   let merge_map_list (ml: (string * SS.t) list) (ml': (string * SS.t) list): (string * SS.t) list =
     *     let dom = List.map fst ml @ List.map fst ml' in
     *     List.fold_left (fun acc x ->
     *         try
     *           let ss = List.assoc x ml in
     *           try
     *             let ss' = List.assoc x ml' in
     *             (x, SS.union ss ss') :: acc
     *           with
     *           | Not_found -> (x, ss) :: acc
     *         with
     *         | Not_found ->
     *            try
     *              let ss' = List.assoc x ml' in
     *              (x, ss') :: acc
     *            with
     *            | Not_found -> acc
     *       ) [] dom
     *   in
     *
     *   let rec aux (st: input_tree): SS.t * S.t * ST.t * (string * SS.t) list = match st with
     *     | Var s ->
     *        let ns = S.fresh () in
     *        (SS.singleton ns, ns, ST.singleton (hole_tree, [], ns), [s, SS.singleton ns])
     *     | Node(s,l) ->
     *        let ll = List.map aux l in
     *        let ns = S.fresh () in
     *        let nt = (s,List.map (fun (_, x, _, _) -> x) ll,ns) in
     *        let allstate, trans, bd = List.fold_left (
     *                                      fun (at, ay, abd) (t, _, y, bd) -> (
     *                                        SS.union at t,
     *                                        ST.union ay y,
     *                                        merge_map_list abd bd
     *                                      )
     *                                    ) (SS.singleton ns, ST.singleton nt, []) ll in
     *        (allstate, ns, trans, bd)
     *   in
     *   let hs = S.fresh () in
     *   let all_state, final_state, trans, bd = aux st in
     *   let r_shape =
     *     {
     *       q = SS.add hs all_state;
     *       d = trans;
     *       f = SS.singleton final_state
     *     }
     *   in
     *   let sa = TA.get_sigma_algebra_from_t r_shape in
     *   let r_shape_dfta =
     *     r_shape
     *     |> TA.determinization_algebra_to_dfta sa
     *     |> fun x -> minimization_dfta_p x None
     *   in
     *   let ra = automata_algebra_on_n sa in
     *
     *   let state_pos = TA.state_position r_shape in
     *   let var_path (v: string) (s: SS.t) =
     *     SS.fold (fun q acc ->
     *         let e = TA.MS.find q state_pos in
     *         RegExp.A(acc, e)
     *       ) s RegExp.N
     *   in
     *   let vari_numvar_eq, r_classes =
     *     List.fold_left (fun ((acc_vari_numvar_eq, acc_r_classes) as acc) (s, ss) ->
     *         if List.exists (fun (x, _) -> s = x) acc_vari_numvar_eq then
     *           acc
     *         else
     *           begin
     *             let re = var_path s ss in
     *             let ns = var_of_regexp re in
     *             ((s, ns) :: acc_vari_numvar_eq,
     *              (re, ns) :: acc_r_classes
     *             )
     *           end
     *       ) ([], []) bd
     *   in
     *   let r_support = TA.find_position hole_tree r_shape state_pos in
     *   let r_env_automata = List.map (fun (x, a) -> RegExp.automata_of_regexp ra x, a) r_classes in
     *   let r_strvarbind = ToolBox.fold (fun (re, rn) strvarbind ->
     *                          StrVarBind.get_var rn strvarbind |> snd
     *                        ) r_env_automata StrVarBind.empty
     *   in
     *   let r_classes_automata = List.filter (fun (x, a) -> not (RegExp.is_cardinality_one x)) r_env_automata in
     *   {
     *     shape = r_shape_dfta;
     *     support = r_support |> RegExp.automata_of_regexp ra ;
     *     classes = r_classes_automata;
     *     env = r_env_automata;
     *     varbind = r_strvarbind;
     *     height = fresh_var ();
     *     size = fresh_var ();
     *   }, vari_numvar_eq *)

    let hole_position (reg_algebra) (u: TA.dfta) =
      let state_pos = TA.state_position_dfta u in
      let nsupport_u = TA.find_position_dfta hole_tree u state_pos in
      let () = debug "nsupport_u: %a" RegExp.print_u nsupport_u in
      let rep = RegExp.automata_of_regexp reg_algebra nsupport_u in
      let () = debug "nsupport_u_auto: %a" RegExp.print rep in
      rep

    (* let from_tree_and_num (st: input_tree) (num: Numerical.t): t =
     *   let u, renaming = from_tree st in
     *   (\* let () = debug "renaming: %a" print_injection renaming in *\)
     *   let env = List.map fst renaming in
     *   let num' = Numerical.change_environment num (Environmentext.int_env_of_list env) in
     *   {
     *     u with
     *     numeric = Numerical.renaming_list renaming num'
     *   } *)



    let merge_splitter (a : RegexpPartition.t ToolBox.StringM.t) (b : RegexpPartition.t ToolBox.StringM.t) : RegexpPartition.t ToolBox.StringM.t =
      ToolBox.StringM.union (fun k a b -> Some (ToolBox.fusion_mem (fun (_,a) (_,b) -> compare a b) a b)) a b

    let print_splitter (* fmt (x : ((RegExp.t * string) list) ToolBox.StringM.t) *) =
      ToolBox.print_map (Format.pp_print_string) (RegexpPartition.print) (ToolBox.StringM.bindings)

    let unify (man: ('a, 'b) man) (u: t) u_num (v: t) v_num =
      let () = debug "u: %a@,on:%a" print u (Flow.print man) u_num in
      let () = debug "v: %a@,on:%a" print v (Flow.print man) v_num in
      let sa = TA.get_sigma_algebra u.shape in
      let auto_algebra = automata_algebra_on_n sa in
      let splitu, splitv = List.fold_left (fun (splitu, splitv) (ue, un) ->
                               let toto = RegExp.meet ue v.support in
                               if RegExp.is_bottom toto then (* [ue] n'est pas dans le support de v*)
                                 (splitu, splitv)
                               else
                                 begin
                                   let (partition, splitv') =
                                     List.fold_left (fun ((partition, splitv) as acc) (ve, vn) ->
                                         let inter = RegExp.meet ue ve in
                                         if not (RegExp.is_bottom (* auto_algebra *) inter) then
                                           let inter_n = var_of_automata inter in
                                           ((inter, inter_n) :: partition,
                                            ToolBox.stringm_replace vn (function Some x  -> (inter, inter_n) :: x | None -> [inter, inter_n]) splitv
                                           )
                                         else
                                           acc
                                       ) ([], splitv) v.env
                                   in
                                   let () = debug "u : %s --- partition : %a" un RegexpPartition.print partition in
                                   (ToolBox.StringM.add un partition splitu, merge_splitter splitv splitv')
                                 end
                             ) (ToolBox.StringM.empty, ToolBox.StringM.empty) u.env in
      let if_non_empty u un =
        if RegExp.is_bottom (* auto_algebra *) u then
          [], []
        else
          [(u, un)], [un]
      in
      let () = debug "splitu: %a"
                 (ToolBox.print_map (Format.pp_print_string) RegexpPartition.print (ToolBox.StringM.bindings))
                 splitu
      in
      let () = debug "splitv: %a"
                 (ToolBox.print_map (Format.pp_print_string) RegexpPartition.print (ToolBox.StringM.bindings))
                 splitv
      in
      let apply_splitter env splitter other_support =
        List.fold_left (fun (nenv_lost, nenv_commun, nenv_n_commun, var_extension) (ue, un) ->
            try
              let x = ToolBox.StringM.find un splitter in
              let () = debug "u : %s --- splitu : %a" un RegexpPartition.print x in
              let joined, vars =
                List.fold_left (fun (joined, vars) (sue, sun) ->
                    (RegExp.join (* auto_algebra *) joined sue, sun:: vars)
                  ) (RegExp.bottom auto_algebra, []) x
              in
              let nue = RegExp.meet (RegExp.compl (* auto_algebra *) joined) ue in
              let nue_supp = RegExp.meet nue other_support in
              let nue_supp_n = var_of_automata nue_supp in
              let () = debug "nue_supp_n: %s" nue_supp_n in
              let nue_non_supp = RegExp.meet nue (RegExp.compl (* auto_algebra *) other_support) in
              let nue_non_supp_n = var_of_automata nue_non_supp in
              let () = debug "nue_non_supp_n: %s" nue_non_supp_n in
              let ns, ns' = if_non_empty nue_supp nue_supp_n in
              let nns, nns' = if_non_empty nue_non_supp nue_non_supp_n in
              let ll = vars @ ns' @ nns' in
              let nl = List.length ll in
              if x = [] && nl = 1 && List.length ns' = 1 then
                ((ue, un) :: nenv_lost,
                 nenv_commun,
                 nenv_n_commun,
                 var_extension
                )
              else if nl = 1 then
                (nenv_lost,
                 x @ nenv_commun,
                 nns @ nenv_n_commun,
                 ToolBox.StringM.add un (nns' @ vars) var_extension)
              else
                (
                  nenv_lost,
                  x @ nenv_commun,
                  nns @ nenv_n_commun,
                  ToolBox.StringM.add un (nns' @ vars) var_extension)
            with
            | Not_found ->
               (nenv_lost, nenv_commun, (ue, un) :: nenv_n_commun, var_extension)
          ) ([], [], [], ToolBox.StringM.empty) env
      in
      let u_lost, n_common_env, n_u_env, u_var_extension = apply_splitter u.env splitu v.support in
      let v_lost, _           , n_v_env, v_var_extension = apply_splitter v.env splitv u.support in
      let () = debug "u_lost: %a" RegexpPartition.print u_lost in
      let () = debug "v_lost: %a" RegexpPartition.print v_lost in
      let n_full_env = n_common_env @ n_u_env @ n_v_env in
      let apply_var_extension num ve vb =
        ToolBox.StringM.fold (fun un l (num, vb) ->
            Numerical.extend (mk_fresh_range ()) man vb un l num
          ) ve (num, vb)
      in
      let n_u_num, n_u_vb = apply_var_extension u_num u_var_extension u.varbind in
      let n_v_num, n_v_vb = apply_var_extension v_num v_var_extension v.varbind in
      let n_u_num, n_u_vb =
        List.fold_left (fun (u_num, u_vb) (re, rn) ->
            Numerical.forget (mk_fresh_range ()) man u_vb rn u_num
          ) (n_u_num, n_u_vb) u_lost
      in
      let n_v_num, n_v_vb =
        List.fold_left (fun (v_num, v_vb) (re, rn) ->
            Numerical.forget (mk_fresh_range ()) man v_vb rn v_num
          ) (n_v_num, n_v_vb) v_lost
      in
      let () = debug "[unif end] u_env: %a" RegexpPartition.print n_u_env in
      let () = debug "[unif end] u_vb: %a" StrVarBind.print n_u_vb in
      let () = debug "[unif end] u_num: %a" (Flow.print man) n_u_num in

      let () = debug "[unif end] v_env: %a" RegexpPartition.print n_v_env in
      let () = debug "[unif end] v_vb: %a" StrVarBind.print n_v_vb in
      let () = debug "[unif end] v_var: %a" (Flow.print man) n_v_num in

      let () = debug "[unif end] common: %a" RegexpPartition.print n_common_env in
      let () = debug "[unif end] full: %a" RegexpPartition.print n_full_env in
      (n_u_env, n_u_num, n_u_vb, n_v_env, n_v_num, n_v_vb, n_common_env, n_full_env)

    module BPart =
      struct
        type 'a t = ('a * string) list list

        let is_in ((_, s') : 'a * string) l : bool =
          List.exists (fun (_, s) -> (s = s')) l

        let class_and_rest (x: ('a * string)) part =
          let rec aux x part acc = match part with
            | ((a::_) as l)::q when is_in x l -> l, List.rev_append acc q
            | p::q -> aux x q (p :: acc)
            | [] -> failwith "BPart could not find element in partitioning"
          in
          aux x part []

        let mk_part l =
          List.map (fun x -> [x]) l

        let mk_equiv x y part =
          let a, part' = class_and_rest x part in
          if is_in y a then
            part
          else
            let b, part'' = class_and_rest y part' in
            (a @ b) :: part''
      end

    let vb_sanitize (u: t) =
      let s = ToolBox.fold (fun (_, rn) ->
                  SetExt.StringSet.add rn
                ) u.env SetExt.StringSet.empty
      in
      let filter (x, _) = SetExt.StringSet.mem x s in
      {
        u with
        varbind = StrVarBind.filter filter u.varbind
      }

    let merge_classes (u: t) (nclass: RegexpPartition.t) =
      let n_part, n_part_name =
        match nclass with
        | (p, _)::q ->
           let n_part =
             List.fold_left (fun acc (e, n) ->
                 RegExp.join acc e
               ) p q
           in
           let n_part_name =
             var_of_automata n_part
           in
           n_part, n_part_name
        | [] -> failwith "[merge_classes] should be called with a non empty partition"
      in
      { u with
        classes = (n_part, n_part_name) :: (List.filter (fun (_, un) ->
                                                not (List.exists (fun (_, vn) -> un = vn) nclass)
                                              ) u.classes);
      }

    let merge_env
          (man: ('b, 'b) man) (u: t)
          (u_num: 'b flow) (nenv: RegexpPartition.t)
        : (t * 'b flow)
      =
      let n_part, n_part_name =
        match nenv with
        | (p, _)::q ->
           let n_part =
             List.fold_left (fun acc (e, n) ->
                 RegExp.join acc e
               ) p q
           in
           let n_part_name =
             var_of_automata n_part
           in
           n_part, n_part_name
        | [] -> failwith "[merge_classes] should be called with a non empty partition"
      in
      let u_num, vb = Numerical.fold
                        (mk_fresh_range ())
                        man u.varbind
                        (List.map snd nenv) n_part_name u_num
      in
      { u with
        env = (n_part, n_part_name) :: (List.filter (fun (_, un) ->
                                            not (List.exists (fun (_, vn) -> un = vn) nenv)
                                          ) u.env);
        varbind = vb;
      }, u_num

    let normalize
          (man: ('a, 'b) man)
          (u: t)
          (u_num: 'a flow)
        : (t * 'a flow)
      =
      (* let rec cross l to_merge vb = match l with
       *   | (ue, un) :: q ->
       *     let eq, neq = find (ue, un) q [] [] u.varbind in
       *     if eq <> [] then cross neq (((ue, un)::eq) :: to_merge) vb
       *     else cross neq to_merge vb
       *   | [] -> (to_merge, vb)
       * and find (ue, un) q eq neq vb = match q with
       *   | (ve, vn) :: r ->
       *     let b, vb = Numerical.eq (mk_fresh_range ()) man un vn u.varbind u_num in
       *     if b then
       *       find (ue, un) r ((ve, vn) :: eq) neq vb
       *     else
       *       find (ue, un) r eq ((ve, vn) :: neq) vb
       *   | [] -> eq, neq
       * in
       * let new_eq_classes, vb = cross u.classes [] u.varbind in *)
      (* let u = {u with varbind = vb} in *)
      (* let u', u_num =
       *   List.fold_left (fun acc x ->
       *       merge_classes acc x
       *     ) u new_eq_classes
       *   |> fun u -> List.fold_left (fun (u, u_num) x ->
       *       merge_env man u u_num x
       *     ) (u, u_num) new_eq_classes
       * in *)
      let () = debug "normalization started on:@,%a@,with:%a@" print u (Flow.print man) u_num in
      let foldable_env, vb = Numerical.find_foldable_variables man u.varbind (u.env |> List.map snd) u_num in
      let () = debug "normalization found: %a" (ToolBox.print_list (ToolBox.print_list_inline Format.pp_print_string)) foldable_env in
      let foldable_env_with_regexp = List.map (fun cl ->
                                         List.map (fun x ->
                                             List.find (fun (ue, un) -> x = un) u.env
                                           ) cl
                                       ) foldable_env
      in
      let u, u_num =
        ToolBox.fold (fun cl (u, u_num) ->
            merge_env man u u_num cl
          ) foldable_env_with_regexp (u, u_num)
      in
      let u = u |> vb_sanitize in
      let () = debug "result is:@,%a@,with:%a@" print u (Flow.print man) u_num in
      (u, u_num)

    let join_eq_classes u v b =
      let bb = match b with
        | None -> true
        | Some ext -> RegExp.is_bottom (*auto_algebra*) ext
      in

      let classes_to_keep u v =
        List.fold_left (fun acc ((e, _) as part) ->
            if List.exists (fun (f, _) -> RegExp.leq (*auto_algebra*) (RegExp.meet e v.support) f) v.classes
            then
              part :: acc
            else acc
          ) [] u.classes
      in
      let cu = classes_to_keep u v in
      let cv = classes_to_keep v u in
      let rec merge_classes res cv cu cuinit = match cuinit with
        | (ue, un) :: q ->
           begin
             try
               let (cv, (ve, vn)) = ToolBox.find_and_remove
                                      (fun (ve, vn) -> not (RegExp.is_bottom (*auto_algebra*) (RegExp.meet ue ve))) cv
               in
               match b with
               | None ->
                  let we = RegExp.join (*auto_algebra*) ue ve in
                  let wn = var_of_automata we in
                  merge_classes ((we, wn) :: res) cv cu q
               | Some ext ->
                  if bb then
                    merge_classes ((ue, un) :: res) cv cu q
                  else
                    let we = RegExp.join (*auto_algebra*) ue (RegExp.meet (RegExp.widening (*auto_algebra*) 6 ue ve) ext) in
                    let wn = var_of_automata we in
                    merge_classes ((we, wn) :: res) cv cu q
             with
             | Not_found ->
                merge_classes res cv ((ue, un) :: cu) q
           end
        | [] ->
           begin
             match b with
             | None -> (res @ cv @ cu)
             | Some _ -> res
           end
      in
      let nclasses = merge_classes [] cv [] cu in
      nclasses

    let meet_eq_classes auto_algebra u v =
      let classes_to_keep u v =
        List.fold_left (fun acc ((e, _) ) ->
            let e' = RegExp.meet e v.support in
            let ne' = var_of_automata e' in
            (e', ne') :: acc
          ) [] u.classes
      in
      let cu = classes_to_keep u v in
      let cv = classes_to_keep v u in
      let rec merge_classes res cv cu cuinit = match cuinit with
        | (ue, un) :: q ->
           begin
             try
               let (cv, (ve, vn)) = ToolBox.find_and_remove
                                      (fun (ve, vn) -> not (RegExp.is_bottom (*auto_algebra*) (RegExp.meet ue ve))) cv
               in
               let we = RegExp.join (*auto_algebra*) ue ve in
               let wn = var_of_automata we in
               merge_classes ((we, wn) :: res) cv cu q
             with
             | Not_found ->
                merge_classes res cv ((ue, un) :: cu) q
           end
        | [] ->
           begin
             (res @ cv @ cu)
           end
      in
      let nclasses = merge_classes [] cv [] cu in
      nclasses

    let common_env man (u: t) u_num (v: t) v_num =
      let sau = TA.get_sigma_algebra u.shape in
      let sav = TA.get_sigma_algebra v.shape in
      let sa  = SA.union sau sav in
      if SA.equal sau sav then
        (u, v, u_num, v_num)
      else
        begin
          let auto_algebra = automata_algebra_on_n sa in
          let nshape_u = TA.change_sigma_algebra sa u.shape in
          let nshape_v = TA.change_sigma_algebra sa v.shape in

          let nsupport_u = RegExp.change_algebra u.support auto_algebra in
          let nsupport_v = RegExp.change_algebra v.support auto_algebra in
          let rename_env env =
            RegexpPartition.fold (fun (re, rn) (acc, renaming) ->
                let re' = RegExp.change_algebra re auto_algebra in
                let rn' = var_of_automata re' in
                (re', rn') :: acc, (if rn <> rn' then (rn, rn') :: renaming else renaming)
              ) env ([], [])
          in

          let nenv_u, renaming_u = rename_env u.env in
          let nenv_v, renaming_v = rename_env v.env in

          let nclasses_u, _ = rename_env u.classes in
          let nclasses_v, _ = rename_env v.classes in

          let num_u, vb_u = Numerical.renaming_list (mk_fresh_range ()) man u.varbind renaming_u u_num in
          let num_v, vb_v = Numerical.renaming_list (mk_fresh_range ()) man v.varbind renaming_v v_num in

          { u with
            shape = nshape_u;
            support = nsupport_u;
            classes = nclasses_u;
            env = nenv_u;
            varbind = vb_u;
          } |> vb_sanitize,
          { v with
            shape = nshape_v;
            support = nsupport_v;
            classes = nclasses_v;
            env = nenv_v;
            varbind = vb_v;
          } |> vb_sanitize,
          num_u,
          num_v
        end

    let same_height_and_size (man) u u_num v v_num =
      u_num, man.exec (mk_rename_var v.height u.height (mk_fresh_range ())) v_num
      |> man.exec (mk_rename_var v.size u.size (mk_fresh_range ()))

    let same_height_and_size_one_num man u v num =
      Numerical.fold_var (mk_fresh_range ()) man [u.height; v.height] u.height num
      |> Numerical.fold_var (mk_fresh_range ()) man [u.size; v.size] u.size

    let meet (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
        : (t * 'b flow * 'b flow) =

      let u, v, u_num, v_num = common_env man u u_num v v_num in
      let u_num, v_num = same_height_and_size man u u_num v v_num in

      let sa = TA.get_sigma_algebra u.shape in
      let auto_algebra = automata_algebra_on_n sa in
      let nshape = TA.meet_dfta u.shape v.shape in
      let nsupport = RegExp.meet u.support v.support in
      let nclasses = meet_eq_classes auto_algebra u v in
      let (n_u_env, n_u_num, n_u_vb, n_v_env, n_v_num, n_v_vb, n_common_env, _) =
        (* TODO: ceci perd de l'info gratuite *)
        unify man u u_num v v_num
      in
      (* let diffu = List.filter (fun (re, rn) ->
       *     not (List.exists (fun (_, rn') -> rn = rn') n_common_env)
       *   ) n_u_env |> List.map snd
       * in
       * let diffv = List.filter (fun (re, rn) ->
       *     not (List.exists (fun (_, rn') -> rn = rn') n_common_env)
       *   ) n_v_env |> List.map snd
       * in *)
      let u_num, v_num, vb_u, vb_v =
        Numerical.meet_different_support man
          u_num v_num
          (n_u_env |> List.map snd) (n_v_env |> List.map snd)
          (List.map snd n_common_env)
          u.varbind v.varbind
      in

      let prep =
        {
          shape = nshape;
          support = nsupport;
          classes = nclasses;
          env = n_common_env;
          varbind = vb_u;
          height = u.height;
          size = u.size;
        }
      in
      (* TODO: on aimerait normaliser prep après, mais on n'a pas le
       résultat du meet*)
      (prep, u_num, v_num)

    let join (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
        : (t * 'b flow * 'b flow) =

      let u, v, u_num, v_num = common_env man u u_num v v_num in
      let u_num, v_num = same_height_and_size man u u_num v v_num in

      let u, u_num = normalize man u u_num in
      let v, v_num = normalize man v v_num in

      let nshape = TA.join_dfta u.shape v.shape in
      let nsupport = RegExp.join (*auto_algebra*) u.support v.support in
      let nclasses = join_eq_classes u v None in
      let (n_u_diff_env, n_u_num, n_u_vb, n_v_diff_env, n_v_num, n_v_vb, n_common_env , n_full_env) =
        unify man u u_num v v_num
      in
      let () = debug "n_u_env:%a@, n_v_env: %a@, n_full_env: %a" RegexpPartition.print n_u_diff_env RegexpPartition.print n_v_diff_env RegexpPartition.print n_full_env in
      let n_v_env = n_common_env @ n_v_diff_env in
      let u_num, v_num, vb_u, vb_v = Numerical.join_different_support
                                       man
                                       n_u_num n_v_num
                                       (n_v_env |> List.map snd)
                                       (n_full_env |> List.map snd)
                                       n_u_vb n_v_vb
      in
      let prep = {
          shape = nshape;
          support = nsupport;
          classes = nclasses;
          env = n_full_env;
          varbind = vb_u;
          size = u.size;
          height = u.height;
        } |> vb_sanitize in
      let () = debug "end join" in
      let () = debug "prep:%a" print prep in
      let () = debug "u_num:%a@,v_num:%a" (Flow.print man) u_num (Flow.print man) v_num in
      (* We would like to normalize *)
      (prep, u_num, v_num)

    let join_same_num (man: ('a, 'b) man) (u: t) (v: t) (num: 'a flow)
        : (t * 'a flow) =
      let nshape = TA.join_dfta u.shape v.shape in
      let num = same_height_and_size_one_num man u v num in
      let nsupport = RegExp.join (*auto_algebra*) u.support v.support in
      let nclasses = join_eq_classes u v None in
      let (n_u_diff_env, n_u_num, n_u_vb, n_v_diff_env, n_v_num, n_v_vb, n_common_env , n_full_env) =
        unify man u num v num
      in
      let n_v_env = n_common_env @ n_v_diff_env in
      let u_num, v_num, vb_u, vb_v = Numerical.join_different_support
                                       man
                                       n_u_num n_v_num
                                       (n_v_env |> List.map snd)
                                       (n_full_env |> List.map snd)
                                       u.varbind v.varbind
      in
      let prep = {
          shape = nshape;
          support = nsupport;
          classes = nclasses;
          env = n_full_env;
          varbind = vb_u;
          height = u.height;
          size = u.size;
        } in
      (* We would like to normalize *)
      (prep, Flow.join man n_u_num n_v_num)

    let subset (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
        : (bool * 'b flow * 'b flow) =

      let u, v, u_num, v_num = common_env man u u_num v v_num in

      let u, u_num = normalize man u u_num in
      let v, v_num = normalize man v v_num in
      let () = debug "starting leq" in
      let () = debug "u: %a" print u in
      let () = debug "v: %a" print v in
      let () = debug "n_u_num: %a" (Flow.print man) u_num in
      let () = debug "n_v_num: %a" (Flow.print man) v_num in
      let find_v_to_u_class_injection u_classes v_classes u_support =
        try
          List.iter (fun (ve, vn) ->
              let ve_r = RegExp.meet ve u_support in
              if RegExp.is_bottom (*re_algebra*) ve_r then
                ()
              else
                try
                  let _ = List.find (fun (ue, un) ->
                              let rep = RegExp.leq (*re_algebra*) ve_r ue in
                              rep
                            ) u_classes
                  in
                  ()
                with
                | Not_found ->
                   if RegExp.is_cardinality_one ve_r then
                     ()
                       (* (injection, v_part :: lonely_class) *)
                   else (* raise Not_found *)
                     raise Not_found
            ) v_classes;
          true
        with
        | Not_found -> false
      in
      if
        RegExp.leq (*re_algebra*) u.support v.support &&
          TA.leq_dfta u.shape v.shape &&
            find_v_to_u_class_injection u.classes v.classes u.support
      then
        let () = debug "entering unification" in
        let (diff_u_env, n_u_num, n_u_vb, diff_v_env, n_v_num, n_v_vb, n_common_env, _) =
          unify man u u_num v v_num
        in
        let () = debug "n_u_num: %a" (Flow.print man) n_u_num in
        let () = debug "n_v_num: %a" (Flow.print man) n_v_num in
        let () = debug "diff_u_env: %a" RegexpPartition.print diff_u_env in
        let () = debug "diff_v_env: %a" RegexpPartition.print diff_v_env in
        let () = debug "n_common_env: %a" RegexpPartition.print n_common_env in
        let () = debug "unify ok" in
        let (n_u_num, n_v_num, _, _) =
          Numerical.env_leq man n_u_num n_v_num
            (diff_v_env |> List.map snd) (n_common_env |> List.map snd)
            n_u_vb n_v_vb
        in
        let () = debug "leaving unification" in
        let () = debug "end leq" in
        let () = debug "n_u_num: %a" (Flow.print man) n_u_num in
        let () = debug "n_v_num: %a" (Flow.print man) n_v_num in
        (true, n_u_num, n_v_num)
      else
        let () = debug "end leq" in
        (false, u_num, v_num)

    (* TODO: is it enough?*)
    let is_bottom range man (u: t) flow =
      TA.is_bottom_dfta u.shape

    (* let subset_same_num (man: ('a, 'b) man) (u: t) (flow: 'a flow) (v: t)
     *   : (bool * 'a flow * 'a flow) =
     *   let u, num = normalize man u flow in
     *   let v, num = normalize man v num in
     *   let find_v_to_u_class_injection u_classes v_classes u_support =
     *     try
     *       List.iter (fun (ve, vn) ->
     *           let ve_r = RegExp.meet ve u_support in
     *           if RegExp.is_bottom (\*re_algebra*\) ve_r then
     *             ()
     *           else
     *             try
     *               let _ = List.find (fun (ue, un) ->
     *                   let rep = RegExp.leq (\*re_algebra*\) ve_r ue in
     *                   rep
     *                 ) u_classes
     *               in
     *               ()
     *             with
     *             | Not_found ->
     *               if RegExp.is_cardinality_one ve_r then
     *                 ()
     *                 (\* (injection, v_part :: lonely_class) *\)
     *               else (\* raise Not_found *\)
     *                 raise Not_found
     *         ) v_classes;
     *       true
     *     with
     *     | Not_found -> false
     *   in
     *   if
     *     RegExp.leq (\*re_algebra*\) u.support v.support &&
     *     TA.leq_dfta u.shape v.shape &&
     *     find_v_to_u_class_injection u.classes v.classes u.support
     *   then
     *     let (n_u_env, n_u_vb, n_v_env, n_v_vb, flow, n_common_env, _) =
     *       (\* let unify annot man (u: t) u_num (v: t) v_num = *\)
     *       unify_same_num man u flow v
     *     in
     *     let (n_u_num, n_v_num, _, _) =
     *       Numerical.env_leq man flow
     *         (n_v_env |> List.map snd) (n_common_env |> List.map snd)
     *         n_u_vb n_v_vb
     *     in
     *     (true)

   *   else
   *     (false) *)

    module Linker =
      struct
        module K =
          struct
            type t = RegExp.t * string
            let compare (_, s) (_, s') = compare s s'
          end
        module M = Map.Make(K)
        module S = Set.Make(K)
        type t =
          {
            u2v : S.t M.t;
            v2u : S.t M.t;
          }


        let print fmt l =
          Format.fprintf fmt "@[<v>u2v:%a@,v2u:%a@]"
            (ToolBox.print_map
               (RegexpPartition.print_pair)
               (ToolBox.print_set RegexpPartition.print_pair S.elements) (M.bindings)
            ) l.u2v
            (ToolBox.print_map
               (RegexpPartition.print_pair)
               (ToolBox.print_set RegexpPartition.print_pair S.elements) (M.bindings)
            ) l.v2u

        let empty = {u2v = M.empty ; v2u = M.empty}
        let add_to_s a b x =
          try
            let e = M.find a x in
            M.add a (S.add b e) x
          with
          | Not_found -> M.add a (S.singleton b) x

        let add u v (t: t) =
          {
            u2v = add_to_s u v t.u2v;
            v2u = add_to_s v u t.v2u;
          }

      end


    let widen (man : ('b, 'b) man)
          (u: t) (u_num: 'b flow)
          (v: t) (v_num: 'b flow) =

      let u_starting_env = u.env in

      let u, v, u_num, v_num = common_env man u u_num v v_num in

      let u, u_num = normalize man u u_num in
      let v, v_num = normalize man v v_num in

      let u_num, v_num = same_height_and_size man u u_num v v_num in

      let () = debug "widening input: %a@, on: %a" print u (Flow.print man) u_num in
      let () = debug "other widening input: %a@, on: %a" print v (Flow.print man) v_num in

      let sa = TA.get_sigma_algebra u.shape in
      let auto_algebra = automata_algebra_on_n sa in

      (* First we stabilize the shape of the automaton *)
      let nshape = TA.widening_dfta_p u.shape v.shape 6 in
      let () = debug "new shape: %a" TA.print_dfta nshape in

      (* We compute the environment *)
      let nsupport = hole_position auto_algebra nshape in
      let () = debug "nsupport: %a" RegExp.pp_print_u (RegExp.regexp_of_automata nsupport) in
      let wid_pos = RegExp.diff nsupport (RegExp.join u.support v.support) in

      let safe_region_in_v =
        RegExp.join
          (RegExp.compl v.support)
          (RegexpPartition.fold (fun (re, rn) acc -> RegExp.join re acc)
             v.env
             (RegExp.bottom auto_algebra)
          )
      in
      let p1_remove, p1' =
        RegexpPartition.fold (fun (re, rn) (to_remove, to_keep) ->
            if RegExp.leq re safe_region_in_v then
              (to_remove, (re, rn) :: (to_keep))
            else ((re, rn) :: to_remove, to_keep)
          ) u.env ([], [])
      in

      let module RegM = Map.Make(struct type t = RegExp.t * string let compare (_, a) (_, b) = compare a b end) in
      let module RegS = Set.Make(struct type t = RegExp.t * string let compare (_, a) (_, b) = compare a b end) in
      let print_regs = ToolBox.print_set (fun fmt (_, s) -> Format.pp_print_string fmt s) RegS.elements in
      let print_regm = ToolBox.print_map (fun fmt (_, s) -> Format.pp_print_string fmt s) print_regs RegM.bindings in

      let add a b m =
        try
          let s = RegM.find a m in
          m |> RegM.add a (RegS.add b s)
        with
        | Not_found -> RegM.add a (RegS.singleton b) m
      in

      let links, links' =
        RegexpPartition.fold (fun (re, rn) (linker, linker') ->
            RegexpPartition.fold (fun (re', rn') (linker, linker') ->
                if not (RegExp.is_bottom (RegExp.meet re re')) then
                  add (re, rn) (re', rn') linker
                , add (re', rn') (re, rn) linker'
                else linker, linker'
              ) v.env (linker, linker')
          ) p1' (RegM.empty, RegM.empty)
      in

      let p2_remove = List.fold_left (fun to_remove n ->
                          match RegM.find_opt n links' with
                          | None -> n :: to_remove
                          | Some _ -> to_remove
                        ) [] (v.env)
      in

      let rec connected_component_one (re, rn) l1 l2 seen1 seen2 bool =
        let l = if bool then l1 else l2 in
        let neighbours = match RegM.find_opt (re, rn) l with | Some s -> s | None -> RegS.empty in
        RegS.fold (fun n (seen1, seen2) ->
            if bool then
              if not (RegS.mem n seen2) then
                connected_component_one n l1 l2 seen1 (RegS.add n seen2) false
              else seen1, seen2
            else
              if not (RegS.mem n seen1) then
                connected_component_one n l1 l2 (RegS.add n seen1) seen2 true
              else seen1, seen2
          ) neighbours (seen1, seen2)
      in
      let () = debug "links : %a" print_regm links  in
      let () = debug "links': %a" print_regm links' in
      let rec build_connected_component v1 v2 l1 l2 result =
        if RegS.is_empty v1 then
          result
        else
          let x = RegS.choose v1 in
          let seen1, seen2 = connected_component_one x l1 l2 (RegS.singleton x) RegS.empty true in
          let () = debug "seenres1: %a" print_regs seen1 in
          let () = debug "seenres2: %a" print_regs seen2 in
          build_connected_component (RegS.diff v1 seen1) (RegS.diff v2 seen2) l1 l2 ((seen1, seen2) :: result)
      in
      let concomp = build_connected_component (RegS.of_list p1') (RegS.of_list v.env) links links' [] in

      let env, u_num, v_num, u_vb, v_vb, res_vb, _ =
        List.fold_left (fun (env, u_num, v_num, u_vb, v_vb, res_vb, wid_pos) (u_set, v_set) ->
            let () = debug "u_set:%a" print_regs u_set in
            let () = debug "v_set:%a" print_regs v_set in
            let a = RegS.fold (fun (re, rn) acc -> RegExp.join re acc) u_set (RegExp.bottom auto_algebra) in
            let b = RegS.fold (fun (re, rn) acc -> RegExp.join re acc) v_set (RegExp.bottom auto_algebra) in
            let () = debug "a:%a" RegExp.print_u (RegExp.regexp_of_automata a) in
            let () = debug "b:%a" RegExp.print_u (RegExp.regexp_of_automata b) in
            let () = debug "wid_pos:%a" RegExp.print_u ((RegExp.regexp_of_automata wid_pos)) in
            let could_occupy = RegExp.widening 4 a (RegExp.join a b) |> RegExp.meet wid_pos in
            let () = debug "wid_res:%a" RegExp.print_u (RegExp.regexp_of_automata could_occupy) in

            let reminder = RegExp.diff wid_pos could_occupy in
            let p = a
                    |> RegExp.join (RegExp.meet b (RegExp.diff v.support u.support))
                    |> RegExp.join could_occupy
            in
            let () = debug "p:%a" RegExp.print_u (RegExp.regexp_of_automata p) in
            let pn = var_of_automata p in
            let u_num, u_vb, res_vb = Numerical.fold_two_vb (mk_fresh_range ()) man u_vb res_vb (RegS.elements u_set |> List.map snd) pn u_num in
            let v_num, v_vb, res_vb = Numerical.fold_two_vb (mk_fresh_range ()) man v_vb res_vb (RegS.elements v_set |> List.map snd) pn v_num in
            ((p, pn) :: env, u_num, v_num, u_vb, v_vb, res_vb, reminder)
          ) ([], u_num, v_num, u.varbind, v.varbind , StrVarBind.empty, wid_pos) concomp
      in

      let u_num, u_vb =
        List.fold_left (fun (u_num, u_vb) (re, rn) ->
            Numerical.forget (mk_fresh_range ()) man u_vb rn u_num
          ) (u_num, u_vb) p1_remove in

      let v_num, v_vb =
        List.fold_left (fun (v_num, v_vb) (re, rn) ->
            Numerical.forget (mk_fresh_range ()) man v_vb rn v_num
          ) (v_num, v_vb) p2_remove in

      let neqclass = join_eq_classes u v (Some(wid_pos)) in

      let should_wid_propagate =
        List.for_all (fun (re, rn) -> List.exists (fun (re', rn') -> RegExp.equ re re') u_starting_env) env
        && List.for_all (fun (re, rn) -> List.exists (fun (re', rn') -> RegExp.equ re re') env) u_starting_env
      in
      let prep =
        {
          shape = nshape;
          support = nsupport;
          classes = neqclass;
          env = env;
          varbind = res_vb;
          height = u.height;
          size = u.size
        } in
      let () = debug "widening result: %a" print prep in
      let () = debug "on environments: %a@,and:%a" (Flow.print man) u_num (Flow.print man) v_num in
      prep, should_wid_propagate, u_num, v_num

    (* let widen2 (man: ('b, 'b) man)
     *       (u: t) (u_num: 'b flow)
     *       (v: t) (v_num: 'b flow) =
     *
     *   let () = debug "widening input: %a@, on: %a" print u (Flow.print man) u_num in
     *   let () = debug "other widening input: %a@, on: %a" print v (Flow.print man) v_num in
     *
     *   let sa = TA.get_sigma_algebra u.shape in
     *   let auto_algebra = automata_algebra_on_n sa in
     *
     *   (\* Shape and support widening *\)
     *   let nshape = TA.widening_dfta_p u.shape v.shape 5 in
     *   let () = debug "shape calculated: %a" TA.print_dfta nshape in
     *   let nsupport = hole_position auto_algebra nshape in
     *   let () = debug "support calculated: %a" RegExp.print nsupport in
     *
     *   (\* Eq_class widening *\)
     *   let widening_position = RegExp.meet nsupport
     *                             (RegExp.compl (\*auto_algebra*\) (RegExp.join (\*auto_algebra*\) u.support v.support)) in
     *
     *   let r_widening u v reste =
     *     (RegExp.widening (\*auto_algebra*\) 6 u v |> RegExp.meet reste)
     *     |> RegExp.join (\*auto_algebra*\) (RegExp.join (\*auto_algebra*\) u v)
     *   in
     *
     *   let neqclass = join_eq_classes u v (Some(widening_position)) in
     *
     *   let () = debug "mark I%!" in
     *   (\* Env and numerical abstraction widening *\)
     *   let links, stable_extension =
     *     List.fold_left (fun (links, stable_extension) (ve, vn) ->
     *         let reminder, links =
     *           List.fold_left (fun (reminder, links) (ue, un) ->
     *               let ue_meet_ve = RegExp.meet ue reminder in
     *               if not (RegExp.is_bottom (\*auto_algebra*\) ue_meet_ve) then
     *                 let reminder = RegExp.diff (\*auto_algebra*\) ve ue_meet_ve in
     *                 (reminder, Linker.add (ue, un) (ve, vn) links)
     *               else
     *                 (reminder, links)
     *             ) (ve, links) u.env
     *         in
     *         if RegExp.is_bottom (\*auto_algebra*\) reminder then
     *           (links, ToolBox.StringS.add vn stable_extension)
     *         else
     *           (links, stable_extension)
     *       ) (Linker.empty, ToolBox.StringS.empty) v.env
     *   in
     *
     *   let () = debug "mark II%!" in
     *   let rec find_all_u2v todosu todosv l visitedu visitedv =
     *     match todosu with
     *     | u::q ->
     *        let vs = try Linker.M.find u l.Linker.u2v
     *                 with Not_found -> Linker.S.empty
     *        in
     *        let todosv, visitedv = Linker.S.fold (fun v (todosv, visitedv)->
     *                                   if Linker.S.mem v visitedv
     *                                   then (todosv, visitedv)
     *                                   else (v :: todosv, Linker.S.add v visitedv)
     *                                 ) vs (todosv, visitedv)
     *        in
     *        find_all_v2u q todosv l visitedu visitedv
     *     | [] ->
     *        (match todosv with
     *         | [] -> visitedu, visitedv
     *         | _ -> find_all_v2u todosu todosv l visitedu visitedv
     *        )
     *   and find_all_v2u todosu todosv l visitedu visitedv =
     *     match todosv with
     *     | v::q ->
     *        let us = try Linker.M.find v l.Linker.v2u
     *                 with Not_found -> Linker.S.empty
     *        in
     *        let todosu, visitedu = Linker.S.fold (fun u (todosu, visitedu) ->
     *                                   if Linker.S.mem u visitedu
     *                                   then (todosu, visitedu)
     *                                   else (u :: todosu, Linker.S.add u visitedu)
     *                                 ) us (todosu, visitedu)
     *        in
     *        find_all_u2v todosu q l visitedu visitedv
     *     | [] ->
     *        (match todosv with
     *         | [] -> visitedu, visitedv
     *         | _ -> find_all_u2v todosu todosv l visitedu visitedv
     *        )
     *   in
     *
     *   (\* let su = List.fold_left (fun acc x -> Linker.S.add x acc) Linker.S.empty u.env in
     *    * let sv = List.fold_left (fun acc x -> Linker.S.add x acc) Linker.S.empty v.env in *\)
     *
     *   let join_l l aa = match l with
     *     | (ue,_)::q ->
     *        let rep = List.fold_left (fun acc (ue, _) -> RegExp.join (\* aa *\) acc ue) ue q in
     *        let repn = var_of_automata rep in
     *        (rep, repn)
     *     | [] -> failwith "tried  to join an empty list"
     *   in
     *
     *   let () = debug "links: %a" Linker.print links in
     *   let rec merge_and_rename_both
     *             (absu: 'b flow) (absv: 'b flow)
     *             (envu: RegexpPartition.t) (envv: RegexpPartition.t)
     *             todosu nlinks
     *             (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
     *     =
     *     if Linker.S.is_empty todosu then (absu, absv, envu, envv, nlinks, vb_u, vb_v)
     *     else
     *       let () = debug "todosu: %a%!" (ToolBox.print_set_inline RegexpPartition.print_pair Linker.S.elements) todosu in
     *       let u = Linker.S.choose todosu in
     *       let cu, cv = find_all_u2v [u] [] links (Linker.S.singleton u) Linker.S.empty in
     *       let () = debug "cu: %a%!" (ToolBox.print_set_inline RegexpPartition.print_pair Linker.S.elements) cu in
     *       let to_merge_u = Linker.S.elements cu in
     *       let to_merge_v = Linker.S.elements cv in
     *       let (_, u_newn) as u_new = join_l to_merge_u auto_algebra in
     *       let (_, v_newn) as v_new = join_l to_merge_v auto_algebra in
     *
     *       let envu = RegexpPartition.merge u_new to_merge_u envu in
     *       let envv = RegexpPartition.merge v_new to_merge_v envv in
     *
     *       let () = debug "envu0: %a" RegexpPartition.print envu in
     *       let () = debug "envv0: %a" RegexpPartition.print envv in
     *
     *       let absu, vb_u = Numerical.fold (mk_fresh_range ()) man vb_u (List.map snd to_merge_u) u_newn absu in
     *       let absv, vb_v = Numerical.fold (mk_fresh_range ()) man vb_v (List.map snd to_merge_v) v_newn absv in
     *
     *       let todosu = Linker.S.diff todosu cu in
     *
     *       let nlinks =
     *         if Linker.S.exists (fun (_, vn) -> ToolBox.StringS.mem vn stable_extension) cv then
     *           (u_new, v_new, true) :: nlinks
     *         else
     *           (u_new, v_new, false) :: nlinks
     *       in
     *
     *       merge_and_rename_both absu absv envu envv todosu nlinks vb_u vb_v
     *   in
     *
     *   let absu, absv, envu, envv, nlinks, vb_u, vb_v = merge_and_rename_both
     *                                                      u_num
     *                                                      v_num
     *                                                      u.env
     *                                                      v.env
     *                                                      (links.Linker.u2v |> Linker.M.bindings |> List.map fst |> Linker.S.of_list)
     *                                                      []
     *                                                      u.varbind
     *                                                      v.varbind
     *   in
     *
     *   let comp_support_u = RegExp.compl (\*auto_algebra*\) u.support in
     *   let comp_support_v = RegExp.compl (\*auto_algebra*\) v.support in
     *
     *   let envu, envv, absu, absv, reste, vb_u, vb_v =
     *     List.fold_left (fun (envu, envv, absu, absv, reste, vb_u, vb_v) (ue, un) ->
     *         try
     *           let (_, (ve, vn), b) = List.find (fun ((_,un'),_,_) -> un = un') nlinks in
     *           let diffuv = RegExp.diff (\*auto_algebra*\) ue ve in
     *           let diffvu = RegExp.diff (\*auto_algebra*\) ve ue in
     *
     *           let newz, new_reste = if b then
     *                                   let rep = r_widening ue ve reste in
     *                                   rep, RegExp.diff (\*auto_algebra*\) reste rep
     *                                 else RegExp.join (\*auto_algebra*\) ue ve, reste in
     *           let newzn = var_of_automata newz in
     *
     *           if RegExp.leq (\*auto_algebra*\) diffuv comp_support_v
     *              && RegExp.leq (\*auto_algebra*\) diffvu comp_support_u then
     *             let absu, vb_u = Numerical.renaming_list (mk_fresh_range ()) man vb_u [un, newzn] absu in
     *             let absv, vb_v = Numerical.renaming_list (mk_fresh_range ()) man vb_v [vn, newzn] absv in
     *             (newz, newzn)::envu, (newz,newzn)::envv,
     *             absu , absv,
     *             new_reste, vb_u, vb_v
     *           else
     *             let absu, vb_u = Numerical.renaming_list (mk_fresh_range ()) man vb_u [un, newzn] absu in
     *             let absv, vb_v = Numerical.renaming_list (mk_fresh_range ()) man vb_v [vn, newzn] absv in
     *             let absu, vb_u = Numerical.forget (mk_fresh_range ()) man vb_u newzn absu in
     *             let absv, vb_v = Numerical.forget (mk_fresh_range ()) man vb_v newzn absv in
     *             (newz, newzn)::envu, (newz,newzn)::envv,
     *             absu, absv,
     *             new_reste,
     *             vb_u, vb_v
     *         with
     *         | Not_found ->
     *            if RegExp.leq (\*auto_algebra*\) ue comp_support_v then
     *              ((ue, un) :: envu, envv, absu, absv, reste, vb_u, vb_v)
     *            else
     *              let absu, vb_u = Numerical.forget (mk_fresh_range ()) man vb_u un absu in
     *              ((ue, un) :: envu, envv, absu, absv, reste, vb_u, vb_v)
     *
     *       ) ([], [], absu, absv, widening_position, vb_u, vb_v) envu
     *   in
     *
     *   let nenv = envu in
     *   let absu, absv, vb_u, vb_v = Numerical.widening_different_support
     *                                  man
     *                                  absu absv
     *                                  (envu |> List.map snd) (envv |> List.map snd)
     *                                  vb_u vb_v
     *   in
     *
     *   let rep =
     *     { u with
     *       shape = nshape;
     *       support = nsupport;
     *       classes = neqclass;
     *       env = nenv;
     *       varbind = vb_u
     *     } |> vb_sanitize
     *   in
     *   let () = debug "widening result: %a" print rep in
     *   let () = debug "widening numerical result: %a@,and: %a" (Flow.print man) absu (Flow.print man) absv in
     *   (rep, absu, absv) *)

    let top (man: ('a, 'b) man) range (sa: TA.sigma_algebra) (flow :'a flow) =
      let sa = holify_sigma_algebra sa in
      let re_algebra = automata_algebra_on_n sa in
      let h, s = fresh_var (), fresh_var () in
      let flow' = man.exec (mk_add_var h range) flow
      |> man.exec (mk_add_var h range) in
      {
        shape = TA.top_dfta sa;
        support = RegExp.top re_algebra;
        classes = [];
        env = [];
        varbind = StrVarBind.empty;
        height = fresh_var ();
        size = fresh_var ();
      }, flow'


    let bottom (man: ('a, 'b) man) range (sa: TA.sigma_algebra) (flow :'a flow) =
      let sa = holify_sigma_algebra sa in
      let re_algebra = automata_algebra_on_n sa in
      let h, s = fresh_var (), fresh_var () in
      let flow' = man.exec (mk_add_var h range) flow
                  |> man.exec (mk_add_var h range) in
      {
        shape = TA.bottom_dfta sa;
        support = RegExp.bottom re_algebra;
        classes = [];
        env = [];
        varbind = StrVarBind.empty;
        height = fresh_var ();
        size = fresh_var ();
      }, flow'


    let add_height_and_size man range h s num =
      num
      |> man.exec (mk_add_var h range)
      |> man.exec (mk_add_var s range)
      |> man.exec (mk_assume (mk_binop (mk_var h range) O_ge (Ast.mk_zero range) ~etyp:Ast.T_int range) range)
      |> man.exec (mk_assume (mk_binop (mk_var s range) O_ge (Ast.mk_zero range) ~etyp:Ast.T_int range) range)
    (** Transformers *)

    (* anything but an integer on head *)
    (* let helper1 (sa: TA.sigma_algebra) =
     *   let () = S.restart () in
     *   let one = S.fresh () in
     *   let zero = S.fresh () in
     *   let hole = S.fresh () in
     *   let sa' = holify_sigma_algebra sa in
     *   let re_algebra = automata_algebra_on_n sa' in
     *   {
     *     shape =
     *       {
     *         TA.dfta_q = TA.SS.of_list [one; zero; hole];
     *         TA.dfta_f = TA.SS.of_list [one];
     *         TA.dfta_d =
     *           TA.SA.fold (fun (sym, n) acc ->
     *               Enumerator.fold_id (fun l acc ->
     *                   TA.DetTrans.add_transition (sym, l, one) acc
     *                 ) [zero; one] n acc
     *             ) sa TA.DetTrans.empty
     *           |> TA.DetTrans.add_transition (hole_tree, [], zero);
     *         TA.dfta_h = hole;
     *         TA.dfta_hc = false;
     *         TA.dfta_a = sa
     *       };
     *     support = RegExp.(
     *       {
     *         states       = SS.of_list [0; 1; 2];
     *         algebra      = re_algebra;
     *         start        = 0;
     *         final        = SS.of_list [1];
     *         trans        = Algebra.fold (fun s trans ->
     *                            trans
     *                            |> DetTrans.add (s, 0) 1
     *                            |> DetTrans.add (s, 1) 1
     *                          ) re_algebra DetTrans.empty;
     *         hole         = 2;
     *         hole_coreach = false;
     *       }
     *               );
     *     classes = [];
     *     env = [];
     *     varbind = StrVarBind.empty
     *   }
     *
     * (\* an integer on head *\)
     * let helper2 (sa: TA.sigma_algebra) =
     *   let () = S.restart () in
     *   let one = S.fresh () in
     *   let hole = S.fresh () in
     *   let sa' = holify_sigma_algebra sa in
     *   let re_algebra = automata_algebra_on_n sa' in
     *   {
     *     shape =
     *       {
     *         TA.dfta_q  = TA.SS.of_list [one; hole];
     *         TA.dfta_f  = TA.SS.of_list [one];
     *         TA.dfta_d  = TA.DetTrans.add_transition (hole_tree, [], one) TA.DetTrans.empty;
     *         TA.dfta_h  = hole;
     *         TA.dfta_hc = false;
     *         TA.dfta_a  = sa
     *       };
     *     support = RegExp.(
     *       {
     *         states       = SS.of_list [0; 2];
     *         algebra      = re_algebra;
     *         start        = 0;
     *         final        = SS.of_list [0];
     *         trans        = DetTrans.empty;
     *         hole         = 2;
     *         hole_coreach = false;
     *       }
     *               );
     *     classes = [];
     *     env = [];
     *     varbind = StrVarBind.empty
     *   } *)

    let underlying_regular_algebra (u: t) =
      u.support.RegExp.algebra
    let underlying_tree_algebra (u: t) =
      u.shape.TA.dfta_a

    let clear_height_and_size man range (u: t) flow =
      man.exec (mk_remove_var u.size range) flow
      |> man.exec (mk_remove_var u.height range)


    let remove_all_nums range man (u: t) flow =
      let u = u |> vb_sanitize in
      StrVarBind.fold (fun (_, v) flow ->
          man.exec (mk_remove_var v range) flow
        ) u.varbind flow |>
        man.exec (mk_remove_var u.size range) |>
        man.exec (mk_remove_var u.height range)

    let sons range man (u: t) flow j =
      let head_shape = TA.head u.shape in

      let auto_algebra = u.support.RegExp.algebra in
      let abs, res, _ =
        List.fold_left (fun (abs, res0, vb_old) (k, v) ->
            let _, abs, res, _ =
              List.fold_left (fun (i, abs, res, vb_old) nshape ->
                  if i = j then
                    begin
                      let height = fresh_var () in
                      let size = fresh_var () in
                      (* let () = debug "mark1" in *)
                      let nsupport = hole_position auto_algebra nshape in
                      (* let () = debug "mark2" in *)
                      let nclasses = RegexpPartition.fold (fun (re, ne) acc ->
                                         let re' = RegExp.derivative re i in
                                         if RegExp.is_bottom re' then
                                           acc
                                         else
                                           let ne' = var_of_automata re' in
                                           (re', ne') :: acc
                                       ) u.classes []
                      in
                      (* let () = debug "mark3" in *)
                      let nenv, renaming = RegexpPartition.fold (fun (re, ne) (nenv, renaming) ->
                                               let () = debug "re: %a" (RegExp.print_u) (RegExp.regexp_of_automata re) in
                                               let re' = RegExp.derivative re i in
                                               let () = debug "re: %a" (RegExp.print_u) (RegExp.regexp_of_automata re') in
                                               if RegExp.is_bottom re' then
                                                 let () = debug "is bottom %a" (RegExp.print) (re') in
                                                 nenv, renaming
                                               else
                                                 let ne' = var_of_automata re' in
                                                 (re', ne') :: nenv, (ne, ne') :: renaming
                                             ) u.env ([], [])
                      in
                      (* let () = debug "mark4" in *)
                      let () = debug "computing son: %d" i in
                      let abs, vbold, vb = Numerical.renaming_list_diff_vb range man vb_old StrVarBind.empty renaming abs in
                      let abs = add_height_and_size man (tag_range range "adding h and s") height size abs in
                      let abs = man.exec (mk_assume (mk_binop (mk_var height range) O_lt (mk_var u.height range) range) range) abs in
                      let abs = man.exec (mk_assume (mk_binop (mk_var size range) O_lt (mk_var u.size range) range) range) abs in
                      (i+1, abs,{
                           shape = nshape;
                           support = nsupport;
                           classes = nclasses;
                           env = nenv;
                           varbind = vb;
                           height = height;
                           size = size;
                         }::res, vbold)
                    end
                  else
                    (i+1, abs, res, vb_old)
                ) (0, abs, [], vb_old) v
            in
            (abs, (k, List.rev res) :: res0, vb_old)
          ) (flow, [], u.varbind) head_shape
      in
      abs, res

    let read_i range man (u: t) (i: int) flow =
      let () = debug "asked to read_i in %a@,on%a@,at %d" print u (Flow.print man) flow i in
      let flow, l = sons range man u flow i in
      let () = debug "sons done" in
      List.fold_left (fun ((curjoin, flow) as acc) (_, ll) ->
          match List.nth_opt ll 0, curjoin with
          | None, _ -> acc
          | Some v, None -> (Some v, flow)
          | Some v, Some curjoin ->
             let u, flow = (join_same_num man v curjoin flow) in
             (Some u, flow)
        ) (None, flow) l
      |> function
        | None, flow ->
           bottom man range (u.shape.TA.dfta_a) flow
        | Some x, flow -> x, flow

    let filter_not_symbol range (man: ('a, 'b) man)
          (u: t)
          (abs: 'a flow) =
      let nshape = TA.final_constant hole_tree u.shape in
      let auto_algebra = automata_algebra_on_n (nshape.TA.dfta_a) in
      let nsupport = hole_position auto_algebra nshape in
      let nclasses = RegexpPartition.fold (fun (re, rn) acc ->
                         let re = RegExp.meet (re) (RegExp.from_word auto_algebra []) in
                         let rn = var_of_automata re in
                         if not (RegExp.is_bottom re) then
                           (re, rn) :: acc
                         else acc
                       ) u.classes []
      in
      let nenv, renaming, removal =
        RegexpPartition.fold (fun (re, rn) (nenv, renaming, removal) ->
            let re' = RegExp.meet (re) (RegExp.from_word auto_algebra []) in
            let rn' = var_of_automata re' in
            if not (RegExp.is_bottom re') then
              ((re', rn') :: nenv, (if rn <> rn' then (rn, rn') :: renaming else renaming), removal)
            else (nenv, renaming, rn :: removal)
          ) u.env ([], [], [])
      in
      let () = debug "removal list :%a" (ToolBox.print_list Format.pp_print_string) removal in
      let num, vb = Numerical.renaming_list range man u.varbind renaming abs in
      let num, vb = ToolBox.fold (fun s (abs, vb) -> Numerical.forget range man vb s abs) removal (num, vb) in
      (* let height = fresh_var () in
       * let size = fresh_var () in
       * let num = add_height_and_size man range height size num in *)
      let num = man.exec (mk_assume (mk_binop (mk_var u.size range) O_eq (Ast.mk_one range) range) range) num in
      let num = man.exec (mk_assume (mk_binop (mk_var u.height range) O_eq (Ast.mk_zero range) range) range) num in
      {shape = nshape;
       support = nsupport;
       classes = nclasses;
       env = nenv;
       varbind = vb;
       height = u.height;
       size = u.size;
      } |> vb_sanitize, num

    let get_number range man u abs =
      let auto_algebra = automata_algebra_on_n (u.shape.TA.dfta_a) in
      if RegExp.leq (RegExp.from_word auto_algebra []) (u.support) then
        let rep = fresh_var () in
        let abs = man.exec (mk_add_var rep range) abs in
        match List.find_opt (fun (re, rn) -> RegExp.leq (RegExp.from_word auto_algebra []) re) u.env with
        | Some (re, rn) ->
           let v' = StrVarBind.find_l rn u.varbind in

           let abs = man.exec (mk_assume (mk_binop (mk_var rep range) O_eq (mk_var v' range) range) range) abs in
           Some rep, (remove_all_nums range man u abs)
        | None ->
           Some rep, (remove_all_nums range man u abs)
      else
        None, (remove_all_nums range man u abs)

    let get_symbol range man u abs =
      TA.head_symbol u.shape |> List.filter (fun s -> not (A.compare s hole_tree = 0)), (remove_all_nums range man u abs)

    let filter_symbol range (man: ('a, 'b) man)
          (u: t)
          (abs: 'a flow) =
      let nshape = TA.not_final_constant hole_tree u.shape in
      let () = debug "not_final_constant %a" TA.print_dfta nshape in
      let auto_algebra = automata_algebra_on_n (nshape.TA.dfta_a) in
      let nsupport = hole_position auto_algebra nshape in
      let nclasses = RegexpPartition.fold (fun (re, rn) acc ->
                         let re = RegExp.diff (re) (RegExp.from_word auto_algebra []) in
                         let rn = var_of_automata re in
                         if not (RegExp.is_bottom re) then
                           (re, rn) :: acc
                         else acc
                       ) u.classes []
      in
      let nenv, renaming, removal =
        RegexpPartition.fold (fun (re, rn) (nenv, renaming, removal) ->
            let rt = (RegExp.from_word auto_algebra []) in
            let re' = RegExp.diff (re) rt in
            let () = debug "rt: %a, re:%a, re':%a" RegExp.print_u (RegExp.regexp_of_automata rt)
                       RegExp.print_u (RegExp.regexp_of_automata re)
                       RegExp.print_u (RegExp.regexp_of_automata re')
            in
            let rn' = var_of_automata re' in
            if not (RegExp.is_bottom re') then
              ((re', rn') :: nenv, (if rn <> rn' then (rn, rn') :: renaming else renaming), removal)
            else (nenv, renaming, rn :: removal)
          ) u.env ([], [], [])
      in
      let () = debug "removal list :%a" (ToolBox.print_list Format.pp_print_string) removal in
      let num, vb = Numerical.renaming_list range man u.varbind renaming abs in
      let num, vb = ToolBox.fold (fun s (abs, vb) -> Numerical.forget range man vb s abs) removal (num, vb) in

      (* let height = fresh_var () in
       * let size = fresh_var () in
       * let num = add_height_and_size man range height size num in *)
      let num = man.exec (mk_assume (mk_binop (mk_var u.size range) O_ge (Ast.mk_one range) range) range) num in
      let num = man.exec (mk_assume (mk_binop (mk_var u.height range) O_ge (Ast.mk_zero range) range) range) num in

      (* let num = clear_height_and_size man range u num in *)

      let rep, flow = {shape = nshape;
                       support = nsupport;
                       classes = nclasses;
                       env = nenv;
                       varbind = vb;
                       height = u.height;
                       size = u.size;
                      } |> vb_sanitize, num
      in
      rep, flow

    let sum range l =
      List.fold_left
        (fun acc x ->
          mk_binop acc Ast.O_plus (mk_var x range) ~etyp:Ast.T_int range
        ) (Ast.mk_one range) l

    let max range v l =
      match l with
      | [] -> [mk_binop (mk_var v range) O_eq (Ast.mk_zero range) range]
      | [u] -> [mk_binop (mk_var v range) O_eq (mk_binop (mk_var u range) Ast.O_plus (Ast.mk_one range) ~etyp:Ast.T_int range) range]
      | _ -> List.fold_left (fun acc x ->
                 (mk_binop (mk_var v range) O_ge (mk_binop (mk_var x range) Ast.O_plus (Ast.mk_one range) ~etyp:Ast.T_int range) range)::acc
               ) [] l

    let build_tree_from_symbol
          range (man: ('a, 'b) man)
          (s: TA.algebra) (ul: t list)
          (abs: 'a flow)
        : t * 'a flow=
      let nshape = TA.build_tree s (List.map (fun x -> x.shape) ul) in
      let auto_algebra = automata_algebra_on_n (nshape.TA.dfta_a) in
      let () = debug "auto_algebra: %a" RegExp.Algebra.print auto_algebra in
      let nsupport = hole_position auto_algebra nshape in
      let nenv, renamingl, _, vb = ToolBox.fold (fun u (nenv, renamingl, i, vb) ->
                                       let nenv', renaming, vb, _ =
                                         RegexpPartition.fold (fun (xe, xn) (nenv, renaming, vb, vbu) ->
                                             let () = debug "xe: %a" RegExp.print xe in
                                             let re = RegExp.integrate xe i |> fun x -> RegExp.change_algebra x auto_algebra in
                                             let () = debug "re: %a" RegExp.print re in
                                             let rn = var_of_automata re in
                                             let rnn, vb = StrVarBind.get_var rn vb in
                                             let xnn, vbu = StrVarBind.get_var xn vbu in
                                             ((re, rn):: nenv, (xnn, rnn) :: renaming, vb, vbu)
                                           ) u.env ([], [], vb, u.varbind)
                                       in
                                       (nenv' @ nenv, renaming :: renamingl, i+1, vb)
                                     ) ul ([], [], 0, StrVarBind.empty)
      in
      let nclasses, _ = ToolBox.fold (fun u (nclasses, i) ->
                            let nclasses' =
                              RegexpPartition.fold (fun (xe, xn) (nclasses) ->
                                  let re = RegExp.integrate xe i |> fun x -> RegExp.change_algebra x auto_algebra in
                                  let rn = var_of_automata re in
                                  ((re, rn):: nclasses)
                                ) u.classes []
                            in
                            (nclasses' @ nclasses, i+1)
                          ) ul ([], 0)
      in
      let abs = List.fold_left (fun abs renamer ->
                    Numerical.renaming_list_var range man renamer abs
                  ) abs renamingl
      in

      let height = fresh_var () in
      let size = fresh_var () in
      let abs = add_height_and_size man range height size abs in
      let s_eq = sum range (List.map (fun x -> x.size) ul) in
      let h_cons = max range height (List.map (fun x -> x.height) ul) in
      let abs = man.exec (mk_assume (mk_binop (mk_var size range) O_eq s_eq range) range) abs in
      let abs = List.fold_left (fun acc x -> man.exec (mk_assume x range) acc) abs h_cons in
      let abs = List.fold_left (fun acc u -> clear_height_and_size man range u acc) abs ul in
      {
        shape = nshape;
        support = nsupport;
        env = nenv;
        classes = nclasses;
        varbind = vb;
        height = height;
        size = size;
      }, abs

    let build_tree_from_expr range (man: ('a, 'b) man) (e: expr) (num: 'a flow):
          t * 'a flow =
      let one = S.fresh () in
      let hole = S.fresh () in
      let epsilon = RegExp.from_word (RegExp.Algebra.of_list []) [] in
      let epsilon_name = var_of_automata epsilon in
      let v, vb = StrVarBind.get_var epsilon_name StrVarBind.empty in

      let height = fresh_var () in
      let size = fresh_var () in
      let num = add_height_and_size man range height size num in
      let num = man.exec (mk_assume (mk_binop (mk_var size range) O_eq (Ast.mk_one range) range) range) num in
      let num = man.exec (mk_assume (mk_binop (mk_var height range) O_eq (Ast.mk_zero range) range) range) num in
      {
        shape =
          {
            TA.dfta_q  = TA.SS.of_list [one; hole];
            TA.dfta_f  = TA.SS.of_list [one];
            TA.dfta_d  = TA.DetTrans.add_transition (hole_tree, [], one) TA.DetTrans.empty;
            TA.dfta_h  = hole;
            TA.dfta_hc = false;
            TA.dfta_a  = SA.of_list [hole_tree, 0]
          };
        support = epsilon;
        classes = [];
        env = [(epsilon, epsilon_name)];
        varbind = vb;
        height = height;
        size = size;
      },
      man.exec (mk_add_var v (tag_range range "add_var")) num
      |> man.exec (mk_assign (mk_var v ~mode:STRONG (tag_range range "var")) e (tag_range range "sub_var assign"))

    let copy range (man: ('a, 'b) man) (u: t) (abs: 'a flow):
          t * t * 'a flow =
      let vb1, vb2, abs =
        StrVarBind.fold (fun (s, v) (vb1, vb2, abs) ->
            let v1, vb1 = StrVarBind.get_var s vb1 in
            let v2, vb2 = StrVarBind.get_var s vb2 in
            (vb1, vb2, Numerical.extend_var range man v [v1; v2] abs)
          ) u.varbind (StrVarBind.empty, StrVarBind.empty, abs)
      in
      let h1 = fresh_var () in
      let h2 = fresh_var () in
      let s1 = fresh_var () in
      let s2 = fresh_var () in
      let abs = Numerical.extend_var range man u.height [h1; h2] abs in
      let abs = Numerical.extend_var range man u.size [s1; s2] abs in
      {u with varbind = vb1; height = h1; size = s1}, {u with varbind = vb2; height = h2; size = s2}, abs

  end

module VString = Make(Tools.State)(Tools.StrSigmaAlgebra)
