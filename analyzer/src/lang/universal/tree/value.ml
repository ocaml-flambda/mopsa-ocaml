open Framework.Essentials
open Numerical

module Uid =
struct
  let i = ref 0
  let fresh () =
    let j = !i in
    incr i;
    j
end

let debug fmt = ToolBox.debug "dom" fmt

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
      varbind : StrVarBind.t
    }

  let holify_sigma_algebra (sa: TA.sigma_algebra) =
    if SA.mem_symbol hole_tree sa then
      sa
    else SA.add (hole_tree, 0) sa

  type 'a tree =
    | Var of string
    | Node of 'a * ('a tree) list

  type input_tree = TA.algebra tree

  let rec print_tree printer fmt t = match t with
    | Var s ->
      Format.fprintf fmt "%s" s
    | Node(s,l) -> if List.length l > 0 then
        Format.fprintf fmt "@[<v 1>%a(@,%a@,@])" printer s
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") (print_tree printer)) l
      else Format.fprintf fmt "%a" printer s

  let print_input_tree = print_tree A.print

  let print fmt (u: t) =
    Format.fprintf fmt "@[<v>⯈ shape: @[<v 2>@,%a@]@,⯈ support: @[<v \
                        2>@,%a@]@,⯈ classes: @[<v 2>@,%a@]@,⯈ env: \
                        @[<v 2>@,%a@]@,⯈ varbind: @[<v 2>@,%a@]@,@]"
      TA.print_dfta u.shape
      RegExp.pp_print_u (RegExp.regexp_of_automata u.support)
      RegexpPartition.print_left u.classes
      RegexpPartition.print u.env
      StrVarBind.print u.varbind
      (* Numerical.print u.numeric *)

  let automata_algebra_on_n (sa: TA.sigma_algebra) =
    let n = SA.max_arity sa in
    ToolBox.fold_int (fun i x -> i::x) n [] |>
    RegExp.Algebra.of_list


  let print_injection =
    ToolBox.print_list_inline (ToolBox.print_pair Format.pp_print_string Format.pp_print_string)

  let from_tree (st: input_tree) =
    let open TA in
    let merge_map_list (ml: (string * SS.t) list) (ml': (string * SS.t) list): (string * SS.t) list =
      let dom = List.map fst ml @ List.map fst ml' in
      List.fold_left (fun acc x ->
          try
            let ss = List.assoc x ml in
            try
              let ss' = List.assoc x ml' in
              (x, SS.union ss ss') :: acc
            with
            | Not_found -> (x, ss) :: acc
          with
          | Not_found ->
            try
              let ss' = List.assoc x ml' in
              (x, ss') :: acc
            with
            | Not_found -> acc
        ) [] dom
    in

    let rec aux (st: input_tree): SS.t * S.t * ST.t * (string * SS.t) list = match st with
      | Var s ->
        let ns = S.fresh () in
        (SS.singleton ns, ns, ST.singleton (hole_tree, [], ns), [s, SS.singleton ns])
      | Node(s,l) ->
        let ll = List.map aux l in
        let ns = S.fresh () in
        let nt = (s,List.map (fun (_, x, _, _) -> x) ll,ns) in
        let allstate, trans, bd = List.fold_left (
            fun (at, ay, abd) (t, _, y, bd) -> (
                SS.union at t,
                ST.union ay y,
                merge_map_list abd bd
              )
          ) (SS.singleton ns, ST.singleton nt, []) ll in
        (allstate, ns, trans, bd)
    in
    let hs = S.fresh () in
    let all_state, final_state, trans, bd = aux st in
    let r_shape =
      {
        q = SS.add hs all_state;
        d = trans;
        f = SS.singleton final_state
      }
    in
    let sa = TA.get_sigma_algebra_from_t r_shape in
    let r_shape_dfta =
      r_shape
      |> TA.determinization_algebra_to_dfta sa
      |> fun x -> minimization_dfta_p x None
    in
    let ra = automata_algebra_on_n sa in

    let state_pos = TA.state_position r_shape in
    let var_path (v: string) (s: SS.t) =
      SS.fold (fun q acc ->
          let e = TA.MS.find q state_pos in
          RegExp.A(acc, e)
        ) s RegExp.N
    in
    let vari_numvar_eq, r_classes =
      List.fold_left (fun ((acc_vari_numvar_eq, acc_r_classes) as acc) (s, ss) ->
          if List.exists (fun (x, _) -> s = x) acc_vari_numvar_eq then
            acc
          else
            begin
              let re = var_path s ss in
              let ns = var_of_regexp re in
              ((s, ns) :: acc_vari_numvar_eq,
               (re, ns) :: acc_r_classes
              )
            end
        ) ([], []) bd
    in
    let r_support = TA.find_position hole_tree r_shape state_pos in
    let r_env_automata = List.map (fun (x, a) -> RegExp.automata_of_regexp ra x, a) r_classes in
    let r_strvarbind = ToolBox.fold (fun (re, rn) strvarbind ->
        StrVarBind.get_var rn strvarbind |> snd
      ) r_env_automata StrVarBind.empty
    in
    let r_classes_automata = List.filter (fun (x, a) -> not (RegExp.is_cardinality_one x)) r_env_automata in
    {
      shape = r_shape_dfta;
      support = r_support |> RegExp.automata_of_regexp ra ;
      classes = r_classes_automata;
      env = r_env_automata;
      varbind = r_strvarbind
      (* numeric = Numerical.top (Environmentext.empty) *)
    }, vari_numvar_eq

  let hole_position (reg_algebra) (u: TA.dfta) =
    let state_pos = TA.state_position_dfta u in
    let nsupport_u = TA.find_position_dfta hole_tree u state_pos in
    RegExp.automata_of_regexp reg_algebra nsupport_u

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

  let unify man (u: t) u_num (v: t) v_num =
    let sa = TA.get_sigma_algebra u.shape in
    let auto_algebra = automata_algebra_on_n sa in
    let splitu, splitv = List.fold_left (fun (splitu, splitv) (ue, un) ->
        let toto = RegExp.meet ue v.support in
        if RegExp.is_bottom (* auto_algebra *) toto then (* [ue] n'est pas dans le support de v*)
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
            (* let () = debug "u : %s --- partition : %a" un RegexpPartition.print partition in
             * if  *)
            (ToolBox.StringM.add un partition splitu, merge_splitter splitv splitv')
          end
      ) (ToolBox.StringM.empty, ToolBox.StringM.empty) u.env in
    let if_non_empty u un =
      if RegExp.is_bottom (* auto_algebra *) u then
        [], []
      else
        [(u, un)], [un]
    in
    let apply_splitter env splitter other_support =
      List.fold_left (fun (nenv_commun, nenv_n_commun, var_extension) (ue, un) ->
          try
            let x = ToolBox.StringM.find un splitter in
            (* let () = debug "u : %s --- splitu : %a" un RegexpPartition.print x in *)
            let joined, vars =
              List.fold_left (fun (joined, vars) (sue, sun) ->
                  (RegExp.join (* auto_algebra *) joined sue, sun:: vars)
                ) (RegExp.bottom auto_algebra, []) x
            in
            let nue = RegExp.meet (RegExp.compl (* auto_algebra *) joined) ue in
            let nue_supp = RegExp.meet nue other_support in
            let nue_supp_n = var_of_automata nue_supp in
            let nue_non_supp = RegExp.meet nue (RegExp.compl (* auto_algebra *) other_support) in
            let nue_non_supp_n = var_of_automata nue_non_supp in
            let ns, ns' = if_non_empty nue_supp nue_supp_n in
            let nns, nns' = if_non_empty nue_non_supp nue_non_supp_n in
            let ll = vars @ ns' @ nns' in
            if List.length ll = 1 then
              (ns @ x @ nenv_commun,
               nns @ nenv_n_commun,
               var_extension)
            else
              (
                ns @ x @ nenv_commun,
                nns @ nenv_n_commun,
                ToolBox.StringM.add un (ns' @ nns' @ vars) var_extension)
          with
          | Not_found ->
            (nenv_commun, (ue, un) :: nenv_n_commun, var_extension)
        ) ([], [], ToolBox.StringM.empty) env
    in
    let n_common_env, n_u_env, u_var_extension = apply_splitter u.env splitu v.support in
    let _           , n_v_env, v_var_extension = apply_splitter v.env splitv u.support in
    let n_full_env = ToolBox.fold (fun ((_, rn) as n) acc ->
        if not (List.exists (fun (_, rn') -> rn = rn') n_v_env) then
          (n :: acc)
        else
          acc
      ) n_u_env n_v_env
    in
    let apply_var_extension num ve vb =
      ToolBox.StringM.fold (fun un l (num, vb) ->
          Numerical.extend (mk_fresh_range ()) man vb un l num
        ) ve (num, vb)
    in
    let n_u_num, n_u_vb = apply_var_extension u_num u_var_extension u.varbind in
    let n_v_num, n_v_vb = apply_var_extension v_num v_var_extension v.varbind in

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


    (* let rec aux build_partition_u2v (un, ue) absu envu absv envv = *)


    (* let u_part, to_extend, alone =
     *   List.fold_left (fun (u_part, to_extend, alone) (ve, vn) ->
     *       let reminder, met, u_untouched =
     *         List.fold_left (fun (reminder, met, u_env) (ue, un) ->
     *             let ue_meet_ve = RegExp.meet ue reminder in
     *             if not (RegExp.is_bottom auto_algebra ue_meet_ve) then
     *               let reminder = RegExp.diff auto_algebra ve ue_meet_ve in
     *               (reminder, (ue, un, ue_meet_ve) :: met, u_env)
     *             else
     *               (reminder, met, (ue, un) :: u_env)
     *           ) (ve, [], []) u.env
     *       in
     *       match met with
     *       | [] ->
     *         u_part,
     *         to_extend,
     *         (ve, vn) :: alone
     *       | (pe, pn, _)::q  ->
     *         let part' = List.fold_left (fun u_part (qe, qn, _) ->
     *             BPart.mk_equiv (pe, pn) (qe, qn) u_part) u_part q
     *         in
     *         if RegExp.is_bottom auto_algebra reminder then
     *           (part', (pe, pn, reminder) :: to_extend, alone)
     *         else
     *           (part', to_extend, alone)
     *     ) (BPart.mk_part u.env, [], []) v.env
     * in
     *
     * let (abs, env, renaming) = List.fold_left (fun (abs, env, renaming) l ->
     *     match l with
     *     | (pu, pn)::q::r ->
     *       let joined =
     *         List.fold_left (fun joined (ue, un) ->
     *             RegExp.join auto_algebra joined ue
     *           ) pu (q::r)
     *       in
     *       let joinedn = var_of_automata joined in
     *       let renaming = List.fold_left (fun renaming (pu, pn) -> ToolBox.StringM.add pn joinedn renaming) renaming l in
     *       (Numerical.merge abs joinedn (List.map snd l), (joined, joinedn) :: env, renaming)
     *     | ((pu, pn) as p):: [] ->
     *       let renaming = ToolBox.StringM.add pn pn renaming in
     *       (abs, p::env, renaming)
     *
     *     | _ -> (abs, env, renaming)
     *   ) (u.numeric, [], ToolBox.StringM.empty) u_part
     * in
     *
     * let widening_position = RegExp.compl auto_algebra v.support in
     *
     * let to_extend = List.fold_left (fun acc (ue, un, ext) ->
     *     let rpzn = ToolBox.StringM.find un renaming in
     *     try
     *       let cur_ext = ToolBox.StringM.find rpzn acc in
     *       acc
     *       |> ToolBox.StringM.remove rpzn
     *       |> ToolBox.StringM.add rpzn (RegExp.join auto_algebra cur_ext ext)
     *     with
     *     | Not_found -> ToolBox.StringM.add rpzn ext acc
     *   ) ToolBox.StringM.empty to_extend
     * in
     *
     *
     * let (common, abs, env, _) = ToolBox.StringM.fold (fun un ext (common, absu, absv, env, room_left) ->
     *     let (ue, un) = RegexpPartition.find_by_name un env in
     *     let w = RegExp.widening auto_algebra ue ext 5 |> RegExp.meet room_left in
     *     let wn = var_of_automata w in
     *     let room_left = RegExp.diff auto_algebra room_left w in
     *     (\* Numerical.renaming_list *\)
     *   ) to_extend ([], abs, env, widening_position)
     * in
     * List.fold_left (fun (u_env, available, abs) (ue, un, r) ->
     *     let w = RegExp.widening auto_algebra ue r 6 in
     *     let wr = RegExp.meet w available in
     *     let wrn = var_of_automata wr in
     *     let abs = Numerical.renaming_list [un,wrn] abs in
     *     (RegexpPartition.replace (wr, wrn) u_env, RegExp.diff auto_algebra available wr, abs)
     *   ) (u_env, widening_position) to_extend *)

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
      (man: ('b, 'b) man)
      (u: t)
      (u_num: 'b flow)
    : (t * 'b flow)
    =
    let rec cross l to_merge vb = match l with
      | (ue, un) :: q ->
        let eq, neq = find (ue, un) q [] [] u.varbind in
        if eq <> [] then cross neq (((ue, un)::eq) :: to_merge) vb
        else cross neq to_merge vb
      | [] -> (to_merge, vb)
    and find (ue, un) q eq neq vb = match q with
      | (ve, vn) :: r ->
        let b, vb = Numerical.eq (mk_fresh_range ()) man un vn u.varbind u_num in
        if b then
          find (ue, un) r ((ve, vn) :: eq) neq vb
        else
          find (ue, un) r eq ((ve, vn) :: neq) vb
      | [] -> eq, neq
    in
    let new_eq_classes, vb = cross u.classes [] u.varbind in
    let u = {u with varbind = vb} in
    let u', u_num =
      List.fold_left (fun acc x ->
          merge_classes acc x
        ) u new_eq_classes
      |> fun u -> List.fold_left (fun (u, u_num) x ->
          merge_env man u u_num x
        ) (u, u_num) new_eq_classes
    in
    let foldable_env = Numerical.find_foldable_variables u_num in
    let foldable_env_with_regexp = List.map (fun cl ->
        List.map (fun x ->
            List.find (fun (ue, un) -> x = un) u'.env
          ) cl
      ) foldable_env
    in
    let u, u_num =
      ToolBox.fold (fun cl (u, u_num) ->
          merge_env man u u_num cl
        ) foldable_env_with_regexp (u, u_num)
    in
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

  let meet (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
    : (t * 'b flow * 'b flow) =
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
        varbind = vb_u
      }
    in
    (* TODO: on aimerait normaliser prep après, mais on n'a pas le
       résultat du meet*)
    (prep, u_num, v_num)

  let join (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
    : (t * 'b flow * 'b flow) =
    let nshape = TA.join_dfta u.shape v.shape in
    let nsupport = RegExp.join (*auto_algebra*) u.support v.support in
    let nclasses = join_eq_classes u v None in
    let (n_u_env, n_u_num, n_u_vb, n_v_env, n_v_num, n_v_vb, _ , n_full_env) =
      unify man u u_num v v_num
    in
    let u_num, v_num, vb_u, vb_v = Numerical.join_different_support
        man
        u_num v_num
        (n_u_env |> List.map snd) (n_v_env |> List.map snd)
        (n_full_env |> List.map snd)
        u.varbind v.varbind
    in
    let prep = {
      shape = nshape;
      support = nsupport;
      classes = nclasses;
      env = n_full_env;
      varbind = vb_u
    } in
    (* We would like to normalize *)
    (prep, u_num, v_num)

  let subset (man: ('b, 'b) man) (u: t) (u_num: 'b flow) (v: t) (v_num: 'b flow)
    : (bool * 'b flow * 'b flow) =
    let u, unum = normalize man u u_num in
    let v, vnum = normalize man v v_num in
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
      let (n_u_env, n_u_num, n_u_vb, n_v_env, n_v_num, n_v_vb, n_common_env, _) =
        (* let unify annot man (u: t) u_num (v: t) v_num = *)
        unify man u u_num v v_num
      in
      let (n_u_num, n_v_num, _, _) =
        Numerical.env_leq man n_u_num n_v_num
          (n_v_env |> List.map snd) (n_common_env |> List.map snd)
          n_u_vb n_v_vb
      in
      (true, n_u_num, n_v_num)
    else
      (false, u_num, v_num)

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

  let widen (man: ('b, 'b) man)
      (u: t) (u_num: 'b flow)
      (v: t) (v_num: 'b flow) =

    let sa = TA.get_sigma_algebra u.shape in
    let auto_algebra = automata_algebra_on_n sa in

    (* Shape and support widening *)
    let nshape = TA.widening_dfta_p u.shape v.shape 5 in
    let nsupport = hole_position auto_algebra nshape in
    (* Eq_class widening *)
    let widening_position = RegExp.meet nsupport
        (RegExp.compl (*auto_algebra*) (RegExp.join (*auto_algebra*) u.support v.support)) in

    let r_widening u v reste =
      (RegExp.widening (*auto_algebra*) 6 u v |> RegExp.meet reste)
      |> RegExp.join (*auto_algebra*) (RegExp.join (*auto_algebra*) u v)
    in

    let neqclass = join_eq_classes u v (Some(widening_position)) in

    let () = debug "mark I%!" in
    (* Env and numerical abstraction widening *)
    let links, stable_extension =
      List.fold_left (fun (links, stable_extension) (ve, vn) ->
          let reminder, links =
            List.fold_left (fun (reminder, links) (ue, un) ->
                let ue_meet_ve = RegExp.meet ue reminder in
                if not (RegExp.is_bottom (*auto_algebra*) ue_meet_ve) then
                  let reminder = RegExp.diff (*auto_algebra*) ve ue_meet_ve in
                  (reminder, Linker.add (ue, un) (ve, vn) links)
                else
                  (reminder, links)
              ) (ve, links) u.env
          in
          if RegExp.is_bottom (*auto_algebra*) reminder then
            (links, ToolBox.StringS.add vn stable_extension)
          else
            (links, stable_extension)
        ) (Linker.empty, ToolBox.StringS.empty) v.env
    in

    let () = debug "mark II%!" in
    let rec find_all_u2v todosu todosv l visitedu visitedv =
      match todosu with
      | u::q ->
        let vs = try Linker.M.find u l.Linker.u2v
          with Not_found -> Linker.S.empty
        in
        let todosv, visitedv = Linker.S.fold (fun v (todosv, visitedv)->
            if Linker.S.mem v visitedv
            then (todosv, visitedv)
            else (v :: todosv, Linker.S.add v visitedv)
          ) vs (todosv, visitedv)
        in
        find_all_v2u q todosv l visitedu visitedv
      | [] ->
        (match todosv with
         | [] -> visitedu, visitedv
         | _ -> find_all_v2u todosu todosv l visitedu visitedv
        )
    and find_all_v2u todosu todosv l visitedu visitedv =
      match todosv with
      | v::q ->
        let us = try Linker.M.find v l.Linker.v2u
          with Not_found -> Linker.S.empty
        in
        let todosu, visitedu = Linker.S.fold (fun u (todosu, visitedu) ->
            if Linker.S.mem u visitedu
            then (todosu, visitedu)
            else (u :: todosu, Linker.S.add u visitedu)
          ) us (todosu, visitedu)
        in
        find_all_u2v todosu q l visitedu visitedv
      | [] ->
        (match todosv with
         | [] -> visitedu, visitedv
         | _ -> find_all_u2v todosu todosv l visitedu visitedv
        )
    in

    (* let su = List.fold_left (fun acc x -> Linker.S.add x acc) Linker.S.empty u.env in
     * let sv = List.fold_left (fun acc x -> Linker.S.add x acc) Linker.S.empty v.env in *)

    let join_l l aa = match l with
      | (ue,_)::q ->
        let rep = List.fold_left (fun acc (ue, _) -> RegExp.join (* aa *) acc ue) ue q in
        let repn = var_of_automata rep in
        (rep, repn)
      | [] -> failwith "tried  to join an empty list"
    in

    let () = debug "links: %a" Linker.print links in
    let rec merge_and_rename_both
        (absu: 'b flow) (absv: 'b flow)
        (envu: RegexpPartition.t) (envv: RegexpPartition.t)
        todosu nlinks
        (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
      =
      if Linker.S.is_empty todosu then (absu, absv, envu, envv, nlinks, vb_u, vb_v)
      else
        let () = debug "todosu: %a%!" (ToolBox.print_set_inline RegexpPartition.print_pair Linker.S.elements) todosu in
        let u = Linker.S.choose todosu in
        let cu, cv = find_all_u2v [u] [] links (Linker.S.singleton u) Linker.S.empty in
        let () = debug "cu: %a%!" (ToolBox.print_set_inline RegexpPartition.print_pair Linker.S.elements) cu in
        let to_merge_u = Linker.S.elements cu in
        let to_merge_v = Linker.S.elements cv in
        let (_, u_newn) as u_new = join_l to_merge_u auto_algebra in
        let (_, v_newn) as v_new = join_l to_merge_v auto_algebra in

        let envu = RegexpPartition.merge u_new to_merge_u envu in
        let envv = RegexpPartition.merge v_new to_merge_v envv in

        let () = debug "envu0: %a" RegexpPartition.print envu in
        let () = debug "envv0: %a" RegexpPartition.print envv in

        let absu, vb_u = Numerical.fold (mk_fresh_range ()) man vb_u (List.map snd to_merge_u) u_newn absu in
        let absv, vb_v = Numerical.fold (mk_fresh_range ()) man vb_v (List.map snd to_merge_v) v_newn absv in

        let todosu = Linker.S.diff todosu cu in

        let nlinks =
          if Linker.S.exists (fun (_, vn) -> ToolBox.StringS.mem vn stable_extension) cv then
            (u_new, v_new, true) :: nlinks
          else
            (u_new, v_new, false) :: nlinks
        in

        merge_and_rename_both absu absv envu envv todosu nlinks vb_u vb_v
    in

    let absu, absv, envu, envv, nlinks, vb_u, vb_v = merge_and_rename_both
        u_num
        v_num
        u.env
        v.env
        (links.Linker.u2v |> Linker.M.bindings |> List.map fst |> Linker.S.of_list)
        []
        u.varbind
        v.varbind
    in

    let comp_support_u = RegExp.compl (*auto_algebra*) u.support in
    let comp_support_v = RegExp.compl (*auto_algebra*) v.support in

    let envu, envv, absu, absv, reste, vb_u, vb_v =
      List.fold_left (fun (envu, envv, absu, absv, reste, vb_u, vb_v) (ue, un) ->
          try
            let (_, (ve, vn), b) = List.find (fun ((_,un'),_,_) -> un = un') nlinks in
            let diffuv = RegExp.diff (*auto_algebra*) ue ve in
            let diffvu = RegExp.diff (*auto_algebra*) ve ue in

            let newz, new_reste = if b then
                let rep = r_widening ue ve reste in
                rep, RegExp.diff (*auto_algebra*) reste rep
              else RegExp.join (*auto_algebra*) ue ve, reste in
            let newzn = var_of_automata newz in

            if RegExp.leq (*auto_algebra*) diffuv comp_support_v
            && RegExp.leq (*auto_algebra*) diffvu comp_support_u then
              let absu, vb_u = Numerical.renaming_list (mk_fresh_range ()) man vb_u [un, newzn] absu in
              let absv, vb_v = Numerical.renaming_list (mk_fresh_range ()) man vb_v [vn, newzn] absv in
              (newz, newzn)::envu, (newz,newzn)::envv,
              absu , absv,
              new_reste, vb_u, vb_v
            else
              let absu, vb_u = Numerical.renaming_list (mk_fresh_range ()) man vb_u [un, newzn] absu in
              let absv, vb_v = Numerical.renaming_list (mk_fresh_range ()) man vb_v [vn, newzn] absv in
              let absu, vb_u = Numerical.forget (mk_fresh_range ()) man vb_u newzn absu in
              let absv, vb_v = Numerical.forget (mk_fresh_range ()) man vb_v newzn absv in
              (newz, newzn)::envu, (newz,newzn)::envv,
              absu, absv,
              new_reste,
              vb_u, vb_v
          with
          | Not_found ->
            if RegExp.leq (*auto_algebra*) ue comp_support_v then
              ((ue, un) :: envu, envv, absu, absv, reste, vb_u, vb_v)
            else
              let absu, vb_u = Numerical.forget (mk_fresh_range ()) man vb_u un absu in
              ((ue, un) :: envu, envv, absu, absv, reste, vb_u, vb_v)

        ) ([], [], absu, absv, widening_position, vb_u, vb_v) envu
    in

    let nenv = envu in
    let absu, absv, vb_u, vb_v = Numerical.widening_different_support
        man
        absu absv
        (envu |> List.map snd) (envv |> List.map snd)
        vb_u vb_v
    in
    let rep =
      {
        shape = nshape;
        support = nsupport;
        classes = neqclass;
        env = nenv;
        varbind = vb_u
      } |> vb_sanitize
    in
    (rep, absu, absv)

  let top (sa: TA.sigma_algebra) =
    let sa = holify_sigma_algebra sa in
    let re_algebra = automata_algebra_on_n sa in
    {
      shape = TA.top_dfta sa;
      support = RegExp.top re_algebra;
      classes = [];
      env = [];
      varbind = StrVarBind.empty;
    }

  let bottom (sa: TA.sigma_algebra)=
    let sa = holify_sigma_algebra sa in
    let re_algebra = automata_algebra_on_n sa in
    {
      shape = TA.bottom_dfta sa;
      support = RegExp.bottom re_algebra;
      classes = [];
      env = [];
      varbind = StrVarBind.empty;
    }

  (** Transformers *)

  (* anything but an integer on head *)
  let helper1 (sa: TA.sigma_algebra) =
    let () = S.restart () in
    let one = S.fresh () in
    let zero = S.fresh () in
    let hole = S.fresh () in
    let sa' = holify_sigma_algebra sa in
    let re_algebra = automata_algebra_on_n sa' in
    {
      shape =
        {
          TA.dfta_q = TA.SS.of_list [one; zero; hole];
          TA.dfta_f = TA.SS.of_list [one];
          TA.dfta_d =
            TA.SA.fold (fun (sym, n) acc ->
                Enumerator.fold_id (fun l acc ->
                    TA.DetTrans.add_transition (sym, l, one) acc
                  ) [zero; one] n acc
              ) sa TA.DetTrans.empty
            |> TA.DetTrans.add_transition (hole_tree, [], zero);
          TA.dfta_h = hole;
          TA.dfta_hc = false;
          TA.dfta_a = sa
        };
      support = RegExp.(
          {
            states       = SS.of_list [0; 1; 2];
            algebra      = re_algebra;
            start        = 0;
            final        = SS.of_list [1];
            trans        = Algebra.fold (fun s trans ->
                trans
                |> DetTrans.add (s, 0) 1
                |> DetTrans.add (s, 1) 1
              ) re_algebra DetTrans.empty;
            hole         = 2;
            hole_coreach = false;
          }
        );
      classes = [];
      env = [];
      varbind = StrVarBind.empty
    }

  (* an integer on head *)
  let helper2 (sa: TA.sigma_algebra) =
    let () = S.restart () in
    let one = S.fresh () in
    let hole = S.fresh () in
    let sa' = holify_sigma_algebra sa in
    let re_algebra = automata_algebra_on_n sa' in
    {
      shape =
        {
          TA.dfta_q  = TA.SS.of_list [one; hole];
          TA.dfta_f  = TA.SS.of_list [one];
          TA.dfta_d  = TA.DetTrans.add_transition (hole_tree, [], one) TA.DetTrans.empty;
          TA.dfta_h  = hole;
          TA.dfta_hc = false;
          TA.dfta_a  = sa
        };
      support = RegExp.(
          {
            states       = SS.of_list [0; 2];
            algebra      = re_algebra;
            start        = 0;
            final        = SS.of_list [0];
            trans        = DetTrans.empty;
            hole         = 2;
            hole_coreach = false;
          }
        );
      classes = [];
      env = [];
      varbind = StrVarBind.empty
    }


  (* let filter_symbol (u: t) =
   *   let sa = TA.get_sigma_algebra u.shape in
   *   (meet (helper1 sa) u, meet (helper2 sa)) *)


  let underlying_regular_algebra (u: t) =
    u.support.RegExp.algebra
  let underlying_tree_algebra (u: t) =
    u.shape.TA.dfta_a

  (* let sons (u: t) =
   *   let head_shape = TA.head u.shape in
   *   let auto_algebra = u.support.RegExp.algebra in
   *   List.map (fun (k, v) ->
   *       (k,
   *        List.mapi (fun i nshape ->
   *          let nsupport = hole_position auto_algebra nshape in
   *          let nclasses = RegexpPartition.fold (fun (re, ne) acc ->
   *              let re' = RegExp.derivative re i in
   *              if RegExp.is_bottom re' then
   *                acc
   *              else
   *                let ne' = var_of_automata re' in
   *                (re', ne') :: acc
   *            ) u.classes []
   *          in
   *          let nenv, renaming = RegexpPartition.fold (fun (re, ne) (nenv, renaming) ->
   *              let re' = RegExp.derivative re i in
   *              if RegExp.is_bottom re' then
   *                nenv, renaming
   *              else
   *                let ne' = var_of_automata re' in
   *                (re', ne') :: nenv, (ne, ne') :: renaming
   *            ) u.env ([], [])
   *          in
   *          let nnum =
   *            Apol.change_environment_l u.numeric (List.map fst renaming) []
   *            |> Apol.renaming_list renaming
   *          in
   *          {
   *            shape = nshape;
   *            support = nsupport;
   *            classes = nclasses;
   *            env = nenv;
   *            numeric = nnum
   *          }
   *          ) v
   *       )
   *     ) head_shape *)

  (* let read_i (u: t) (i: int) =
   *   let l = sons u in
   *   List.fold_left (fun acc (_, ll) ->
   *       match List.nth_opt ll i, acc with
   *       | None, _ -> acc
   *       | Some v, None -> Some v
   *       | Some v, Some acc -> Some (join v acc)
   *     ) None l
   *   |> function
   *   | None -> bottom (u.shape.TA.dfta_a)
   *   | Some x -> x *)

  (* let build_tree_from_symbol (s: TA.algebra) (ul: t list): t =
   *   let nshape = TA.build_tree s (List.map (fun x -> x.shape) ul) in
   *   let auto_algebra = automata_algebra_on_n (nshape.TA.dfta_a) in
   *   let nsupport = hole_position auto_algebra nshape in
   *   let nenv, renamingl, _ = ToolBox.fold (fun u (nenv, renamingl, i) ->
   *       let nenv', renaming =
   *         RegexpPartition.fold (fun (xe, xn) (nenv, renaming) ->
   *             let re = RegExp.integrate xe i |> fun x -> RegExp.change_algebra x auto_algebra in
   *             let rn = var_of_automata re in
   *             ((re, rn):: nenv, (xn, rn) :: renaming)
   *           ) u.env ([], [])
   *       in
   *       (nenv' @ nenv, renaming :: renamingl, i+1)
   *     ) ul ([], [], 0)
   *   in
   *   let nclasses, _ = ToolBox.fold (fun u (nclasses, i) ->
   *       let nclasses' =
   *         RegexpPartition.fold (fun (xe, xn) (nclasses) ->
   *             let re = RegExp.integrate xe i |> fun x -> RegExp.change_algebra x auto_algebra in
   *             let rn = var_of_automata re in
   *             ((re, rn):: nclasses)
   *           ) u.classes []
   *       in
   *       (nclasses' @ nclasses, i+1)
   *     ) ul ([], 0)
   *   in
   *   let nnum =
   *     let numl = List.map2
   *         (fun r x -> Numerical.renaming_list r x.numeric)
   *         (List.rev renamingl)
   *         ul
   *     in
   *     ToolBox.fold (fun x -> function
   *         | None -> Some x
   *         | Some y -> Some (Numerical.cartesian_product y x)
   *       ) numl None
   *     |> function
   *     | None -> Numerical.top Environmentext.empty
   *     | Some y -> y
   *   in
   *   {
   *     shape = nshape;
   *     support = nsupport;
   *     env = nenv;
   *     classes = nclasses;
   *     numeric = nnum;
   *   } *)

  (* let build_tree_from_int (i: int): t =
   *   let one = S.fresh () in
   *   let hole = S.fresh () in
   *   let epsilon = RegExp.from_word (RegExp.Algebra.of_list [0]) [] in
   *   let epsilon_name = var_of_automata epsilon in
   *   {
   *     shape =
   *       {
   *         TA.dfta_q  = TA.SS.of_list [one; hole];
   *         TA.dfta_f  = TA.SS.of_list [one];
   *         TA.dfta_d  = TA.DetTrans.add_transition (hole_tree, [], one) TA.DetTrans.empty;
   *         TA.dfta_h  = hole;
   *         TA.dfta_hc = false;
   *         TA.dfta_a  = SA.of_list [hole_tree, 0]
   *       };
   *     support = epsilon;
   *     classes = [];
   *     env = [(epsilon, epsilon_name)];
   *     numeric =
   *       let e = Environmentext.int_env_of_list [epsilon_name] in
   *       let lc =
   *         let le = Apron.Linexpr1.make e in
   *         Apron.Linexpr1.set_coeff le
   *           (Apron.Var.of_string epsilon_name) (Apron.Coeff.s_of_int 1);
   *         Apron.Linexpr1.set_cst le (Apron.Coeff.s_of_int i);
   *         Apron.Lincons1.make le Apron.Lincons1.EQ
   *       in
   *       Numerical.of_lincons_list e [lc]
   *   }
   *
   * let change_sigma_algebra (sa: TA.SA.t) (u: t) =
   *   let reg_algebra = automata_algebra_on_n sa in
   *   let nshape = u.shape |> TA.change_sigma_algebra sa in
   *   let nsupport = hole_position reg_algebra nshape in
   *   let nenv, renaming = ToolBox.fold (fun (re, rn) (nenv, renaming) ->
   *       let re' = RegExp.change_algebra re reg_algebra in
   *       let rn' = var_of_automata re' in
   *       ((re', rn'):: nenv, (rn, rn') :: renaming)
   *     ) u.env ([], [])
   *   in
   *   let nclasses = ToolBox.fold (fun (re, rn) nclasses ->
   *       let re' = RegExp.change_algebra re reg_algebra in
   *       let rn' = var_of_automata re' in
   *       ((re', rn'):: nenv)
   *     ) u.classes ([])
   *   in
   *   let num = Numerical.renaming_list renaming u.numeric in
   *   {
   *     shape = nshape;
   *     support = nsupport;
   *     env = nenv;
   *     classes = nclasses;
   *     numeric = num
   *   } *)
end
