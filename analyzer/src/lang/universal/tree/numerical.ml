open Mopsa

let debug fmt = ToolBox.debug "numerical" fmt
(* let to_num_var uid v = *)

(* include Apol
 * let debug fmt = ToolBox.debug "numerical" fmt
 *
 * let print fmt u =
 *   Format.fprintf fmt "@[env: %a, abs: %a@]"
 *     Environmentext.print_g (A.env u)
 *     print u
 *
 * let single_earray tc =
 *   let env = Linconsext.get_env tc in
 *   let rep = Linconsext.array_make env 1 in
 *   Linconsext.array_set rep 0 tc;
 *   rep
 *
 * let try_harder current u_origin v_origin =
 *   let env_u = A.env u_origin in
 *   let env_v = A.env v_origin in
 *   let tcons = (to_lincons_list u_origin) @ (to_lincons_list v_origin) in
 *   List.fold_left (fun cur tc ->
 *       let cur' = A.meet_lincons_array man cur (single_earray tc) in
 *       let cur_u = change_environment cur env_u in
 *       let cur_v = change_environment cur env_v in
 *       if is_leq u_origin cur_u && is_leq v_origin cur_v then
 *         cur'
 *       else
 *         cur
 *     ) current tcons
*)

let meet_different_support
    (man: ('a, 'b) man)
    (u: 'a flow) (v: 'a flow)
    (rmu: string list) (rmv: string list)
    (common_env: string list)
    (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
  =
  let rmu, vb_u = StrVarBind.get_var_list rmu vb_u in
  let rmv, vb_v = StrVarBind.get_var_list rmv vb_v in
  let vb_u, vb_v, renaming = ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
      let var_u, vb_u = StrVarBind.get_var s vb_u in
      let var_v, vb_v = StrVarBind.get_var s vb_v in
      (vb_u, vb_v, (var_v, var_u) :: renaming)
    ) common_env (vb_u, vb_v, [])
  in
  let u = ToolBox.fold (fun var abs ->
      man.exec
        (mk_stmt (S_remove_var var) (mk_fresh_range ()))
        abs
    ) rmu u
  in
  let v = ToolBox.fold (fun var abs ->
    man.exec
      (mk_stmt (S_remove_var var) (mk_fresh_range ()))
      abs
  ) rmv v
  in
  let v = ToolBox.fold (fun (var, var') abs ->
      man.exec
        (mk_stmt (S_rename_var(var, var')) (mk_fresh_range ()))
        abs
    ) renaming v
  in
  u, v, vb_u, vb_v

let join_different_support
    (man: ('a, 'b) man)
    (u: 'a flow) (v: 'a flow)
    (envv: string list)
    (full_env: string list)
    (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
  =
  let () = debug "full_env: %a" (ToolBox.print_list Format.pp_print_string) full_env in
  let () = debug "envv: %a" (ToolBox.print_list Format.pp_print_string) envv in
  let vb_u, vb_v, renaming =
    ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
        let var_u, vb_u = StrVarBind.get_var s vb_u in
        let var_v, vb_v = StrVarBind.get_var s vb_v in
        if List.mem s envv then
          (vb_u, vb_v, (var_v, var_u) :: renaming)
        else
          (vb_u, vb_v, renaming)
      ) full_env (vb_u, vb_v, [])
  in
  let () = debug "renaming: %a" (ToolBox.print_list (ToolBox.print_pair pp_var pp_var)) renaming in
  (* Ici on n'ajoute pas les variables car c'est la sémantique du join
     de mopsa *)
  let v = ToolBox.fold (fun (var, var') abs ->
      man.exec
        (mk_stmt (S_rename_var(var, var')) (mk_fresh_range ()))
        abs
    ) renaming v
  in
  u, v, vb_u, vb_v


(* let join_different_support2
 *     (man: ('a, 'b) man)
 *     (u: 'a flow) (v: 'a flow)
 *     (envu: string list) (envv: string list)
 *     (full_env: string list)
 *     (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
 *   =
 *   let vb_u, vb_v, renaming = ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
 *       let var_u, vb_u = StrVarBind.get_var s vb_u in
 *       let var_v, vb_v = StrVarBind.get_var s vb_v in
 *       if List.mem s envv then
 *         (vb_u, vb_v, (var_v, var_u) :: renaming)
 *       else
 *         (vb_u, vb_v, renaming)
 *     ) full_env (vb_u, vb_v, [])
 *   in
 *   let v = ToolBox.fold (fun (var, var') abs ->
 *       man.exec
 *         (mk_stmt (S_rename_var(var, var')) (mk_fresh_range ()))
 *         abs
 *     ) renaming v
 *   in
 *   (\* Gros problème : on ne peut rajouter les contraintes post join*\)
 *   u, v, vb_u, vb_v *)


(* let join_different_support u v common =
 *   let u_common, u_sup = split_list u common in
 *   let v_common, v_sup = split_list v common in
 *
 *   let num_common = join u_common v_common in
 *
 *   let glue1 = glue num_common u_sup in
 *   let rep = glue glue1 v_sup in
 *   let rep' = try_harder rep u v in
 *   rep' *)

let widening_different_support
    (man: ('b, 'b) man)
    (u: 'b flow) (v: 'b flow)
    (envu: string list) (envv: string list)
    (* (full_env: string list) *)
    (vb_u: StrVarBind.t) (vb_v: StrVarBind.t) =
  let vb_u, vb_v, renaming = ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
      let var_u, vb_u = StrVarBind.get_var s vb_u in
      let var_v, vb_v = StrVarBind.get_var s vb_v in
      if List.mem s envv then
        (vb_u, vb_v, (var_v, var_u) :: renaming)
      else
        (vb_u, vb_v, renaming)
    ) envu (vb_u, vb_v, [])
  in
  (* Ici on n'ajoute pas les variables car c'est la sémantique du
     widening de mopsa *)
  let v = ToolBox.fold (fun (var, var') abs ->
      man.exec
        (mk_stmt (S_rename_var(var, var')) (mk_fresh_range ()))
        abs
    ) renaming v
  in
  (* Gros problème : on ne peut rajouter les contraintes post join*)
  u, v, vb_u, vb_v

  (* let u_common, u_sup = split_list u common in
   * let v_common, v_sup = split_list v common in
   *
   * let num_common = widening u_common (join u_common v_common) in
   *
   * let glue1 = glue num_common u_sup in
   * let rep = glue glue1 v_sup in
   * rep *)




let eq
    (range: Location.range)
    (man: ('b, 'b) man)
    (u: string)
    (v: string)
    (vb: StrVarBind.t)
    (num: 'b flow)
  : (bool * StrVarBind.t)
  =
  let vu, vb = StrVarBind.get_var u vb in
  let vv, vb = StrVarBind.get_var v vb in
  let e = (mk_binop
             (mk_var ~mode:STRONG vu (tag_range range "vu"))
             O_eq
             (mk_var ~mode:STRONG vu (tag_range range "vu"))
             ~etyp:Ast.T_int (tag_range range "eq")
          )
  in
  (man.ask (Numeric.Relational.Q_sat e) num, vb)


 (* let assume_eq u v abs =
 *   let open Apron in
 *   let env = Environmentext.int_env_of_list [u; v] in
 *   let linexpr =
 *     let r = Linexpr1.make env in
 *     let u_var = Var.of_string u
 *     and v_var = Var.of_string v in
 *     Linexpr1.set_list r [(Coeff.s_of_int 1, u_var); (Coeff.s_of_int (-1), v_var)] None;
 *     r
 *   in
 *   let lincons =
 *     Lincons1.make linexpr Lincons1.EQ
 *   in
 *   let learray =
 *     let r = Lincons1.array_make env 1 in
 *     Lincons1.array_set r 0 lincons;
 *     r
 *   in
 *   A.meet_lincons_array man  abs learray
*)
(* let wrap_eq_variables (l : string list) (n : string) abs =
 *   let env = A.env abs in
 *   let rep, reste =
 *     match l with
 *     | p::q -> p, q
 *     | _ -> failwith "[merge_classes] should be called with a non empty \
 *                      partition"
 *   in
 *   let a = List.map Apron.Var.of_string reste |> Array.of_list in
 *   let env' = Environmentext.remove env a in
 *   let abs' = change_environment abs env' in
 *   renaming_list [(rep, n)] abs' *)

let find_foldable_variables (man: ('a, 'b) man) vb (vl: string list) (abs: 'a flow) : string list list * StrVarBind.t =
  let vl, vb = StrVarBind.get_var_list vl vb in
  let foldable_vars = man.ask (Numeric.Relational.Q_fold vl) abs in
  try
    List.map (fun l ->
        List.map (fun s ->
            StrVarBind.find_r s vb
          ) l
      ) foldable_vars, vb
  with
  | Not_found -> Exceptions.panic "[universal.tree.numerical.find_foldable_variables] Could not look back"

(* let find_foldable_variables abs =
 *   let env = A.env abs in
 *   let lenv = Environmentext.fold (fun acc v -> (Apron.Var.to_string v) :: acc) [] env in
 *   let rec cross l acc = match l with
 *     | p::q ->
 *       let reste, acc' = find_foldable p q [] [] in
 *       cross reste ((p :: acc'):: acc)
 *     | []   -> acc
 *   and find_foldable x l reste acc = match l with
 *     | y::q ->
 *       let abs' = renaming_list [(x, y); (y, x)] abs in
 *       if is_leq abs abs' && is_leq abs' abs then
 *         find_foldable x q reste (y::acc)
 *       else
 *         find_foldable x q (y :: reste) acc
 *     | _ -> reste, acc
 *   in
 *   List.filter (fun cl -> match cl with | p::q::r -> true | _ -> false) (cross lenv []) *)

let fold_var range man (l : var list) (n : var) abs =
  let abs' = man.exec (mk_stmt (S_fold(n, l)) (tag_range range "fold")) abs in
  abs'

let fold range man (vb: StrVarBind.t) (l : string list) (n : string) abs =
  let v, vb = StrVarBind.get_var n vb in
  let vl, vb = StrVarBind.get_var_list l vb in
  let abs' = man.exec (mk_stmt (S_fold(v, vl)) (tag_range range "fold")) abs in
  abs', vb

let fold_two_vb range man (vb: StrVarBind.t) (vb': StrVarBind.t) (l : string list) (n : string) abs =
  begin
    let v, vb' = StrVarBind.get_var n vb' in
    let vl, vb = StrVarBind.get_var_list l vb in
    let abs' = man.exec (mk_stmt (S_fold(v, vl)) (tag_range range "fold")) abs in
    abs', vb, vb'
  end

(* let merge rannge (abs: 'b flow) s list vb =
 *   match list with
 *   | [] -> failwith "merge of list of size 0"
 *   | p::q ->
 *     let abs = A.fold man abs (List.map Apron.Var.of_string list |> Array.of_list) in
 *     let abs = renaming_list [p,s] abs in
 *     abs *)

  (* let apply_injection (i: (string * string) list) (lonely: string list) abs =
   *   let open ToolBox in
   *   let eq_classes = List.fold_left (fun acc (v, u) ->
   *       try
   *         let s = StringM.find u acc in
   *         let s = StringS.add  v s   in
   *         StringM.remove u acc |>
   *         StringM.add u s
   *       with
   *       | Not_found ->
   *         StringM.add u (StringS.singleton v) acc
   *     ) StringM.empty i
   *   in
   *   let abs, vars_to_remove, renaming =
   *     StringM.fold (fun u vs (abs, vars_to_remove, renaming) ->
   *         let v = StringS.choose vs in
   *         let vs = StringS.remove v vs in
   *         (
   *           StringS.fold (fun v' abs ->
   *               assume_eq v v' abs
   *             ) vs abs,
   *           StringS.union vars_to_remove vs,
   *           (v, u) :: renaming
   *         )
   *       ) eq_classes (abs, StringS.empty, [])
   *   in
   *   let new_env = Environmentext.int_env_of_list
   *       (lonely @ List.map fst renaming)
   *   in
   *   let abs' = change_environment abs new_env in
   *   let abs' = renaming_list renaming abs' in
   *   abs'
   *
   * let lce_leq abs abs' =
   *   let env = Environmentext.lce (A.env abs) (A.env abs') in
   *   let abs, abs'= change_environment abs env, change_environment abs' env in
   *   is_leq abs abs'
  *)

let env_leq (man: ('b, 'b) man) (u: 'b flow) (v: 'b flow)
    (envv: string list) (common: string list)
    (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
  : ('b flow * 'b flow * StrVarBind.t * StrVarBind.t)
  =
  let rmv, vb_v = StrVarBind.get_var_list envv vb_v in
  let vb_u, vb_v, renaming = ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
      let var_u, vb_u = StrVarBind.get_var s vb_u in
      let var_v, vb_v = StrVarBind.get_var s vb_v in
      (vb_u, vb_v, (var_v, var_u) :: renaming)
    ) common (vb_u, vb_v, [])
  in
  let v = ToolBox.fold (fun var abs ->
      man.exec
        (mk_stmt (S_remove_var var) (mk_fresh_range ()))
        abs
    ) rmv v
  in
  let v = ToolBox.fold (fun (var, var') abs ->
      man.exec
        (mk_stmt (S_rename_var(var, var')) (mk_fresh_range ()))
        abs
    ) renaming v
  in
  u, v, vb_u, vb_v

let env_leq_same_num (man: ('a, 'b) man) (flow: 'a flow)
    (envv: string list) (common: string list)
    (vb_u: StrVarBind.t) (vb_v: StrVarBind.t)
  : ('b flow * 'b flow * StrVarBind.t * StrVarBind.t)
  =
  let rmv, vb_v = StrVarBind.get_var_list envv vb_v in
  let vb_u, vb_v, renaming = ToolBox.fold (fun s (vb_u, vb_v, renaming) ->
      let var_u, vb_u = StrVarBind.get_var s vb_u in
      let var_v, vb_v = StrVarBind.get_var s vb_v in
      (vb_u, vb_v, (var_v, var_u) :: renaming)
    ) common (vb_u, vb_v, [])
  in
  let flow = ToolBox.fold (fun var abs ->
      man.exec
        (mk_stmt (S_remove_var var) (mk_fresh_range ()))
        abs
    ) rmv flow
  in
  let flowu, flowv = ToolBox.fold (fun (var_v, var_u) (flowu, flowv) ->
      let flowu = man.exec (mk_stmt (S_remove_var (var_v)) (mk_fresh_range ())) flowu in
      let flowv = man.exec (mk_stmt (S_remove_var (var_u)) (mk_fresh_range ())) flowv in
      let flowv = man.exec
          (mk_stmt (S_rename_var(var_v, var_u)) (mk_fresh_range ()))
          flowv
      in
      (flowu, flowv)
    ) renaming (flow, flow)
  in
  flowu, flowv, vb_u, vb_v

  (* let env  = Environmentext.int_env_of_list env in
   * let abs  = change_environment abs  env in
   * let abs' = change_environment abs' env in
   * is_leq abs abs' *)

(* let extend range (man: ('b, 'b) man) vb (s: string) (list: string list) (abs: 'b flow) =
 *   let v, vb = StrVarBind.get_var s vb in
 *   let vl, vb = ToolBox.fold (fun s (vl, vb) ->
 *       let v, vb = StrVarBind.get_var s vb in
 *       (v :: vl, vb)
 *     ) list ([], vb)
 *   in
 *   let abs = man.exec (mk_stmt (Ast.S_expand(v, vl)) (tag_range range "expand")) abs in
 *   (abs, vb) *)

let extend range (man: ('a, 'b) man) vb (s: string) (sl: string list) (abs: 'a flow) =
  let v, vb = StrVarBind.get_var s vb in
  let vl, vb = ToolBox.fold (fun s (vl, vb) ->
      let v, vb = StrVarBind.get_var s vb in
      (v :: vl, vb)
    ) sl ([], vb)
  in
  let abs = man.exec (mk_stmt (S_expand(v, vl)) (tag_range range "expand")) abs in
  (abs, vb)

let extend_var range (man: ('a, 'b) man) (v: var) (vl: var list) (abs: 'a flow) =
  man.exec (mk_stmt (S_expand(v, vl)) (tag_range range "expand")) abs

(* let extend_stmtl range vb (s: string) (list: string list) =
 *   let v, vb = StrVarBind.get_var s vb in
 *   let vl, vb = ToolBox.fold (fun s (vl, vb) ->
 *       let v, vb = StrVarBind.get_var s vb in
 *       (v :: vl, vb)
 *     ) list ([], vb)
 *   in
 *   ([mk_stmt (Ast.S_expand(v, vl)) (tag_range range "expand")], vb) *)

let renaming_list range (man: ('a, 'b) man) (vb: StrVarBind.t) (renamer: (string * string) list) (abs: 'a flow) =
  let renamer, vb = ToolBox.fold (fun (s, s') (renamer, vb) ->
      let v , vb = StrVarBind.get_var s  vb in
      let v', vb = StrVarBind.get_var s' vb in
      ((v, v')::renamer, vb)
    ) renamer ([], vb)
  in
  (ToolBox.fold (fun (v, v') abs ->
      man.exec (mk_stmt (S_rename_var(v, v')) range) abs
    ) renamer abs, vb)


let renaming_list_diff_vb range (man: ('a, 'b) man) (vb: StrVarBind.t) (vb': StrVarBind.t) (renamer: (string * string) list) (abs: 'a flow) =
  let renamer, vb, vb' = ToolBox.fold (fun (s, s') (renamer, vb, vb') ->
      let v , vb = StrVarBind.get_var s  vb in
      let v', vb' = StrVarBind.get_var s' vb' in
      ((v, v')::renamer, vb, vb')
    ) renamer ([], vb, vb')
  in
  (ToolBox.fold (fun (v, v') abs ->
      man.exec (mk_stmt (S_rename_var(v, v')) range) abs
    ) renamer abs, vb, vb')

let renaming_list_var range (man: ('a, 'b) man) (renamer: (var * var) list) (abs: 'a flow) =
  (ToolBox.fold (fun (v, v') abs ->
      man.exec (mk_stmt (S_rename_var(v, v')) range) abs
     ) renamer abs)

let forget range (man: ('a, 'b) man) (vb: StrVarBind.t) (s: string) (abs: 'a flow) =
  let v, vb = StrVarBind.get_var s vb in
  (man.exec (mk_stmt (S_remove_var v) range) abs, vb)

  (* let vos = Apron.Var.of_string
   *
   * type g =
   *   | R | V
   *
   * let f env l' =
   *   let env = Environmentext.int_env_of_list env in
   *   let build_g (t, l) =
   *     let le = Apron.Linexpr1.make env in
   *     List.iter (fun (x, v) -> Apron.Linexpr1.set_coeff le (vos x) (Apron.Coeff.s_of_float v)) l;
   *     Apron.Generator1.make le (match t with
   *         | R -> Apron.Generator1.RAY
   *         | V -> Apron.Generator1.VERTEX)
   *   in
   *   let gl = List.map build_g l' in
   *   (\* let () = debug "f_gen: %a" (ToolBox.print_list Generatorext.print) gl in *\)
   *   let rep = of_generator_list env gl in
   *   (\* let () = debug "f_rep: %a" print rep in *\)
   *   rep
   *
   * let v i =
   *   if i = 0 then "x"
   *   else if i = 1 then "y"
   *   else ("v_"^(string_of_int (i- 2)))
   *
   * let full_env n =
   *   Environmentext.real_env_of_list (ToolBox.fold_int (fun i acc -> v i :: acc) n [])
   *
   * let one_cut env i x =
   *   let tcons1 = Apron.Parser.lincons1_of_string
   *       env
   *       ((v i) ^ "=" ^ (string_of_int i))
   *   in
   *   tcons1
   *
   * let cut_polyhedra abs l =
   *   let e = A.env abs in
   *   let tcl = List.mapi (fun i x -> one_cut e (i+2) x) l in
   *   let tc = of_lincons_list e tcl in
   *   meet abs tc
   *
   * let forget s abs =
   *   A.forget_array man abs [|(vos s)|] false
   * (\* let pack_generators abs n =
   *  *   let l = to_generator_list abs in
   *  *   let vl = List.fold_left (
   *  *       fun (accg, accr) x ->
   *  *         (match Generatorext.get_typ x with
   *  *          | Apron.Generator0.RAY ->
   *  *            Toolbox.fold_int (fun (i, res) x ->
   *  *                let c = Generatorext.get_coeff x (v (i+2)) in
   *  *                (Apron.Coeff.)
   *  *              )
   *  *          | Apron.Generator0.VERTEX -> (??)
   *  *          | Apron.Generator0.LINE
   *  *          | Apron.Generator0.LINEMOD
   *  *          | Apron.Generator0.RAYMOD -> assert false)
   *  *     ) *\)
   * (\* let generate_polyhedra () = *\)
   *
   * let rec select_diff n l =
   *   let x = Random.int n in
   *   if List.mem x l then
   *     select_diff n l
   *   else x
   *
   * let main () =
   *   (\* let open Apron in
   *    * let u = "u"
   *    * and v = "v"
   *    * and w = "w"
   *    * and x = "x" in
   *    * let u_var = Var.of_string u
   *    * and v_var = Var.of_string v
   *    * and w_var = Var.of_string w
   *    * and x_var = Var.of_string x in
   *    * let env = (Environmentext.int_env_of_list [u; v]) in
   *    * let linexpr =
   *    *   let r = Linexpr1.make env in
   *    *   Linexpr1.set_list r [(Coeff.s_of_int (-1), u_var); (Coeff.s_of_int (-1), v_var);] (Some (Coeff.s_of_int (-1)));
   *    *   r
   *    * in
   *    * let lincons =
   *    *   Lincons1.make linexpr Lincons1.SUP
   *    * in *\)
   *   (\* let abs = of_lincons_list env [lincons] in
   *    * let () = debug "%a" print abs in
   *    * let abs' = A.fold man abs [|u_var; v_var|] in
   *    * let () = debug "%a" print abs' in
   *    * let abs'' = A.expand man abs' u_var [|v_var|] in
   *    * let () = debug "%a" print abs'' in () *\)
   *   let ff = f in
   *   let rec next () =
   *     let prof1 = 0 (\* select_diff 20 [] *\) in
   *     let prof2 = 1 (\* select_diff 20 [prof1] *\) in
   *     let prof3 = 2 (\* select_diff 20 [prof2 ; prof1] *\) in
   *
   *     let (a, b) = (0, 1) (\* (select_diff 20 [], select_diff 20 []) *\) in
   *     let (c, d) = (1, 2) (\* (select_diff 20 [], select_diff 20 []) *\) in
   *     let (e, f) = (4, 3) (\* (select_diff 20 [], select_diff 20 []) *\) in
   *
   *     (\* let prof1 = -1 in
   *      * let prof2 = -4 in *\)
   *     (\* (V, [("x", 0); ("y", 0); ("z", 1);]); *\)
   *
   *     let prof1 = float_of_int prof1 in
   *     let prof2 = float_of_int prof2 in
   *     let prof3 = float_of_int prof3 in
   *
   *     let a = float_of_int a in
   *     let b = float_of_int b in
   *     let c = float_of_int c in
   *     let d = float_of_int d in
   *     let e = float_of_int e in
   *     let f = float_of_int f in
   *
   *     let abs = ff ["x"; "y"; "z"]
   *         [
   *           (\* (V, [("x", 0); ("y", 0); ("z", 1);]); *\)
   *           (V, [("x", a); ("y", a); ("z", prof1);]);
   *           (V, [("x", a); ("y", b); ("z", prof1);]);
   *           (V, [("x", b); ("y", a); ("z", prof1);]);
   *           (V, [("x", b); ("y", b); ("z", prof1);]);
   *           (V, [("x", c); ("y", c); ("z", prof2);]);
   *           (V, [("x", c); ("y", d); ("z", prof2);]);
   *           (V, [("x", d); ("y", c); ("z", prof2);]);
   *           (V, [("x", d); ("y", d); ("z", prof2);]);
   *           (V, [("x", e); ("y", e); ("z", prof3);]);
   *           (V, [("x", e); ("y", f); ("z", prof3);]);
   *           (V, [("x", f); ("y", e); ("z", prof3);]);
   *           (V, [("x", f); ("y", f); ("z", prof3);]);
   *
   *           (\* (R, [("x", 0); ("y", 0); ("z", 1); ("t", 12)]);
   *            * (R, [("x", 1); ("y", 0); ("z", 1); ("t", 12)]);
   *            * (R, [("x", 0); ("y", 1); ("z", 1); ("t", 12)]);
   *            * (R, [("x", 2); ("y", 2); ("z", 2); ("t", 24)]); *\)
   *           (\* (R, [("x", 2); ("y", 2); ("z", 2); ("t", 1)]);
   *            * (R, [("x", 2); ("y", 4); ("z", 2); ("t", 1)]);
   *            * (R, [("x", 4); ("y", 2); ("z", 2); ("t", 1)]);
   *            * (R, [("x", 4); ("y", 4); ("z", 2); ("t", 1)]); *\)
   *         ]
   *     in
   *     let abs2 = ff ["x"; "y"; "z"]
   *         [
   *           (V, [("x",0.) ; ("y", 0.) ; ("z", 0.5)]);
   *           (R, [("x",1.) ; ("y", 0.) ; ("z", 0.)]);
   *           (R, [("x",0.) ; ("y", 1.) ; ("z", 0.)]);
   *           (R, [("x",-.1.); ("y", 0.) ; ("z", 0.)]);
   *           (R, [("x",0.) ; ("y",-.1.); ("z", 0.)]);
   *         ]
   *     in
   *     (\* let () = debug "%a" print abs2 in *\)
   *     let abs3 = (meet abs abs2) in
   *     let () = debug "%a" print abs3 in
   *     let gl = to_generator_list abs3 in
   *     let () = debug "cut: %a" (ToolBox.print_list Generatorext.print) gl in
   *     (\* let () = debug "%a" print abs in *\)
   *     let abs' = A.fold man abs [|vos "x"; vos "y"|] in
   *     (\* let () = debug "%a" print abs' in *\)
   *     let abs'' = A.expand man abs' (vos "x") [|(vos "y")|] in
   *     (\* let () = debug "%a" print abs'' in *\)
   *     let bo = is_leq abs abs'' && is_leq abs'' abs in
   *     if bo then
   *       next ()
   *     else
   *       let () = debug "(a, b): (%f, %f)" a b in
   *       let () = debug "(c, d): (%f, %f)" c d in
   *       let () = debug "(e, f): (%f, %f)" e f in
   *       let () = debug "prof1: %f; prof2: %f; prof3: %f@." prof1 prof2 prof3 in
   *       ()
   *   in
   *   next ()
   * (\* let () = debug "%b" (is_leq abs abs'' && is_leq abs'' abs) in () *\)
   *
   * let cartesian_product a b =
   *   let nenv = Environmentext.lce (A.env a) (A.env b) in
   *   let a' = Apol.change_environment a nenv in
   *   let b' = Apol.change_environment b nenv in
   *   Apol.meet a' b' *)
