include Apol
let debug fmt = ToolBox.debug "numerical" fmt

let print fmt u =
  Format.fprintf fmt "@[env: %a, abs: %a@]"
    Environmentext.print_g (A.env u)
    print u

let single_earray tc =
  let env = Linconsext.get_env tc in
  let rep = Linconsext.array_make env 1 in
  Linconsext.array_set rep 0 tc;
  rep

let try_harder current u_origin v_origin =
  let env_u = A.env u_origin in
  let env_v = A.env v_origin in
  let tcons = (to_lincons_list u_origin) @ (to_lincons_list v_origin) in
  List.fold_left (fun cur tc ->
      let cur' = A.meet_lincons_array man cur (single_earray tc) in
      let cur_u = change_environment cur env_u in
      let cur_v = change_environment cur env_v in
      if is_leq u_origin cur_u && is_leq v_origin cur_v then
        cur'
      else
        cur
    ) current tcons

let meet_different_support u v common =
  let u_common, u_sup = split_list u common in
  let v_common, v_sup = split_list v common in
  meet u_common v_common

let join_different_support u v common =
  let u_common, u_sup = split_list u common in
  let v_common, v_sup = split_list v common in

  let num_common = join u_common v_common in

  let glue1 = glue num_common u_sup in
  let rep = glue glue1 v_sup in
  let rep' = try_harder rep u v in
  rep'


let widening_different_support u v common =
  let u_common, u_sup = split_list u common in
  let v_common, v_sup = split_list v common in

  let num_common = widening u_common (join u_common v_common) in

  let glue1 = glue num_common u_sup in
  let rep = glue glue1 v_sup in
  rep

let eq u v abs =
  let open Apron in
  let linexpr =
    let r = Linexpr1.make (Environmentext.int_env_of_list [u; v]) in
    let u_var = Var.of_string u
    and v_var = Var.of_string v in
    Linexpr1.set_list r [(Coeff.s_of_int 1, u_var); (Coeff.s_of_int (-1), v_var)] None;
    r
  in
  let lincons =
    Lincons1.make linexpr Lincons1.EQ
  in
  sat_lincons abs lincons

let assume_eq u v abs =
  let open Apron in
  let env = Environmentext.int_env_of_list [u; v] in
  let linexpr =
    let r = Linexpr1.make env in
    let u_var = Var.of_string u
    and v_var = Var.of_string v in
    Linexpr1.set_list r [(Coeff.s_of_int 1, u_var); (Coeff.s_of_int (-1), v_var)] None;
    r
  in
  let lincons =
    Lincons1.make linexpr Lincons1.EQ
  in
  let learray =
    let r = Lincons1.array_make env 1 in
    Lincons1.array_set r 0 lincons;
    r
  in
  A.meet_lincons_array man  abs learray

let wrap_eq_variables (l : string list) (n : string) abs =
  let env = A.env abs in
  let rep, reste =
    match l with
    | p::q -> p, q
    | _ -> failwith "[merge_classes] should be called with a non empty \
                     partition"
  in
  let a = List.map Apron.Var.of_string reste |> Array.of_list in
  let env' = Environmentext.remove env a in
  let abs' = change_environment abs env' in
  renaming_list [(rep, n)] abs'

let find_foldable_variables abs =
  let env = A.env abs in
  let lenv = Environmentext.fold (fun acc v -> (Apron.Var.to_string v) :: acc) [] env in
  let rec cross l acc = match l with
    | p::q ->
      let reste, acc' = find_foldable p q [] [] in
      cross reste ((p :: acc'):: acc)
    | []   -> acc
  and find_foldable x l reste acc = match l with
    | y::q ->
      let abs' = renaming_list [(x, y); (y, x)] abs in
      if is_leq abs abs' && is_leq abs' abs then
        find_foldable x q reste (y::acc)
      else
        find_foldable x q (y :: reste) acc
    | _ -> reste, acc
  in
  List.filter (fun cl -> match cl with | p::q::r -> true | _ -> false) (cross lenv [])

let wrap_variables (l : string list) (n : string) abs =
  let tab = (List.map Apron.Var.of_string l |> Array.of_list) in
  let abs' = A.fold man abs tab in
  renaming_list [Apron.Var.to_string tab.(0), n] abs'

let apply_injection (i: (string * string) list) (lonely: string list) abs =
  let open ToolBox in
  let eq_classes = List.fold_left (fun acc (v, u) ->
      try
        let s = StringM.find u acc in
        let s = StringS.add  v s   in
        StringM.remove u acc |>
        StringM.add u s
      with
      | Not_found ->
        StringM.add u (StringS.singleton v) acc
    ) StringM.empty i
  in
  let abs, vars_to_remove, renaming =
    StringM.fold (fun u vs (abs, vars_to_remove, renaming) ->
        let v = StringS.choose vs in
        let vs = StringS.remove v vs in
        (
          StringS.fold (fun v' abs ->
              assume_eq v v' abs
            ) vs abs,
          StringS.union vars_to_remove vs,
          (v, u) :: renaming
        )
      ) eq_classes (abs, StringS.empty, [])
  in
  let new_env = Environmentext.int_env_of_list
      (lonely @ List.map fst renaming)
  in
  let abs' = change_environment abs new_env in
  let abs' = renaming_list renaming abs' in
  abs'

let lce_leq abs abs' =
  let env = Environmentext.lce (A.env abs) (A.env abs') in
  let abs, abs'= change_environment abs env, change_environment abs' env in
  is_leq abs abs'

let env_leq abs abs' env =
  let env  = Environmentext.int_env_of_list env in
  let abs  = change_environment abs  env in
  let abs' = change_environment abs' env in
  is_leq abs abs'

let extend abs s list =
  (* let () = debug "markn0" in *)
  let abs = A.expand man abs (Apron.Var.of_string s) (List.map Apron.Var.of_string list |> Array.of_list) in
  (* let () = debug "markn1" in *)
  let env = Environmentext.remove (A.env abs) ([|Apron.Var.of_string s|]) in
  (* let () = debug "markn2" in *)
  let abs = change_environment abs env in
  (* let () = debug "markn3" in *)
  abs

let merge abs s list =
  match list with
  | [] -> failwith "merge of list of size 0"
  | p::q ->
    let abs = A.fold man abs (List.map Apron.Var.of_string list |> Array.of_list) in
    let abs = renaming_list [p,s] abs in
    abs

let vos = Apron.Var.of_string

type g =
  | R | V

let f env l' =
  let env = Environmentext.int_env_of_list env in
  let build_g (t, l) =
    let le = Apron.Linexpr1.make env in
    List.iter (fun (x, v) -> Apron.Linexpr1.set_coeff le (vos x) (Apron.Coeff.s_of_float v)) l;
    Apron.Generator1.make le (match t with
        | R -> Apron.Generator1.RAY
        | V -> Apron.Generator1.VERTEX)
  in
  let gl = List.map build_g l' in
  (* let () = debug "f_gen: %a" (ToolBox.print_list Generatorext.print) gl in *)
  let rep = of_generator_list env gl in
  (* let () = debug "f_rep: %a" print rep in *)
  rep

let v i =
  if i = 0 then "x"
  else if i = 1 then "y"
  else ("v_"^(string_of_int (i- 2)))

let full_env n =
  Environmentext.real_env_of_list (ToolBox.fold_int (fun i acc -> v i :: acc) n [])

let one_cut env i x =
  let tcons1 = Apron.Parser.lincons1_of_string
      env
      ((v i) ^ "=" ^ (string_of_int i))
  in
  tcons1

let cut_polyhedra abs l =
  let e = A.env abs in
  let tcl = List.mapi (fun i x -> one_cut e (i+2) x) l in
  let tc = of_lincons_list e tcl in
  meet abs tc

let forget s abs =
  A.forget_array man abs [|(vos s)|] false
(* let pack_generators abs n =
 *   let l = to_generator_list abs in
 *   let vl = List.fold_left (
 *       fun (accg, accr) x ->
 *         (match Generatorext.get_typ x with
 *          | Apron.Generator0.RAY ->
 *            Toolbox.fold_int (fun (i, res) x ->
 *                let c = Generatorext.get_coeff x (v (i+2)) in
 *                (Apron.Coeff.)
 *              )
 *          | Apron.Generator0.VERTEX -> (??)
 *          | Apron.Generator0.LINE
 *          | Apron.Generator0.LINEMOD
 *          | Apron.Generator0.RAYMOD -> assert false)
 *     ) *)
(* let generate_polyhedra () = *)

let rec select_diff n l =
  let x = Random.int n in
  if List.mem x l then
    select_diff n l
  else x

let main () =
  (* let open Apron in
   * let u = "u"
   * and v = "v"
   * and w = "w"
   * and x = "x" in
   * let u_var = Var.of_string u
   * and v_var = Var.of_string v
   * and w_var = Var.of_string w
   * and x_var = Var.of_string x in
   * let env = (Environmentext.int_env_of_list [u; v]) in
   * let linexpr =
   *   let r = Linexpr1.make env in
   *   Linexpr1.set_list r [(Coeff.s_of_int (-1), u_var); (Coeff.s_of_int (-1), v_var);] (Some (Coeff.s_of_int (-1)));
   *   r
   * in
   * let lincons =
   *   Lincons1.make linexpr Lincons1.SUP
   * in *)
  (* let abs = of_lincons_list env [lincons] in
   * let () = debug "%a" print abs in
   * let abs' = A.fold man abs [|u_var; v_var|] in
   * let () = debug "%a" print abs' in
   * let abs'' = A.expand man abs' u_var [|v_var|] in
   * let () = debug "%a" print abs'' in () *)
  let ff = f in
  let rec next () =
    let prof1 = 0 (* select_diff 20 [] *) in
    let prof2 = 1 (* select_diff 20 [prof1] *) in
    let prof3 = 2 (* select_diff 20 [prof2 ; prof1] *) in

    let (a, b) = (0, 1) (* (select_diff 20 [], select_diff 20 []) *) in
    let (c, d) = (1, 2) (* (select_diff 20 [], select_diff 20 []) *) in
    let (e, f) = (4, 3) (* (select_diff 20 [], select_diff 20 []) *) in

    (* let prof1 = -1 in
     * let prof2 = -4 in *)
    (* (V, [("x", 0); ("y", 0); ("z", 1);]); *)

    let prof1 = float_of_int prof1 in
    let prof2 = float_of_int prof2 in
    let prof3 = float_of_int prof3 in

    let a = float_of_int a in
    let b = float_of_int b in
    let c = float_of_int c in
    let d = float_of_int d in
    let e = float_of_int e in
    let f = float_of_int f in

    let abs = ff ["x"; "y"; "z"]
        [
          (* (V, [("x", 0); ("y", 0); ("z", 1);]); *)
          (V, [("x", a); ("y", a); ("z", prof1);]);
          (V, [("x", a); ("y", b); ("z", prof1);]);
          (V, [("x", b); ("y", a); ("z", prof1);]);
          (V, [("x", b); ("y", b); ("z", prof1);]);
          (V, [("x", c); ("y", c); ("z", prof2);]);
          (V, [("x", c); ("y", d); ("z", prof2);]);
          (V, [("x", d); ("y", c); ("z", prof2);]);
          (V, [("x", d); ("y", d); ("z", prof2);]);
          (V, [("x", e); ("y", e); ("z", prof3);]);
          (V, [("x", e); ("y", f); ("z", prof3);]);
          (V, [("x", f); ("y", e); ("z", prof3);]);
          (V, [("x", f); ("y", f); ("z", prof3);]);

          (* (R, [("x", 0); ("y", 0); ("z", 1); ("t", 12)]);
           * (R, [("x", 1); ("y", 0); ("z", 1); ("t", 12)]);
           * (R, [("x", 0); ("y", 1); ("z", 1); ("t", 12)]);
           * (R, [("x", 2); ("y", 2); ("z", 2); ("t", 24)]); *)
          (* (R, [("x", 2); ("y", 2); ("z", 2); ("t", 1)]);
           * (R, [("x", 2); ("y", 4); ("z", 2); ("t", 1)]);
           * (R, [("x", 4); ("y", 2); ("z", 2); ("t", 1)]);
           * (R, [("x", 4); ("y", 4); ("z", 2); ("t", 1)]); *)
        ]
    in
    let abs2 = ff ["x"; "y"; "z"]
        [
          (V, [("x",0.) ; ("y", 0.) ; ("z", 0.5)]);
          (R, [("x",1.) ; ("y", 0.) ; ("z", 0.)]);
          (R, [("x",0.) ; ("y", 1.) ; ("z", 0.)]);
          (R, [("x",-.1.); ("y", 0.) ; ("z", 0.)]);
          (R, [("x",0.) ; ("y",-.1.); ("z", 0.)]);
        ]
    in
    (* let () = debug "%a" print abs2 in *)
    let abs3 = (meet abs abs2) in
    let () = debug "%a" print abs3 in
    let gl = to_generator_list abs3 in
    let () = debug "cut: %a" (ToolBox.print_list Generatorext.print) gl in
    (* let () = debug "%a" print abs in *)
    let abs' = A.fold man abs [|vos "x"; vos "y"|] in
    (* let () = debug "%a" print abs' in *)
    let abs'' = A.expand man abs' (vos "x") [|(vos "y")|] in
    (* let () = debug "%a" print abs'' in *)
    let bo = is_leq abs abs'' && is_leq abs'' abs in
    if bo then
      next ()
    else
      let () = debug "(a, b): (%f, %f)" a b in
      let () = debug "(c, d): (%f, %f)" c d in
      let () = debug "(e, f): (%f, %f)" e f in
      let () = debug "prof1: %f; prof2: %f; prof3: %f@." prof1 prof2 prof3 in
      ()
  in
  next ()
(* let () = debug "%b" (is_leq abs abs'' && is_leq abs'' abs) in () *)

let cartesian_product a b =
  let nenv = Environmentext.lce (A.env a) (A.env b) in
  let a' = Apol.change_environment a nenv in
  let b' = Apol.change_environment b nenv in
  Apol.meet a' b'
