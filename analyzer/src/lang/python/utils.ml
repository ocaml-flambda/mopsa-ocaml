open Framework.Ast
open Universal.Ast
open Framework.Pp
open Universal.Pp
open Ast
(* open XAst
 * open Addr *)

let debug fmt = Debug.debug ~channel:"python.utils" fmt

(* let isinstance instance cls =
 *   debug "checking isinstance:@\n  instance = %a@\n  cls = %a" pp_addr instance pp_addr cls;
 *   match instance.akind, cls.akind with
 *   | U_instance(base, _), U_class _
 *   | U_instance(base, _), B_class _ ->
 *     begin
 *       debug "checking isinstance:@\n  base = %a@\n  cls = %a" pp_addr base pp_addr cls;
 *       let rec loop = function
 *         | [] -> debug "not isinstance"; false
 *         | bases ->
 *           debug "loop iteration:@\n  bases = %a"
 *             (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_addr) bases
 *           ;
 *           if List.mem cls bases then true
 *           else
 *             let bases' = List.fold_left (fun acc base ->
 *                 let base_bases =
 *                   match base.akind with
 *                   | U_class(_ , bases) ->
 *                     bases
 *                   | B_class "object" ->
 *                     []
 *                   | B_class(name) -> begin
 *                       try
 *                         let base =
 *                           Builtins.class_base name |>
 *                           Builtins.builtin_address
 *                         in
 *                         [base]
 *                       with Not_found ->
 *                         []
 *                     end
 *                   | _ -> assert false
 *                 in
 *                 base_bases @ acc
 *               ) [] bases
 *             in
 *             loop bases'
 *       in
 *       loop [base]
 *     end
 *
 *   | U_instance _ , _ -> false
 *
 *   | U_class _, B_class "class" -> true
 *   | U_class _, _ -> false
 *
 *   | U_function _, B_class "function" -> true
 *   | U_function _, _ -> false
 *
 *   | B_class "type", _ -> false
 *
 *   | B_class _, B_class "type" -> true
 *   | B_class _, _ -> false
 *
 *   | _ -> assert false
 *
 * let issubclass cls1 cls2 =
 *   let mro =
 *     match cls1.akind with
 *     | U_class(_, mro) -> cls1 :: mro
 *     | B_class _ -> cls1 :: Builtins.mro cls1
 *     | _ -> []
 *   in
 *   List.mem cls2 mro *)


let rec partition_list_by_length n l =
  if n = 0 then [], l
  else
    match l with
    | hd :: tl ->
      let lhd, ltl = partition_list_by_length (n-1) tl in
      hd :: lhd, ltl
    | _ -> assert false

(* let mk_try_stopiteration body except =
 *   mk_try
 *     body
 *     [mk_except
 *        (Some (mk_addr (Builtins.builtin_address "StopIteration")))
 *        None
 *        except
 *     ]
 *     (mk_stmt nop)
 *     (mk_stmt nop)
 *
 * let mk_hasattr obj attr =
 *   mk_exp (PyCall (mk_addr (Builtins.builtin_address "hasattr"),
 *                   [obj; mk_constant (String attr) ~etyp:TString],
 *                   []
 *                  )) *)
