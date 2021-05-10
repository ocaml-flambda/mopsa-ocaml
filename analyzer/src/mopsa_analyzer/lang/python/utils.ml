(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Mopsa
open Ast

let debug fmt = Debug.debug ~channel:"python.utils" fmt

let rec partition_list_by_length n l =
  if n = 0 then [], l
  else
    match l with
    | hd :: tl ->
      let lhd, ltl = partition_list_by_length (n-1) tl in
      hd :: lhd, ltl
    | _ -> assert false

let mk_builtin_raise exn range =
  mk_stmt (S_py_raise (Some (mk_py_object (Addr.find_builtin exn) range))) range

let mk_builtin_raise_args exn args range =
  mk_stmt (S_py_raise (Some (mk_py_call (mk_py_object (Addr.find_builtin exn) range) args range))) range

let mk_builtin_raise_msg exn msg range =
  let open Universal.Ast in
  mk_builtin_raise_args exn [mk_constant ~etyp:(T_py (Some Str)) (C_string msg) range] range

let mk_builtin_call f params range =
  mk_py_call (mk_py_object (Addr.find_builtin f) range) params range

let mk_hasattr obj attr range =
  mk_builtin_call "hasattr" [obj; Universal.Ast.mk_string attr range] range

let mk_object_hasattr obj attr range =
  mk_hasattr (mk_py_object obj range) attr range

let mk_addr_hasattr obj attr range =
  mk_hasattr (mk_addr obj range) attr range

let mk_try_stopiteration body except range =
  mk_try
    body
    [mk_except
       (Some (mk_py_object (Addr.find_builtin "StopIteration") range)) (* (mk_addr (fst @@ Addr.find_builtin "StopIteration") range)) *)
       None
       except
    ]
    (Universal.Ast.mk_block [] range)
    (Universal.Ast.mk_block [] range)
    range


let check_instances ?(arguments_after_check=0) funname man flow range exprs instances processing =
  let open Mopsa in
  let rec aux pos iexprs lexprs linstances flow =
    match lexprs, linstances with
    | _, [] ->
      if arguments_after_check = List.length lexprs then
        processing iexprs (Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow)
      else
        let msg = Format.asprintf "%s: too many arguments: %d given, %d expected" funname (List.length exprs) (arguments_after_check + List.length instances) in
        man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
          Eval.empty
    | e::es, i::is ->
      assume (Addr.mk_py_isinstance_builtin e i range) man flow
        ~fthen:(fun flow ->
          Flow.add_safe_check Alarms.CHK_PY_TYPEERROR e.erange flow |>
          aux (pos+1) iexprs es is)
        ~felse:(fun flow ->
          let msg = Format.asprintf "%s: expected instance of '%s', but found %a at argument #%d" funname i pp_expr e pos in
          man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
            Eval.empty
          )
    | [], _ ->
       let msg = Format.asprintf "%s: too few arguments: %d given, %d expected" funname (List.length exprs) (arguments_after_check + List.length instances) in
       man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
         Eval.empty
  in
  Cases.bind_list exprs man.eval flow |>
  Cases.bind_result (fun exprs flow -> aux 1 exprs exprs instances flow)

let check_instances_disj ?(arguments_after_check=0) funname man flow range exprs instances processing =
  (* FIXME: error messages *)
  let open Mopsa in
  let rec aux pos iexprs lexprs linstances flow =
    match lexprs, linstances with
    | _, [] ->
       if arguments_after_check = List.length lexprs then
         Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
           processing iexprs
      else
        let msg = Format.asprintf "%s: too many arguments: %d given, %d expected" funname (List.length exprs) (arguments_after_check + List.length instances) in
        man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
          Eval.empty
    | e::es, i::is ->
      let mk_onecond = fun i -> Addr.mk_py_isinstance_builtin e i range in
      let cond = List.fold_left (fun acc el ->
          mk_binop ~etyp:(T_py None) acc O_py_or (mk_onecond el) range)
          (mk_onecond @@ List.hd i) (List.tl i) in
      assume cond man flow
        ~fthen:(fun flow ->
          Flow.add_safe_check Alarms.CHK_PY_TYPEERROR e.erange flow |>
          aux (pos+1) iexprs es is)
        ~felse:(fun flow ->
          let msg = Format.asprintf "%s: expected instance ∈ {%a}, but found %a at argument #%d"
              funname
              (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) i
              pp_expr e
              pos in
            man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
              Eval.empty
          )
    | _ ->
       let msg = Format.asprintf "%s: too few arguments: %d given, %d expected" funname (List.length exprs) (arguments_after_check + List.length instances) in
      man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
      Eval.empty
  in
  Cases.bind_list exprs man.eval flow |>
  Cases.bind_result (fun exprs flow -> aux 1 exprs exprs instances flow)

let strip_object (e:expr) =
  let ekind = match ekind e with
    | E_py_object (addr, oe) ->
      let addr = {addr with addr_partitioning = G_all } in
      E_py_object (addr, oe)
    | _ -> assert false in
  {e with ekind}

let new_wrapper man range flow newcls argcls ~fthennew =
  man.eval argcls flow >>$
    (fun ecls flow ->
      assume
        (Addr.mk_py_issubclass_builtin_r argcls newcls range)
        man flow
        ~fthen:(fun flow ->
          Flow.add_safe_check Alarms.CHK_PY_TYPEERROR argcls.erange flow |>
            fthennew
        )
        ~felse:(fun flow ->
          let msg = Format.asprintf "%s.__new__(%a): %a is not a subtype of int" newcls pp_expr argcls pp_expr ecls in
          man.exec (mk_builtin_raise_msg "TypeError" msg range) flow >>%
            Eval.empty)
    )
  |> OptionExt.return

let bind_list_args ?(cleaners=true) man args flow range (f: var list -> 'b flow -> ('b, 'c) Cases.cases) =
  let cs = Flow.get_callstack flow in
  let module RangeSet = SetExt.Make(struct type t = range let compare = compare_range end) in
  let stmt, vars, _ = List.fold_left (fun (stmts, vars, argranges) arg ->
                          assert(not @@ RangeSet.mem arg.erange argranges);
                          let tmp = mk_range_attr_var arg.erange (Format.asprintf "%xd(bla)" (Hashtbl.hash_param 30 100 cs)) (T_py None) in
                          (mk_assign (mk_var tmp arg.erange) arg arg.erange) :: stmts, tmp :: vars, RangeSet.add arg.erange argranges
                        ) ([], [], RangeSet.empty) (List.rev args) in
  let stmt = Universal.Ast.mk_block stmt range in
  let cases = f vars (man.exec stmt flow |> post_to_flow man) in
  if cleaners then
    Cases.add_cleaners (List.map (fun x -> mk_remove_var x range) vars) cases
  else cases


let change_var_type t v = mkv (Format.asprintf "%s:%a" v.vname pp_typ t) ~mode:v.vmode v.vkind t

let change_evar_type t evar =
  match ekind evar with
  | E_var (v, mode) -> {evar with ekind = E_var (change_var_type t v, mode); etyp=t}
  | _ -> assert false

let extract_oobject e = match ekind e with
  | E_py_object (_, Some a) -> a
  | _ -> assert false

let get_eobj_itv man flow e =
  let open Universal.Ast in
  let mk_itv_from_z z  = Bot.Nb (ItvUtils.IntItv.cst z) in
  match ekind @@ extract_oobject e with
  | E_constant (C_int z) -> mk_itv_from_z z
  | E_unop (O_minus, {ekind=E_constant (C_int z)}) -> mk_itv_from_z (Z.neg z)
  | _ ->
     let e_num = extract_oobject e in
     debug "need to ask the numerical domain for the value of %a" pp_expr e_num;
     man.ask (Universal.Numeric.Common.mk_int_interval_query e_num) flow

(* tries to evaluate python expr.
   If the evaluation fails and raises an exception, the exception flow is caught, put back to cur and `on_empty` is then run *)
let try_eval_expr ?(on_empty=fun exc_exp exc_str exc_msg flow -> assert false) ~(on_result:expr -> 'a flow -> ('a, expr) cases) (man: ('a, 'b) man) ?(route=Core.Route.toplevel) expr (flow: 'a flow) : ('a, expr) cases option =
  man.eval ~route:route expr flow >>=?
    fun case flow' ->
    Some (
      match case with
      | Result (expr', _, _) ->
         on_result expr' flow'
      | Empty ->
         (* find the T_py_exception tokens in flow' and not in flow *)
         let caught_flow, flow_ok = Flow.fold (fun (caught, acc_ok) tk env ->
                                        match tk with
                                        | Alarms.T_py_exception (expr, name, msg, exc_kind) when not @@ Flow.mem tk flow ->
                                           (expr, name, msg, env)::caught, acc_ok
                                        | _ -> caught, Flow.add tk env man.lattice acc_ok
                                      ) ([], Flow.bottom_from flow') flow' in
         let results = List.fold_left (fun acc (expr, name, msg, env) ->
                           match on_empty expr name msg (Flow.add T_cur env man.lattice flow_ok) with
                           | None -> acc
                           | Some s -> s :: acc)
                         [] caught_flow in
         Eval.join_list ~empty:(fun () -> Eval.empty flow_ok) results
      | NotHandled -> assert false
    )

let check man cond range flow =
  man.exec (Universal.Ast.mk_assert cond range) flow >>%
  man.eval (mk_py_none range)
