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
open Sig.Abstraction.Stateless
open Ast
open MapExt
open Addr
open Universal.Ast
(* gérer les appels sur int + constantes *)

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.types.t_int"
    end)

  let alarms = []


  let interface = {
    iexec = {provides = []; uses = [Universal.Zone.Z_u_int]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u, Universal.Zone.Z_u_int; Universal.Zone.Z_u, Universal.Zone.Z_u_float]}
  }

  let init _ _ flow = flow

  let is_arith_unop_fun = function
    | "int.__pos__"
    | "int.__neg__"
    | "int.__invert__" -> true
    | _ -> false

  (* FIXME: I'd like to merge only if true/false have different flows *)
  (* let merge_tf_top man range c =
   *   if Cases.cardinal c = 2 then
   *     let () = debug "2 cases, we're good!" in
   *     let t, f, oflow = Cases.fold (fun oe flow (ftrue, ffalse, oflow) ->
   *                           if oflow <> None && not (Flow.subset man.lattice flow (OptionExt.none_to_exn oflow)) then
   *                             (ftrue, ffalse, oflow)
   *                           else
   *                             match oe with
   *                             | Some e when compare_expr e (mk_py_object (Addr_env.addr_true (), Some (mk_int 1 ~typ:T_int e.erange)) e.erange) = 0 ->
   *                                (true, ffalse, OptionExt.apply (fun flow' -> Some (Flow.join man.lattice flow flow')) (Some flow) oflow)
   *                             | Some e when compare_expr e (mk_py_object (Addr_env.addr_false (), Some (mk_int 0 ~typ:T_int e.erange)) e.erange) = 0 ->
   *                                (ftrue, true, OptionExt.apply (fun flow' -> Some (Flow.join man.lattice flow flow')) (Some flow) oflow)
   *                             | _ -> (ftrue, ffalse, oflow)
   *                         ) c (false, false, None) in
   *     let () = debug "t = %b, f = %b" t f in
   *     if t && f then
   *       man.eval (mk_py_top T_bool range) (OptionExt.none_to_exn oflow)
   *     else
   *       c
   *   else c *)


  let eval zs exp (man: ('a, unit, 's) man) (flow: 'a flow) =
    let range = erange exp in
    match ekind exp with
    | E_constant (C_top T_bool) ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_bool_top, Some (mk_top T_int range)) range) flow |> OptionExt.return

    | E_constant (C_bool true) ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_true, Some (mk_int 1 ~typ:T_int range)) range) flow |> OptionExt.return

    | E_constant (C_bool false) ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_false, Some (mk_int 0 ~typ:T_int range)) range) flow |> OptionExt.return

    | E_constant (C_top T_int)
    | E_constant (C_int _)
    | E_constant (C_int_interval _) ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_integers, Some {exp with etyp=T_int}) range) flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "bool", _)}, _)}, [arg], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bool.__new__", _))}, _)}, [_; arg], []) ->
      (* According to the documentation: By default, an object is
         considered true unless its class defines either a __bool__()
         method that returns False or a __l en__() method that returns
         zero, when called with the object.  *)
      man.eval arg flow |>
      Eval.bind (fun earg flow ->
          assume (mk_py_isinstance_builtin earg "bool" range)
            ~fthen:(Eval.singleton earg)
            ~felse:(fun flow ->
                assume
                  (mk_py_hasattr earg "__bool__" range)
                  ~fthen:(fun flow ->
                      let attr = mk_py_attr earg "__bool__" range in
                      man.eval (mk_py_call attr [] range) flow
                    )
                  ~felse:(fun flow ->
                      assume
                        (mk_py_hasattr earg "__len__" range)
                        ~fthen:(fun flow ->
                            let attr = mk_py_attr earg "__len__" range in
                            let comp = mk_binop (mk_py_call attr [] range) O_ne (mk_zero range) range in
                            man.eval comp flow)
                        ~felse:(fun flow ->
                            man.eval (mk_py_true range) flow)
                        man flow
                    )
                  man flow
              )
            man flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("NoneType.__bool__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["NoneType"] (fun eargs flow ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__new__", _))}, _)}, [cls], []) ->
      Utils.new_wrapper man range flow "int" cls
        ~fthennew:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range))

    | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__new__", _))}, _)} as f), [cls; arg], []) ->
      Utils.new_wrapper man range flow "int" cls
        ~fthennew:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_call(f, [cls; arg; mk_int 10 range], [])})

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__new__", _))}, _)}, [cls; str; base], []) ->
      Utils.new_wrapper man range flow "int" cls
        ~fthennew:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range))

    (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f,  _))}, _)}, [e1; e2], [])
      when is_compare_op_fun "int" f ->
      bind_list [e1; e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_partitioning a = a.addr_partitioning in
          match addr_partitioning @@ fst @@ object_of_expr e1, addr_partitioning @@ fst @@ object_of_expr e2 with
          | Addr_env.G_py_bool (Some b1), Addr_env.G_py_bool (Some b2) ->
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_bool (b1 = b2) range) flow
          | _ ->
            assume
              (mk_py_isinstance_builtin e1 "int" range)
              ~fthen:(fun true_flow ->
                  assume
                    (mk_py_isinstance_builtin e2 "int" range)
                    ~fthen:(fun true_flow ->
                        (* FIXME: best way? *)
                      assume
                          (mk_binop (Utils.extract_oobject e1) (Operators.methfun_to_binop f) (Utils.extract_oobject e2) ~etyp:T_int range) man true_flow
                          ~zone:Universal.Zone.Z_u_int
                          ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> man.eval (mk_py_false range) flow)
                      (* |> merge_tf_top man range *)
                          (* man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range) true_flow *)
                      )
                    ~felse:(fun false_flow ->
                        let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                    man true_flow
                )
              ~felse:(fun false_flow ->
                  Format.fprintf Format.str_formatter "descriptor '%s' requires a 'int' object but received '%a'" f pp_expr e1;
                  let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) false_flow in
                  Eval.empty_singleton flow)
              man flow
        )
      |>  OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e1; e2], [])
      when is_arith_binop_fun "int" f ->
      bind_list [e1; e2] (man.eval~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          assume
            (mk_py_isinstance_builtin e1 "int" range)
            ~fthen:(fun true_flow ->
                assume
                  (mk_py_isinstance_builtin e2 "int" range)
                  ~fthen:(fun true_flow ->
                      match f with
                      | "int.__truediv__"
                      | "int.__rtruediv__" ->
                         assume
                           (mk_binop (if is_reverse_operator f then e1 else e2) O_eq (mk_zero range) range)
                           man true_flow
                           ~fthen:(fun flow ->
                             man.exec (Utils.mk_builtin_raise_msg "ZeroDivisionError" "division by zero" range) flow |> Eval.empty_singleton
                           )
                           ~felse:(fun flow ->
                             if is_reverse_operator f then
                               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_float, Some (mk_binop (Utils.extract_oobject e2) (Operators.methfun_to_binop f) (Utils.extract_oobject e1) range ~etyp:(T_float F_DOUBLE))) range) flow
                             else
                               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_float, Some (mk_binop (Utils.extract_oobject e1) (Operators.methfun_to_binop f) (Utils.extract_oobject e2) range ~etyp:(T_float F_DOUBLE))) range) flow
                           )
                      | _ ->
                         let res =
                           fun flow ->
                           if is_reverse_operator f then
                             Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_integers, Some (mk_binop (Utils.extract_oobject e2) (match Operators.methfun_to_binop f with
                                                                                                                                                     | O_py_floor_div -> O_div
                                                                                                                                                     | x -> x) (Utils.extract_oobject e1) range ~etyp:T_int)) range) flow
                           else
                             Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_integers, Some (mk_binop (Utils.extract_oobject e1) (match Operators.methfun_to_binop f with
                                                                                                                                                     | O_py_floor_div -> O_div
                                                                                                                                                     | x -> x) (Utils.extract_oobject e2) range ~etyp:T_int)) range) flow in
                         if is_arith_div_fun "int" f then
                           assume (mk_binop (if is_reverse_operator f then e1 else e2) O_eq (mk_zero range) range) man flow
                             ~fthen:(fun flow ->
                               man.exec (Utils.mk_builtin_raise_msg "ZeroDivisionError" "integer division or modulo by zero" range) flow |> Eval.empty_singleton
                             )
                             ~felse:res
                         else res flow

                  )
                  ~felse:(fun false_flow ->
                      let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                  man true_flow
              )
            ~felse:(fun false_flow ->
                Format.fprintf Format.str_formatter "descriptor '%s' requires a 'int' object but received '%a'" f pp_expr e1;
                let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) false_flow in
                Eval.empty_singleton flow)
            man flow
        )
      |>  OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e], [])
      when is_arith_unop_fun f ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
      Eval.bind (fun el flow ->
          assume
            (mk_py_isinstance_builtin e "int" range)
            ~fthen:(fun true_flow ->
                Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_integers, Some (mk_unop (Operators.methfun_to_unop f) (Utils.extract_oobject el) range ~etyp:T_int)) range) true_flow)
            ~felse:(fun false_flow ->
                let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
            man flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__bool__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["int"]
        (fun e flow ->
           (* FIXME: best way? *)
           assume
             (mk_binop (Utils.extract_oobject @@ List.hd e) O_eq (mk_int 0 ~typ:T_int range) ~etyp:T_int range) man flow
             ~zone:Universal.Zone.Z_u_int
             ~fthen:(fun flow -> man.eval (mk_py_false range) flow)
             ~felse:(fun flow -> man.eval (mk_py_true range) flow)
           (* |> merge_tf_top man range *)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__str__" as f, _))}, _)}, args, [])
    (* todo: weird, tp_str set to 0 in longobject.c *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__repr__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["int"]
        (fun _ flow -> man.eval (mk_py_top T_string range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("int.__float__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["int"]
        (fun e flow ->
          let max_intfloat = mk_constant (C_int (Z.of_string "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791")) ~etyp:T_int range in
          assume ~zone:Universal.Zone.Z_u_int
            (mk_binop
               (mk_binop (mk_unop O_minus max_intfloat ~etyp:T_int range) O_le  (Utils.extract_oobject @@ List.hd e) range)
               O_log_and
               (mk_binop (Utils.extract_oobject @@ List.hd e) O_le max_intfloat range)
               range)
            man flow
          ~fthen:(fun flow ->
            man.eval ~zone:(Universal.Zone.Z_u, Universal.Zone.Z_u_float) (mk_unop O_cast  ~etyp:(T_float F_DOUBLE) (Utils.extract_oobject @@ List.hd e) range) flow |>
              Eval.bind (fun e flow -> Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_float, Some e) range) flow))
          ~felse:(fun flow ->
            man.exec (Utils.mk_builtin_raise_msg "OverflowError" "int too large to convert to float" range) flow |> Eval.empty_singleton)
        ) |> OptionExt.return
    | _ -> None

  let exec _ _ _ _ = None
  let ask _ _ _ = None
end

let () = register_stateless_domain (module Domain)
