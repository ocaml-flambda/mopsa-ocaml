(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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

(** Python standard library. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Addr
open Ast
open Universal.Ast
open MapExt

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.libs.stdlib"
    end)

  let interface = {
    iexec = { provides = []; uses = [Zone.Z_py] };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj] }
  }

  let alarms = []

  type stub_signature = {in_args: string list;
                         out_type: Mopsa.typ}
  type stub_db = stub_signature StringMap.t

  let add_signature funname in_args out_type db =
    let out_type = match out_type with
      | "bool" -> T_bool
      | "int" -> T_int
      | "float" -> T_float F_DOUBLE
      | "str" -> T_string
      | "NoneType" -> T_py_none
      | "NotImplementedType" -> T_py_not_implemented
      | _ -> assert false in
    StringMap.add funname {in_args; out_type} db

  let stub_base =
    StringMap.empty |>
    add_signature "bin" ["int"] "str" |>
    add_signature "chr" ["int"] "str" |>
    add_signature "ord" ["str"] "int" |>
    add_signature "hex" ["int"] "str"

  let process_simple f man flow range exprs instances return =
    Utils.check_instances f man flow range exprs instances (fun _ flow -> man.eval (mk_py_top return range) flow)

  let init _ _ flow = flow

  let exec _ _ _ _ = None

  let eval zones exp man flow =
    let range = exp.erange in
    match ekind exp with
    (* Calls to iter built-in function *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("iter", _))}, _)},
                [obj], []) ->
      (* Check that the class of obj has an attribute __iter__ *)
      man.eval obj flow |>
      Eval.bind (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow |>
          Eval.bind (fun cls' flow ->
              let cls = object_of_expr cls' in
              assume
                (Utils.mk_object_hasattr cls "__iter__" range)
                ~fthen:(fun true_flow ->
                    (* Call iter and check that it returns an object with an attribute __next__ *)
                    man.eval (mk_py_call (mk_py_object_attr cls "__iter__" range) [eobj] range) true_flow |>
                    Eval.bind (fun iter flow ->
                        assume
                          (Utils.mk_hasattr iter "__next__" range)
                          ~fthen:(fun true_flow -> Eval.singleton iter true_flow)
                          ~felse:(fun false_flow ->
                              Format.fprintf Format.str_formatter "iter() returned non-iterator of type '%a'" pp_addr_kind (akind @@ fst @@ object_of_expr iter);
                              let msg = Format.flush_str_formatter () in
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow |>
                              Eval.empty_singleton)
                          man flow
                      )
                  )
                ~felse:(fun false_flow ->
                    Format.fprintf Format.str_formatter "'%a' object is not iterable" pp_addr_kind (akind @@ fst cls);
                    let msg = Format.flush_str_formatter () in
                    man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow |>
                    Eval.empty_singleton)
                man flow
            )
        )
      |> OptionExt.return

    (* Calls to len built-in function *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("len", _))}, _)},
                [obj], [])  ->
      (* Check that the class of obj has an attribute __len__ *)
      man.eval obj flow |>
      Eval.bind (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow |>
          Eval.bind (fun cls flow ->
              let cls = object_of_expr cls in
              assume
                (Utils.mk_object_hasattr cls "__len__" range)
                ~fthen:(fun true_flow ->
                    (* Call __len__ and check that it returns an integer *)
                    man.eval (mk_py_call (mk_py_object_attr cls "__len__" range) [eobj] range) true_flow |>
                    Eval.bind (fun len flow ->
                        assume
                          (mk_py_isinstance_builtin len "int" range)
                          ~fthen:(fun true_flow ->
                              Eval.singleton len true_flow)
                          ~felse:(fun false_flow ->
                              Format.fprintf Format.str_formatter "'%a' object cannot be interpreted as an integer" pp_addr_kind (akind @@ fst @@ object_of_expr len);
                              let msg = Format.flush_str_formatter () in
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow |>
                              Eval.empty_singleton)
                          man flow
                      )
                  )
                ~felse:(fun false_flow ->
                    Format.fprintf Format.str_formatter "object of type '%a' has no len()" pp_addr_kind (akind @@ fst cls);
                    man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) false_flow |>
                    Eval.empty_singleton)
                man flow
            )
        )
      |> OptionExt.return

    (* Calls to built-in function next *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("next", _))}, _)},
                [obj], [])  ->
      (* Check that the class of obj has an attribute __next__ *)
      man.eval obj flow |>
      Eval.bind (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow |>
          Eval.bind (fun cls flow ->
              let cls = object_of_expr cls in
              assume
                (Utils.mk_object_hasattr cls "__next__" range)
                ~fthen:(fun true_flow ->
                    man.eval (mk_py_call (mk_py_object_attr cls "__next__" range) [obj] range) true_flow
                  )
                ~felse:(fun false_flow ->
                    Format.fprintf Format.str_formatter "'%a' object is not an iterator" pp_addr_kind (akind @@ fst cls);
                    man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) false_flow |>
                    Eval.empty_singleton)
                man flow
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("input", _))}, _)}, args, [])  ->
      let tyerror = fun flow ->
        Format.fprintf Format.str_formatter "input expected at most 1 arguments, got %d" (List.length args);
        man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton in
      if List.length args <= 1 then
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_string range) flow |> OptionExt.return
      else
        tyerror flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("sum", _))}, _)} as call, [els], [])  ->
      let args' = els :: (mk_constant T_int (C_int (Z.of_int 0)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, args', [])} flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("sum", _))}, _)}, [ els; start ], [])  ->
      (* let's desugar sum into tmp = 0; for x in els: tmp = tmp + x; tmp *)
      let counter = mk_range_attr_var range "counter" T_any in
      let counter_var = mk_var counter range in
      let target = mk_range_attr_var range "target" T_any in
      let target_var = mk_var target range in
      let assign = mk_assign counter_var (mk_constant T_int (C_int (Z.of_int 0)) range) range in
      let pass = mk_block [] range in
      let for_loop = mk_stmt (S_py_for (target_var, els,
                                        mk_assign counter_var (mk_binop counter_var O_plus target_var range) range,
                                        pass)) range in
      let stmt = mk_block (assign :: for_loop :: []) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
      man.exec stmt flow |>
      man.eval counter_var |>
      Eval.add_cleaners [mk_remove_var counter range; mk_remove_var target range] |>
      OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (s, _))}, _)}, [e1; e2], []) when s = "max" || s = "min" ->
      (* desugaring max(e1, e2) into e1 if e1 > e2 else e2 *)
      let comp_op = if s = "max" then O_gt else O_lt in
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr
                                                   (E_py_if
                                                      (mk_binop e1 comp_op e2 range,
                                                       e1,
                                                       e2)
                                                   ) range) flow |>
      OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (s, _))}, _)}, [iterable], []) when s = "max" || s = "min" ->
      (* desugaring max(iterable) into:
       *    iter_var = iter(iterable)
       *    maxi_var = next(iter_var)
       *    for target_var in iter_var:
       *        if target_var > maxi_var:
       *            maxi_var = target_var *)
      let comp_op = if s = "max" then O_gt else if s = "min" then O_lt else assert false in
      let iter = mk_range_attr_var range "iter" T_any in
      let iter_var = mk_var iter range in
      let maxi = mk_range_attr_var range "extrema" T_any in
      let maxi_var = mk_var maxi range in
      let target = mk_range_attr_var range "target" T_any in
      let target_var = mk_var target range in

      let cleaners = List.map (fun x -> mk_remove_var x range) [iter; maxi; target] in
      let pass = mk_block [] range in

      let assign_iter = mk_assign iter_var (Utils.mk_builtin_call "iter" [iterable] range) range in
      Format.fprintf Format.str_formatter "%s() arg is an empty sequence" s;
      let assign_max =
        Utils.mk_try_stopiteration
          (mk_assign maxi_var (Utils.mk_builtin_call "next" [iter_var] range) range)
          (Utils.mk_builtin_raise_msg "ValueError" (Format.flush_str_formatter ()) range)
          range in
      let for_stmt = mk_stmt (S_py_for (target_var, iter_var,
                                        mk_if (mk_binop target_var comp_op maxi_var range)
                                          (mk_assign maxi_var target_var range)
                                          pass range
                                       , pass)) range in
      let stmt = mk_block (assign_iter :: assign_max :: for_stmt :: []) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
      man.exec stmt flow |>
      man.eval maxi_var |>
      Eval.add_cleaners cleaners |>
      OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("max", _))}, _)}, [e1; e2], []) ->
      (* desugaring max(e1, e2) into if e1 > e2 then e1 else e2 *)
      let expr = mk_expr (E_py_if (mk_binop e1 O_gt e2 range, e1, e2)) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_expr expr;
      man.eval expr flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("print", _))}, _)}, [], [])  ->
      man.eval (mk_py_none range) flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("print", _))}, _)}, objs, [])  ->
      bind_list objs (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun eobj flow -> man.eval (mk_py_none range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("hash", _))}, _)}, args, []) ->
      let tyerror = fun flow ->
        Format.fprintf Format.str_formatter "hash() takes exactly one argument (%d given)" (List.length args);
        man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton in
      Cases.bind_list args man.eval flow |>
      Cases.bind_some (fun eargs flow ->
          if List.length eargs <> 1 then tyerror flow else
            let el = List.hd eargs in
            man.eval (mk_py_call (mk_py_object_attr (object_of_expr el) "__hash__" range) [] range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("repr", _))}, _)}, [v], [])  ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type v range) flow |>
      Eval.bind (fun etype flow ->
          assume
            (mk_py_hasattr etype "__repr__" range)
            man flow
            ~fthen:(fun flow ->
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr etype "__repr__" range) [] range) flow |>
                Eval.bind (fun repro flow ->
                    assume (mk_py_isinstance_builtin repro "str" range) man flow
                      ~fthen:(Eval.singleton repro)
                      ~felse:(fun flow ->  man.exec (Utils.mk_builtin_raise_msg "TypeError" "__repr__ returned non-string" range) flow |> Eval.empty_singleton)
                  )
              )
            ~felse:(
              (* there is a default implementation saying "<%s object at %p>" % (name(type(v)), v as addr I guess *)
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_string range)
            )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, []) when StringMap.mem f stub_base ->
      debug "function %s in stub_base, processing@\n" f;
      let {in_args; out_type} = StringMap.find f stub_base in
      process_simple f man flow range args in_args out_type
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("sorted", _))}, _)}, [obj], [])  ->
      (* todo: call list on obj first *)
      let seq = mk_range_attr_var range "sorted" T_any in
      let flow = man.exec (mk_assign (mk_var seq range) obj range) flow in
      man.eval (Utils.mk_builtin_call "list.sort" [mk_var seq range] range) flow |>
      Eval.bind (fun _ flow ->
          man.eval (mk_var seq range) flow |>
          Eval.add_cleaners [mk_remove_var seq range]
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "reversed", _)}, _)}, args, [])  ->
      (* FIXME: reversed_new_impl *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr (List.hd args) "__reversed__" range) [] range) flow
      (* man.eval (Utils.mk_builtin_call "list.__reversed__" args range) flow *)
      |> OptionExt.return

    (* FIXME: in libs.py_std or flows.exn? *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("BaseException.__init__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:(List.length args - 1) f man flow range args ["BaseException"]
        (fun args flow ->
           let exc = List.hd args in
           let flow = if List.length args = 1 then flow else
               man.exec (mk_assign
                           (mk_py_attr exc "args" range)
                           (mk_expr (E_py_tuple (List.tl args)) range) range) flow
           in
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range) flow
        )
      |> OptionExt.return

    (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "bin")}, _)}, args, [])  ->
     *   Utils.check_instances man flow range args ["int"]
     *     (fun _ flow -> man.eval (mk_py_top T_string range) flow)
     *   |> OptionExt.return *)

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("abs", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length eargs <> 1 then
            let () = Format.fprintf Format.str_formatter "abs() takes exactly one argument (%d given)" (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton
          else
            let v = List.hd eargs in
            assume (mk_py_isinstance_builtin v "int" range) man flow
              ~fthen:(man.eval (mk_py_top T_int range))
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin v "float" range) man flow
                    ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                    ~felse:(fun flow ->
                        Format.fprintf Format.str_formatter "bad operand type for abs()";  (* FIXME *)
                        man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                        Eval.empty_singleton
                      )
                )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("all" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["list"] (fun _ -> man.eval (mk_py_top T_bool range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("round" as f, _))}, _)}, args, []) ->
      Utils.check_instances_disj f man flow range args [["float"; "int"]] (fun _ -> man.eval (mk_py_top T_int range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("divmod", _))}, _)}, args, []) ->
      (* FIXME: error messages etc *)
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length args <> 2 then tyerror flow else
            let argl, argr = match eargs with l::r::[] -> l, r | _ -> assert false in
            assume (mk_py_isinstance_builtin argl "int" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance_builtin argr "int" range) man flow
                    ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top T_int range; mk_py_top T_int range]) range))
                    ~felse:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "float" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:tyerror
                      )
                )
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin argl "float" range) man flow
                    ~fthen:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "int" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:(fun flow ->
                              assume (mk_py_isinstance_builtin argr "float" range) man flow
                                ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                                ~felse:tyerror
                            )

                      )
                    ~felse:tyerror
                )
        )
      |> OptionExt.return


    | _ ->
      None

  let ask _ _ _ = None

end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
