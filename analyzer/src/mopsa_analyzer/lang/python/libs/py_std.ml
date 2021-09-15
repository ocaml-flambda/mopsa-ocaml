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
open Sig.Abstraction.Stateless
open Addr
open Ast
open Universal.Ast
open MapExt

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.libs.stdlib"
    end)

  let checks = []

  type stub_signature = {in_args: string list;
                         out_type: Mopsa.typ}
  type stub_db = stub_signature StringMap.t

  let add_signature funname in_args out_type db =
      let out_type = match out_type with
        | "bool" -> T_py (Some Bool)
        | "int" -> T_py (Some Int)
        | "float" -> T_py (Some (Float F_DOUBLE))
        | "str" -> T_py (Some Str)
        | "NoneType" -> T_py (Some NoneType)
        | "NotImplementedType" -> T_py (Some NotImplemented)
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

  let exec _ _ _ = None

  let eval exp man flow =
    let range = exp.erange in
    match ekind exp with
    (* Calls to iter built-in function *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("iter", _))}, _)},
                [obj], []) ->
      (* Check that the class of obj has an attribute __iter__ *)
      man.eval obj flow >>$
        (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow >>$
            (fun cls' flow ->
              let cls = object_of_expr cls' in
              assume
                (Utils.mk_object_hasattr cls "__iter__" range)
                ~fthen:(fun true_flow ->
                    (* Call iter and check that it returns an object with an attribute __next__ *)
                    man.eval (mk_py_call (mk_py_object_attr cls "__iter__" range) [eobj] range) true_flow >>$
                    (fun iter flow ->
                        assume
                          (Utils.mk_hasattr iter "__next__" range)
                          ~fthen:(fun true_flow ->
                            Flow.add_safe_check Alarms.CHK_PY_TYPEERROR iter.erange true_flow |>
                            Eval.singleton iter)
                          ~felse:(fun false_flow ->
                            let msg = Format.asprintf "iter() returned non-iterator of type '%a'" pp_addr_kind (akind @@ fst @@ object_of_expr iter) in
                            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                              Eval.empty)
                          man flow
                      )
                  )
                ~felse:(fun false_flow ->
                    let msg = Format.asprintf "'%a' object is not iterable" pp_addr_kind (akind @@ fst cls) in
                    man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                    Eval.empty)
                man flow
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("iter", _))}, _)},
                [callable; sentinel], []) ->
       man.eval (Utils.mk_builtin_call "callable_iterator" [callable; sentinel] range) flow |> OptionExt.return


    (* Calls to len built-in function *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("len", _))}, _)},
                [obj], [])  ->
      (* Check that the class of obj has an attribute __len__ *)
      man.eval obj flow >>$
        (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow >>$
            (fun cls flow ->
              let cls = object_of_expr cls in
              assume
                (Utils.mk_object_hasattr cls "__len__" range)
                ~fthen:(fun true_flow ->
                    (* Call __len__ and check that it returns an integer *)
                    man.eval (mk_py_call (mk_py_object_attr cls "__len__" range) [eobj] range) true_flow >>$
                    (fun len flow ->
                        assume
                          (mk_py_isinstance_builtin len "int" range)
                          ~fthen:(fun true_flow ->
                            Flow.add_safe_check Alarms.CHK_PY_TYPEERROR len.erange true_flow |>
                              Eval.singleton len)
                          ~felse:(fun false_flow ->
                              let msg = Format.asprintf "'%a' object cannot be interpreted as an integer" pp_addr_kind (akind @@ fst @@ object_of_expr len) in
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                              Eval.empty)
                          man flow
                      )
                  )
                ~felse:(fun false_flow ->
                  let msg = Format.asprintf "object of type '%a' has no len()" pp_addr_kind (akind @@ fst cls) in
                  man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                    Eval.empty)
                man flow
            )
        )
      |> OptionExt.return

    (* Calls to built-in function next *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("next", _))}, _)},
                [obj], [])  ->
      (* Check that the class of obj has an attribute __next__ *)
      man.eval obj flow >>$
        (fun eobj flow ->
          man.eval (mk_py_type eobj range) flow >>$
          (fun cls flow ->
              let cls = object_of_expr cls in
              assume
                (Utils.mk_object_hasattr cls "__next__" range)
                ~fthen:(fun true_flow ->
                    Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range true_flow |>
                    man.eval (mk_py_call (mk_py_object_attr cls "__next__" range) [eobj] range)
                  )
                ~felse:(fun false_flow ->
                  let msg = Format.asprintf "'%a' object is not an iterator" pp_addr_kind (akind @@ fst cls) in
                    man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                    Eval.empty)
                man flow
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("input", _))}, _)}, args, [])  ->
       let tyerror = fun flow ->
         let msg = Format.asprintf "input expected at most 1 arguments, got %d" (List.length args) in
         man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>% Eval.empty in
       if List.length args <= 1 then
         Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |> man.eval (mk_py_top T_string range) |> OptionExt.return
      else
        tyerror flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("sum", _))}, _)} as call, [els], [])  ->
      let args' = els :: (mk_zero ~typ:(T_py None) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, args', [])} flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("sum", _))}, _)}, [ els; start ], [])  ->
      (* let's desugar sum into tmp = 0; for x in els: tmp = tmp + x; tmp *)
      let counter = mk_range_attr_var range "counter" (T_py None) in
      let counter_var = mk_var counter range in
      let target = mk_range_attr_var range "target" (T_py None) in
      let target_var = mk_var target range in
      let assign = mk_assign counter_var (mk_zero ~typ:(T_py None) range) range in
      let pass = mk_block [] range in
      let for_loop = mk_stmt (S_py_for (target_var, els,
                                        mk_assign counter_var (mk_binop ~etyp:(T_py None) counter_var O_plus target_var range) range,
                                        pass)) range in
      let stmt = mk_block (assign :: for_loop :: []) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
      man.exec stmt flow >>%
      man.eval counter_var |>
      Cases.add_cleaners [mk_remove_var counter range; mk_remove_var target range] |>
      OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (s, _))}, _)}, [e1; e2], []) when s = "max" || s = "min" ->
      (* desugaring max(e1, e2) into e1 if e1 > e2 else e2 *)
      let comp_op = if s = "max" then O_gt else O_lt in
      man.eval   (mk_expr ~etyp:(T_py None)
                                                   (E_py_if
                                                      (mk_binop ~etyp:(T_py None) e1 comp_op e2 range,
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
      let iter = mk_range_attr_var range "iter" (T_py None) in
      let iter_var = mk_var iter range in
      let maxi = mk_range_attr_var range "extrema" (T_py None) in
      let maxi_var = mk_var maxi range in
      let target = mk_range_attr_var range "target" (T_py None) in
      let target_var = mk_var target range in

      let cleaners = List.map (fun x -> mk_remove_var x range) [iter; maxi; target] in
      let pass = mk_block [] range in

      let assign_iter = mk_assign iter_var (Utils.mk_builtin_call "iter" [{iterable with erange = tag_range iterable.erange "assign_iter"}] (tag_range range "assign_iter")) (tag_range range "assign_iter") in
      let msg = Format.asprintf "%s() arg is an empty sequence" s in
      let assign_max =
        (* FIXME: safe check ? *)
        Utils.mk_try_stopiteration
          (mk_assign maxi_var (Utils.mk_builtin_call "next" [iter_var] range) range)
          (Utils.mk_builtin_raise_msg "ValueError" msg range)
          range in
      let for_stmt = mk_stmt (S_py_for (target_var, iter_var,
                                        mk_if (mk_binop ~etyp:(T_py None) target_var comp_op maxi_var range)
                                          (mk_assign maxi_var target_var range)
                                          pass range
                                       , pass)) range in
      let stmt = mk_block (assign_iter :: assign_max :: for_stmt :: []) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
      man.exec stmt flow >>%
      man.eval maxi_var |>
      Cases.add_cleaners cleaners |>
      OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("max", _))}, _)}, [e1; e2], []) ->
      (* desugaring max(e1, e2) into if e1 > e2 then e1 else e2 *)
      let expr = mk_expr ~etyp:(T_py None) (E_py_if (mk_binop ~etyp:(T_py None) e1 O_gt e2 range, e1, e2)) range in
      debug "Rewriting %a into %a@\n" pp_expr exp pp_expr expr;
      man.eval expr flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("print", _))}, _)}, [], [])  ->
      man.eval (mk_py_none range) flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("print", _))}, _)}, objs, [])  ->
      bind_list objs (man.eval  ) flow |>
      bind_result (fun eobj flow -> man.eval (mk_py_none range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("hash", _))}, _)}, args, []) ->
      let tyerror = fun flow ->
        let msg = Format.asprintf "hash() takes exactly one argument (%d given)" (List.length args) in
        man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>% Eval.empty in
      Cases.bind_list args man.eval flow |>
      Cases.bind_result (fun eargs flow ->
          if List.length eargs <> 1 then tyerror flow else
            let el = List.hd eargs in
            Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
            man.eval (mk_py_call (mk_py_object_attr (object_of_expr el) "__hash__" range) [] range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("repr", _))}, _)}, [v], [])  ->
      man.eval (mk_py_type v range) flow >>$
      (fun etype flow ->
          assume
            (mk_py_hasattr etype "__repr__" range)
            man flow
            ~fthen:(fun flow ->
                man.eval (mk_py_call (mk_py_attr etype "__repr__" range) [v] range) flow >>$
                  (fun repro flow ->
                    assume (mk_py_isinstance_builtin repro "str" range) man flow
                      ~fthen:(fun flow ->
                        Flow.add_safe_check Alarms.CHK_PY_TYPEERROR repro.erange flow |>
                        Eval.singleton repro)
                      ~felse:(fun flow ->  man.exec (Utils.mk_builtin_raise_msg "TypeError" "__repr__ returned non-string" range) flow >>% Eval.empty)
                  )
              )
            ~felse:(fun flow ->
              (* there is a default implementation saying "<%s object at %p>" % (name(type(v)), v as addr I guess *)
              Flow.add_safe_check Alarms.CHK_PY_TYPEERROR etype.erange flow |>
                man.eval (mk_py_top T_string range)
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
      let seq = mk_range_attr_var range "sorted" (T_py None) in
      man.exec (mk_assign (mk_var seq range) obj range) flow >>%
      man.eval (Utils.mk_builtin_call "list.sort" [mk_var seq range] range) >>$
        (fun _ flow ->
          man.eval (mk_var seq range) flow |>
          Cases.add_cleaners [mk_remove_var seq range]
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("cell.__init__", _))}, _)}, args, []) ->
       let post =
         if List.length args > 1 then
           man.exec (mk_assign (mk_py_attr (List.hd args) "cell_contents" range) (List.nth args 1) range) flow
         else Post.return flow in
       post >>% man.eval (mk_py_none range) |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("cell.__getattribute__", _))}, _)}, cell::attr::[], []) ->
       let cell_contents = (mk_string ~etyp:(T_py None) "cell_contents" range) in
       assume (eq attr cell_contents ~etyp:(T_py None) range) man flow
         ~fthen:(fun flow ->
           assume (mk_py_ll_hasattr cell cell_contents range) man flow
             ~fthen:(fun flow -> man.eval (mk_py_ll_getattr cell cell_contents range) flow)
             ~felse:(fun flow ->
               man.exec (Utils.mk_builtin_raise "NameError" range) flow >>% Eval.empty
             )
         )
         ~felse:(fun flow ->
           man.eval (mk_py_call (mk_py_object (find_builtin "object.__getattribute__") range) [cell;attr] range) flow
         )
       |> OptionExt.return

    (* FIXME: in libs.py_std or flows.exn? *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("BaseException.__init__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:(List.length args - 1) f man flow range args ["BaseException"]
        (fun args flow ->
           let exc = List.hd args in
           let flow = if List.length args = 1 then flow else
                        man.exec (mk_assign
                                    (mk_py_attr exc "args" range)
                           (mk_expr ~etyp:(T_py None) (E_py_tuple (List.tl args)) range) range) flow |> post_to_flow man in
              man.eval   (mk_py_none range) flow
        )
      |> OptionExt.return

    (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "bin")}, _)}, args, [])  ->
     *   Utils.check_instances man flow range args ["int"]
     *     (fun _ flow -> man.eval (mk_py_top T_string range) flow)
     *   |> OptionExt.return *)

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("abs", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_result (fun eargs flow ->
          if List.length eargs <> 1 then
            let msg = Format.asprintf "abs() takes exactly one argument (%d given)" (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>% Eval.empty
          else
            let v = List.hd eargs in
            assume (mk_py_isinstance_builtin v "int" range) man flow
              ~fthen:(fun flow ->
                let flow = Flow.add_safe_check Alarms.CHK_PY_TYPEERROR v.erange flow in
                assume (mk_binop ~etyp:(T_py None) v O_ge (mk_int 0 ~typ:(T_py None) range) range) man flow
                  ~fthen:(fun flow ->
                    man.eval (Utils.mk_builtin_call "int" [v] range) flow)
                  ~felse:(fun flow -> man.eval (mk_unop ~etyp:(T_py None) O_minus v range) flow)
                )
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin v "float" range) man flow
                    ~fthen:(fun flow ->
                      let flow = Flow.add_safe_check Alarms.CHK_PY_TYPEERROR v.erange flow in
                      assume (mk_binop ~etyp:(T_py None) v O_ge {(mk_float 0. range) with etyp=(T_py (Some (Float F_DOUBLE)))} range) man flow
                        ~fthen:(fun flow -> man.eval v flow)
                        ~felse:(fun flow -> man.eval (mk_unop ~etyp:(T_py None) O_minus v range) flow)
                    )
                    ~felse:(fun flow ->
                      let msg = Format.asprintf "bad operand type for abs()" in  (* FIXME *)
                        man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                        Eval.empty
                      )
                )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("all" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["list"] (fun _ -> man.eval (mk_py_top (T_py (Some Bool)) range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("round", _))}, _)}, args, []) ->
       man.eval (List.hd args) flow >>$ (fun number flow ->
       assume (mk_py_hasattr number "__round__" range) man flow
         ~fthen:(fun flow ->
           man.eval (mk_py_call (mk_py_attr number "__round__" ~etyp:(T_py None) range) [] range) flow
         )
         ~felse:(fun flow ->
           man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.asprintf "type %a doesn't define __round__ method" pp_expr number) range) flow >>% Eval.empty
         )
      )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("divmod", _))}, _)}, args, []) ->
      (* FIXME: error messages etc *)
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow >>% Eval.empty in
      bind_list args man.eval flow |>
      bind_result (fun eargs flow ->
          if List.length args <> 2 then tyerror flow else
            let argl, argr = match eargs with l::r::[] -> l, r | _ -> assert false in
            assume (mk_py_isinstance_builtin argl "int" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance_builtin argr "int" range) man flow
                    ~fthen:(fun flow ->
                      Flow.add_safe_check Alarms.CHK_PY_TYPEERROR argr.erange flow |>
                        man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [
                                                                 mk_binop ~etyp:(T_py None) argl O_py_floor_div argr range;
                                                                 mk_binop ~etyp:(T_py None) argl O_mod argr range
                                    ]) range))
                    ~felse:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "float" range) man flow
                          ~fthen:(fun flow ->
                            Flow.add_safe_check Alarms.CHK_PY_TYPEERROR argr.erange flow |>
                            man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:tyerror
                      )
                )
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin argl "float" range) man flow
                    ~fthen:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "int" range) man flow
                          ~fthen:(fun flow ->
                            Flow.add_safe_check Alarms.CHK_PY_TYPEERROR argr.erange flow |>
                            man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:(fun flow ->
                              assume (mk_py_isinstance_builtin argr "float" range) man flow
                                ~fthen:(fun flow ->
                                  Flow.add_safe_check Alarms.CHK_PY_TYPEERROR argr.erange flow |>
                                  man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                                ~felse:tyerror
                            )

                      )
                    ~felse:tyerror
                )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("id", _))}, _)}, [x], []) ->
       man.eval (mk_py_top T_int range) flow |> OptionExt.return

    | _ ->
      None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end

let () = register_stateless_domain (module Domain)
