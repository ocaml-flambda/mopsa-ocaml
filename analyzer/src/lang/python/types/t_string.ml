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

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.types.t_string"
      end)

    type stub_signature = {in_args: string list;
                           out_type: Mopsa.typ}
    type stub_db = stub_signature StringMap.t

    let add_signature funname in_args out_type db =
      let out_type = match out_type with
        | "bool" -> T_bool
        | "int" -> T_int
        | "float" -> T_float F_DOUBLE
        | "str" -> T_string
        | "bytes" -> T_py_bytes
        | "NoneType" -> T_py_none
        | "NotImplementedType" -> T_py_not_implemented
        | _ -> assert false in
      StringMap.add funname {in_args; out_type} db

  let extract_oobject e = match ekind e with
    | E_py_object (_, Some a) -> a
    | _ -> assert false

  let stub_base =
      let str = "str" and int = "int" and bool = "bool" and bytes = "bytes" in
      StringMap.empty |>
      add_signature "str.__contains__" [str; str] bool |>
      add_signature "bytes.__len__" [bytes] int |>

      add_signature "str.capitalize" [str] str |>
      add_signature "str.casefold" [str] str |>
      add_signature "str.center" [str; int; str] str |>
      add_signature "str.count" [str; str] int |>
      add_signature "str.endswith" [str; str] bool |>
      add_signature "str.expandtabs" [str] str |>
      add_signature "str.find" [str; str] int |>
      add_signature "str.ljust" [str; int] str |>
      add_signature "str.lower" [str] str |>
      add_signature "str.lstrip" [str] str |>
      add_signature "str.replace" [str; str; str] str |>
      add_signature "bytes.replace" [bytes; bytes; bytes] bytes |>
      add_signature "str.rfind" [str; str] int |>
      add_signature "str.rjust" [str; int] str |>
      add_signature "str.rstrip" [str] str |>
      add_signature "str.startswith" [str; str] bool |>
      add_signature "str.strip" [str] str |>
      add_signature "str.swapcase" [str] str |>
      add_signature "str.title" [str] str |>
      add_signature "str.upper" [str] str |>
      add_signature "str.zfill" [str; int] str |>

      add_signature "str.isalnum" [str] bool |>
      add_signature "str.isalpha" [str] bool |>
      add_signature "str.isascii" [str] bool |>
      add_signature "str.isdecimal" [str] bool |>
      add_signature "str.isdigit" [str] bool |>
      add_signature "str.isidentifier" [str] bool |>
      add_signature "str.islower" [str] bool |>
      add_signature "str.isnumeric" [str] bool  |>
      add_signature "str.isprintable" [str] bool  |>
      add_signature "str.isspace" [str] bool |>
      add_signature "str.istitle" [str] bool |>
      add_signature "str.isupper" [str] bool


    let process_simple f man flow range exprs instances return =
      Utils.check_instances f man flow range exprs instances (fun _ flow -> man.eval (mk_py_top return range) flow)

    let alarms = []

    let init _ _ flow = flow

    let is_str_binop_fun = function
      (* FIXME: clean *)
      | "str.__add__"
        (* | "str.__mod__" *)
        (* | "str.__mul__" *)
        (* | "str.__rmod__" *)
        (* | "str.__rmul__" *)
            -> true
      | _ -> false


    let allocate_builtin ?(mode=STRONG) man range flow bltin oe =
      (* allocate addr, and map this addr to inst bltin *)
      let range = tag_range range "alloc_%s" bltin in
      let cls = fst @@ find_builtin bltin in
      man.eval ~route:(Semantic "U/Heap") (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow >>$
        (fun eaddr flow ->
          let addr = match ekind eaddr with
            | E_addr a -> a
            | _ -> assert false in
          man.exec ~route:(Semantic "Python") (mk_add eaddr range) flow >>%
          Eval.singleton (mk_py_object (addr, oe) range)
        )




    let rec eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_constant (C_string _)
      | E_constant (C_top T_string) ->
         Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_strings, Some {exp with etyp=T_string}) range) flow |> OptionExt.return

      | E_constant (C_top T_py_bytes)
      | E_py_bytes _ ->
        allocate_builtin man range flow "bytes" (Some exp) |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, []) when StringMap.mem f stub_base ->
        debug "function %s in stub_base, processing@\n" f;
        let {in_args; out_type} = StringMap.find f stub_base in
        process_simple f man flow range args in_args out_type
        |> OptionExt.return

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__new__", _))}, _)}), [cls; obj], []) ->
        (* from unicode_new in unicodeobject.c
           1) if type is not str then call unicode_subtype_new (not handled but asserts that type is subtype of str)
           2) other cases unhandled too
           3) call PyObject_Str if encoding and errors are set to null  ()
           4) otherwise call PyUnicode_FromEncodedObject (not handled) *)
        (* now, PyObject_Str defined in object.c:
           1) unhandled cases
           2) if no __str__ field call repr bltin / PyObject_Repr
           3) otherwise call __str__ and check return type to be str *)
        (* now PyObject_Repr defined in object.c:
           1) unhandled cases
           2) if no __repr__ field, there is a default implementation saying "<%s object at %p>" % (name(type(v)), v as addr I guess)
           3) otherwise call repr, check return type to be str *)
        (* short version: we handle call to __str__ and repr *)
        man.eval ~route:(Semantic "Python") (mk_py_type obj range) flow >>$
 (fun etype flow ->
            assume
              (mk_py_hasattr etype "__str__" range) man flow
              ~fthen:(fun flow ->
                  man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_attr etype "__str__" range) [obj] range) flow >>$
 (fun stro flow ->
                      assume (mk_py_isinstance_builtin stro "str" range) man flow
                        ~fthen:(Eval.singleton stro)
                        ~felse:(fun flow -> man.exec (Utils.mk_builtin_raise_msg "TypeError" "__str__ returned non-string" range) flow >>% Eval.empty_singleton)
                    )
                )
              ~felse:(man.eval ~route:(Semantic "Python") (mk_py_call (mk_py_object (find_builtin "repr") range) [obj] range))
          )
        |> OptionExt.return
        (* man.eval ~route:(Semantic "Python") (mk_py_top T_string range) flow |> OptionExt.return *)

      (* 𝔼⟦ str.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e1; e2], [])
        when is_compare_op_fun "str" f ->
        bind_list [e1; e2] (man.eval  ~route:(Semantic "Python")) flow |>
        bind_some (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            assume
              (mk_py_isinstance_builtin e1 "str" range)
              ~fthen:(fun true_flow ->
                  assume
                    (mk_py_isinstance_builtin e2 "str" range)
                    ~fthen:(fun true_flow ->
                      (* FIXME: best way? *)
                        assume
                          (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) ~etyp:T_int range) man true_flow
                          ~route:(Semantic "U/String")
                          ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> man.eval (mk_py_false range) flow)
                        (* |> T_int.Domain.merge_tf_top man range *)
                    (* man.eval ~route:(Semantic "Python") (mk_py_top T_bool range) true_flow *)
                    )
                    ~felse:(fun false_flow ->
                        let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                        man.eval ~route:(Semantic "Python") expr false_flow)
                    man true_flow
                )
              ~felse:(fun false_flow ->
                let msg = Format.asprintf "descriptor '%s' requires a 'str' object but received '%a'" f pp_expr e1 in
                man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                  Eval.empty_singleton )
              man flow
          )
        |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e1; e2], [])
        when is_str_binop_fun f ->
        bind_list [e1; e2] (man.eval  ~route:(Semantic "Python")) flow |>
        bind_some (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            assume
              (mk_py_isinstance_builtin e1 "str" range)
              ~fthen:(fun true_flow ->
                  assume
                    (mk_py_isinstance_builtin e2 "str" range)
                    ~fthen:(fun true_flow ->
                        man.eval ~route:(Semantic "Python") (mk_py_top T_string range) true_flow
                        (* fixme: just to perform the addr alloc, clearly not the best *)
                        >>$
 (fun res flow ->
                            match ekind res with
                            | E_py_object (addr, _) ->
                              Eval.singleton {res with ekind = E_py_object(addr,
                                                                           Some (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) range ~etyp:T_string))} flow
                            | _ -> assert false
                          )
                      )
                    ~felse:(fun false_flow ->
                        let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                        man.eval ~route:(Semantic "Python") expr false_flow)
                    man true_flow
                )
              ~felse:(fun false_flow ->
                let msg = Format.asprintf "descriptor '%s' requires a 'str' object but received '%a'" f pp_expr e1 in
                man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                  Eval.empty_singleton)
              man flow
          )
        |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__rmul__" as f, _))}, _)}, args, [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__mul__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args ["str"; "int"]
          (fun eargs flow ->
             let e1, e2 = match eargs with [a; b] -> a, b | _ -> assert false in
             man.eval ~route:(Semantic "Python") (mk_py_top T_string range) flow
             (* fixme: just to perform the addr alloc, clearly not the best *)
             >>$
 (fun res flow ->
                 match ekind res with
                 | E_py_object (addr, _) ->
                   Eval.singleton {res with ekind = E_py_object(addr,
                                                                Some (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) range ~etyp:T_string))} flow
                 | _ -> assert false
               )
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__mod__" as f, _))}, _)}, args, []) ->
        Utils.check_instances ~arguments_after_check:1 f man flow range args ["str"]
          (fun eargs flow ->
             (* TODO: constant strings are kept in the objects, so we could raise less alarms *)
             let tyerror_f = post_to_flow man (man.exec (Utils.mk_builtin_raise_msg "ValueError" "incomplete format" range) flow) in
             let flow = Flow.copy_ctx tyerror_f flow in
             let res = man.eval (mk_py_top T_string range) flow in
             let tyerror = tyerror_f |> Eval.empty_singleton in
             Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx res tyerror :: res :: [])
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__rmod__" as f, _))}, _)}, args, []) ->
        Utils.check_instances ~arguments_after_check:1 f man flow range args ["str"]
          (fun eargs flow ->
             (* TODO: constant strings are kept in the objects, so we could raise less alarms *)
             let tyerror_f = post_to_flow man (man.exec (Utils.mk_builtin_raise_msg "ValueError" "incomplete format" range) flow) in
             let flow = Flow.copy_ctx tyerror_f flow in
             let res = man.eval (mk_py_top T_string range) flow in
             let tyerror = tyerror_f |> Eval.empty_singleton in
             Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx res tyerror :: res :: [])
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__getitem__" as f, _))}, _)}, args, []) ->
        Utils.check_instances_disj f man flow range args
          [["str"]; ["int"; "slice"]]
          (fun _ flow -> man.eval (mk_py_top T_string range) flow)
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.__getitem__" as f, _))}, _)}, args, []) ->
        Utils.check_instances_disj f man flow range args
          [["bytes"]; ["int"; "slice"]]
          (fun _ flow -> man.eval (mk_py_top T_py_bytes range) flow)
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__iter__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["str"]
          (fun args flow ->
             let str = List.hd args in
             let it_addr = mk_alloc_addr (Objects.Py_list.A_py_iterator ("str_iterator", None)) range in
             man.eval ~route:(Semantic "U/Heap") it_addr flow >>$
 (fun eit_addr flow ->
                 let it_addr = match ekind eit_addr with E_addr a -> a | _ -> assert false in
                 man.exec ~route:(Semantic "Python") (mk_assign (mk_var (Objects.Py_list.Domain.itseq_of_addr it_addr) range) str range) flow >>%
                 Eval.singleton (mk_py_object (it_addr, None) range)
               )
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str_iterator.__next__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["str_iterator"]
          (fun _ flow ->
             let stopiteration_f = post_to_flow man (man.exec (Utils.mk_builtin_raise "StopIteration" range) flow) in
             let flow = Flow.copy_ctx stopiteration_f flow in
             let els = man.eval (mk_py_top T_string range) flow in
             let stopiteration = stopiteration_f |> Eval.empty_singleton in
             Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx els stopiteration :: els :: [])
          )
        |> OptionExt.return

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.encode", _))}, _)}) as caller, [arg], [])
      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.decode", _))}, _)}) as caller, [arg], []) ->
        let encoding = mk_string "utf-8" range in
        eval {exp with ekind = E_py_call(caller, [arg; encoding], [])} man flow

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.encode" as f, _))}, _)}), [arg; encoding], []) ->
        (* FIXME: check encoding? *)
        Utils.check_instances f man flow range [arg; encoding]
          ["str"; "str"]
          (fun eargs flow ->
            let earg, eencoding = match eargs with [e1;e2] -> e1, e2 | _ -> assert false in
            assume (mk_binop eencoding O_eq (mk_string "utf-8" range) range) man flow
              ~route:(Semantic "Python")
              ~fthen:(fun flow ->
                man.eval ~route:(Semantic "Python") (mk_expr (E_constant (C_top T_py_bytes)) range) flow
              )
              ~felse:(fun flow ->
                let msg = Format.asprintf "unknown encoding: %a" pp_expr eencoding in
                man.exec (Utils.mk_builtin_raise_msg "LookupError" msg range) flow >>%
                Eval.empty_singleton
              )
          )
        |> OptionExt.return

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.decode" as f, _))}, _)}), [arg; encoding], []) ->
        (* FIXME: check encoding? *)
        Utils.check_instances f man flow range [arg; encoding]
          ["bytes"; "str"]
          (fun eargs flow ->
            let earg, eencoding = match eargs with [e1;e2] -> e1, e2 | _ -> assert false in
            assume (mk_binop eencoding O_eq (mk_string "utf-8" range) range) man flow
              ~route:(Semantic "Python")
              ~fthen:(fun flow ->
                man.eval ~route:(Semantic "Python") (mk_expr (E_constant (C_top T_string)) range) flow)
              ~felse:(fun flow ->
                let msg = Format.asprintf "unknown encoding: %a" pp_expr eencoding in
                man.exec (Utils.mk_builtin_raise_msg "LookupError" msg range) flow >>%
                  Eval.empty_singleton)
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.strip" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["bytes"]
          (fun eargs flow -> man.eval ~route:(Semantic "Python") (mk_py_top T_py_bytes range) flow)
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__repr__" as f, _))}, _)}, args, [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__str__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["str"]
          (fun eargs flow -> Eval.singleton (List.hd eargs) flow)
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.format" as f, _))}, _)}, args, kwargs) ->
        Utils.check_instances ~arguments_after_check:(List.length args - 1) f man flow range args ["str"]
          (fun eargs flow ->
             (* TODO: constant strings are kept in the objects, so we could raise less alarms *)
             let tyerror_f = post_to_flow man (man.exec (Utils.mk_builtin_raise_msg "ValueError" "incomplete format" range) flow) in
             let flow = Flow.copy_ctx tyerror_f flow in
             let res = man.eval (mk_py_top T_string range) flow in
             let tyerror = tyerror_f |> Eval.empty_singleton in
             Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx res tyerror :: res :: [])
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.index" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args
          ["str"; "str"]
          (fun eargs flow ->
             Eval.join
               (man.eval ~route:(Semantic "Python") (mk_py_top T_int range) flow)
               (man.exec (Utils.mk_builtin_raise_msg "ValueError" "substring not found" range) flow >>% Eval.empty_singleton)
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.__len__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args ["str"]
          (fun eargs flow ->
             man.eval ~route:(Semantic "U/String") (mk_expr (E_len (extract_oobject @@ List.hd eargs)) range) flow >>$
 (fun l flow -> man.eval ~route:(Semantic "Python") l flow)
          )
        |> OptionExt.return

      | _ -> None

    let exec _ _ _ = None
    let ask _ _ _ = None
  end

let () =  register_stateless_domain (module Domain)
