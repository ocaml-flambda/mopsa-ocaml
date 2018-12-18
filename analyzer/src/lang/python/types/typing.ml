(* TODO: move S_assume and eval of not into t_bool domain? *)
open Mopsa
open Ast
open Universal.Ast
open Addr
open Data_model.Attribute
open MapExt
open Objects.Function

type expr_kind +=
   | E_get_type_partition of Typingdomain.polytype
   | E_type_partition of Typingdomain.typeid

let () =
  register_expr_pp (fun default fmt exp ->
      match ekind exp with
      | E_get_type_partition ptype -> Format.fprintf fmt "E_get_tp %a" Typingdomain.pp_polytype ptype
      | E_type_partition tid -> Format.fprintf fmt "TypeId %d" tid
      | _ -> default fmt exp);
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_get_type_partition p1, E_get_type_partition p2 -> Exceptions.panic "fixme"
      | E_type_partition i1, E_type_partition i2 -> Pervasives.compare i1 i2
      | _ -> next e1 e2)

let opt_pyty_summaries : bool ref = ref false

let () =
  register_option (
      "-pyty-summaries", Arg.Set opt_pyty_summaries, " enable interprocedural summaries for analyses using python types (default: false)")

type summaries = Typingdomain.summary list

type ('a, _) Annotation.key +=
   | A_py_summaries: ('a, summaries) Annotation.key

let pp_summaries fmt summaries = Format.pp_print_list Typingdomain.pp_summary fmt summaries

let () =
  Annotation.(register_stateless_annot {
      eq = (let f: type a b. (a, b) Annotation.key -> (summaries, b) eq option =
             function
             | A_py_summaries -> Some Eq
             | _ -> None in f);
      print = (fun fmt s -> Format.fprintf fmt "Summaries: %a" pp_summaries s)
    }) ()



module Domain =
  struct
    type t = Typingdomain.domain

    type _ domain += D_python_typing : t domain

    let id = D_python_typing
    let name = "python.types.typing"
    let identify : type a. a domain -> (t, a) eq option = function
      | D_python_typing -> Some Eq
      | _ -> None

    type _ Framework.Query.query +=
       | Q_types : Framework.Ast.expr -> Typingdomain.polytype Framework.Query.query

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = { export = [any_zone]; import = [any_zone]; }
    let eval_interface = { export = [any_zone, any_zone]; import = []; }

    let join _ = Typingdomain.join
    let subset = Typingdomain.leq
    let meet = Typingdomain.meet
    let widen = Typingdomain.widening
    let top = Typingdomain.top
    let bottom = Typingdomain.bottom
    let is_top = Typingdomain.is_top
    let is_bottom = Typingdomain.is_bottom
    let print = Typingdomain.print

    let return_id_of_type man flow range ptype =
      let cur = Flow.get_domain_cur man flow in
      let tid, ncur = Typingdomain.get_type ~local_use:true cur ptype in
      let flow = Flow.set_domain_cur ncur man flow in
      Eval.singleton (mk_expr (E_type_partition tid) range) flow

    let stub_base = ref StringMap.empty

    let init progr man flow =
      let open Typingdomain in
      let bool = builtin_inst "bool" in
      let complex = builtin_inst "complex" in
      let complex_cls = let a, b= Addr.builtin_cl_and_mro "complex" in Class (a, b) in
      let int = builtin_inst "int" in
      let float = builtin_inst "float" in
      let none = builtin_inst "NoneType" in
      let str = builtin_inst "str" in
      let str_cls = let a, b = Addr.builtin_cl_and_mro "str" in Class (a, b) in
      let slice = builtin_inst "slice" in
      let slice_cls = let a, b = Addr.builtin_cl_and_mro "slice" in Class (a, b) in
      let dict_cls = let a, b = Addr.builtin_cl_and_mro "dict" in Class (a, b) in
      let lstub_base =
        StringMap.empty |>
          (* TODO: add rewriting for default arguments?... *)
          (* or in stdlib.py? *)
          add_signature "abs" [int] [int] [] |>
          add_signature "abs" [float] [float] [] |>
          (* add a range for Typevars ? so abs can be alpha -> alpha, alpha \in {float, int} *)
          add_signature "bin" [int] [str] [] |>
          add_signature "chr" [int] [str] [] |>
          add_signature "ord" [str] [int] [] |>
          add_signature "dir" [] [List str] [] |>
          add_signature "dir" [builtin_inst "object"] [List str] [] |>
          add_signature "divmod" [int; int] [FiniteTuple [int; int]] [] |>
          add_signature "divmod" [float; float] [FiniteTuple [float; float]] [] |>
          (* i don't like this int; float case *)
          add_signature "divmod" [int; float] [FiniteTuple [float; float]] [] |>
          add_signature "hash" [builtin_inst "object"] [int] [] |>
          add_signature "all" [List (Typevar 0)] [bool] [] |>
          add_signature "sorted" [List (Typevar 0)] [List (Typevar 0)] [] |>
          add_signature "sorted" [Set (Typevar 0)] [List (Typevar 0)] [] |>
          add_signature "sorted" [Dict (Typevar 0, Typevar 1)] [List (Typevar 0)] [] |>
          add_signature "sorted" [str;] [List str] [] |>

          add_signature "math.ceil" [float] [int] [] |>
          add_signature "math.copysign" [float; float] [float] [] |>
          add_signature "math.erf" [float] [float] [] |>
          add_signature "math.erfc" [float] [float] [] |>
          add_signature "math.exp" [float] [float] [] |>
          add_signature "math.expm1" [float] [float] [] |>
          add_signature "math.fabs" [float] [float] [] |>
          add_signature "math.factorial" [int] [int] [] |>
          add_signature "math.floor" [float] [int] [] |>
          add_signature "math.fmod" [float; float] [float] [] |>
          add_signature "math.frexp" [float] [FiniteTuple [float; int]] [] |>
          add_signature "math.fsum" [List float] [float] [] |> (* add iterator as well *)
          add_signature "math.gamma" [float] [float] [] |>
          add_signature "math.gcd" [int; int] [int] [] |>
          add_signature "math.hypot" [float; float] [float] [] |>
          add_signature "math.isclose" [float; float] [bool] [] |>
          add_signature "math.isfinite" [float] [bool] [] |>
          add_signature "math.isinf" [float] [bool] [] |>
          add_signature "math.isnan" [float] [bool] [] |>
          add_signature "math.ldexp" [float; int] [float] [] |>
          add_signature "math.lgamma" [float] [float] [] |>
          add_signature "math.log" [float; float] [float] [] |>
          add_signature "math.log1p" [float] [float] [] |>
          add_signature "math.log10" [float] [float] [] |>
          add_signature "math.log2" [float] [float] [] |>
          add_signature "math.modf" [float] [FiniteTuple [float; float]] [] |>
          add_signature "math.sqrt" [float] [float] [] |>

          add_signature "complex.__new__" [complex_cls] [complex] [] |>
          add_signature "complex.__new__" [complex_cls; float] [complex] [] |>
          add_signature "complex.__new__" [complex_cls; float; float] [complex] [] |>
          add_signature "str.capitalize" [str] [str] [] |>
          add_signature "str.center" [str; int] [str] [] |>
          add_signature "str.center" [str; int; str] [str] [] |>
          add_signature "str.join" [str; List str] [str] [] |>
          add_signature "str.join" [str; Generator str] [str] [] |>

          add_signature "str.lower" [str] [str] [] |>
          add_signature "str.upper" [str] [str] [] |>
          add_signature "str.split" [str] [List str] [] |>
          add_signature "str.split" [str; str] [List str] [] |>
          add_signature "str.split" [str; str; int] [List str] [] |>
          add_signature "str.replace" [str; str; str] [str] [] |>
          add_signature "str.replace" [str; str; str; int] [str] [] |>
          add_signature "str.rstrip" [str] [str] [] |>
          add_signature "str.rstrip" [str; str] [str] [] |>
          add_signature "str.strip" [str] [str] [] |>
          add_signature "str.strip" [str; str] [str] [] |>
          add_signature "str.swapcase" [str] [str] [] |>
          add_signature "str.title" [str] [str] [] |>
          add_signature "str.__new__" [str_cls] [str] [] |>
          add_signature "str.__len__" [str] [int] [] |>
          add_signature "str.__mul__" [str; int] [str] [] |>
          add_signature "str.__rmul__" [str; int] [str] [] |>
          add_signature "str.__getitem__" [str; int] [str] [] (* ["IndexError"] *) |>
          add_signature "str.__getitem__" [str; slice] [str] [] |>
          add_signature "str.__contains__" [str; str] [bool] [] |>
          add_signature "str.isalnum" [str] [bool] [] |>
          add_signature "str.isalpha" [str] [bool] [] |>
          add_signature "str.isdecimal" [str] [bool] [] |>
          add_signature "str.isdigit" [str] [bool] [] |>
          add_signature "str.islower" [str] [bool] [] |>
          add_signature "str.isnumeric" [str] [bool] [] |>
          add_signature "str.isspace" [str] [bool] [] |>
          add_signature "str.istitle" [str] [bool] [] |>
          add_signature "str.isupper" [str] [bool] [] |>

          add_signature "list.count" [List (Typevar 0); Top] [int] [] |>
          add_signature "list.remove" [List (Typevar 0); Top] [none] ["ValueError"] |>
          add_signature "list.pop" [List (Typevar 0)] [Typevar 0] ["IndexError"] |>
          add_signature "list.reverse" [List (Typevar 0)] [none] [] |>
          add_signature "list.sort" [List (Typevar 0)] [none] [] |>
          add_signature "list.__getitem__" [List (Typevar 0); int] [Typevar 0] (*["IndexError"]*) [] |>
          add_signature "list.__getitem__" [List (Typevar 0); slice] [List (Typevar 0)] [] |>
          add_signature "list.__iter__" [List (Typevar 0)] [Iterator (List (Typevar 0), -1)] [] |>
          add_signature "list.__mul__" [List (Typevar 0); int] [List (Typevar 0)] [] |>
          add_signature "list.__rmul__" [List (Typevar 0); int] [List (Typevar 0)] [] |>
          add_signature "list.__contains__" [List (Typevar 0); Top] [bool] [] |>
          add_signature "list_iterator.__next__" [Iterator (List (Typevar 0), -1)] [Typevar 0] ["StopIteration"] |>
          (* TODO: add list[bottom] to signal empty list ? *)

          add_signature "slice.__new__" [slice_cls; int; none; none] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; int; none; int] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; int; int; none] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; int; int; int] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; none; int; none] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; none; int; int] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; none; none; int] [slice] [] |>
          add_signature "slice.__new__" [slice_cls; none; none; none] [slice] [] |>

          add_signature "dict.copy" [Dict (Typevar 0, Typevar 1)] [Dict (Typevar 0, Typevar 1)] [] |>
          add_signature "dict.keys" [Dict (Typevar 0, Typevar 1)] [DictKeys (Dict (Typevar 0, Typevar 1))] [] |>
          add_signature "dict.values" [Dict (Typevar 0, Typevar 1)] [DictValues (Dict (Typevar 0, Typevar 1))] [] |>
          add_signature "dict.items" [Dict (Typevar 0, Typevar 1)] [DictItems (Dict (Typevar 0, Typevar 1))] [] |>
          add_signature "dict.get" [Dict (Typevar 0, Typevar 1); Top] [Typevar 1] [] |>
          add_signature "dict.get" [Dict (Typevar 0, Typevar 1); Top] [none] [] |>
          add_signature "dict.get" [Dict (Typevar 0, Typevar 1); Top; Typevar 2] [Typevar 1] [] |>
          add_signature "dict.get" [Dict (Typevar 0, Typevar 1); Top; Typevar 2] [Typevar 2] [] |>
          add_signature "dict.pop" [Dict (Typevar 0, Typevar 1); Top] [Typevar 1] [] (* ["KeyError"] *) |>
          add_signature "dict.popitem" [Dict (Typevar 0, Typevar 1)] [FiniteTuple [Typevar 0; Typevar 1]] [] |>

          add_signature "dict.__new__" [dict_cls] [Dict (Bot, Bot)] [] |>
          add_signature "dict.__iter__" [Dict (Typevar 0, Typevar 1)] [Iterator (Dict (Typevar 0, Typevar 1), -1)] [] |>
          add_signature "dict.__getitem__" [Dict (Typevar 0, Typevar 1); Top] [Typevar 1] [] (* ["KeyError"] *) |>
          add_signature "dict.__contains__" [Dict (Typevar 0, Typevar 1); Top] [bool] [] |>
          add_signature "dict_iterator.__next__" [Iterator (Dict (Typevar 0, Typevar 1), -1)] [Typevar 0] ["StopIteration"] |>
          add_signature "dict_values.__iter__" [DictValues (Dict (Typevar 0, Typevar 1))] [Iterator (DictValues (Dict (Typevar 0, Typevar 1)), -1)] [] |>
          add_signature "dict_keys.__iter__" [DictKeys (Dict (Typevar 0, Typevar 1))] [Iterator (DictKeys (Dict (Typevar 0, Typevar 1)), -1)] [] |>
          add_signature "dict_items.__iter__" [DictItems (Dict (Typevar 0, Typevar 1))] [Iterator (DictItems (Dict (Typevar 0, Typevar 1)), -1)] [] |>
          add_signature "dict_valueiterator.__next__" [Iterator (DictValues (Dict (Typevar 0, Typevar 1)), -1)] [Typevar 1] ["StopIteration"] |>
          add_signature "dict_keyiterator.__next__" [Iterator (DictKeys (Dict (Typevar 0, Typevar 1)), -1)] [Typevar 0] ["StopIteration"] |>
          add_signature "dict_keyiterator.__next__" [Iterator (Dict (Typevar 0, Typevar 1), -1)] [Typevar 0] ["StopIteration"] |>
          add_signature "dict_itemiterator.__next__" [Iterator (DictItems (Dict (Typevar 0, Typevar 1)), -1)] [FiniteTuple [Typevar 0; Typevar 1]] ["StopIteration"] |>

          add_signature "set.__contains__" [Set (Typevar 0); Top] [bool] [] |>
          add_signature "tuple.__contains__" [FiniteTuple [Typevar 0; Typevar 1]; Top] [bool] [] |>
          add_signature "tuple.__contains__" [FiniteTuple [Typevar 0; Typevar 1; Typevar 2]; Top] [bool] [] |>

          add_signature "int.__bool__" [int] [bool] []
      in
      stub_base := lstub_base;
      debug "stub_base = %a@\n" pp_sb !stub_base;
      Flow.set_domain_env T_cur top man flow |> OptionExt.return

    let exec zone stmt man flow =
      debug "exec %a@\n" pp_stmt stmt;
      match skind stmt with
      | S_assign({ekind = E_var (v, STRONG)}, {ekind = E_py_undefined t}) ->
         let cur = Flow.get_domain_cur man flow in
         let t_with_undefs = Typingdomain.{lundef = not t; gundef = t; def = None} in
         Flow.set_domain_cur (Typingdomain.set_var cur v t_with_undefs) man flow |> Post.return

      | S_assign({ekind = E_var (v, STRONG)}, {ekind = E_var (w, STRONG)}) ->
         let cur = Flow.get_domain_cur man flow in
         Flow.set_domain_cur (Typingdomain.set_var_eq cur v w) man flow |> Post.return


      | S_assign({ekind = E_var (v, STRONG)} as l, e) ->
         OptionExt.return
           (man.eval e flow |>
              Post.bind man @@
                fun e flow ->
                let cur = Flow.get_domain_cur man flow in
                begin match ekind e with
                (* | E_get_type_partition ptype ->
                 *    (\*let pos, cur' = Typingdomain.get_type cur ptype in*\)
                 *    let cur' = Typingdomain.set_var cur v Typingdomain.{lundef=false; gundef=false; def=Some ptype} in
                 *    let flow = Flow.set_domain_cur cur' man flow in
                 *    debug "\t%a@\n" print (Flow.get_domain_cur man flow);
                 *    Post.of_flow flow *)
                | E_type_partition i ->
                   let ncur = Typingdomain.set_var_tid cur v i in
                   let flow = Flow.set_domain_cur ncur man flow in
                   Post.of_flow flow

                | E_py_object (addr, _) ->
                   begin
                     let ty = match addr.addr_kind with
                       | A_py_class (c, mro) -> Typingdomain.Class (c, mro)
                       | A_py_module m ->         Typingdomain.Module m
                       | A_py_function f ->       Typingdomain.Function f
                       | A_py_method (func, self) ->
                          let func = match (fst func).addr_kind with
                            | A_py_function f -> f
                            | _ -> assert false in
                          let self = match ekind self with
                            | E_type_partition i -> i
                            | _ -> assert false in
                          Typingdomain.Method (func, self)
                       | _ -> debug "typing/exec/assign/E_py_object: %a@\n" Universal.Ast.pp_addr addr;
                              failwith "ni"
                     in
                     let flow = Flow.set_domain_cur (Typingdomain.set_var cur v Typingdomain.{lundef=false; gundef=false; def=Some ty}) man flow in
                     Post.of_flow flow
                   end
                | E_constant _ ->
                   Post.of_flow @@ man.exec {stmt with skind=(S_assign(l, e))} flow
                | _ -> panic_at stmt.srange "typing/exec/S_assign: exp %a ni@\n" pp_expr e
                end)

      | S_assign({ekind = E_py_attribute({ekind = E_var (v, mode)}, attr)}, rval) ->
         man.eval rval flow |>
           Post.bind man
             (fun reval flow ->
               let cur = Flow.get_domain_cur man flow in
               match ekind reval with
               | E_type_partition i ->
                  Flow.set_domain_cur (Typingdomain.set_var_attr_ty cur v attr i) man flow |> Post.of_flow
               | _ -> Exceptions.panic "%a" pp_expr reval
             )
         |> OptionExt.return

      | S_assign({ekind = E_py_attribute(lexpr, attr)}, rval) ->
         Exceptions.panic "lexpr.attr = rval, with lexpr <> E_Var@\n"

      | S_remove { ekind = E_var (v, _) } ->
         debug "Removing var %a@\n" pp_var v;
         let cur = Flow.get_domain_cur man flow in
         let flow = Flow.set_domain_cur (Typingdomain.rm_var cur v) man flow in
         Post.return flow

      (* S⟦ ?e ⟧ *)
      | S_assume e ->
         begin match ekind e with
         | E_constant (C_top T_bool) | E_constant (C_bool true) -> Post.of_flow flow |> OptionExt.return
         | E_constant (C_bool false) -> Post.of_flow (Flow.set_domain_cur bottom man flow) |> OptionExt.return
         | _ ->
         man.eval e flow |>
           Post.bind man
             (fun expr flow ->
               debug "Assuming %a@\n" pp_expr expr;
               match ekind expr with
               | E_constant (C_top T_bool) -> Post.of_flow flow
               | E_constant (C_bool true) -> Post.of_flow flow
               | E_constant (C_bool false) -> Post.of_flow (Flow.set_domain_cur bottom man flow)
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let pt = Typingdomain.TypeIdMap.find i cur.d2 in
                  begin match pt with
                        | Instance {classn=Class (C_builtin "bool", _); _} -> Post.of_flow flow
                        | Instance {classn=Class (C_builtin "int", _); _} -> Post.of_flow flow
                        | _ ->
                           let range = srange stmt in
                           man.exec (mk_assume (Utils.mk_builtin_call "bool" [expr] range) range) flow |> Post.of_flow
                  end
               | _ -> Exceptions.panic "%a" pp_expr e
             )
         |> OptionExt.return
         end
      | _ -> None

    let summary_constructor man flow range c ls =
      Eval.eval_list ls man.eval flow |>
        Eval.bind (fun list_els flow ->
            let open Typingdomain in
            let cur = Flow.get_domain_cur man flow in
            let dummy_annot = Flow.get_all_annot flow in
            let els_types = List.fold_left (fun acc el ->
                                match ekind el with
                                | E_type_partition tid ->
                                   let pty = TypeIdMap.find tid cur.d2 in
                                   let mty = concretize_poly pty cur.d3 in
                                   Monotypeset.union dummy_annot mty acc
                                | _ -> Exceptions.panic "%a@\n" pp_expr el) Monotypeset.empty list_els in
            let pos_list, cur =
              match Monotypeset.cardinal els_types with
              | 0 ->
                 get_type ~local_use:true cur (c Bot)
              | 1 ->
                 let ty = Monotypeset.choose els_types in
                 get_type ~local_use:true cur (c (poly_cast ty))
              | _ ->
                 let ptype, cur = get_mtypes cur els_types in
                 get_type ~local_use:true cur (c ptype) in
            let flow = Flow.set_domain_cur cur man flow in
            Eval.singleton (mk_expr (E_type_partition pos_list) range) flow)
      |> OptionExt.return

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_get_type_partition pty ->
         if is_bottom @@ Flow.get_domain_cur man flow then
           Eval.empty_singleton flow |> OptionExt.return
         else
           return_id_of_type man flow range pty |> OptionExt.return

      | E_type_partition i ->
         if is_bottom @@ Flow.get_domain_cur man flow then
           Eval.empty_singleton flow |> OptionExt.return
         else
           Eval.singleton exp flow |> OptionExt.return

      | E_constant (C_top T_bool)
        | E_constant (C_bool _ ) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "bool") |> OptionExt.return

      | E_constant (C_top T_int)
        | E_constant (C_int _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "int") |> OptionExt.return

      | E_constant (C_top (T_float _))
        | E_constant (C_float _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "float") |> OptionExt.return

      | E_constant (C_top T_string)
        | E_constant (C_string _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "str") |> OptionExt.return

      | E_py_bytes _ ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "bytes") |> OptionExt.return

      | E_constant C_py_not_implemented ->
         let builtin_notimpl = Typingdomain.builtin_inst "NotImplementedType" in
         return_id_of_type man flow range builtin_notimpl |> OptionExt.return

      | E_constant C_py_none ->
         let builtin_none = Typingdomain.builtin_inst "NoneType" in
         return_id_of_type man flow range builtin_none |> OptionExt.return

      | E_alloc_addr akind ->
         begin match akind with
         (* | A_py_method (f, obj) ->
          *    let f = match (fst f).addr_kind with
          *      | A_py_function f -> f
          *      | _ -> assert false in
          *    (\*            Exceptions.panic "todo"*\)
          *    let cur = Flow.get_domain_cur man flow in
          *    let ty = Typingdomain.Method (f, 0) in
          *    let tid, ncur = Typingdomain.get_type cur ty in
          *    let flow = Flow.set_domain_cur ncur man flow in
          *    Eval.singleton (mk_expr (E_type_partition tid) range) flow
          *    (\* Eval.singleton (mk_py_object ({addr_kind = akind; addr_uid = (-1)}, mk_py_empty range) range) flow *\) *)
         | A_py_method (func, self) ->
            man.eval (mk_py_object ({addr_kind = akind; addr_uid = (-1)}, mk_py_empty range) range) flow
         | _ ->
            let addr = {addr_kind = akind; addr_uid=(-1)} in
            Eval.singleton (mk_addr addr range) flow
         end
         |> OptionExt.return

      | E_var (v, _) ->
         let cur = Flow.get_domain_cur man flow in
         begin
           try
             debug "%a@\n" print cur;
             let polytype = Typingdomain.get_polytype cur v in
             let expr = match polytype with
               | Typingdomain.Class (c, b) -> mk_py_object ({addr_kind=A_py_class (c, b); addr_uid=(-1)}, mk_expr (ekind exp) range) range
               | Typingdomain.Function f -> mk_py_object ({addr_kind=A_py_function f; addr_uid=(-1)}, mk_expr (ekind exp) range) range
               | Typingdomain.Module m -> mk_py_object ({addr_kind=A_py_module m; addr_uid=(-1)}, mk_expr (ekind exp) range) range
               | _ ->
                  let tid = Typingdomain.typeindex_of_var cur.d1 v in
                  mk_expr (E_type_partition tid) range in
             Eval.singleton expr flow
             (* FIXME: properly handle every case *)
             (* let ak = Typingdomain.get_addr_kind cur v in
              * let a = {addr_kind=ak; addr_uid=(-1)} in
              * Eval.singleton (mk_py_object (a, mk_expr (ekind exp) range) range) flow *)
             |> OptionExt.return
           with Not_found ->
             if Addr.is_builtin_name v.vname then
               let a = Addr.find_builtin v.vname in
               let _ = debug "builtin variable@\n" in
               Eval.singleton (mk_py_object a range) flow |> OptionExt.return
             else
               Eval.empty_singleton flow |> OptionExt.return
               (* let cur = Flow.get_domain_cur man flow in
                * let tid, cur = Typingdomain.get_type cur Bot in
                * let flow = Flow.set_domain_cur cur man flow in
                * let () = debug "Cur is now %a@\n" print cur in
                * Eval.singleton (mk_expr (E_type_partition tid) range) flow |> OptionExt.return *)
         end

      | E_py_ll_hasattr(e, attr) ->
         let attr = match ekind attr with
           | E_constant (C_string s) -> s
           | _ -> assert false in
      (* FIXME? as this is not a builtin constructor, we assume e is already evaluated *)
         begin match ekind e with
         | E_py_object ({addr_kind = A_py_module (M_user(name, globals))}, _) ->
            Eval.singleton (mk_py_bool (List.exists (fun v -> v.vname = attr) globals) range) flow
         | E_py_object ({addr_kind = A_py_module _}, _)
           | E_py_object ({addr_kind = A_py_class (C_builtin _, _)}, _) ->
            Eval.singleton (mk_py_bool (Addr.is_builtin_attribute (object_of_expr e) attr) range) flow
         | E_py_object ({addr_kind = A_py_class (C_user c, b)}, _) ->
            Eval.singleton (mk_py_bool (List.exists (fun v -> v.vname = attr) c.py_cls_static_attributes) range) flow
         | E_type_partition i ->
            let cur = Flow.get_domain_cur man flow in
            debug "cur=%a@\n" print cur;
            let open Typingdomain in
            let pt = TypeIdMap.find i cur.d2 in
            let rec process pt cur =
              begin match pt with
              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
                 [Eval.singleton (mk_py_true range) flow]
              | Instance {classn; uattrs; oattrs} when not (StringMap.exists (fun k _ -> k = attr) uattrs || StringMap.exists (fun k _ -> k = attr) oattrs) ->
                 [Eval.singleton (mk_py_false range) flow]
              | Instance {classn; uattrs; oattrs} ->
                 let dt, df = filter_attr cur i attr in
                 let flowt = Flow.set_domain_cur dt man flow in
                 let flowf = Flow.set_domain_cur df man flow in
                 (* debug "Bad hasattr on partition %d, initial domain: %a@\n spliting in two cases: %a and %a@\n" i print cur print dt print df; *)
                 [Eval.singleton (mk_py_true range) flowt; Eval.singleton (mk_py_false range) flowf]
              | Iterator (x, _) ->
                 (* FIXME: should be the type(Iterator _) that has this attribute, I think *)
                 [Eval.singleton (mk_py_bool (attr = "next") range) flow]
              | List _ | Set _ | Dict _ ->
                 [Eval.singleton (mk_py_false range) flow]
              | Typevar alpha ->
                 let mtys = TypeVarMap.find alpha cur.d3 in
                 Monotypeset.fold (fun mty acc -> process (poly_cast mty) {cur with d3 = TypeVarMap.add alpha (Monotypeset.singleton mty) cur.d3} @ acc)  mtys []
              (* let cls = Addr.find_builtin "list" in
               * Eval.singleton (mk_py_bool (Addr.is_builtin_attribute cls attr) range) flow *)
              | _ -> Exceptions.panic "ll_hasattr %a@\n" Typingdomain.pp_polytype pt
              end
            in
            let cur = Flow.get_domain_cur man flow in
            Eval.join_list (process pt cur)
         | _ ->
            Eval.empty_singleton flow
         end
         |> OptionExt.return

      | E_py_ll_getattr(e, attr) ->
         (* FIXME? as this is not a builtin constructor, but only used by the analysis, we assume that e has attribute attr *)
         let attr = match ekind attr with
           | E_constant (C_string s) -> s
           | _ -> assert false in
         begin
           match ekind e with
           | E_py_object ({addr_kind = A_py_class (C_builtin c, b)}, _) ->
              Eval.singleton (mk_py_object (Addr.find_builtin_attribute (object_of_expr e) attr) range) flow
           | E_py_object ({addr_kind = A_py_class (C_user c, b)}, _) ->
              let f = List.find (fun x -> x.vname = attr) c.py_cls_static_attributes in
              man.eval (mk_var f range) flow
           | E_py_object ({addr_kind = A_py_module (M_builtin m)}, _) ->
              Eval.singleton (mk_py_object (Addr.find_builtin_attribute (object_of_expr e) attr) range) flow
           | E_py_object ({addr_kind = A_py_module (M_user (name, globals))}, _) ->
           (* Eval.singleton (mk_py_object (Addr.find_builtin_attribute (object_of_expr e) attr) range) flow *)
              let v = List.find (fun x -> x.vname = attr) globals in
              man.eval (mk_var v range) flow
           | E_type_partition i ->
              let cur = Flow.get_domain_cur man flow in
              let pt = Typingdomain.TypeIdMap.find i cur.d2 in
              begin
                match pt with
                | Instance {classn; uattrs; oattrs} ->
                   let ty = StringMap.find attr uattrs in
                   let tid, ncur = Typingdomain.get_type cur ty in
                   let flow = Flow.set_domain_cur ncur man flow in
                   Eval.singleton (mk_expr (E_type_partition tid) range) flow
                | _ -> Exceptions.panic "E_py_ll_getattr: shouldn't happen?"
              end
           | _ -> Exceptions.panic "E_py_ll_getattr: todo"
         end
         |> OptionExt.return

      | E_unop(Framework.Ast.O_log_not, {ekind=E_constant (C_bool b)}) ->
         Eval.singleton (mk_py_bool (not b) range) flow
         |> OptionExt.return

      | E_unop(Framework.Ast.O_log_not, e') ->
         man.eval e' flow |>
           Eval.bind
             (fun exp flow ->
             (* FIXME: test if instance of bool and proceed accordingly *)
               match ekind exp with
               | E_constant (C_top T_bool)
                 (*| E_get_type_partition (Typingdomain.Instance {Typingdomain.classn=Typingdomain.Class (C_builtin "bool", _)})*) -> Eval.singleton exp flow
               | E_constant (C_bool true) ->  Eval.singleton (mk_py_false range) flow
               | E_constant (C_bool false) -> Eval.singleton (mk_py_true range) flow
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let pt = Typingdomain.TypeIdMap.find i cur.d2 in
                  begin match pt with
                  | Instance {classn=Class (C_builtin "bool", _); _} -> Eval.singleton (mk_py_top T_bool range) flow
                  | Instance {classn=Class (C_builtin "int", _); _} -> Eval.singleton (mk_py_top T_bool range) flow
                  | _ -> (* FIXME: cast into bool and see what happens *)
                     man.eval (mk_unop Framework.Ast.O_log_not (Utils.mk_builtin_call "bool" [exp] range) range) flow
                  end
               | _ -> failwith "not: ni"
             )
         |> OptionExt.return


      | E_binop(O_py_is, e1, e2) ->
         man.eval (mk_py_top T_bool range) flow |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind
             (fun e_arg flow ->
               let cur = Flow.get_domain_cur man flow in
               let constr (a, b) = [(a, b, cur)] in
               let classes = match ekind e_arg with
                 | E_type_partition i -> Typingdomain.type_of cur i
                 | E_py_object ({addr_kind = A_py_class (c, b)}, _) -> constr @@ builtin_cl_and_mro "type"
                 | E_py_object ({addr_kind = A_py_module _}, _) -> constr @@ builtin_cl_and_mro "module"
                 | E_py_object ({addr_kind = A_py_function _}, _) -> constr @@ builtin_cl_and_mro "function"
                 | _ -> debug "type(%a)?@\n" pp_expr e_arg; assert false in
               let proceed (cl, mro, cur) =
                 let flow = Flow.set_domain_cur cur man flow in
                 let obj = mk_py_object ({addr_kind=A_py_class (cl, mro); addr_uid=(-1)}, mk_expr (ekind exp) range) range in
                 Eval.singleton obj flow in
               debug "classes : %d" @@ List.length classes;
               if List.length classes = 1 then
                  proceed (List.hd classes)
               else
                 List.fold_left (fun acc (cl, mro, cur) ->
                     debug "ADDR: %a@\n" pp_addr {addr_kind=A_py_class (cl, mro); addr_uid=(-1)};
                     Eval.join acc (proceed (cl, mro, cur))) (proceed @@ List.hd classes) (List.tl classes)
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
         (* FIXME: TypeError if arg 1 is not a class, arg 2 is not a class or tuple of classes *)
         Eval.eval_list [cls; cls'] man.eval flow |>
           Eval.bind (fun evals flow ->
               let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
               match ekind cls, ekind cls' with
                   | E_py_object ({addr_kind = A_py_class (c, mro)}, _),
                     E_py_object ({addr_kind = A_py_class (c', mro')} as a', _) ->
                      if List.exists (fun x -> Universal.Ast.compare_addr (fst x) a' = 0) mro then
                        Eval.singleton (mk_py_true range) flow
                      else
                        Eval.singleton (mk_py_false range) flow
                   | _ -> Exceptions.panic "FIXME"
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
         (* probablement évaluer dans la zone python des types ? *)
         Eval.eval_list [obj; attr] man.eval flow |>
           Eval.bind (fun evals flow ->
               let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
               debug "eobj = %a, eattr = %a@\ncur = %a@\n" pp_expr eobj pp_expr eattr print (Flow.get_domain_cur man flow);
               match ekind eobj, ekind eattr with
               | E_type_partition tid, E_py_object ({addr_kind = A_py_class (cls, b)}, _) ->
                  let cur = Flow.get_domain_cur man flow in
                  (* FIXME: split the case of unions? *)
                  let dt, df = Typingdomain.filter_inst cur tid (Typingdomain.Class (cls, b)) in
                  let flowt = Flow.set_domain_cur dt man flow and
                      flowf = Flow.set_domain_cur df man flow in
                  (* FIXME: just join ? *)
                  begin match is_bottom dt, is_bottom df with
                  | true, true -> Eval.empty_singleton flowt
                  | true, false -> Eval.singleton (mk_py_false range) flowf
                  | false, true -> Eval.singleton (mk_py_true range) flowt
                  | false, false ->
                     Eval.join
                       (Eval.singleton (mk_py_true range) flowt)
                       (Eval.singleton (mk_py_false range) flowf)
                  end
               | E_py_object ({addr_kind = A_py_function _}, _), E_py_object ({addr_kind = A_py_class (C_builtin c, _)}, _) ->
                  if c = "function" then
                    Eval.singleton (mk_py_true range) flow
                  else
                    Eval.singleton (mk_py_false range) flow
               | E_py_object ({addr_kind = A_py_class _}, _), E_py_object ({addr_kind = A_py_class (C_builtin c, _)}, _) ->
                  if c = "type" then
                    Eval.singleton (mk_py_true range) flow
                  else
                    Eval.singleton (mk_py_false range) flow
               | E_py_object ({addr_kind = A_py_class (c, mro)}, _), E_py_object ({addr_kind = A_py_class (c', mro')}, _) ->
                  Exceptions.panic "Left MRO %a@\nRight MRO %a@\n"
                    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                       (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x range)))
                    mro
                    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                       (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x range)))
                    mro'
               | E_py_object ({addr_kind = A_py_module _}, _), _ ->
                  begin match ekind eattr with
                  | E_py_object ({addr_kind = A_py_class (C_builtin c, _)}, _) when c = "object" || c = "module" ->
                     Eval.singleton (mk_py_true range) flow
                  | _ ->
                     Eval.singleton (mk_py_false range) flow
                  end
               | E_constant (C_bool _), E_py_object ({addr_kind = A_py_class (c', mro')}, _) ->
                  let r = match c' with
                    | C_builtin "bool" -> true
                    | _ -> false in
                  Eval.singleton (mk_py_bool r range) flow
               | _ ->
                  if is_bottom (Flow.get_domain_cur man flow) then Eval.empty_singleton flow else
                  Exceptions.panic "todo: implement isinstance(%a, %a)@\n" pp_expr eobj pp_expr eattr
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
         Eval.eval_list args man.eval flow |>
           Eval.bind
             (fun args flow ->
               match args with
               | [] ->
                  debug "Error in creating a new instance";
                  man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                    Eval.empty_singleton
               | cls::tl ->
                  let open Typingdomain in
                  let cls, mro = match (fst (object_of_expr cls)).addr_kind with
                    | A_py_class (c, mro) -> c, mro
                    | _ -> assert false in
                  let inst = Instance {classn=Class (cls, mro); uattrs=StringMap.empty; oattrs=StringMap.empty} in
                  let cur = Flow.get_domain_cur man flow in
                  let tid, ncur = Typingdomain.get_type cur inst in
                  let flow = Flow.set_domain_cur ncur man flow in
                  Eval.singleton (mk_expr (E_type_partition tid) range) flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
         man.eval (mk_py_none range) flow |> OptionExt.return

      (* FIXME: move somewhere else, that's completely modular *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "NoneType.__eq__")}, _)}, args, []) ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let e1, e2 = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin e1 "NoneType" range)
                   ~fthen:(fun flow ->
                     Eval.assume (mk_py_isinstance_builtin e2 "NoneType" range)
                       ~fthen:(fun flow ->
                         Eval.singleton (mk_py_true range) flow)
                       ~felse:(fun flow ->
                         Eval.singleton (mk_py_false range) flow)
                       man flow
                   )
                   ~felse:(fun flow ->
                     man.eval (mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range) flow
                   )
                   man flow
               ) |> OptionExt.return
         else tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "max")}, _)}, [iterable], []) ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         man.eval iterable flow |>
           Eval.bind (fun eit flow ->
               Eval.assume
                 (mk_py_hasattr eit "__iter__" range)
                 ~fthen:(fun flow ->
                   (* (* Eval.assume
                    *  *   (mk_py_hasattr eit "__next__" range) *)
                    *   ~fthen:(fun flow -> *)
                       let cur = Flow.get_domain_cur man flow in
                       let ptype = match ekind eit with
                         | E_type_partition tid ->
                            begin match Typingdomain.TypeIdMap.find tid cur.d2 with
                            | Iterator (List x, _) | List x
                              | Iterator (Set x, _) | Set x -> x
                            (* FIXME: dicts *)
                            | Iterator (FiniteTuple s, _) | FiniteTuple s ->
                               Exceptions.panic "max tuple: todo"
                            | Iterator (Instance {classn=Class (C_builtin "str", _)}, _) ->
                               Typingdomain.builtin_inst "str"
                            | _-> assert false
                            end
                         | _ -> assert false in
                       return_id_of_type man flow range ptype
                       )
                     (* ~felse:tyerror
                      * man flow) *)
                 ~felse:tyerror
                 man flow
             )
         |> OptionExt.return

      (* generators *)
      | E_py_generator_comprehension (expr, comprehensions) ->
         let rec unfold_pseudolc to_clean stmt_acc aux_compr = match aux_compr with
           | [] -> to_clean, List.rev stmt_acc
           | (target, iter, conds)::tl ->
              let i_tmp = mk_tmp () in
              let i_var = mk_var i_tmp range in
              let s1 = mk_assign i_var (Utils.mk_builtin_call "iter" [iter] range) range in
              let s2 = mk_assign target (Utils.mk_builtin_call "next" [i_var] range) range in
              let b_tmp = mk_tmp () in
              let b_var = mk_var b_tmp range in
              let inline_conds = List.fold_left (fun acc cond -> mk_expr (E_py_if (cond, acc, mk_py_false range)) range) (mk_py_true range) (List.rev conds) in
              (* FIXME: enforce that s3 is bool? *)
              let s3 = mk_assign b_var inline_conds range in
              let target_var = match ekind target with
                | E_var (v, _) -> v
                | _ -> assert false in
              unfold_pseudolc (i_tmp::b_tmp::target_var::to_clean) (s3::s2::s1::stmt_acc) tl in
         let to_clean, stmt_list = unfold_pseudolc [] [] comprehensions in
         let stmt = mk_block stmt_list range in
         let cleaners = List.map (fun x -> mk_remove_var x range) to_clean in
         let empty_stmt = mk_block [] range in
         debug "Generator Comprehension: Rewriting %a into @[@\nexec:@[@\n%a@]@\neval:@[@\n%a@]@\ncleaners:@[@\n%a@]@]@\n" pp_expr exp pp_stmt stmt pp_expr expr pp_stmt (mk_block cleaners range);
         man.exec (Utils.mk_try_stopiteration stmt empty_stmt range) flow |>
           man.eval expr |>
           Eval.bind (fun expr flow ->
               let cur = Flow.get_domain_cur man flow in
               let ty_in_gen = match ekind expr with
                 | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                 | _ -> assert false in
               return_id_of_type man flow range (Typingdomain.Generator ty_in_gen)
             ) |>
           Eval.add_cleaners cleaners |>
           OptionExt.return

      (* stubs *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin fname)}, _)}, args, []) when StringMap.mem fname !stub_base ->
         (* TODO: the filtering does not work for Iterator[...] I think, as it checks instances but not recursively? *)
         debug "Searching stubs for %s@\n" fname;
         (* TODO: add support for polymorphism *)
         (* TODO: add support for multiple signatures of the same function *)
         let open Typingdomain in
         let f_signatures = get_signatures fname !stub_base in
         let f_signatures = List.filter (fun x -> List.length args = List.length x.in_args) f_signatures in
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length f_signatures = 0 then tyerror flow |> OptionExt.return
         else
           (* search for constraints of the form TypeVar -i = ty *)
           (* subst all those *)
           (* keep compatible signatures *)
           (* fold the results *)
           let is_compatible_signature in_types in_signature domain =
             let vars = List.fold_left (fun acc el -> Typevarset.union acc (collect_vars el)) Typevarset.empty in_signature in
             let empty_d3 = Typevarset.fold (fun var acc -> TypeVarMap.add var (Monotypeset.add Top Monotypeset.empty) acc) vars TypeVarMap.empty in
             List.fold_left2 (fun acc in_ty sign_ty ->
                 let r = polytype_leq (in_ty, domain.d3) (sign_ty, empty_d3) in
                 debug "leq? %a[%a] %a[%a]@\nresult is %b@\n" pp_polytype in_ty pp_d3 domain.d3 pp_polytype sign_ty pp_d3 empty_d3 r;
                 acc && r) true in_types in_signature in
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let cur = Flow.get_domain_cur man flow in
                 let in_types = List.map (fun exp -> match ekind exp with
                                                     | E_type_partition tid -> TypeIdMap.find tid cur.d2
                                                     (* TODO: handle classes and functions too... *)
                                                     | E_py_object (addr, _) ->
                                                        begin match addr.addr_kind with
                                                            | A_py_class (c, mro) -> Typingdomain.Class (c, mro)
                                                            | A_py_module m ->         Typingdomain.Module m
                                                            | A_py_function f ->       Typingdomain.Function f
                                                            | A_py_method (func, self) ->
                                                               let func = match (fst func).addr_kind with
                                                                 | A_py_function f -> f
                                                                 | _ -> assert false in
                                                               let self = match ekind self with
                                                                 | E_type_partition i -> i
                                                                 | _ -> assert false in
                                                               Typingdomain.Method (func, self)
                                                            | _ -> debug "typing/stubs %a@\n" Universal.Ast.pp_addr addr;
                                                                   Exceptions.panic "ni"
                                                        end
                                                     | _ -> debug "%a@\n" pp_expr exp; assert false) eargs in
                 let f_signatures = List.filter (fun x -> is_compatible_signature in_types x.in_args cur) f_signatures in
                 (* fun fact, we don't do the isinstance checks, but lower-level things *)
                 debug "f_signatures has size %d@\n" (List.length f_signatures);
                 if List.length f_signatures = 0 then tyerror flow
                 else
                   (* if List.length f_signatures = 1 then *)
                   let process_sign sign =
                     (* let sign = List.hd f_signatures in *)
                     let () = debug "Signature is %a@\n" pp_signature sign in
                     let vars = List.fold_left (fun acc el -> Typevarset.union acc (collect_vars el)) Typevarset.empty sign.in_args in
                     let empty_d3 = Typevarset.fold (fun var acc -> TypeVarMap.add var (Monotypeset.add Top Monotypeset.empty) acc) vars TypeVarMap.empty in
                     let eval_ret =
                       let cstrs = collect_constraints in_types cur.d3 sign.in_args empty_d3 [IntMap.empty, cur.d3 (*cur.d3?*) (*TypeVarMap.empty*)] in
                       if cstrs = [] then let () = debug "collect_constraints: None!@\n" in assert false
                       else
                         List.map (fun (cstrs, d3) ->
                       (* begin match collect_constraints in_types cur.d3 sign.in_args empty_d3 [IntMap.empty, (\* cur.d3 ? *\) TypeVarMap.empty] with
                        * | None -> debug "collect_constraints: None!@\n"; assert false (\* [] *\)
                        * | Some (cstrs, d3) -> *)
                             debug "constraints found: %a@\n" (IntMap.fprint map_printer_endl (fun fmt k -> Format.fprintf fmt "(%d)" k) pp_polytype) cstrs;
                             (* FIXME: use d3, or remove it? *)
                             (* TODO: change output to be a singleton for both stubs and summaries... *)
                             let output = assert ((List.length sign.out_args) = 1); List.hd sign.out_args in
                             let output = subst_ctrs output cstrs in
                             debug "output_ty = %a, d3=%a@\n" pp_polytype output pp_d3 d3;
                             let tid, ncur = get_type ~local_use:true cur output in
                             let flow_ret = Flow.set_domain_cur {ncur with d3} man flow in
                             Eval.singleton (mk_expr (E_type_partition tid) range) flow_ret) cstrs
                     in
                     let eval_exns = List.map (fun exn -> man.exec (Utils.mk_builtin_raise exn range) flow |> Eval.empty_singleton) sign.possible_exn in
                     eval_ret @ eval_exns in
                   let evals = List.fold_left (fun acc sign -> (process_sign sign) @ acc) [] f_signatures in
                   Eval.join_list evals
                   (* if List.length sign.possible_exn = 0 then eval_ret
                    * else
                    *   let eval_exns =
                    *     if List.length sign.possible_exn = 1 then
                    *       man.exec (Utils.mk_builtin_raise (List.hd sign.possible_exn) range) flow |> Eval.empty_singleton
                    *     else
                    *       List.fold_left (fun acc exn -> Eval.join acc (man.exec (Utils.mk_builtin_raise exn range) flow |> Eval.empty_singleton)) (Eval.empty_singleton flow) sign.possible_exn in
                    *   Eval.join eval_exns eval_ret *)
                 (* else
                  *   Exceptions.panic "todo" *)
               )
           |> OptionExt.return


      (*********
       * Lists *
       *********)
      | E_py_list ls ->
         (* FIXME: not modular *)
         summary_constructor man flow range (fun x -> List x) ls

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__add__")}, _)}, args, []) ->
         (* just check that both are instances of lists, and merge the types of both lists? *)
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let lst, lst' = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "list" range)
                   ~fthen:(fun flow ->
                     Eval.assume (mk_py_isinstance_builtin lst' "list" range)
                       ~fthen:(fun flow ->
                         let cur = Flow.get_domain_cur man flow in
                         let tyl = match ekind lst with
                           | E_type_partition tid -> Typingdomain.TypeIdMap.find tid cur.d2
                           | _ -> assert false in
                         let tyl' = match ekind lst' with
                           | E_type_partition tid -> Typingdomain.TypeIdMap.find tid cur.d2
                           | _ -> assert false in
                         let t, d3, pos_d3 = Typingdomain.join_poly (tyl, cur.d3) (tyl', cur.d3) cur.d3 cur.pos_d3 in
                         let cur = {cur with d3; pos_d3} in
                         let flow = Flow.set_domain_cur cur man flow in
                         return_id_of_type man flow range t
                       )
                       ~felse:tyerror
                       man flow
                   )
                   ~felse:tyerror
                   man flow
               )
           |> OptionExt.return
         else
           tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.append")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let lst, el = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "list" range)
                   ~fthen:(fun flow ->
                     let cur = Flow.get_domain_cur man flow in
                     let pty_ellist =
                       let pty_list = match ekind lst with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                       match pty_list with
                       | List x -> x
                       | _ -> assert false in
                     let pty_el = match ekind el with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                     if Typingdomain.polytype_leq (pty_el, cur.d3) (pty_ellist, cur.d3) then
                       Eval.singleton lst flow
                     else
                       let ty, d3, pos_d3 = Typingdomain.join_poly (pty_el, cur.d3) (pty_ellist, cur.d3) cur.d3 cur.pos_d3 in
                       let () = debug "Result of the merge is %a@\n" Typingdomain.pp_polytype ty in
                       let cur = {cur with d3; pos_d3} in
                       let list_tid, cur = Typingdomain.get_type ~local_use:true cur (List ty) in
                       let flow = Flow.set_domain_cur cur man flow in
                       (* match ekind @@ List.hd args with
                        * | E_var _ -> *)
                          man.exec (mk_assign (List.hd args) (mk_expr (E_type_partition list_tid) range) range) flow |>
                            Eval.singleton (mk_py_none range)
                       (* | _ -> Exceptions.panic "list.append on non-variable: todo...%a@\n" pp_expr (List.hd args) *)
                   )
                   ~felse:tyerror
                   man flow
               ) |> OptionExt.return
         else
           tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.extend")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let lst, ext = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "list" range)
                   ~fthen:(fun flow ->
                     let cur = Flow.get_domain_cur man flow in
                     let pty_ellist =
                       let pty_list = match ekind lst with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                       match pty_list with
                       | List x -> x
                       | _ -> assert false in
                     Eval.assume (mk_py_isinstance_builtin ext "list" range)
                       ~fthen:(fun flow ->
                         let pty_elext =
                           let pty_ext = match ekind ext with
                             | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                             | _ -> assert false in
                           match pty_ext with | List x -> x | _ -> assert false in
                         if Typingdomain.polytype_leq (pty_elext, cur.d3) (pty_ellist, cur.d3) then
                           Eval.singleton lst flow
                         else
                           let ty, d3, pos_d3 = Typingdomain.join_poly (pty_elext, cur.d3) (pty_ellist, cur.d3) cur.d3 cur.pos_d3 in
                           let () = debug "Result of the merge is %a@\n" Typingdomain.pp_polytype ty in
                           let cur = {cur with d3; pos_d3} in
                           let list_tid, cur = Typingdomain.get_type ~local_use:true cur (List ty) in
                           let flow = Flow.set_domain_cur cur man flow in
                           match ekind @@ List.hd args with
                           | E_var _ ->
                              man.exec (mk_assign (List.hd args) (mk_expr (E_type_partition list_tid) range) range) flow |>
                                Eval.singleton (mk_py_none range)
                           | _ -> Exceptions.panic "list.extend on non-variable: todo...%a@\n" pp_expr (List.hd args)
                       )
                       ~felse:tyerror
                       man flow
                   )
                   ~felse:tyerror
                   man flow
               ) |> OptionExt.return
         else
           tyerror flow |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.index")}, _)}, [list; value], []) ->
         Eval.eval_list [list; value] man.eval flow |>
           Eval.bind (fun eargs flow ->
               let elist, evalue = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume (mk_py_isinstance_builtin elist "list" range)
                 ~fthen:(fun flow ->
                   man.eval (mk_py_top T_int range) flow |>
                   Eval.join (man.exec (Utils.mk_builtin_raise "ValueError" range) flow |> Eval.empty_singleton)
                 )
                 ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                     Eval.empty_singleton
                 )
               man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__setitem__")}, _)}, args, []) ->
         (* args = list, el, value *)
         (* transformer ça en Weak Update list = el ? *)
         Exceptions.panic "list.__setitem__ not implemented@\n"

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__getitem__")}, _)}, args, []) ->
       *    Eval.eval_list args man.eval flow |>
       *      Eval.bind (fun eargs flow ->
       *          let lst, idx = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
       *          Eval.assume (mk_py_isinstance_builtin lst "list" range)
       *            ~fthen:(fun flow ->
       *              Eval.assume (mk_py_isinstance_builtin idx "int" range)
       *                ~fthen:(fun flow ->
       *                  (* TODO: create new expr_kind so this part of the code is modular in each analysis? *)
       *                  let cur = Flow.get_domain_cur man flow in
       *                  let lst_tid = match ekind lst with
       *                    | E_type_partition i -> i
       *                    | _ -> assert false in
       *                  let tid, ncur = Typingdomain.gather_list_types cur lst_tid in
       *                  let flow = Flow.set_domain_cur ncur man flow in
       *                  Eval.singleton (mk_expr (E_type_partition tid) range) flow
       *                )
       *                ~felse:(fun flow ->
       *                  Eval.assume (mk_py_isinstance_builtin idx "slice" range)
       *                    ~fthen:(fun flow -> Exceptions.panic "FIXME: slices are unsupported yet")
       *                    ~felse:(fun flow ->
       *                      man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
       *                        Eval.empty_singleton
       *                    )
       *                    man flow
       *                )
       *                man flow
       *            )
       *            ~felse:(fun flow ->
       *              man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
       *                Eval.empty_singleton
       *            )
       *          man flow
       *          (* check that lst inherits from list, otherwise raise a typeerror *)
       *          (* check that idx is either an integer or a slide *)
       *          (* return the type(s) of elements inside the container *)
       *        )
       *    |> OptionExt.return *)

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__iter__")}, _)}, [arg], []) ->
       *    man.eval arg flow |>
       *      Eval.bind (fun earg flow ->
       *          Eval.assume (mk_py_isinstance_builtin earg "list" range)
       *            ~fthen:(fun flow ->
       *              let cur = Flow.get_domain_cur man flow in
       *              let lis:Typingdomain.polytype = match ekind arg with
       *                | E_type_partition i ->
       *                   begin match Typingdomain.TypeIdMap.find i cur.d2 with
       *                   | List t -> List t
       *                   | _ -> assert false end
       *                | _ -> assert false in
       *              return_id_of_type man flow range (Typingdomain.Iterator (lis, -1)) (* builtin_inst "list_iterator"*)
       *            )
       *            ~felse:(fun flow ->
       *              man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
       *                Eval.empty_singleton
       *            )
       *            man flow
       *        )
       *    |> OptionExt.return *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list_iterator.__iter__")}, _)}, [self], []) ->
         (* FIXME: check that it's a list_iterator ? *)
         man.eval self flow |> OptionExt.return


      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list_iterator.__next__")}, _)}, [arg], []) ->
       *    man.eval arg flow |>
       *      Eval.bind (fun earg flow ->
       *          match ekind earg with
       *          | E_type_partition i ->
       *             let cur = Flow.get_domain_cur man flow in
       *             let evl =
       *               begin match Typingdomain.TypeIdMap.find i cur.d2 with
       *               | Iterator (List x, _) -> return_id_of_type man flow range x
       *               | _ -> assert false
       *               end
       *             in Eval.join evl (man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton)
       *          | _ -> assert false)
       *    |> OptionExt.return *)

      (********
       * Sets *
       ********)
      | E_py_set els ->
         summary_constructor man flow range (fun x -> Set x) els

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.add")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let lst, el = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "set" range)
                   ~fthen:(fun flow ->
                     let cur = Flow.get_domain_cur man flow in
                     let pty_ellist =
                       let pty_list = match ekind lst with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                       match pty_list with
                       | Set x -> x
                       | _ -> assert false in
                     let pty_el = match ekind el with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                     if Typingdomain.polytype_leq (pty_el, cur.d3) (pty_ellist, cur.d3) then
                       Eval.singleton lst flow
                     else
                       let ty, d3, pos_d3 = Typingdomain.join_poly (pty_el, cur.d3) (pty_ellist, cur.d3) cur.d3 cur.pos_d3 in
                       let () = debug "Result of the merge is %a@\n" Typingdomain.pp_polytype ty in
                       let cur = {cur with d3; pos_d3} in
                       let list_tid, cur = Typingdomain.get_type ~local_use:true cur (Set ty) in
                       let flow = Flow.set_domain_cur cur man flow in
                       match ekind @@ List.hd args with
                       | E_var _ ->
                          man.exec (mk_assign (List.hd args) (mk_expr (E_type_partition list_tid) range) range) flow |>
                            Eval.singleton (mk_py_none range)
                       | _ -> Exceptions.panic "list.append on non-variable: todo...%a@\n" pp_expr (List.hd args)
                   )
                   ~felse:tyerror
                   man flow
               ) |> OptionExt.return
         else
           tyerror flow |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__iter__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               Eval.assume (mk_py_isinstance_builtin earg "set" range)
                 ~fthen:(fun flow ->
                   let cur = Flow.get_domain_cur man flow in
                   let lis:Typingdomain.polytype = match ekind arg with
                     | E_type_partition i ->
                        begin match Typingdomain.TypeIdMap.find i cur.d2 with
                        | Set t -> Set t
                        | _ -> assert false end
                     | _ -> assert false in
                   return_id_of_type man flow range (Typingdomain.Iterator (lis, -1)) (* builtin_inst "list_iterator"*)
                 )
                 ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                     Eval.empty_singleton
                 )
                 man flow
             )
         |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__next__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               match ekind earg with
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let evl =
                    begin match Typingdomain.TypeIdMap.find i cur.d2 with
                    | Iterator (Set x, _) -> return_id_of_type man flow range x
                    | _ -> assert false
                    end
                  in Eval.join evl (man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton)
               | _ -> assert false)
         |> OptionExt.return


      (*********
       * Dicts *
       *********)
      | E_py_dict (ks, vs) ->
         (* FIXME: not modular *)
         let mtys_list annot cur lst =
           List.fold_left (fun acc el ->
               match ekind el with
               | E_type_partition tid ->
                  let pty = Typingdomain.TypeIdMap.find tid cur.Typingdomain.d2 in
                  let mty = Typingdomain.concretize_poly pty cur.Typingdomain.d3 in
                  Typingdomain.Monotypeset.union annot mty acc
               | _ -> Exceptions.panic "%a@\n" pp_expr el) Typingdomain.Monotypeset.empty lst in
         Eval.eval_list ks man.eval flow |>
           Eval.bind (fun e_ks flow ->
               Eval.eval_list vs man.eval flow |>
                 Eval.bind (fun e_vs flow ->
                     let cur = Flow.get_domain_cur man flow in
                     let dummy_annot = Flow.get_all_annot flow in
                     let keys_tys = mtys_list dummy_annot cur e_ks in
                     let values_tys = mtys_list dummy_annot cur e_vs in
                     let open Typingdomain in
                     let pos, cur = match Monotypeset.cardinal keys_tys,
                                          Monotypeset.cardinal values_tys with
                       | 0, 0 ->
                          get_type cur (Dict (Bot, Bot))
                       | 0, _ -> assert false
                       | 1, 1 ->
                          let k_ty = Monotypeset.choose keys_tys in
                          let v_ty = Monotypeset.choose values_tys in
                          get_type cur (Dict (poly_cast k_ty, poly_cast v_ty))
                       | 1, _ ->
                          let k_ty = Monotypeset.choose keys_tys in
                          let v_ptype, cur = get_mtypes cur values_tys in
                          get_type ~local_use:true cur (Dict (poly_cast k_ty, v_ptype))
                       | _ ->
                          let k_type, cur = get_mtypes cur keys_tys in
                          let v_type, cur = get_mtypes cur values_tys in
                          get_type ~local_use:true cur (Dict (k_type, v_type)) in
                     let flow = Flow.set_domain_cur cur man flow in
                     Eval.singleton (mk_expr (E_type_partition pos) range) flow
                   )
             )
         |> OptionExt.return
         (*       let cur = Flow.get_domain_cur man flow in
          *       let dummy_annot = Flow.get_all_annot flow in
          *       let els_types =  in
          *       let pos_list, cur =
          *         match Typingdomain.Monotypeset.cardinal els_types with
          *         | 0 ->
          *            Typingdomain.get_type ~local_use:true cur (c Typingdomain.Bot)
          *         | 1 ->
          *            let ty = Typingdomain.Monotypeset.choose els_types in
          *            Typingdomain.get_type ~local_use:true cur (c (Typingdomain.poly_cast ty))
          *         | _ ->
          *            let pos_types, cur = Typingdomain.get_mtypes cur els_types in
          *            Typingdomain.get_type ~local_use:true cur (c (Typevar pos_types)) in
          *       let flow = Flow.set_domain_cur cur man flow in
          *       Eval.singleton (mk_expr (E_type_partition pos_list) range) flow)
          * |> OptionExt.return *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__setitem__")}, _)}, args, []) ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args = 3 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let dict, key, value = match eargs with [e1;e2;e3] -> e1,e2,e3 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin dict "dict" range)
                   ~fthen:(fun flow ->
                     let open Typingdomain in
                     let cur = Flow.get_domain_cur man flow in
                     let pty_dictkey, pty_dictvalue =
                       let pty_dict = match ekind dict with
                         | E_type_partition i -> TypeIdMap.find i cur.d2
                         | _ -> assert false in
                       match pty_dict with
                       | Dict (k, v) -> k, v
                       | _ -> assert false in
                     let pty_key = match ekind key with
                       | E_type_partition i -> TypeIdMap.find i cur.d2
                       | _ -> assert false in
                     let pty_value = match ekind value with
                       | E_type_partition i -> TypeIdMap.find i cur.d2
                       | _ -> assert false in
                     if polytype_leq (pty_key, cur.d3) (pty_dictkey, cur.d3) &&
                          polytype_leq (pty_value, cur.d3) (pty_dictvalue, cur.d3) then
                       Eval.singleton dict flow
                     else
                       let ty_key, d3, pos_d3 = join_poly (pty_key, cur.d3) (pty_dictkey, cur.d3) cur.d3 cur.pos_d3 in
                       let ty_value, d3, pos_d3 = join_poly (pty_value, d3) (pty_dictvalue, d3) d3 pos_d3 in
                       let () = debug "Result of the merge is keys=%a, values=%a@\n" pp_polytype ty_key pp_polytype ty_value in
                       let cur = {cur with d3; pos_d3} in
                       let dict_tid, cur = get_type ~local_use:true cur (Dict (ty_key, ty_value)) in
                       let flow = Flow.set_domain_cur cur man flow in
                       match ekind @@ List.hd args with
                       | E_var _ ->
                          man.exec (mk_assign (List.hd args) (mk_expr (E_type_partition dict_tid) range) range) flow |>
                            Eval.singleton (mk_py_none range)
                       | _ -> Exceptions.panic "dict.__setitem__ on non-variable: todo...%a@\n" pp_expr (List.hd args)
                   )
                   ~felse:tyerror
                   man flow
               ) |> OptionExt.return
         else
           tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.clear")}, _)}, args, []) ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args = 1 then
           match ekind @@ List.hd args with
           | E_var _ ->
              man.exec (mk_assign (List.hd args) (mk_expr (E_py_dict ([],[])) range) range) flow |> Eval.singleton (mk_py_none range) |> OptionExt.return
           | _ -> Exceptions.panic "dict.clear on non-variable@\n"
         else tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.update")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let lst, ext = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin lst "dict" range)
                   ~fthen:(fun flow ->
                     let cur = Flow.get_domain_cur man flow in
                     let pty_ellist =
                       let pty_list = match ekind lst with
                       | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                       | _ -> assert false in
                       match pty_list with
                       | Dict (k, v) -> (k, v)
                       | _ -> assert false in
                     Eval.assume (mk_py_isinstance_builtin ext "dict" range)
                       ~fthen:(fun flow ->
                         let pty_elext =
                           let pty_ext = match ekind ext with
                             | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                             | _ -> assert false in
                           match pty_ext with | Dict (k, v) -> (k, v) | _ -> assert false in
                         if Typingdomain.polytype_leq (fst pty_elext, cur.d3) (fst pty_ellist, cur.d3) &&
                              Typingdomain.polytype_leq (snd pty_elext, cur.d3) (snd pty_ellist, cur.d3)
                         then
                           Eval.singleton lst flow
                         else
                           let ty_k, d3, pos_d3 = Typingdomain.join_poly (fst pty_elext, cur.d3) (fst pty_ellist, cur.d3) cur.d3 cur.pos_d3 in
                           let ty_v, d3, pos_d3 = Typingdomain.join_poly (snd pty_elext, d3) (snd pty_ellist, d3) d3 pos_d3 in
                           let () = debug "Result of the merge is %a %a@\n" Typingdomain.pp_polytype ty_k Typingdomain.pp_polytype ty_v in
                           let cur = {cur with d3; pos_d3} in
                           let list_tid, cur = Typingdomain.get_type ~local_use:true cur (Dict (ty_k, ty_v)) in
                           let flow = Flow.set_domain_cur cur man flow in
                           match ekind @@ List.hd args with
                           | E_var _ ->
                              man.exec (mk_assign (List.hd args) (mk_expr (E_type_partition list_tid) range) range) flow |>
                                Eval.singleton (mk_py_none range)
                           | _ -> Exceptions.panic "list.append on non-variable: todo...%a@\n" pp_expr (List.hd args)
                       )
                       ~felse:tyerror
                       man flow
                   )
                   ~felse:tyerror
                   man flow
               ) |> OptionExt.return
         else
           tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_iterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_keyiterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_valueiterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_itemiterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      (**********
       * Tuples *
       **********)
      | E_py_tuple ls ->
         Eval.eval_list ls man.eval flow |>
           Eval.bind (fun tuple_els flow ->
               let cur = Flow.get_domain_cur man flow in
               let list_types = List.map (fun x -> match ekind x with
                                                   | E_type_partition tid ->
                                                      Typingdomain.TypeIdMap.find tid cur.d2
                                                   | E_py_object (addr, _) ->
                                                      begin
                                                        let ty = match addr.addr_kind with
                                                          | A_py_class (c, mro) -> Typingdomain.Class (c, mro)
                                                          | A_py_module m ->         Typingdomain.Module m
                                                          | A_py_function f ->       Typingdomain.Function f
                                                          | A_py_method (func, self) ->
                                                             let func = match (fst func).addr_kind with
                                                               | A_py_function f -> f
                                                               | _ -> assert false in
                                                             let self = match ekind self with
                                                               | E_type_partition i -> i
                                                               | _ -> assert false in
                                                             Typingdomain.Method (func, self)
                                                          | _ -> debug "E_py_tuple: %a@\n" Universal.Ast.pp_addr addr;
                                                                 Exceptions.panic "ni"
                                                        in ty
                                                      end
                                                   | _ -> Exceptions.panic "%a@\n" pp_expr x) tuple_els in
               let pos_tuple, cur = Typingdomain.get_type cur (Typingdomain.FiniteTuple list_types) in
               let flow = Flow.set_domain_cur cur man flow in
               Eval.singleton (mk_expr (E_type_partition pos_tuple) range) flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple.__getitem__")}, _)}, args, []) ->
         Eval.eval_list args man.eval flow |>
           Eval.bind (fun eargs flow ->
               let tuple, idx = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume (mk_py_isinstance_builtin tuple "tuple" range)
                 ~fthen:(fun flow ->
                   Eval.assume (mk_py_isinstance_builtin idx "int" range)
                     ~fthen:(fun flow ->
                       let cur = Flow.get_domain_cur man flow in
                       let dummy_annot = Flow.get_all_annot flow in
                       let ty = match ekind tuple with
                         | E_type_partition tid ->
                            let pty = match Typingdomain.TypeIdMap.find tid cur.d2 with
                              | Typingdomain.FiniteTuple l -> l
                              | _ -> assert false in pty
                         | _ -> assert false in
                       (* FIXME: no choice but to join all types... *)
                       let merged_types =
                         List.fold_left (fun acc el ->
                             let mty = Typingdomain.concretize_poly el cur.d3 in
                             Typingdomain.Monotypeset.union dummy_annot mty acc
                           ) Typingdomain.Monotypeset.empty ty in
                       debug "merged_types:%a@\n" Typingdomain.Monotypeset.print merged_types;
                       let pos_tupleel, cur =
                         if Typingdomain.Monotypeset.cardinal merged_types > 1 then
                           let ptype, cur = Typingdomain.get_mtypes cur merged_types in
                           Typingdomain.get_type ~local_use:true cur ptype
                         else
                           Typingdomain.get_type cur (Typingdomain.poly_cast (Typingdomain.Monotypeset.choose merged_types))
                       in
                       let flow = Flow.set_domain_cur cur man flow in
                       Eval.singleton (mk_expr (E_type_partition pos_tupleel) range) flow
                     )
                     ~felse:(fun flow ->
                       Eval.assume (mk_py_isinstance_builtin idx "slice" range)
                         ~fthen:(fun flow -> Exceptions.panic "FIXME: slices are unsupported yet")
                         ~felse:(fun flow ->
                           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                             Eval.empty_singleton
                         )
                         man flow)
                     man flow
                 )
                 ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                     Eval.empty_singleton)
                 man flow)
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple.__iter__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               Eval.assume (mk_py_isinstance_builtin earg "tuple" range)
                 ~fthen:(fun flow ->
                   let cur = Flow.get_domain_cur man flow in
                   let lis:Typingdomain.polytype = match ekind earg with
                     | E_type_partition i ->
                        begin match Typingdomain.TypeIdMap.find i cur.d2 with
                        | FiniteTuple t -> FiniteTuple t
                        | _ -> assert false end
                     | _ -> assert false in
                   return_id_of_type man flow range (Typingdomain.Iterator (lis, 0)) (* builtin_inst "list_iterator"*)
                 )
                 ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                     Eval.empty_singleton
                 )
                 man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple_iterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple_iterator.__next__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               match ekind earg with
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let tuple, pos =
                    match Typingdomain.TypeIdMap.find i cur.d2 with
                    | Iterator (FiniteTuple l, d) -> l, d
                    | _ -> assert false in
                  if pos < List.length tuple then
                    let el = List.nth tuple pos in
                    let newiterator = Typingdomain.Iterator (FiniteTuple tuple, pos+1) in
                    (* FIXME: multiple iterators... *)
                    let cur = {cur with d2 = Typingdomain.TypeIdMap.add i newiterator cur.d2} in
                    let flow = Flow.set_domain_cur cur man flow in
                    return_id_of_type man flow range el
                  else
                    man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |>
                      Eval.empty_singleton
               | _ -> assert false)
         |> OptionExt.return

      (***********
       * Strings *
       ***********)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__iter__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               Eval.assume (mk_py_isinstance_builtin earg "str" range)
                 ~fthen:(fun flow ->
                   let cur = Flow.get_domain_cur man flow in
                   let str:Typingdomain.polytype = match ekind earg with
                     | E_type_partition i ->
                        Typingdomain.TypeIdMap.find i cur.d2
                     | _ -> assert false in
                   return_id_of_type man flow range (Typingdomain.Iterator (str, -1)) (* builtin_inst "list_iterator"*)
                 )
                 ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                     Eval.empty_singleton
                 )
                 man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)} as call, [arg], []) ->
         let args' = arg :: (mk_py_none range) :: (mk_constant T_int (C_int (Z.of_int (-1))) range) :: [] in
         man.eval {exp with ekind = E_py_call(call, args', [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)} as call, [arg; sep], []) ->
         let args' = arg :: sep :: (mk_constant T_int (C_int (Z.of_int (-1))) range) :: [] in
         man.eval {exp with ekind = E_py_call(call, args', [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)}, [arg; sep; maxsplit], []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         Eval.eval_list [arg; sep; maxsplit] man.eval flow |>
           Eval.bind (fun eargs flow ->
               let earg, esep, emaxsplit = match eargs with [e1; e2; e3] -> e1, e2, e3 | _ -> assert false in
               Eval.assume (mk_py_isinstance_builtin earg "str" range)
                 ~fthen:(fun flow ->
                   Eval.assume (mk_py_isinstance_builtin maxsplit "int" range)
                     ~fthen:(fun flow ->
                       Eval.assume (mk_py_isinstance_builtin sep "str" range)
                         ~fthen:(fun flow -> return_id_of_type man flow range (Typingdomain.List (Typingdomain.builtin_inst "str")))
                         ~felse:tyerror
                         man flow
                     )
                     ~felse:tyerror
                     man flow
                 )
                 ~felse:tyerror
                 man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str_iterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str_iterator.__next__")}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               match ekind earg with
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let evl =
                    begin match Typingdomain.TypeIdMap.find i cur.d2 with
                    | Iterator (Instance {classn=Class (C_builtin "str", _)} as i, _) -> return_id_of_type man flow range i
                    | _ -> assert false
                    end
                  in Eval.join evl (man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton)
               | _ -> assert false)
         |> OptionExt.return


      (*********
       * Range *
       *********)
      (* FIXME: most of that part should be moved from OCaml code to python type signatures *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)} as call, cls :: [up], []) ->
         let args' = (mk_constant T_int (C_int (Z.of_int 0)) range)::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
         man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)} as call, cls :: [down; up], []) ->
         let args' = down::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
         man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)}, cls :: args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         (if ListExt.length args = 3 then
            Eval.eval_list args man.eval flow |>
              Eval.bind (fun eargs flow ->
                  let down, up, step = match eargs with [e1;e2;e3] -> e1, e2, e3 | _ -> assert false in
                  Eval.assume (mk_py_isinstance_builtin down "int" range)
                    ~fthen:(fun flow ->
                      Eval.assume (mk_py_isinstance_builtin up "int" range)
                        ~fthen:(fun flow ->
                          Eval.assume (mk_py_isinstance_builtin step "int" range)
                            ~fthen:(fun flow ->
                              return_id_of_type man flow range (Typingdomain.builtin_inst "range"))
                            ~felse:tyerror
                            man flow
                        )
                        ~felse:tyerror
                        man flow
                    )
                    ~felse:tyerror
                    man flow
                )
          else
           tyerror flow)
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__contains__")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let ra, el = match eargs with [e1; e2] -> e1, e2 | _ -> assert false in
                 Eval.assume (mk_py_isinstance_builtin ra "range" range)
                   ~fthen:(fun flow ->
                     Eval.assume (mk_py_isinstance_builtin el "int" range)
                       ~fthen:(fun flow ->
                         return_id_of_type man flow range (Typingdomain.builtin_inst "bool"))
                       ~felse:tyerror
                       man flow
                   )
                   ~felse:tyerror
                   man flow
               )
           |> OptionExt.return
         else
           tyerror flow
           |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__len__")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 1 then
           man.eval (List.hd args) flow |>
             Eval.bind (fun ra flow ->
                 Eval.assume (mk_py_isinstance_builtin ra "range" range)
                   ~fthen:(fun flow ->
                     return_id_of_type man flow range (Typingdomain.builtin_inst "int"))
                   ~felse:tyerror
                   man flow
               )
           |> OptionExt.return
         else
           tyerror flow
           |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__iter__")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 1 then
           man.eval (List.hd args) flow |>
             Eval.bind (fun ra flow ->
                 Eval.assume (mk_py_isinstance_builtin ra "range" range)
                   ~fthen:(fun flow ->
                     return_id_of_type man flow range (Typingdomain.builtin_inst "range_iterator"))
                   ~felse:tyerror
                   man flow
               )
           |> OptionExt.return
         else
           tyerror flow
           |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range_iterator.__iter__")}, _)}, [self], []) ->
         man.eval self flow |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range_iterator.__next__")}, _)}, args, []) ->
         (* FIXME: ne pas oublier le StopIteration. Pour les autres next *)
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 1 then
           man.eval (List.hd args) flow |>
             Eval.bind (fun ra flow ->
                 Eval.assume (mk_py_isinstance_builtin ra "range_iterator" range)
                   ~fthen:(fun flow ->
                     Eval.join
                       (return_id_of_type man flow range (Typingdomain.builtin_inst "int"))
                       (man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton)
                   )
                   ~felse:tyerror
                   man flow
               )
           |> OptionExt.return
         else
           tyerror flow
           |> OptionExt.return

      (*****************************
       * Mopsa-related type tests  *
       *****************************)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_list_of")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let open Typingdomain in
                 let li, ty = match eargs with [e1;e2] -> e1, e2 | _ -> assert false in
                 let cur = Flow.get_domain_cur man flow in
                 let li, cur = match ekind li with
                   | E_type_partition i ->
                      begin match TypeIdMap.find i cur.d2 with
                      | List x ->
                         let rec process x = match x with
                           | Instance {classn} -> classn, cur
                           | List _ -> let x, y = Addr.builtin_cl_and_mro "list" in Class (x, y), cur
                           | Dict _ -> let x, y = Addr.builtin_cl_and_mro "dict" in Class (x, y), cur
                           | Typevar a ->
                              let mtys = TypeVarMap.find a cur.d3 in
                              let new_d3 = TypeVarMap.add a (Monotypeset.map (fun x -> mono_cast (fst @@ process (poly_cast x))) mtys) cur.d3 in
                              let d = {cur with d3 = new_d3} in
                              x, d
                           | _ -> Exceptions.panic "todo, assertlistof %a@\n@\n" pp_polytype x in
                         let ty, cur = process x in
                         List ty, cur
                      | _ -> assert false end
                   | _ -> assert false in
                 let ty = match ekind ty with
                   | E_type_partition i -> TypeIdMap.find i cur.d2
                   | E_py_object (addr, _) ->
                      begin match addr.addr_kind with
                      | A_py_class (c, mro) -> Class (c, mro)
                      | _ -> assert false
                      end
                   | _ -> assert false in
                 debug "list_of_leq? li=%a ty=%a d3=%a@\n" pp_polytype li pp_polytype (List ty) pp_d3 cur.d3;
                 Libs.Py_mopsa.check man (mk_py_bool (polytype_leq (li, cur.d3) (List ty, cur.d3)) range) range (Flow.set_domain_cur cur man flow)
               )
           |> OptionExt.return
         else tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_set_of")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let open Typingdomain in
                 let li, ty = match eargs with [e1;e2] -> e1, e2 | _ -> assert false in
                 let cur = Flow.get_domain_cur man flow in
                 let ty = match ekind ty with
                   | E_type_partition i -> TypeIdMap.find i cur.d2
                   | E_py_object (addr, _) ->
                      begin match addr.addr_kind with
                          | A_py_class (c, mro) -> Instance {classn=(Class (c, mro)); uattrs=StringMap.empty; oattrs=StringMap.empty}
                          (* | A_py_class (c, mro) -> Typingdomain.Class (c, mro)
                           * | A_py_module m ->         Typingdomain.Module m
                           * | A_py_function f ->       Typingdomain.Function f
                           * | A_py_method (func, self) ->
                           *    let func = match (fst func).addr_kind with
                           *      | A_py_function f -> f
                           *      | _ -> assert false in
                           *    let self = match ekind self with
                           *      | E_type_partition i -> i
                           *      | _ -> assert false in
                           *    Typingdomain.Method (func, self) *)
                          | _ -> Exceptions.panic "ni@\n"
                      end
                   | _ -> assert false in
                 let li = match ekind li with
                   | E_type_partition i -> TypeIdMap.find i cur.d2
                   | _ -> assert false in
                 Libs.Py_mopsa.check man (mk_py_bool (polytype_leq (li, cur.d3) (Set ty, cur.d3)) range) range flow
               )
           |> OptionExt.return
         else tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_dict_of")}, _)}, args, []) ->
         let tyerror = fun flow ->
           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
             Eval.empty_singleton in
         if List.length args = 3 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let open Typingdomain in
                 let dict, ty_k, ty_v = match eargs with [e1;e2;e3] -> e1, e2, e3 | _ -> assert false in
                 let cur = Flow.get_domain_cur man flow in
                 let process = fun ty -> match ekind ty with
                   | E_type_partition i -> TypeIdMap.find i cur.d2
                   | E_py_object (addr, _) ->
                      begin match addr.addr_kind with
                          | A_py_class (c, mro) -> Instance {classn=(Class (c, mro)); uattrs=StringMap.empty; oattrs=StringMap.empty}
                          (* | A_py_module m ->         Typingdomain.Module m
                           * | A_py_function f ->       Typingdomain.Function f
                           * | A_py_method (func, self) ->
                           *    let func = match (fst func).addr_kind with
                           *      | A_py_function f -> f
                           *      | _ -> assert false in
                           *    let self = match ekind self with
                           *      | E_type_partition i -> i
                           *      | _ -> assert false in
                           *    Typingdomain.Method (func, self) *)
                          | _ -> Exceptions.panic "ni@\n"
                      end
                   | _ -> assert false in
                 let ty_k = process ty_k in
                 let ty_v = process ty_v in
                 let dict = match ekind dict with
                   | E_type_partition i -> TypeIdMap.find i cur.d2
                   | _ -> assert false in
                 debug "assert_dict_of %a [%a[%a] <=? %a[%a]]@\n" pp_expr (mk_py_bool (polytype_leq (dict, cur.d3) (Dict (ty_k, ty_v), cur.d3)) range) pp_polytype dict pp_d3 cur.d3 pp_polytype (Dict (ty_k, ty_v)) pp_d3 cur.d3;
                 Libs.Py_mopsa.check man (mk_py_bool (polytype_leq (dict, cur.d3) (Dict (ty_k, ty_v), cur.d3)) range) range flow
               )
           |> OptionExt.return
         else tyerror flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_tuple_of")}, _)}, args, []) ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args = 2 then
           Eval.eval_list args man.eval flow |>
             Eval.bind (fun eargs flow ->
                 let open Typingdomain in
                 let var, tu_classes = match eargs with [e1;e2] -> e1, e2 | _ -> assert false in
                 let cur = Flow.get_domain_cur man flow in
                 let ty_var = match ekind var with
                   | E_type_partition i ->
                      begin match TypeIdMap.find i cur.d2 with
                      | FiniteTuple t ->
                         FiniteTuple (List.map (function
                                          | Instance {classn} -> classn
                                          | List _ ->
                                             let x, y = Addr.builtin_cl_and_mro "list" in
                                             Class (x, y)
                                          | _ -> Exceptions.panic "otod, assert_tuple_of@\n") t)
                      | _ -> assert false end
                   | _ -> assert false in
                 let ty_classes = match ekind tu_classes with
                   | E_type_partition i ->
                      begin match TypeIdMap.find i cur.d2 with
                      | FiniteTuple t -> FiniteTuple t
                      | _ -> assert false end
                   | _ -> assert false in
                 debug "assert_tuple of %a: [%a[%a] <=? %a[%a]]@\n" pp_expr (mk_py_bool (polytype_leq (ty_var, cur.d3) (ty_classes, cur.d3)) range) pp_polytype ty_var pp_d3 cur.d3 pp_polytype ty_classes pp_d3 cur.d3;
                 Libs.Py_mopsa.check man (mk_py_bool (polytype_leq (ty_var, cur.d3) (ty_classes, cur.d3)) range) range flow
               ) |> OptionExt.return
         else tyerror flow |> OptionExt.return

        (*************
         * Summaries *
         *************)
      | E_py_sum_call (f, args) ->
         let func = match ekind f with
           | E_function (User_defined func) -> func
           | _ -> assert false in
         if !opt_pyty_summaries then begin
             Eval.eval_list args man.eval flow |>
               Eval.bind (fun eargs flow ->
                   let cur = Flow.get_domain_cur man flow in
                   let annots = try Flow.get_annot A_py_summaries flow with Not_found -> [] in
                   let inputs = List.map (fun exp -> match ekind exp with
                                                     | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                                                     | _ -> debug "%a@\n" pp_expr exp; assert false) eargs in
                   let closure = [] in
                   let d3 = (Flow.get_domain_cur man flow).d3 in
                   if Typingdomain.exist_compatible_summary func inputs closure d3 annots then
                     (
                       let sp, cl = Typingdomain.get_compatible_summary func inputs closure d3 annots in
                       let s_i, s_io, s_o, s_d3 = sp in
                       (* change the inputs into the type of s_io *)
                       let cur = List.fold_left2 (fun cur e_arg ptype ->
                                     match ekind e_arg with
                                     | E_type_partition i ->
                                        (* FIXME: this update is too brutal if different variables are pointing here *)
                                        let d2 = Typingdomain.TypeIdMap.add i ptype cur.Typingdomain.d2 in
                                        {cur with d2}
                                     | _ -> assert false
                                   ) cur eargs s_io in
                       let s_o = match s_o with
                         | [x] -> x
                         | _ -> assert false in
                       let tid, cur = Typingdomain.get_type cur s_o in
                       let flow = Flow.set_domain_cur cur man flow in
                       Eval.singleton (mk_expr (E_type_partition tid) range) flow
                     (* get the output type s_o and return it *)
                     (* fixme: d3 use at least *)
                     )
                   else
                     (
                       debug "no summary found, proceeding with the inlining@\n";
                       man.eval (mk_call func args range) flow |>
                         Eval.bind (fun output flow ->
                             man.eval output flow |>
                               Eval.bind (fun e_output flow ->
                                   let cur = Flow.get_domain_cur man flow in
                                   let inputs_out = List.map (fun exp -> match ekind exp with
                                                                         | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                                                                         | _ -> debug "%a@\n" pp_expr exp; assert false) eargs in
                                   let outputs = List.map (fun exp -> match ekind exp with
                                                                      | E_type_partition i -> Typingdomain.TypeIdMap.find i cur.d2
                                                                      | _ -> debug "%a@\n" pp_expr exp; assert false) [e_output] in
                                   let function_summary = ((inputs, inputs_out, outputs, cur.d3), []) in
                                   let function_summaries =
                                     if ListExt.mem_assoc func annots then
                                       let tl = ListExt.assoc func annots in
                                       (func, function_summary::tl)
                                     else
                                       (func, [function_summary]) in
                                   let updated_summaries = function_summaries::(ListExt.remove_assoc func annots) in
                                   debug "Summaries are now:@\n%a@\n" pp_summaries updated_summaries;
                                   let flow = Flow.set_annot A_py_summaries updated_summaries flow in
                                   Eval.singleton output flow)
                           )
                     )
                 )
             |> OptionExt.return
           end
         else
           man.eval (mk_call func args range) flow
           |> OptionExt.return



      | _ ->
         debug "Warning: no eval for %a" pp_expr exp;
         None


    let is_type_query : type r. r Framework.Query.query -> bool =
      function
      | Q_types _ -> true
      | _ -> false

    let ask : type r. r Framework.Query.query -> ('a, t) man -> 'a flow -> r option =
      fun query man flow ->
      match query with
      | Q_types t ->
         let cur = Flow.get_domain_cur man flow in
         let tid = match ekind t with
           | E_type_partition i -> i
           | _ -> assert false in
         Some (Typingdomain.TypeIdMap.find tid cur.d2)
      | _ -> None

  end

let () = register_domain (module Domain)
