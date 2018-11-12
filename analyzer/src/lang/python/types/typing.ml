(* TODO: move S_assume and eval of not into t_bool domain? *)
open Framework.Essentials
open Ast
open Universal.Ast
open Addr
open Data_model.Attribute
open MapExt

type expr_kind +=
   | E_get_type_partition of Typingdomain.polytype
   | E_type_partition of Typingdomain.typeid

let () =
  register_pp_expr (fun default fmt exp ->
      match ekind exp with
      | E_get_type_partition ptype -> Format.fprintf fmt "E_get_tp %a" Typingdomain.pp_polytype ptype
      | E_type_partition tid -> Format.fprintf fmt "TypeId %d" tid
      | _ -> default fmt exp);
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_get_type_partition p1, E_get_type_partition p2 -> Debug.fail "fixme"
      | E_type_partition i1, E_type_partition i2 -> Pervasives.compare i1 i2
      | _ -> next e1 e2)



module Domain =
  struct
    type t = Typingdomain.domain

    type _ domain += D_python_typing : t domain

    let id = D_python_typing
    let name = "python.types.typing"
    let identify : type a. a domain -> (t, a) eq option = function
      | D_python_typing -> Some Eq
      | _ -> None

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
      let tid, ncur = Typingdomain.get_type cur ptype in
      let flow = Flow.set_domain_cur ncur man flow in
      Eval.singleton (mk_expr (E_type_partition tid) range) flow |> OptionExt.return

    let init progr man flow =
      Some ( Flow.set_domain_env T_cur Typingdomain.top man flow )

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
                       | A_py_function f ->       Typingdomain.Function (f, [])
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
                | _ -> Framework.Exceptions.panic_at stmt.srange "typing/exec/S_assign: exp %a ni@\n" pp_expr e
                end)

      | S_assign({ekind = E_py_attribute({ekind = E_var (v, mode)}, attr)}, rval) ->
         man.eval rval flow |>
           Post.bind man
             (fun reval flow ->
               let cur = Flow.get_domain_cur man flow in
               match ekind reval with
               | E_type_partition i ->
                  Flow.set_domain_cur (Typingdomain.set_var_attr_ty cur v attr i) man flow |> Post.of_flow
               | _ -> Debug.fail "%a" pp_expr reval
             )
         |> OptionExt.return


      | S_remove_var v ->
         debug "Removing var %a@\n" pp_var v;
         let cur = Flow.get_domain_cur man flow in
         let flow = Flow.set_domain_cur (Typingdomain.rm_var cur v) man flow in
         Post.return flow

      (* S⟦ ?e ⟧ *)
      | S_assume e ->
         man.eval e flow |>
           Post.bind man
             (fun expr flow ->
               debug "Assuming %a@\n" pp_expr expr;
               match ekind expr with
               | E_constant (C_top T_bool) -> Post.of_flow flow
               | E_constant (C_bool true) -> Post.of_flow flow
               | E_constant (C_bool false) -> Post.of_flow (Flow.bottom (Flow.get_all_annot flow))
               | E_type_partition i ->
                  let cur = Flow.get_domain_cur man flow in
                  let pt = Typingdomain.TypeIdMap.find i cur.d2 in
                  begin match pt with
                        | Instance {classn=Class (C_builtin "bool", _); _} -> Post.of_flow flow
                        | _ -> Debug.fail "%a" Typingdomain.pp_polytype pt
                  end
               | _ -> Debug.fail "%a" pp_expr e
             )
         |> OptionExt.return

      | _ -> None

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_constant (C_top T_bool)
        | E_constant (C_bool _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "bool")

      | E_constant (C_top T_int)
        | E_constant (C_int _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "int")

      | E_constant (C_top (T_float _))
        | E_constant (C_float _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "float")

      | E_constant (C_string _) ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "str")

      | E_py_bytes _ ->
         return_id_of_type man flow range (Typingdomain.builtin_inst "bytes")

      | E_py_list ls ->
         (* FIXME: not modular *)
         Eval.eval_list ls man.eval flow |>
           Eval.bind (fun list_els flow ->
               let cur = Flow.get_domain_cur man flow in
               let dummy_annot = Flow.get_all_annot flow in
               let els_types = List.fold_left (fun acc el ->
                                   match ekind el with
                                   | E_type_partition tid ->
                                      let pty = Typingdomain.TypeIdMap.find tid cur.d2 in
                                      let mty = Typingdomain.concretize_poly pty cur.d3 in
                                      Typingdomain.Monotypeset.union dummy_annot mty acc
                                   | _ -> Debug.fail "%a@\n" pp_expr el) Typingdomain.Monotypeset.empty list_els in
               let pos_types, cur = Typingdomain.get_mtypes cur els_types in
               let pos_list, cur = Typingdomain.get_type ~local_use:true cur (List (Typevar pos_types)) in
               let flow = Flow.set_domain_cur cur man flow in
               Eval.singleton (mk_expr (E_type_partition pos_list) range) flow)
         |> OptionExt.return

      | E_constant C_py_not_implemented ->
         let builtin_notimpl = Typingdomain.builtin_inst "NotImplementedType" in
         return_id_of_type man flow range builtin_notimpl

      | E_constant C_py_none ->
         let builtin_none = Typingdomain.builtin_inst "NoneType" in
         return_id_of_type man flow range builtin_none

      | E_alloc_addr akind ->
         begin match akind with
         (* | A_py_method (f, obj) ->
          *    let f = match (fst f).addr_kind with
          *      | A_py_function f -> f
          *      | _ -> assert false in
          *    (\*            Debug.fail "todo"*\)
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
               | Typingdomain.Function (f, _) -> mk_py_object ({addr_kind=A_py_function f; addr_uid=(-1)}, mk_expr (ekind exp) range) range
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
               let cur = Flow.get_domain_cur man flow in
               let tid, cur = Typingdomain.get_type cur Bot in
               let flow = Flow.set_domain_cur cur man flow in
               let () = debug "Cur is now %a@\n" print cur in
               Eval.singleton (mk_expr (E_type_partition tid) range) flow |> OptionExt.return
         end
      | E_py_ll_hasattr(e, attr) ->
         let attr = match ekind attr with
           | E_constant (C_string s) -> s
           | _ -> assert false in
      (* FIXME? as this is not a builtin constructor, we assume e is already evaluated *)
         begin match ekind e with
         | E_py_object ({addr_kind = A_py_module _}, _)
           | E_py_object ({addr_kind = A_py_class (C_builtin _, _)}, _) ->
            Eval.singleton (mk_py_bool (Addr.is_builtin_attribute (object_of_expr e) attr) range) flow
         | E_py_object ({addr_kind = A_py_class (C_user c, b)}, _) ->
            Eval.singleton (mk_py_bool (List.exists (fun v -> v.vname = attr) c.py_cls_static_attributes) range) flow
         | E_type_partition i ->
            let cur = Flow.get_domain_cur man flow in
            debug "cur=%a@\n" print cur;
            let pt = Typingdomain.TypeIdMap.find i cur.d2 in
            begin match pt with
            | Typingdomain.Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
               Eval.singleton (mk_py_true range) flow
            | Typingdomain.Instance {classn; uattrs; oattrs} when not (StringMap.exists (fun k _ -> k = attr) uattrs || StringMap.exists (fun k _ -> k = attr) oattrs) ->
               Eval.singleton (mk_py_false range) flow
            | Typingdomain.Instance {classn; uattrs; oattrs} ->
               let cur = Flow.get_domain_cur man flow in
               let dt, df = Typingdomain.filter_attr cur i attr in
               let flowt = Flow.set_domain_cur dt man flow in
               let flowf = Flow.set_domain_cur df man flow in
               (* debug "Bad hasattr on partition %d, initial domain: %a@\n spliting in two cases: %a and %a@\n" i print cur print dt print df; *)
               Eval.join
                 (Eval.singleton (mk_py_true range) flowt)
                 (Eval.singleton (mk_py_false range) flowf)
            | _ -> Debug.fail "ll_hasattr"
            end
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
                | _ -> Debug.fail "E_py_ll_getattr: shouldn't happen?"
              end
           | _ -> Debug.fail "E_py_ll_getattr: todo"
         end
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
                  | _ -> Debug.fail "%a" Typingdomain.pp_polytype pt
                  end
               | _ -> failwith "not: ni"
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind
             (fun e_arg flow ->
               let cl = match ekind e_arg with
                 | E_type_partition i ->
                    let cur = Flow.get_domain_cur man flow in
                    let pt = Typingdomain.TypeIdMap.find i cur.d2 in
                 (* | E_get_type_partition pt -> *)
                    let ty = match pt with
                      | Typingdomain.Instance {Typingdomain.classn=Typingdomain.Class (c, b)} -> (c, b)
                      | Typingdomain.Class _ -> C_builtin "type", List.map Addr.find_builtin ["type"; "object"]
                      | Typingdomain.List _ -> C_builtin "list", List.map Addr.find_builtin ["list"; "object"]
                      | _ -> assert false
                    in ty
                 | E_py_object ({addr_kind = A_py_class (c, b)}, _) ->
                    let tyo = Addr.kind_of_object (Addr.find_builtin "type") in
                    begin match tyo with
                    | A_py_class (c, b) -> c, b
                    | _ -> assert false
                    end
                 | E_py_object ({addr_kind = A_py_module _}, _) ->
                    let tyo = Addr.kind_of_object (Addr.find_builtin "module") in
                    begin match tyo with
                    | A_py_class (c, b) -> c, b
                    | _ -> assert false
                    end
                 | E_py_object ({addr_kind = A_py_function _}, _) ->
                    let tyo = Addr.kind_of_object (Addr.find_builtin "function") in
                    begin match tyo with
                    | A_py_class (c, b) -> c, b
                    | _ -> assert false
                    end
                 | _ -> debug "type(%a)?@\n" pp_expr e_arg; assert false in
               let obj = ({addr_kind=A_py_class (fst cl, snd cl); addr_uid=(-1)}, mk_expr (ekind exp) range) in
               Eval.singleton (mk_py_object obj range) flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
         (* FIXME: TypeError if arg 1 is not a class, arg 2 is not a class or tuple of classes *)
         Eval.eval_list [cls; cls'] man.eval flow |>
           Eval.bind (fun evals flow ->
               let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
               match ekind cls, ekind cls' with
                   | E_py_object ({addr_kind = A_py_class (c, mro)} as a, _),
                     E_py_object ({addr_kind = A_py_class (c', mro')} as a', _) ->
                      if List.exists (fun x -> Universal.Ast.compare_addr (fst x) a' = 0) mro then
                        Eval.singleton (mk_py_true range) flow
                      else
                        Eval.singleton (mk_py_false range) flow
                   | _ -> Debug.fail "FIXME"
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
         (* probablement évaluer dans la zone python des types ? *)
         Eval.eval_list [obj; attr] man.eval flow |>
           Eval.bind (fun evals flow ->
               let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
               debug "eobj = %a, eattr = %a@\n" pp_expr eobj pp_expr eattr;
               match ekind eobj, ekind eattr with
               | E_type_partition tid, E_py_object ({addr_kind = A_py_class (cls, b)}, _) ->
                  let cur = Flow.get_domain_cur man flow in
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
                  Debug.fail "Left MRO %a@\nRight MRO %a@\n"
                    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                       (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (Range_fresh (-1)))))
                    mro
                    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                       (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (Range_fresh (-1)))))
                    mro'
               | E_py_object ({addr_kind = A_py_module _}, _), _ ->
                  begin match ekind eattr with
                  | E_py_object ({addr_kind = A_py_class (C_builtin c, _)}, _) when c = "object" || c = "module" ->
                     Eval.singleton (mk_py_true range) flow
                  | _ ->
                     Eval.singleton (mk_py_false range) flow
                  end
               | _ ->
                  if is_bottom (Flow.get_domain_cur man flow) then Eval.empty_singleton flow else
                  Debug.fail "todo: implement isinstance(%a, %a)@\n" pp_expr eobj pp_expr eattr
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

      | _ ->
         debug "Warning: no eval for %a" pp_expr exp;
         None


    let ask query man flow = None
  end

let () = register_domain (module Domain)
