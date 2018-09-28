(* TODO: move S_assume and eval of not into t_bool domain? *)
open Framework.Essentials
open Ast
open Universal.Ast
open Addr

type expr_kind +=
   | E_get_type_partition of Typingdomain.polytype
   | E_type_partition of Typingdomain.typeid

let () =
  register_pp_expr (fun default fmt exp ->
      match ekind exp with
      | E_get_type_partition ptype -> Format.fprintf fmt "E_get_tp %a" Typingdomain.pp_polytype ptype
      | E_type_partition tid -> Format.fprintf fmt "TypeId %d" tid
      | _ -> default fmt exp)


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

    let exec_interface = { export = [Zone.Z_py]; import = [any_zone]; }
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
         Framework.Exceptions.panic "var/var assignment todo"

      | S_assign({ekind = E_var (v, STRONG)} as l, e) ->
         OptionExt.return
           (man.eval e flow |>
              Post.bind man @@
                fun e flow ->
                let cur = Flow.get_domain_cur man flow in
                begin match ekind e with
                | E_get_type_partition ptype ->
                   (*let pos, cur' = Typingdomain.get_type cur ptype in*)
                   let cur' = Typingdomain.set_var cur v Typingdomain.{lundef=false; gundef=false; def=Some ptype} in
                   let flow = Flow.set_domain_cur cur' man flow in
                   debug "\t%a@\n" print (Flow.get_domain_cur man flow);
                   Post.of_flow flow
                | E_py_object (addr, _) ->
                   begin
                     let ty = match addr.addr_kind with
                       | A_py_class (c, bases) -> Typingdomain.Class (c, bases)
                       | A_py_module m ->         Typingdomain.Module m
                       | A_py_function f ->       Typingdomain.Function (f, [])
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
               | _ -> failwith "bla"
             )
         |> OptionExt.return

      | _ -> None

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_constant C_py_not_implemented ->
         let builtin_notimpl = Typingdomain.builtin_inst "NotImplementedType" in
         let expr = mk_expr (E_get_type_partition builtin_notimpl) range in
         Eval.singleton expr flow |> OptionExt.return

      | E_alloc_addr akind ->
         let addr = {addr_kind = akind; addr_uid=(-1)} in
         Eval.singleton (mk_addr addr range) flow |> OptionExt.return

      | E_var (v, _) ->
         let cur = Flow.get_domain_cur man flow in
         let polytype = Typingdomain.get_polytype cur v in
         let expr = match polytype with
           | Typingdomain.Class (c, b) -> mk_py_object ({addr_kind=A_py_class (c, b); addr_uid=(-1)}, mk_expr (ekind exp) range) range
           | Typingdomain.Function (f, _) -> mk_py_object ({addr_kind=A_py_function f; addr_uid=(-1)}, mk_expr (ekind exp) range) range
           | Typingdomain.Module m -> mk_py_object ({addr_kind=A_py_module m; addr_uid=(-1)}, mk_expr (ekind exp) range) range
           | _ -> mk_expr (E_get_type_partition polytype) range in
         Eval.singleton expr flow
         (* FIXME: properly handle every case *)
         (* let ak = Typingdomain.get_addr_kind cur v in
          * let a = {addr_kind=ak; addr_uid=(-1)} in
          * Eval.singleton (mk_py_object (a, mk_expr (ekind exp) range) range) flow *)
         |> OptionExt.return

      | E_py_attribute (e, attr) ->
         man.eval e flow |>
         Eval.bind (fun exp flow ->
             match ekind exp with
             | E_py_object ({addr_kind = A_py_module _}, _) ->
                Eval.singleton (mk_py_object (Addr.find_builtin_attribute (object_of_expr exp) attr) range) flow
             | E_py_object ({addr_kind = A_py_class (C_builtin c, _)}, _) ->
                let r = find_builtin_attribute (object_of_expr exp) attr in
                Eval.singleton (mk_py_object r range) flow

             | _ ->
                debug "%a@\n" pp_expr exp; assert false
           )
         |> OptionExt.return

      | E_unop(Framework.Ast.O_log_not, e') ->
         man.eval e' flow |>
           Eval.bind
             (fun exp flow ->
             (* FIXME: test if instance of bool and proceed accordingly *)
               match ekind exp with
               | E_constant (C_top T_bool) -> Eval.singleton exp flow
               | E_constant (C_bool true) ->  Eval.singleton (mk_py_false range) flow
               | E_constant (C_bool false) -> Eval.singleton (mk_py_true range) flow
               | _ -> failwith "not: ni"
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
         man.eval arg flow |>
           Eval.bind
             (fun e_arg flow ->
               let cl = match ekind e_arg with
                 | E_get_type_partition pt ->
                    let ty = match pt with
                      | Typingdomain.Instance {Typingdomain.classn=Typingdomain.Class (c, b)} -> (c, b)
                      | Typingdomain.Class _ -> C_builtin "type", [Addr.find_builtin "object"]
                      | _ -> assert false
                    in ty
                 | E_py_object ({addr_kind = A_py_class (c, b)}, _) ->
                    let tyo = Addr.kind_of_object (Addr.find_builtin "type") in
                    begin match tyo with
                    | A_py_class (c, b) -> c, b
                    | _ -> assert false
                    end
                 | _ -> debug "type(%a)?@\n" pp_expr e_arg; assert false in
               let obj = ({addr_kind=A_py_class (fst cl, snd cl); addr_uid=(-1)}, mk_expr (ekind exp) range) in
               Eval.singleton (mk_py_object obj range) flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
         (* probablement évaluer dans la zone python des types ? *)
         (Eval.eval_list [obj; attr] man.eval flow |>
            Eval.bind (fun evals flow ->
                let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
                match ekind eobj, ekind eattr with
                | E_get_type_partition ty, E_py_object ({addr_kind = A_py_class (cls, b)}, _) ->
                   let cur = Flow.get_domain_cur man flow in
                   let dt, df = Typingdomain.filter_ty_inst cur ty (Typingdomain.Class (cls, b)) in
                   let flowt = Flow.set_domain_cur dt man flow and
                       flowf = Flow.set_domain_cur df man flow in
                   begin match is_bottom dt, is_bottom df with
                   | true, true -> Eval.empty_singleton flowt
                   | true, false -> Eval.singleton (mk_py_false range) flowf
                   | false, true -> Eval.singleton (mk_py_true range) flowt
                   | false, false ->
                      Eval.join
                        (Eval.singleton (mk_py_true range) flowt)
                        (Eval.singleton (mk_py_false range) flowf)
                   end
                | _ -> Debug.fail "todo: implement isinstance(%a, %a)@\n" pp_expr eobj pp_expr eattr
             )
         )
         |> OptionExt.return

      (* | E_py_object ({addr_kind = A_py_class (c, b)}, _) ->
       *    debug "blai@\n";
       *    None *)
      | _ ->
         debug "Warning: no eval for %a" pp_expr exp;
         None

    let ask query man flow = None
  end

let () = register_domain (module Domain)
