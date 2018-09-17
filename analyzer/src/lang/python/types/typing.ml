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
      | E_get_type_partition ptype -> Typingdomain.pp_polytype fmt ptype
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

    let exec_interface = { export = [Zone.Z_py]; import = []; }
    let eval_interface = { export = [Framework.Zone.Z_top, Framework.Zone.Z_top]; import = []; }

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
      | S_assign({ekind = E_var v}, {ekind = E_py_undefined t}, mode) ->
         let cur = Flow.get_domain_cur man flow in
         let t_with_undefs = Typingdomain.{lundef = not t; gundef = t; def = None} in
         Flow.set_domain_cur (Typingdomain.set_var cur v t_with_undefs) man flow |> Post.return
      | S_assign({ekind = E_var v}, e, mode) ->
         Option.return
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
                     match addr.addr_kind with
                     | A_py_class (c, bases) ->
                        let flow = Flow.set_domain_cur (Typingdomain.set_var cur v Typingdomain.{lundef=false; gundef=false; def=Some (Class (c, bases))}) man flow in
                        Post.of_flow flow
                     | _ -> debug "typing/exec/assign/E_py_object: %a@\n" Universal.Ast.pp_addr addr;
                            failwith "ni"
                   end
                | _ -> failwith "typing/eval/S_assign: ni"
                end)
      | _ -> None

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_alloc_addr akind ->
         let addr = {addr_kind = akind; addr_uid=(-1)} in
         Eval.singleton (mk_addr addr range) flow |> Option.return
      | _ -> debug "Warning: no eval for %a" pp_expr exp;
             None

    let ask query man flow = None
  end

let () = register_domain (module Domain)
