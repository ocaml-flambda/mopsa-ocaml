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

(** Inliner of imported packages. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.import"
      end)

    let imported_modules = Hashtbl.create 100

    exception Module_not_found of string

    let checks = []

    let rec exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_import(modul, _, _) when String.contains modul '.'->
         panic_at range "import of sub-module %s not supported" modul

      | S_py_import_from(modul, _, _, _) when String.contains modul '.'->
         panic_at range "import from sub-module %s not supported" modul

      | S_py_import(modul, vasname, vroot) ->
        begin debug "stmt = %a" pp_stmt stmt;
        try
          let obj, flow, _ = import_module man modul range flow in
          let v = match vasname with
            | None -> vroot
            | Some v -> v
          in
          man.exec (mk_assign (mk_var v range) (mk_py_object obj range) range) flow >>%
          Post.return |>
          OptionExt.return
        with Module_not_found m ->
          man.exec
            (Utils.mk_builtin_raise_msg "ModuleNotFoundError" (
                 Format.asprintf "No module named '%s'" m
               ) range)
            flow >>% Post.return |> OptionExt.return
        end

      | S_py_import_from(modul, name, _, vmodul) ->
        (* FIXME: objects defined in modul other than name should not appear *)
        debug "importing %s from module %s" name modul;
        let obj, flow, ispyi = import_module man modul range flow in
        debug "import ok, adding a few eq";
        if ispyi then
          match kind_of_object obj with
          | A_py_module (M_user (_, globals)) ->
            begin
              try
                let e = find_type_alias_by_name name in
                debug "adding one more alias: %a -> %a" pp_var vmodul pp_expr e;
                debug "removing var %a from the environment" pp_var vmodul;
                add_type_alias vmodul e;
                man.exec (mk_remove_var vmodul range) flow
              with Not_found ->
                let v =
                  try List.find (fun v -> get_orig_vname v = name) globals
                  with Not_found ->
                    panic_at range "import: name %s not found in module %s" name modul
                in
                let stmt = mk_assign (mk_var vmodul range) (mk_var v range) range in
                man.exec stmt flow
            end
            >>% Post.return |> OptionExt.return
          | _ -> assert false
        else
         let e =
           match kind_of_object obj with
           | A_py_module(M_user(_, globals)) ->
             let v =
               try List.find (fun v -> get_orig_vname v = name) globals
               with Not_found ->
                 panic_at range "import: name %s not found in module %s" name modul
             in
             mk_var v range
           | A_py_module(M_builtin m) ->
              let obj = find_builtin_attribute obj name in
              mk_py_object obj range
           | _ -> assert false
         in
         let stmt = mk_assign (mk_var vmodul range) e range in
         let () = debug "assign %a!" pp_stmt stmt in
         man.exec stmt flow >>%
         Post.return |>
         OptionExt.return

      | _ ->
         None


    (** Search for the module in the search path and parse its body *)
    and import_module man name range flow =
      if is_builtin_module name
      then find_builtin_module name, flow, false
      else
        let (addr, expr), flow, is_stub =
          try
            let (a, e), is_stub = Hashtbl.find imported_modules name in
            debug "module %s already imported, cache hit!" name;
            (a, e), flow, is_stub
          with Not_found ->
            begin
              let dir = Paths.get_lang_stubs_dir "python" () in
              let filename =
                let tentative1 = dir ^ "/" ^ name ^ ".py" in
                let tentative2 = name ^ ".py" in
                let tentative3 = dir ^ "/typeshed/" ^ name ^ ".pyi" in
                if Sys.file_exists tentative1 then tentative1
                else if Sys.file_exists tentative2 then tentative2
                else if Sys.file_exists tentative3 then tentative3
                else
                  let () = warn_at range "module %s not found (searched in %s and in %s and in the current directory)" name dir (dir ^ "/typeshed/") in
                  raise (Module_not_found name) in
              let () = debug "importing file %s" filename in
              let (a, e), body, is_stub, flow =
                if filename = dir ^ "/typeshed/" ^ name ^ ".pyi" then
                  let o, b, flow = import_stubs_module man (dir ^ "/typeshed") name flow in
                  o, b, true, flow
                else
                  let prog = Frontend.parse_program [filename] in
                  let globals, body =
                    match prog.prog_kind with
                    | Py_program(_, globals, body) -> globals, body
                    | _ -> assert false
                  in
                  let addr = {
                    addr_kind = A_py_module (M_user(name, globals));
                    addr_partitioning = G_all;
                    addr_mode = STRONG;
                  }
                  in
                  (addr, None), body, false, flow in
              let flow' = man.exec body flow in
              Hashtbl.add imported_modules name ((a, e), is_stub);
              (a, e), post_to_flow man flow', is_stub
            end
        in
        (addr, expr), flow, is_stub

    (** Parse and import a builtin module *)
    and import_builtin_module base name =
      let file = name ^ ".py" in
      let dir = Paths.get_lang_stubs_dir "python" () in
      let filename = dir ^ "/" ^ file in

      if not (Sys.file_exists filename)
      then panic "builtin module %s not found" file;

      let path = dir ^ "/" ^ file in
      let stmt =
        match (Frontend.parse_program [path]).prog_kind with
        | Ast.Py_program (_, _, b) -> b
        | _ -> assert false in
      (* FIXME: does this work for recursive functions? *)
      let rec parse base stmt =
        match skind stmt with
        | S_py_class(cls) ->
          let name = mk_dot_name base (get_orig_vname cls.py_cls_var) in
          let bases = List.map (fun base ->
              match ekind base with
              | E_var (v, _) -> find_builtin (get_orig_vname v)
              | _ -> assert false
            ) cls.py_cls_bases
          in
          let kind =
            if Libs.Py_mopsa.is_unsupported_clsdec cls then C_unsupported name
            else C_builtin name
          in
          create_builtin_class kind name cls bases (srange stmt);
          parse (Some name) cls.py_cls_body

        | S_py_function(fundec) ->
          let fun_name = mk_dot_name base (get_orig_vname fundec.py_func_var) in
          let fundec = {fundec with py_func_var = set_orig_vname fun_name fundec.py_func_var} in
          let kind =
            if Libs.Py_mopsa.is_stub_fundec fundec then F_user fundec else
            if Libs.Py_mopsa.is_unsupported_fundec fundec then F_unsupported fun_name
            else F_builtin (fun_name, Libs.Py_mopsa.builtin_type_name
                              (if name = "stdlib" then "builtin_function_or_method" else "function")
                              fundec)
          in
          let addr = {
            addr_kind = A_py_function kind;
            addr_partitioning = G_all;
            addr_mode = STRONG;
          }
          in
          add_builtin_function (addr, None) ()

        | S_block(block,_) ->
          List.iter (parse base) block

        | S_py_import(name, _, _) when is_builtin_name name -> ()

        | _ -> panic "stmt %a not supported in %s" pp_stmt stmt file

      in
      parse base stmt;
      if name <> "stdlib" then
        let addr = {
          addr_kind = A_py_module(M_builtin name);
          addr_partitioning = G_all;
          addr_mode = STRONG;
        }
        in
        add_builtin_module (addr, None) ()
      else
        ()

    and import_stubs_module man base name flow =
      debug "import_stubs_module %s %s" base name;
      let prog = Frontend.parse_program [(base ^ "/" ^ name ^ ".pyi")] in
      let globals, stmts = match prog.prog_kind with
        | Py_program(_, g, b) -> g, b
        | _ -> assert false in
      let rec parse basename stmt globals flow : stmt * var list * 'a flow  =
        let range = srange stmt in
        match skind stmt with
        | S_assign ({ekind = E_var (v, _)}, e) ->
          debug "%s: adding alias %a[%s] = %a" base pp_var v v.vname pp_expr e;
          let ee = Visitor.map_expr (fun exp -> match ekind exp with
              | E_var (v, m) -> begin try Keep (Hashtbl.find type_aliases v) with Not_found -> Keep exp end
              | _ -> VisitParts exp) (fun s -> VisitParts s) e in
          add_type_alias v ee;
          {stmt with skind = S_block ([], [])},
          List.filter (fun var -> compare_var var v <> 0) globals,
          flow

        (* FIXME: if a = int in typing and b = a in base, what happens? *)
        | S_py_import (s, _, _)
        | S_py_import_from (s, _, _, _) ->
          debug "hum, maybe we should import %s first" s;
          {stmt with skind = S_block ([], [])}, globals, man.exec stmt flow |> post_to_flow man


        | S_py_annot _
        | S_py_class _
        | S_py_function _ ->
          let stmt = Visitor.map_stmt (fun e -> match ekind e with
              | E_var (v, m) ->
                begin
                  try
                    let substexpr = Hashtbl.find type_aliases v in
                    (* let rec recsubst expr =
                     *   match ekind expr with
                     *   | E_var (v, _) ->
                     *     begin
                     *       try recsubst (Hashtbl.find type_aliases v)
                     *       with Not_found -> expr
                     *     end
                     *   | _ -> expr in
                     * Keep (recsubst substexpr) *)
                    Keep substexpr
                  with Not_found ->
                    Keep e
                end
              | _ -> VisitParts e) (fun s -> VisitParts s) stmt
          in
          let is_typingoverload_fundec = function
            (* FIXME *)
            | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("typing",_)}, _)}, "overload")}]} -> true
            | {py_func_decors = [{ekind = E_var( {vkind = V_uniq ("overload",_)}, _)}]} -> true
            | _ -> false in
          begin match skind stmt with
            | S_py_annot _ ->
              stmt, globals, flow
            | S_py_class c ->
              let bases, abases = List.fold_left (fun (b, ab) expr -> match ekind expr with
                  | E_py_index_subscript (c, _) -> (c::b, expr::ab)
                  | _ -> (expr::b, ab)
                ) ([], []) c.py_cls_bases in
              let bases, abases = List.rev bases, List.rev abases in
              let r = bind_list bases man.eval flow |>
                        bind_result (fun ebases flow ->
                          (* FIXME: won't work with Generic[T] I guess *)
                          let obases = match ebases with
                            | [] -> [find_builtin "object"]
                            | _ -> List.map object_of_expr ebases in
                          let ebases = match ebases with
                            | [] -> [mk_py_object (find_builtin "object") range]
                            | _ -> ebases in
                          let name = mk_dot_name basename (get_orig_vname c.py_cls_var) in
                          let py_cls_a_body, globals, flow = parse (Some name) c.py_cls_body globals flow in
                          let newc =
                            { py_cls_a_var = set_orig_vname name c.py_cls_var;
                              py_cls_a_body;
                              py_cls_a_bases = ebases;
                              py_cls_a_static_attributes = c.py_cls_static_attributes;
                              py_cls_a_abases = abases;
                              py_cls_a_range = c.py_cls_range;
                            } in
                          let addr = {
                            addr_kind = A_py_class (C_annot newc, obases);
                            addr_partitioning = G_all;
                            addr_mode = STRONG;
                          } in
                          let mro = c3_lin (addr, None) in
                          let addr = {addr with addr_kind = A_py_class (C_annot newc, mro)} in
                          debug "add_typed %a" pp_addr addr;
                          let () = add_typed (addr, None) in
                          Cases.return () flow
                        )
              in
              let flow = post_to_flow man r in
              {stmt with skind = S_block ([], [])}, globals, flow


            | S_py_function f ->
              let name = mk_dot_name basename (get_orig_vname f.py_func_var) in
              let newf =
                { py_funca_var = set_orig_vname name f.py_func_var;
                  py_funca_decors = f.py_func_decors;
                  py_funca_range = f.py_func_range;
                  py_funca_ret_var = f.py_func_ret_var;
                  py_funca_sig =
                    [{
                      py_funcs_parameters = f.py_func_parameters;
                      py_funcs_defaults = List.map (OptionExt.apply (fun _ -> true) false) f.py_func_defaults;
                      py_funcs_exceptions =
                        (debug "for %a, py_func_body = %a" pp_var f.py_func_var pp_stmt f.py_func_body;
                          match skind f.py_func_body with
                          | S_block (s, _) ->
                            List.fold_left (fun acc st -> match skind st with
                                | S_py_raise (Some e) -> e::acc
                                | _ -> acc) [] s
                         | _ -> []);
                      py_funcs_types_in = f.py_func_types_in;
                      py_funcs_type_out = f.py_func_type_out;
                    }]
                } in
              let addr = {
                addr_kind = A_py_function (F_annot newf);
                addr_partitioning = G_all;
                addr_mode = STRONG;
              } in
              let () =
                if is_typingoverload_fundec f then
                  let () = debug "typing overload on %a" pp_var f.py_func_var in
                  add_typed_overload (addr, None)
              else
                add_typed (addr, None) in
              debug "done";
              {stmt with skind = S_block ([], [])}, globals, flow
            | _ -> assert false
          end

        | S_block(block, _) ->
          let newblock, newglobals, newflow = List.fold_left (fun (nb, ng, nf) s ->
              let news, g, nf = parse basename s ng nf in
              match skind news with
              | S_block ([], _)-> nb, g, nf
              | _ -> news::nb, g, nf
            ) ([], globals, flow) block in
          let newblock = List.rev newblock in
          {stmt with skind = S_block (newblock, [])}, newglobals, newflow

        | S_expression {ekind = (E_constant C_py_ellipsis)}
        | S_expression {ekind = (E_constant (C_string _))}
        | Universal.Heap.Recency.S_perform_gc ->
          {stmt with skind = S_block ([], [])}, globals, flow


        | _ -> panic_at range "stmt %a not supported in stubs file %s" pp_stmt stmt name
      in
      let body, globals, flow = parse None stmts globals flow in
      let addr = { addr_kind = A_py_module(M_user (name, globals));
                   addr_partitioning = G_all;
                   addr_mode = STRONG; } in
      (addr, None), body, flow

    let init prog man flow =
      import_builtin_module (Some "mopsa") "mopsa";
      import_builtin_module None "stdlib";
      flow

    let eval _ _ _ = None
    let ask _ _ _ = None
    let print_expr _ _ _ _ = ()
  end

let () = register_stateless_domain (module Domain)
