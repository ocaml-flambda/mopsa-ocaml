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
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.import"
      end)

    let imported_modules = Hashtbl.create 100

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = []};
      ieval = {provides = []; uses = []}
    }

    let rec exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_import(modul, _, _) when String.contains modul '.'->
         panic_at range "import of sub-module %s not supported" modul

      | S_py_import_from(modul, _, _, _) when String.contains modul '.'->
         panic_at range "import from sub-module %s not supported" modul

      | S_py_import(modul, vasname, vroot) ->
         let obj, flow, _ = import_module man modul range flow in
         let v = match vasname with
           | None -> vroot
           | Some v -> v
         in
         man.exec (mk_assign (mk_var v range) (mk_py_object obj range) range) flow |>
         Post.return |>
         Option.return

      | S_py_import_from(modul, name, _, vmodul) ->
        (* FIXME: objects defined in modul other than name should not appear *)
        let obj, flow, ispyi = import_module man modul range flow in
        (* FIXME: terrible disjunction *)
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
            |> Post.return |> Option.return
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
         man.exec stmt flow |>
         Post.return |>
         Option.return

      | _ ->
         None


    (** Search for the module in the search path and parse its body *)
    and import_module man name range flow =
      if is_builtin_name name
      then find_builtin name, flow, false
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
                else panic_at range "module %s not found (searched in %s and in %s and in the current directory)" name dir (dir ^ "/typeshed/") in
              let (a, e), body, is_stub, flow =
                if filename = dir ^ "/typeshed/" ^ name ^ ".pyi" then
                  let o, b, flow = import_stubs_module man (dir ^ "/typeshed") name flow in
                  o, b, true, flow
                else
                  let prog = Frontend.parse_program [filename] in
                  let globals, body =
                    match prog.prog_kind with
                    | Py_program(globals, body) -> globals, body
                    | _ -> assert false
                  in
                  let addr = {
                    addr_kind = A_py_module (M_user(name, globals));
                    addr_group = G_all;
                    addr_mode = STRONG;
                  }
                  in
                  (addr, None), body, false, flow in
              let flow' = man.exec body flow in
              Hashtbl.add imported_modules name ((a, e), is_stub);
              (a, e), flow', is_stub
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
      let stmt = Frontend.parse_file path in
      (* FIXME: pour les fonctions récursives, ça marche ça ? *)
      (* pour les variables globales : collecter les variables globales, puis faire des man.exec dessus ? *)
      (* et pour les modules normaux, il y a aussi un pb sur les noms de variables, non ? *)
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
          let name = mk_dot_name base (get_orig_vname fundec.py_func_var) in
          let fundec = {fundec with py_func_var = set_orig_vname name fundec.py_func_var} in
          let kind =
            if Libs.Py_mopsa.is_stub_fundec fundec then F_user fundec else
            if Libs.Py_mopsa.is_unsupported_fundec fundec then F_unsupported name
            else F_builtin name
          in
          let addr = {
            addr_kind = A_py_function kind;
            addr_group = G_all;
            addr_mode = STRONG;
          }
          in
          add_builtin_function (addr, None) ()

        | S_block(block) ->
          List.iter (parse base) block

        | S_py_import(name, _, _) when is_builtin_name name -> ()

        | _ -> panic "stmt %a not supported in %s" pp_stmt stmt file

      in
      parse base stmt;
      if name <> "stdlib" then
        let addr = {
          addr_kind = A_py_module(M_builtin name);
          addr_group = G_all;
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
        | Py_program(g, b) -> g, b
        | _ -> assert false in
      let rec parse stmt globals flow : stmt * var list * 'a flow  =
        let range = srange stmt in
        match skind stmt with
        | S_assign ({ekind = E_var (v, _)}, e) ->
          (* let v = set_orig_vname (mk_dot_name (Some name) (get_orig_vname v)) v in *)
          debug "%s: adding alias %a[%s] = %a" base pp_var v v.vname pp_expr e;
          add_type_alias v e;
          (* add to map *)
          {stmt with skind = S_block []},
          List.filter (fun var -> compare_var var v <> 0) globals,
          flow

        (* FIXME: if a = int in typing and b = a in base, what happens? *)
        | S_py_import (s, _, _)
        | S_py_import_from (s, _, _, _) ->
          debug "hum, maybe we should import %s first" s;
          {stmt with skind = S_block []}, globals, man.exec stmt flow


        (* FIXME: and substitution for other modules? *)
        | S_py_annot _
        | S_py_class _
        | S_py_function _ ->
          let stmt = Visitor.map_stmt (fun e -> match ekind e with
              | E_var (v, m) ->
                begin
                  try
                    if get_orig_vname v = "AnyStr" then
                      debug "AnyStr: in type_aliases: %b" (Hashtbl.mem type_aliases v);
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
            | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("typing",_)}, _)}, "overload")}]} -> true
            (* FIXME *)
            | {py_func_decors = [{ekind = E_var( {vkind = V_uniq ("overload",_)}, _)}]} -> true
            | _ -> false in
          begin match skind stmt with
            | S_py_annot _ | S_py_class _ -> stmt, globals, flow
            | S_py_function f ->
              let newf =
                { py_funca_var = f.py_func_var;
                  py_funca_decors = f.py_func_decors;
                  py_funca_range = f.py_func_range;
                  py_funca_ret_var = f.py_func_ret_var;
                  py_funca_sig =
                    [{
                      py_funcs_parameters = f.py_func_parameters;
                      py_funcs_defaults = List.map (Option.apply (fun _ -> true) false) f.py_func_defaults;
                      py_funcs_types_in = f.py_func_types_in;
                      py_funcs_type_out = f.py_func_type_out;
                    }]
                } in
              let addr = {
                addr_kind = A_py_function (F_annot newf);
                addr_group = G_all;
                addr_mode = STRONG;
              } in
              let () =
                if is_typingoverload_fundec f then
                  let () = debug "typing overload on %a" pp_var f.py_func_var in
                  add_typed_function_overload (addr, None)
              else
                add_typed_function (addr, None) in
              debug "done";
              {stmt with skind = S_block []}, globals, flow
            | _ -> assert false
          end

        | S_block(block) ->
          let newblock, newglobals, newflow = List.fold_left (fun (nb, ng, nf) s ->
              let news, g, nf = parse s ng nf in
              match skind news with
              | S_block [] -> nb, g, nf
              | _ -> news::nb, g, nf
            ) ([], globals, flow) block in
          let newblock = List.rev newblock in
          {stmt with skind = S_block newblock}, newglobals, newflow

        | _ -> panic_at range "stmt %a not supported in stubs file %s" pp_stmt stmt name
      in
      let body, globals, flow = parse stmts globals flow in
      let addr = { addr_kind = A_py_module(M_user (name, globals));
                   addr_group = G_all;
                   addr_mode = STRONG; } in
      (addr, None), body, flow

    let init prog man flow =
      import_builtin_module (Some "mopsa") "mopsa";
      import_builtin_module None "stdlib";
      (* import_builtin_module (Some "math") "math"; *)
      flow

    let eval _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
