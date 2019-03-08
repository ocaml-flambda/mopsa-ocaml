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
open Ast
open Addr
open Universal.Ast

module Domain =
  struct
    let name = "python.desugar.import"
    let debug fmt = Debug.debug ~channel:name fmt

    let opt_stubs = ref []

    let () =
      register_domain_option name {
        key = "-stub";
        doc = " path to stub directory";
        spec = Arg.String(fun f -> opt_stubs := f :: !opt_stubs);
        default = "";
      }

    let exec_interface = {export = [Zone.Z_py]; import = []}
    let eval_interface = {export = []; import = []}

    let rec exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_import(modul, _, _) when String.contains modul '.'->
         panic_at range "import of sub-module %s not supported" modul

      | S_py_import_from(modul, _, _, _) when String.contains modul '.'->
         panic_at range "import from sub-module %s not supported" modul

      | S_py_import(modul, vasname, vroot) ->
         let obj, flow = import_module man modul range flow in
         let v = match vasname with
           | None -> vroot
           | Some v -> v
         in
         man.exec (mk_assign (mk_var v range) (mk_py_object obj range) range) flow |>
           Post.return

      | S_py_import_from(modul, name, _, vmodul) ->
         let obj, flow = import_module man modul range flow in
         let e =
           match kind_of_object obj with
           | A_py_module(M_user(_, globals)) ->
              let v = List.find (fun v -> v.org_vname = name) globals in
              mk_var v range
           | A_py_module(M_builtin m) ->
              let obj = find_builtin_attribute obj name in
              mk_py_object obj range
           | _ -> assert false
         in
         man.exec (mk_assign (mk_var vmodul range) e range) flow |>
           Post.return

      | _ ->
         None


    (** Search for the module in the search path and parse its body *)
    and import_module man name range flow =
      if is_builtin_name name then
        find_builtin name, flow
      else
        let dir =
          !opt_stubs |>
          List.find_opt
            (fun dir ->
               let filename = dir ^ "/" ^ name ^ ".py" in
               Sys.file_exists filename
            )
        in
        match dir with
        | None -> panic_at range "module %s not found in stubs" name
        | Some dir ->
           let filename = dir ^ "/" ^ name ^ ".py" in
           let prog = Frontend.parse_program [filename] in
           let globals, body =
             match prog.prog_kind with
             | Py_program(body, globals) -> body, globals
             | _ -> assert false
           in
           let addr = {
               addr_kind = A_py_module (M_user(name, globals));
               addr_uid = 0;
               addr_mode = STRONG;
             }
           in
           let flow' = man.exec body flow in
           (addr, None), flow'


    (** Parse and import a builtin module *)
    and import_builtin_module base name =
      let file = name ^ ".py" in
      let dir = !opt_stubs |>
                List.find_opt
                  (fun dir ->
                     let filename = dir ^ "/" ^ file in
                     Sys.file_exists filename
                  )
      in
      match dir with
      | None -> panic "builtin module %s not found" file
      | Some dir ->
         let path = dir ^ "/" ^ file in
         let stmt = Frontend.parse_file path in
         (* FIXME: pour les fonctions récursives, ça marche ça ? *)
         (* pour les variables globales : collecter les variables globales, puis faire des man.exec dessus ? *)
         (* et pour les modules normaux, il y a aussi un pb sur les noms de variables, non ? *)
         let rec parse base stmt =
           match skind stmt with
           | S_py_class(cls) ->
              let name = mk_dot_name base cls.py_cls_var.org_vname in
              let bases = List.map (fun base ->
                              match ekind base with
                              | E_var (v, _) -> find_builtin v.org_vname
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
              let name = mk_dot_name base fundec.py_func_var.org_vname in
              let fundec = {fundec with py_func_var = {fundec.py_func_var with org_vname = name}} in
              let kind =
                if Libs.Py_mopsa.is_stub_fundec fundec then F_user fundec else
                  if Libs.Py_mopsa.is_unsupported_fundec fundec then F_unsupported name
                  else F_builtin name
              in
              let addr = {
                  addr_kind = A_py_function kind;
                  addr_uid = 0;
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
               addr_uid = 0;
               addr_mode = STRONG;
             }
           in
           add_builtin_module (addr, None) ()
         else
           ()

    let init prog man flow =
      import_builtin_module (Some "mopsa") "mopsa";
      import_builtin_module None "stdlib";
      (* import_builtin_module (Some "math") "math"; *)
      OptionExt.return flow

    let eval _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
