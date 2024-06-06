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
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast

type ('a, _) query += Q_python_addr_of_module : string -> ('a, addr option) query

let () = register_query {
             join = (let f: type a r. query_pool -> (a, r) query -> r -> r -> r =
                       fun next query a b ->
                       match query with
                       | Q_python_addr_of_module _ -> assert false
                       | _ -> next.pool_join query a b in
                     f);
             meet = (let f: type a r. query_pool -> (a, r) query -> r -> r -> r =
                       fun next query a b ->
                       match query with
                       | Q_python_addr_of_module _ -> assert false
                       | _ -> next.pool_meet query a b in
                     f)
           }

module Domain =
  struct

    module Modules  = struct
      module ModuleSet
        = Framework.Lattices.Powerset.Make
            (struct
              type t = (addr * expr option) * bool
              let compare ((a1, oe1), b1) ((a2, oe2), b2) =
                compare_addr a1 a2
              let print printer ((a, oe), b) = (unformat pp_addr) printer a
            end)

      include ModuleSet

      let max_size = 1
      let bound (x:t) : t =
        match x with
        | Nt s ->
           if Set.cardinal s <= max_size then x
           else
             panic "bound %a" (format print) x
        | _ -> TOP

      let join a1 a2 = ModuleSet.join a1 a2 |> bound

      let add v t =
        add v t |> bound

      let find_singleton (x:t) =
        match x with
        | Nt s ->
           if ModuleSet.cardinal x = 1 then ModuleSet.choose x
           else if ModuleSet.cardinal x = 0 then raise Not_found
           else assert false
        | _ -> raise Not_found
    end

    module Str = struct
      type t = string
      let compare = compare
      let print = unformat Format.pp_print_string
    end

    module ModulesMap = Framework.Lattices.Partial_map.Make(Str)(Modules)

    include ModulesMap

    include Framework.Core.Id.GenDomainId(
                struct
                  type nonrec t = t
                  let name = "python.desugar.import"
                end)

    exception Module_not_found of string

    let checks = []

    let rec exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_import(modul, vasname, vroot) ->
        begin debug "stmt = %a" pp_stmt stmt;
        try
          let obj, flow, _ = import_module man modul range flow in
          let v = match vasname with
            | None -> vroot
            | Some v -> v
          in
          debug "performing assignement %a = %a" pp_var v Pp.pp_py_object obj;
          Flow.add_safe_check Alarms.CHK_PY_MODULENOTFOUNDERROR range flow |>
            man.exec (mk_assign (mk_var v range) (mk_py_object obj range) range) >>%
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
        debug "%a import_module ok, adding a few eq" pp_stmt stmt;
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
                    panic_at range "import: name %s not found in module %s@.globals = %a" name modul (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) globals
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
                    panic_at range "import: name %s not found in %a" name pp_addr_kind (kind_of_object obj);
             in
             mk_var v range
           | A_py_module(M_builtin m) ->
              let obj = find_builtin_attribute obj name in
              mk_py_object obj range
           | A_py_c_module m ->
              mk_py_object_attr obj name ~etyp:(T_py None) range
           | _ -> assert false
         in
         let stmt = mk_assign (mk_var vmodul range) e range in
         let () = debug "assign %a! vtyp vmodul = %a" pp_stmt stmt pp_typ (vtyp vmodul) in
         (* debug "%a" (format @@ Flow.print man.lattice.print) flow; *)
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
        let name = String.map (fun c -> if c = '.' then '/' else c) name in
        let (addr, expr), flow, is_stub =
          try
            let (a, e), is_stub = Modules.find_singleton @@ ModulesMap.find name (get_singleton_env_from_flow T_cur man flow) in
            debug "module %s already imported, cache hit!" name;
            (a, e), flow, is_stub
          with Not_found ->
            begin
              let dir = Paths.get_lang_stubs_dir "python" () in
              let filename =
                let file_candidates = [dir ^ "/" ^ name ^ ".py";
                                       name ^ ".py";
                                       name ^ "/__init__.py";
                                       dir ^ "/typeshed/" ^ name ^ ".pyi";
                                       (* name ^ "module.c"; *)
                                       "mopsa.db"
                                       ]
                in
                match List.find_opt Sys.file_exists file_candidates with
                | Some s -> s
                | None ->
                   let () = warn_at range "module %s not found (file candidates: %a)" name (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) file_candidates in
                   raise (Module_not_found name)
              in
              let () = debug "[%s] importing file %s" name filename in
              let probable_name =
                let s = String.split_on_char '/' name in
                List.nth s (List.length s - 1) in
              let (a, e), body, is_stub, flow =
                if filename = dir ^ "/typeshed/" ^ name ^ ".pyi" then
                  let o, b, flow = import_stubs_module man (dir ^ "/typeshed") name flow in
                  o, b, true, flow
                else if List.mem filename [(*name ^ "module.c";*)
                            "mopsa.db"] then
                  let () =
                    C.Lang.Frontend.opt_make_target := probable_name in
                  let prog = C.Lang.Frontend.parse_program [filename] in
                  (* let () = debug "Parsed C program %a" pp_program prog in *)
                  let () = C.Iterators.Program.Domain.opt_entry_function := "PyInit_" ^ probable_name in
                  (* panic_at range "c.coverage hook mem: %b" (Hook.mem_hook "c.coverage"); *)
                  let () = debug "Searching for entry function %s" !C.Iterators.Program.Domain.opt_entry_function
                             (* (Format.pp_print_list (fun fmt fdec -> Format.pp_print_string fmt fdec.C.Ast.c_func_org_name)) (match prog.prog_kind with | C.Ast.C_program c -> c.c_functions | _ -> assert false) *)
                  in
                  let flow = C.Iterators.Program.Domain.init prog man flow |> Option.get |> post_to_flow man in
                  let () =
                    if Hook.is_hook_active "c.coverage" then
                      C.Hooks.Coverage.Hook.init (Flow.get_ctx flow) in
                  let body = mk_stmt (S_program(prog, None)) range in
                  let addr =
                    {
                      addr_kind = A_py_c_module probable_name; (*, prog);*)
                      addr_partitioning = G_all;
                      addr_mode = STRONG;
                    }
                  in
                  (addr, None), body, false, flow
                else
                  let prog = Frontend.parse_program [filename] in
                  let globals, body =
                    match prog.prog_kind with
                    | Py_program(_, globals, body) ->
                       debug "parsed %s, globals = %a, body = %a" filename (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) globals pp_stmt body;
                       globals, body
                    | _ -> assert false
                  in
                  let name_assign =
                    mk_assign
                      (mk_var (List.find (fun x -> get_orig_vname x = "__name__") globals) range)
                      {(mk_string probable_name range) with etyp=(T_py (Some Str))}
                      range
                  in
                  let file_assign =
                    mk_assign
                      (mk_var (List.find (fun x -> get_orig_vname x = "__file__") globals) range)
                      {(mk_string filename range) with etyp=(T_py (Some Str))}
                      range
                  in
                  let body = Universal.Ast.mk_block (name_assign::file_assign::body::[]) range in
                  let addr = {
                    addr_kind = A_py_module (M_user(probable_name, globals));
                    addr_partitioning = G_all;
                    addr_mode = STRONG;
                  }
                  in
                  (addr, None), body, false, flow in
              let () = debug "pre body" in
              let flow' = post_to_flow man @@ man.exec body flow in
              let () = debug "post body" in
              let flow' =
               ( get_env T_cur man flow >>$ fun cur flow ->
                 set_env T_cur (ModulesMap.add name (Modules.singleton ((a, e), is_stub)) cur) man flow'
               ) |>
               post_to_flow man
              in
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
            if is_unsupported_clsdec cls then C_unsupported name
            else C_builtin name
          in
          create_builtin_class kind name cls bases (srange stmt);
          parse (Some name) cls.py_cls_body

        | S_py_function(fundec) ->
          let fun_name = mk_dot_name base (get_orig_vname fundec.py_func_var) in
          let fundec = {fundec with py_func_var = set_orig_vname fun_name fundec.py_func_var} in
          let kind =
            if is_stub_fundec fundec then F_user fundec else
            if is_unsupported_fundec fundec then F_unsupported fun_name
            else F_builtin (fun_name, builtin_type_name
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

        | Universal.Heap.Recency.S_perform_gc -> ()

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
        | S_assign ({ekind = E_var (v, _)}, {ekind = E_constant _ | E_py_tuple _}) ->
           stmt, globals, flow

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
      set_env T_cur empty man flow |>
      Option.some

    let eval _ _ _ = None

    let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> ('a, r) cases option =
      fun query man flow ->
      match query with
      | Q_python_addr_of_module s ->
         get_env T_cur man flow >>$? fun cur flow ->
         let ret = OptionExt.lift (fun cs ->
             assert(Modules.cardinal cs = 1);
             let (a, _), _ = Modules.choose cs in
             a)
             (find_opt s cur)
         in
         Some (Cases.singleton ret flow)

      | _ -> None

    let print_expr _ _ _ _ = ()

    let print_state printer a =
      (* () *)
    pprint ~path:[Key "Imported Python Modules"] printer (pbox ModulesMap.print a)

    let merge _ _ _ = assert false
  end

let () = register_standard_domain (module Domain)
