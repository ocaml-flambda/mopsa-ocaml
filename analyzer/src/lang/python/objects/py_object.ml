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

(** An environment is a total map from variables to addresses. *)

open Mopsa
open Sig.Domain.Intermediate
open Ast
open Addr
open Universal.Ast
open Alarms


module Domain =
struct

  include Framework.Core.Id.GenStatelessDomainId(struct let name = "python.objects.object" end)

  let interface = {
    iexec = { provides = []; uses = []; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let alarms = []

  let init prog man flow = flow

  let exec _ _ _ _ = None

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
      bind_list args (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun args flow ->
          match args with
          | [] ->
            debug "Error during creation of a new instance@\n";
            man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton
          | cls :: tl ->
            let c = fst @@ object_of_expr cls in
            man.eval  ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance c) range) flow |>
            Eval.bind (fun eaddr flow ->
                let addr = match ekind eaddr with
                  | E_addr a -> a
                  | _ -> assert false in
                man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
                Eval.singleton (mk_py_object (addr, None) range)
              )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range) flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__getattribute__")}, _)}, [instance; attribute], []) ->


      assume (mk_expr (E_py_ll_hasattr (instance, attribute)) range)
        ~fthen:(fun flow ->
            debug "instance attribute found locally@\n";
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr(instance, attribute)) range) flow |>
            Eval.bind (fun exp flow ->
                match akind @@ fst @@ object_of_expr instance with
                | A_py_function (F_user f) when List.exists (fun x -> match ekind x with E_var (v, _) -> get_orig_vname v = "classmethod" | _ -> false) f.py_func_decors ->
                  eval_alloc man (A_py_method (object_of_expr exp, instance)) range flow |>
                  bind_some (fun addr flow ->
                      let obj = (addr, None) in
                      Eval.singleton (mk_py_object obj range) flow
                    )
                | _ ->
                  Eval.singleton exp flow
              )
          )
        ~felse:(fun flow ->
            debug "searching in the classes now@\n";
            (* if exp is a class, we just call the attribute
               (after searching in the mro). if exp is an
               instance, we take its class, search in the mro
               and create a method *)
            (* to test if an object o is a class, we call isinstance(o, type) *)
            assume
              (mk_py_isinstance_builtin instance "type" range)
              ~fthen:(fun flow ->
                  let mro = mro (object_of_expr instance) in
                  debug "mro = %a@\n" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) (List.map (fun x -> mk_py_object x range) mro);
                  let rec search_mro flow mro = match mro with
                    | [] ->
                      debug "No attribute found for %a@\n" pp_expr instance;
                      Format.fprintf Format.str_formatter "'%a' object has no attribute '%s'" pp_expr instance (match ekind attribute with
                          | E_constant (C_string attr) -> attr | _ -> assert false);
                      man.exec (Utils.mk_builtin_raise_msg "AttributeError" (Format.flush_str_formatter ()) range) flow |>
                      Eval.empty_singleton
                    | cls::tl ->
                      assume
                        (mk_expr (E_py_ll_hasattr (mk_py_object cls range, attribute)) range)
                        ~fthen:(fun flow ->
                            man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr (mk_py_object cls range, attribute)) range) flow
                          )
                        ~felse:(fun flow -> search_mro flow tl)
                        man flow
                  in search_mro flow mro
                )
              ~felse:(fun flow ->
                  man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type instance range) flow |>
                  Eval.bind (fun class_of_exp flow ->
                      let mro = mro (object_of_expr class_of_exp) in
                      debug "mro of %a: %a" pp_expr class_of_exp (Format.pp_print_list (fun fmt (a, _) -> pp_addr fmt a)) mro;
                      let rec search_mro flow mro = match mro with
                        | [] ->
                          debug "No attribute found for %a@\n" pp_expr instance;
                          Format.fprintf Format.str_formatter "'%a' object has no attribute '%s'" pp_expr instance (match ekind attribute with | E_constant (C_string attr) -> attr | _ -> assert false);
                          man.exec (Utils.mk_builtin_raise_msg "AttributeError" (Format.flush_str_formatter ()) range) flow |>
                          Eval.empty_singleton
                        | cls::tl ->
                          assume
                            (mk_expr (E_py_ll_hasattr (mk_py_object cls range, attribute)) range)
                            ~fthen:(fun flow ->
                                (* FIXME: disjunction between instances an non-instances *)
                                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr (mk_py_object cls range, attribute)) range) flow |>
                                Eval.bind (fun obj' flow ->
                                    assume
                                      (mk_py_isinstance_builtin obj' "function" range) man flow
                                      ~fthen:(fun flow ->
                                          debug "obj'=%a; exp=%a@\n" pp_expr obj' pp_expr instance;
                                          eval_alloc man (A_py_method (object_of_expr obj', instance)) range flow |>
                                          bind_some (fun addr flow ->
                                              let obj = (addr, None) in
                                              Eval.singleton (mk_py_object obj range) flow)
                                        )
                                      ~felse:(fun flow ->
                                          (* assume_eval
                                           *   (mk_py_isinstance_builtin obj' "method" range) man flow
                                           *   ~fthen:(fun flow ->
                                           *       match akind @@ fst @@ object_of_expr obj' with
                                           *       | A_py_method (({addr_kind = A_py_function (F_user f)}, _), _) when List.exists (fun x -> match ekind x with E_var (v, _) -> v.org_vname = "classmethod" | _ -> false) f.py_func_decors ->
                                           *         eval_alloc ~mode:WEAK man (A_py_method (object_of_expr obj', class_of_exp)) range flow |>
                                           *       Eval.bind (fun addr flow ->
                                           *           let obj = (addr, None) in
                                           *           Eval.singleton (mk_py_object obj range) flow)
                                           *       | _ -> Exceptions.panic "%a@\n" pp_expr obj'
                                           *     )
                                           *   ~felse:(Eval.singleton obj') *)
                                          Eval.singleton obj' flow
                                        )
                                  )
                              )
                            ~felse:(fun flow -> search_mro flow tl)
                            man flow
                      in search_mro flow mro)
                )
              man flow
          )
        man flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__setattr__")}, _)}, [lval; attr; rval], []) ->
      (* FIXME: data descriptors usw *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_setattr (lval, attr, Some rval)) range) flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__delattr__")}, _)}, [lval; attr], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_setattr (lval, attr, None)) range) flow
      |> Option.return

    | _ -> None

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow
end


let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain);
