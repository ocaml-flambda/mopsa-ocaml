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
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Alarms


let mk_py_ll_hasattr instance attr range =
  mk_expr ~etyp:(T_py None) (E_py_ll_hasattr(instance, attr)) range

let mk_py_ll_getattr instance attr range =
  mk_expr ~etyp:(T_py None) (E_py_ll_getattr(instance, attr)) range

let mk_py_ll_setattr instance attr valu range =
  mk_expr ~etyp:(T_py None) (E_py_ll_setattr(instance, attr, Some valu)) range

let mk_py_ll_delattr instance attr range =
  mk_expr ~etyp:(T_py None) (E_py_ll_setattr(instance, attr, None)) range


module Domain =
struct

  include Framework.Core.Id.GenStatelessDomainId(struct let name = "python.objects.object" end)

  let alarms = []

  let init prog man flow = flow

  let exec _ _ _ = None

  (* corresponding to _PyType_Lookup *)
  let rec search_mro man attr ~cls_found ~nothing_found range mro flow =
    match mro with
    | [] -> nothing_found flow
    | cls :: tl ->
      assume (mk_py_ll_hasattr (mk_py_object cls range) attr range)
        man flow
        ~fthen:(cls_found cls)
        ~felse:(search_mro man attr ~cls_found ~nothing_found range tl)

  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("type.__new__", _))}, _)}, args, kwargs)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__new__", _))}, _)}, args, kwargs) ->
      bind_list args (man.eval   ) flow |>
      bind_some (fun args flow ->
          match args with
          | [] ->
            debug "Error during creation of a new instance@\n";
            man.exec (Utils.mk_builtin_raise "TypeError" range) flow >>% Eval.empty_singleton
          | cls :: tl ->
            let c = fst @@ object_of_expr cls in
            man.eval    (mk_alloc_addr (A_py_instance c) range) flow >>$
 (fun eaddr flow ->
                let addr = match ekind eaddr with
                  | E_addr a -> a
                  | _ -> assert false in
                man.exec   (mk_add eaddr range) flow >>%
                Eval.singleton (mk_py_object (addr, None) range)
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__init__", _))}, _)}, args, []) ->
      man.eval    (mk_py_none range) flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("type.__getattribute__", _))}, _)}, [ptype; attribute], []) ->
      man.eval   (mk_py_type ptype range) flow >>$
        (fun metatype flow ->
          let lookintype o_meta_attribute o_meta_get flow =
            man.eval   ptype flow >>$
              (fun ptype flow ->
                let mro_ptype = mro (object_of_expr ptype) in
                search_mro man attribute
                  ~cls_found:(fun cls flow ->
                      man.eval
                        (mk_py_ll_getattr (mk_py_object cls range) attribute range) flow >>$
                        (fun attribute flow ->
                          assume (mk_py_hasattr (mk_py_type attribute range) "__get__" range)
                            man flow
                            ~fthen:(fun flow ->
                              (* FIXME: a NULL argument has been replaced with a None during evaluation *)
                              man.eval
                                (mk_py_call
                                   (mk_py_attr (mk_py_type attribute range) "__get__" range)
                                   [attribute; mk_py_none range; ptype]
                                   range
                                ) flow
                            )
                            ~felse:(Eval.singleton attribute)
                        )
                    )
                  ~nothing_found:(fun flow ->
                      match o_meta_get, o_meta_attribute with
                      | Some meta_get, _ ->
                        man.eval (mk_py_call meta_get [OptionExt.none_to_exn o_meta_attribute; ptype; metatype] range) flow
                      | None, Some meta_attribute ->
                        Eval.singleton meta_attribute flow
                      | None, None ->
                         let msg = Format.asprintf "type object '%a' has no attribute '%s'" pp_expr ptype (match ekind attribute with | E_constant (C_string attr) -> attr | _ -> assert false) in
                        man.exec (Utils.mk_builtin_raise_msg "AttributeError" msg range) flow >>%
                        Eval.empty_singleton
                    )
                  range mro_ptype flow
              )
          in

          let mro_metatype = mro (object_of_expr metatype) in
          search_mro man attribute
            ~cls_found:(fun cls flow ->
                man.eval
                  (mk_py_ll_getattr (mk_py_object cls range) attribute range)
                  flow >>$
 (fun meta_attribute flow ->
                    assume
                      (mk_py_hasattr (mk_py_type meta_attribute range) "__get__" range)
                      man flow
                      ~fthen:(fun flow ->
                          man.eval
                            (mk_py_attr (mk_py_type meta_attribute range) "__get__" range)
                            flow >>$
 (fun meta_get flow ->
                              assume
                                (mk_py_hasattr (mk_py_type meta_attribute range) "__set__" range)
                                man flow
                                ~fthen:(man.eval (mk_py_call meta_get [meta_attribute; ptype; metatype] range))
                                ~felse:(lookintype (Some meta_attribute) (Some meta_get))
                            )
                        )
                      ~felse:(lookintype (Some meta_attribute) None)
                  )
              )
            ~nothing_found:(lookintype None None)
            range mro_metatype flow

        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__getattribute__", _))}, _)}, [instance; attribute], []) ->
      man.eval   (mk_py_type instance range) flow >>$
 (fun class_of_exp flow ->
          let mro = mro (object_of_expr class_of_exp) in
          debug "mro of %a: %a" pp_expr class_of_exp (Format.pp_print_list (fun fmt (a, _) -> pp_addr fmt a)) mro;
          let tryinstance ~fother flow =
            assume (mk_py_ll_hasattr instance attribute range) man flow
              ~fthen:(man.eval   (mk_py_ll_getattr instance attribute range))
              ~felse:fother in
          search_mro man attribute
            ~cls_found:(fun cls flow ->
                man.eval
                  (mk_py_ll_getattr (mk_py_object cls range) attribute range)
                  flow >>$
 (fun descr flow ->
                    assume
                      (mk_py_hasattr (mk_py_type descr range) "__get__" range)
                      (* FIXMES:
                         1) it's __set__ or __del__
                         2) also,  GenericGetAttrWithDict uses low level field accesses (like descr->ob_type->tp_descr_get), but these fields get inherited during type creation according to the doc, so even a low-level access actually is something more complicated. The clean fix would be to handle this at class creation for special fields.

                         get/set ~> https://docs.python.org/3/c-api/typeobj.html#cols
                      *)
                      (* hasattr is bad but needed for inheritance, to see *)
                      man flow
                      ~fthen:(fun flow ->
                        assume (mk_binop ~etyp:(T_py None) (mk_py_hasattr (mk_py_type descr range) "__set__" range) O_py_or (mk_py_hasattr (mk_py_type descr range) "__del__" range) range) man flow
                            ~fthen:(man.eval (mk_py_call (mk_py_attr (mk_py_type descr range) "__get__" range) [descr; instance; class_of_exp] range))
                            ~felse:(tryinstance ~fother:(man.eval (mk_py_call (mk_py_attr (mk_py_type descr range) "__get__" range) [descr; instance; class_of_exp] range)))
                        )
                      ~felse:(tryinstance ~fother:(Eval.singleton descr))
                  )
              )
            ~nothing_found:(tryinstance ~fother:(fun flow ->
                debug "No attribute found for %a, attr = %a@\n" pp_expr instance pp_expr attribute;
                let msg = Format.asprintf "'%a' object has no attribute '%s'" pp_expr instance
                  (match ekind attribute with
                   | E_constant (C_string attr) -> attr
                   | E_py_object ({addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}}, Some {ekind = E_constant (C_string attr)}) -> attr
                   | _ -> assert false) in
                man.exec (Utils.mk_builtin_raise_msg "AttributeError" msg range) flow >>%
                Eval.empty_singleton)
              )
            range mro flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("type.__setattr__", _))}, _)}, [lval; attr; rval], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__setattr__", _))}, _)}, [lval; attr; rval], []) ->
      man.eval   (mk_py_type lval range) flow >>$
 (fun class_of_lval flow ->
          let mro = mro (object_of_expr class_of_lval) in
          search_mro man attr
            ~cls_found:(fun cls flow ->
                man.eval   (mk_py_ll_getattr (mk_py_object cls range) attr range) flow >>$
 (fun obj' flow ->
                    assume (mk_py_hasattr obj' "__set__" range)
                      man flow
                      ~fthen:(fun flow ->
                          man.eval (mk_py_call (mk_py_attr obj' "__set__" range) [lval; rval] range) flow
                        )
                      ~felse:(
                        man.eval
                          (mk_py_ll_setattr lval attr rval range)
                      )
                  )
              )
            ~nothing_found:(man.eval
                              (mk_py_ll_setattr lval attr rval range))
            range mro flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("type.__delattr__", _))}, _)}, [lval; attr], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__delattr__", _))}, _)}, [lval; attr], []) ->
      man.eval   (mk_py_type lval range) flow >>$
 (fun class_of_lval flow ->
          let mro = mro (object_of_expr class_of_lval) in
          search_mro man attr
            ~cls_found:(fun cls flow ->
                man.eval
                  (mk_py_ll_getattr (mk_py_object cls range) attr range) flow >>$
 (fun obj' flow ->
                    assume (mk_py_hasattr obj' "__delete__" range)
                      man flow
                      ~fthen:(fun flow ->
                          man.eval (mk_py_call (mk_py_attr obj' "__delete__" range) [lval] range) flow
                        )
                      ~felse:(
                        man.eval
                          (mk_py_ll_delattr lval attr range)
                      )
                  )
              )
            ~nothing_found:(man.eval
                              (mk_py_ll_delattr lval attr range))
            range mro flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__init_subclass__", _))}, _)}, cls::args, []) ->
      man.eval   (mk_py_none range) flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__repr__" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("object.__str__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["object"]
        (fun _ flow -> man.eval (mk_py_top T_string range) flow)
      |> OptionExt.return


    | _ -> None

  let ask _ _ _ = None

end


let () = register_stateless_domain (module Domain);
