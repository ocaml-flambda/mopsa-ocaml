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

(** Handling of class definition and instantiation. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.class"
    end)

  let checks = []

  let init _ _ flow = None


  let rec eval  exp man (flow: 'a flow) =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [{ekind = E_constant (C_string name)}; {ekind = E_py_tuple bases}; {ekind = E_py_dict ([], [])}], []) ->
       bind_list bases man.eval flow >>$ (fun bases flow ->
       (* FIXME: mechanism more general than S_py_class, since we don't assign any variable to the class object, and just return it. Refactoring needed *)
       let py_cls_var = mk_fresh_uniq_var name  (T_py None) () in
       (* TODO: change py_cls_var:var in py_cls_dec by a string? *)
       let py_cls_bases =
         match bases with
         | [] -> [find_builtin "object"]
         | _ -> List.map (fun x -> let a, e = object_of_expr x in (a, None))  bases
       in
       let clsdec = {
           py_cls_var;
           py_cls_bases=List.map (fun c -> mk_py_object c range) py_cls_bases;
           py_cls_body=mk_block [] range;
           py_cls_static_attributes=[];
           py_cls_decors=[];
           py_cls_keywords=[];
           py_cls_range=range} in
       let mro = c3_lin ({addr_kind= (A_py_class (C_user clsdec, py_cls_bases)); addr_partitioning=G_all; addr_mode = STRONG}, None) in
       eval_alloc man (A_py_class (C_user clsdec, mro)) range flow >>$ (fun addr flow ->
         Eval.singleton (mk_py_object (addr, None) range) flow
       )) |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [name; bases; dict], []) ->
        panic_at range "type(..., ..., ...) not supported"

    | E_py_call({ekind = E_py_object ({addr_kind=A_py_class (C_builtin "type", _)}, _)}, args, []) ->
      None

    | E_py_call({ekind = E_py_object (({addr_kind=A_py_class (C_builtin "bool", _)}, _))}, args, kwargs) ->
       None

    | E_py_call({ekind = E_py_object (({addr_kind=(A_py_class _ | A_py_c_class _)}, _) as cls)} as ecls, args, kwargs) ->
      debug "class call  %a@\n@\n" pp_expr exp;
      (* FIXME: this is actually type.__call__(cls) *)
      (* Call __new__ and __init__ *)

      let rec bind count args vars (flow: 'a post) f =
      (* we don't want to evaluate args twice, (in __new__ and __init__)
         we don't want a cascading disjunction either, so we assign them into tmps... *)
        match args with
        | [] ->
           f (List.rev vars) flow
        | hd :: tl ->
           flow >>%
           man.eval hd >>$
             (fun ehd flow ->
                 let cs = Flow.get_callstack flow in
                 let tmp = mk_range_attr_var hd.erange (Format.asprintf "#%d%a%xd" count pp_expr ecls (Hashtbl.hash_param 30 100 cs)) (T_py None) in
                 bind (count+1) tl (tmp::vars) (man.exec   (mk_assign (mk_var tmp hd.erange) ehd range) flow) f) in
      bind 0 args [] (Post.return flow)
        (fun vars flow ->
          let tmps = List.map (fun v ->
                         match vkind v with
                         | V_range_attr (r, _) -> mk_var v r
                         | _ -> assert false) vars in
          let new_call = mk_py_kall (mk_py_object_attr cls "__new__" (tag_range range "__new__")) ((mk_py_object cls range)
                                                                             :: tmps) kwargs (tag_range range "__new__") in
          flow >>%
            man.eval   new_call |>
            Cases.add_cleaners (List.map (fun x -> match ekind x with
                                                  | E_var (x, _) -> mk_remove_var x range
                                                  | _ -> assert false) tmps) >>$
            (fun inst flow ->
                assume
                  (mk_py_isinstance inst ecls range)
                  ~fthen:(fun flow ->
                    debug "init!@\n";
                    man.eval   (mk_py_kall (mk_py_object_attr cls "__init__" (tag_range range "__init__")) (inst :: tmps) kwargs (tag_range range "__init__")) flow >>$
                      (fun r flow ->
                          assume
                            (mk_py_isinstance_builtin r "NoneType" range)
                            ~fthen:(fun flow ->
                              Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
                              man.eval inst)
                            ~felse:(fun flow ->
                              let msg = Format.asprintf "__init__() should return None, not %a" pp_expr r in
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                              Eval.empty
                            )
                            man flow
                  ))
                  ~felse:(fun flow -> Eval.singleton inst flow)
                  man flow
              )
        )
      |> OptionExt.return

    | _ -> None

  let rec exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    (* 𝕊⟦ class cls: body ⟧ *)
    | S_py_class cls ->
      debug "definition of class %a" pp_var cls.py_cls_var;
      bind_list cls.py_cls_bases man.eval flow >>$ (fun bases flow ->
          let bases' =
            match bases with
            | [] -> [find_builtin "object"]
            | _ -> List.map (fun x -> let a, e = object_of_expr x in (a, None))  bases
          in
          if is_builtin_clsdec cls then
            let name = builtin_clsdec_name cls in
            create_builtin_class (C_builtin name) name cls bases' range;
            man.exec cls.py_cls_body flow >>%
            Post.return
          else
          if is_unsupported_clsdec cls then
            let name = get_orig_vname cls.py_cls_var in
            create_builtin_class (C_unsupported name) name cls bases' range;
            man.exec cls.py_cls_body flow >>%
            Post.return
          else
            try
              debug "bases' = %a@\n" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) (List.map (fun x -> mk_py_object x range) bases');
              let mro = c3_lin ({addr_kind= (A_py_class (C_user cls, bases')); addr_partitioning=G_all; addr_mode = STRONG}, None) in
              debug "MRO of %a: %a@\n" pp_var cls.py_cls_var
                (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                   (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (srange stmt))))
                mro;

              eval_alloc man (A_py_class (C_user cls, mro)) stmt.srange flow |>
              bind_result (fun addr flow ->
                  let obj = (addr, None) in
                  man.exec (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range) flow >>% fun flow ->
                  debug "Body of class is %a@\n" pp_stmt cls.py_cls_body;
                  man.exec cls.py_cls_body flow >>% fun flow ->
                  let parent = List.hd @@ List.tl mro in
                  man.eval (mk_py_call (mk_py_object_attr parent "__init_subclass__" range) [mk_py_object obj range] range) flow >>$
                    (fun _ flow ->
                      Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
                      Post.return)
                )
            with C3_lin_failure ->
              Exceptions.warn "C3 linearization failure during class declaration %a@\n" pp_var cls.py_cls_var;
              man.exec (Utils.mk_builtin_raise_msg "TypeError" "Cannot create a consistent method resolution order (MRO)" range) flow
              >>% Post.return
        )
      |> OptionExt.return

    | _ -> None


  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
