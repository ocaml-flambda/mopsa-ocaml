(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of class definition and instantiation. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr


module Domain =
  struct

    exception C3_lin_failure

    type _ domain += D_python_objects_class : unit domain

    let id = D_python_objects_class
    let name = "python.objects.class"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_objects_class -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [any_zone]; import = []}
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ flow = Some flow

    let create_builtin_class kind name cls bases range =
      let addr = {
          addr_kind = A_py_class(kind, bases);
          addr_uid = 0;
        }
      in
      Addr.add_builtin_class (addr, mk_py_empty range) ()

    (** Method resolution order of an object *)
    (* let rec mro (obj: py_object) : py_object list =
     *   match kind_of_object obj with
     *   | A_py_class(_, bases) ->
     *      let res = c3_lin obj in
     *
     *      let stupid_range = Range_fresh (-1) in
     *      debug "MRO of %a: %a@\n" pp_expr (mk_py_object obj stupid_range)
     *        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
     *           (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x stupid_range)))
     *        res;
     *
     *      res
     *   | _ -> assert false *)

    let rec c3_lin (obj: py_object) : py_object list =
      (* Spec of c3_lin : (C(B1, ..., BN) meaning class C inherits directly from B1, ..., BN
       *    c3_lin(C(B1, ..., BN)) = C::merge(c3_lin(B1), ..., c3_lin(BN), [B1], ..., [BN])
       *    c3_lin(object) = [object]
       *
       *    and merge(L1, ..., Ln) =
       *          let k = min_{1 <= i <= n} { k | hd(L_k) \not \in tail(L_j) \forall j \neq k } in
       *          let c = hd(L_k) in
       *          c :: merge(L1 \ {c}, ..., Ln \ {c})
       * ** Examples
       *      Due to wikipedia:
       *          class O: pass
       *          class A(O): pass
       *          class B(O): pass
       *          class C(O): pass
       *          class D(O): pass
       *          class E(O): pass
       *          class K1(A, B, C): pass
       *          class K2(D, B, E): pass
       *          class K3(D, A): pass
       *          class Z(K1, K2, K3): pass
       *
       *          a = Z()
       *      Then, the MRO is Z, K1, K2, K3, D, A, B, C, E, O
       *
       *      Found in "Linearization in Multiple Inheritance", by Michael Petter, Winter term 2016:
       *          class G: pass
       *          class F: pass
       *          class E(F): pass
       *          class D(G): pass
       *          class C(D, E): pass
       *          class B(F, G): pass
       *          class A(B, C): pass
       *
       *          a = A()
       *
       *      No MRO in this case
       *)
      match kind_of_object obj with
      | A_py_class (C_builtin "object", b) -> [obj]
      | A_py_class (c, [])  -> [obj]
      | A_py_class (c, bases) ->
         let l_bases = List.map c3_lin bases in
         let bases = List.map (fun x -> [x]) bases in
         obj :: merge (l_bases @ bases)
      | _ -> assert false

    and merge (l: py_object list list) : py_object list =
      match search_c l with
      | Some c ->
         let l' = List.filter (fun x -> x <> [])
                    (List.map (fun li -> List.filter (fun x -> Universal.Ast.compare_addr (fst c) (fst x) <> 0) li)
                       l) in
         (* l' is l with all c removed *)
         begin match l' with
         | [] -> [c]
         | _ -> c :: merge l'
         end
      | None -> raise C3_lin_failure

    and search_c (l: py_object list list) : py_object option =
      let indexed_l = List.mapi (fun i ll -> (i, ll)) l in
      List.fold_left
        (fun acc (i, li) ->
          if acc <> None || li = [] then acc
          else
            let c = List.hd li in
            let a = List.for_all (fun (k, lk) ->
                        i = k || lk = [] || not (List.exists (fun x -> Universal.Ast.compare_addr (fst c) (fst x) = 0) (List.tl lk))) indexed_l
            in
            if a then Some c else acc
        )
        None indexed_l

    let rec eval zones exp man flow =
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ C() | isinstance(C, type) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind=A_py_class (C_builtin "type", _)}, _)}, args, []) ->
         (* handled in myytypes *)
         (* FIXME: what about the value analysis? *)
         None
      | E_py_call({ekind = E_py_object cls}, args, []) when Addr.isclass cls ->
         (* Call __new__ *)
         man.eval (mk_py_call (mk_py_object_attr cls "__new__" range) ((mk_py_object cls range) :: args) range) flow |>
           Eval.bind
             (fun eobj flow ->
               Eval.assume
                 (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [eobj; exp] range)
                 ~fthen:(fun flow ->
                   debug "init!@\n";
                   man.eval (mk_py_call (mk_py_object_attr cls "__init__" range) (eobj :: args) range) flow |>
                     Eval.bind (fun r flow ->
                         Eval.assume
                           (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [r; mk_py_object (Addr.find_builtin "NoneType") range] range)
                           ~fthen:(fun flow -> Eval.singleton eobj flow)
                           ~felse:(fun flow ->
                             let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                             Eval.empty_singleton flow
                           )
                           man flow
                 ))
                 ~felse:(fun flow -> Eval.singleton eobj flow)
                 man flow
             )
         |> Option.return

      | _ -> None

    let rec exec zone stmt (man:('a, unit) man) (flow:'a flow) : 'a post option =
      let range = srange stmt in
      match skind stmt with
      (* 𝕊⟦ class cls: body ⟧ *)
      | S_py_class cls ->
         debug "definition of class %a" pp_var cls.py_cls_var;
         Eval.eval_list cls.py_cls_bases man.eval flow |>
           Post.bind man
             (fun bases flow ->
               let bases' =
                 match bases with
                 | [] -> [Addr.find_builtin "object"]
                 | _ -> List.map object_of_expr  bases
               in
               if Libs.Mopsa.is_builtin_clsdec cls then
                 let name = Libs.Mopsa.builtin_clsdec_name cls in
                 create_builtin_class (C_builtin name) name cls bases' range;
                 Post.of_flow flow
               else
                 if Libs.Mopsa.is_unsupported_clsdec cls then
                   let name = cls.py_cls_var.vname in
                   create_builtin_class (C_unsupported name) name cls bases' range;
                   Post.of_flow flow
                 else
                   try
                     let mro = c3_lin ({addr_kind= (A_py_class (C_user cls, bases')); addr_uid=(-1)}, mk_py_empty range) in
                     debug "MRO of %a: %a@\n" pp_var cls.py_cls_var
                       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                          (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (Range_fresh (-1)))))
                       mro;

                     Addr.eval_alloc man (A_py_class (C_user cls, bases')) stmt.srange flow |>
                       Post.bind man
                         (fun addr flow ->
                           let obj = (addr, mk_py_empty range) in
                           let flow = man.exec (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range) flow in
                           debug "Body of class is %a@\n" pp_stmt cls.py_cls_body;
                           man.exec cls.py_cls_body flow |>
                             Post.of_flow
                         )
                   with C3_lin_failure ->
                     Debug.warn "C3 linearization failure during class declaration %a@\n" pp_var cls.py_cls_var;
                     man.exec (Utils.mk_builtin_raise "TypeError" range) flow
                     |> Post.of_flow
             )
         |> Option.return

      | _ -> None

    (* FIXME: unused? *)
    (* (\* Parse the body of the class *\)
     * let rec parse base stmt =
     *   match skind stmt with
     *   | S_py_class(cls) ->
     *      let name = mk_dot_name base cls.py_cls_var.vname in
     *      let bases = List.map (fun base ->
     *                      match ekind base with
     *                      | E_var v -> Addr.find_builtin v.vname
     *                      | _ -> assert false
     *                    ) cls.py_cls_bases
     *      in
     *      let kind =
     *        if Libs.Mopsa.is_unsupported_clsdec cls then C_unsupported name
     *        else C_builtin name
     *      in
     *      let addr = {
     *          addr_kind = A_py_class (kind, bases);
     *          addr_uid = 0;
     *        }
     *      in
     *      Addr.add_builtin_class (addr, mk_py_empty range) ();
     *      parse (Some name) cls.py_cls_body
     *
     *   | S_py_function(fundec) ->
     *      let name = mk_dot_name base fundec.py_func_var.vname in
     *      let fundec = {fundec with py_func_var = {fundec.py_func_var with vname = name}} in
     *      let kind =
     *        if Libs.Mopsa.is_builtin_fundec fundec then F_builtin name else
     *          if Libs.Mopsa.is_unsupported_fundec fundec then F_unsupported name
     *          else F_user fundec
     *      in
     *      let addr = {
     *          addr_kind = A_py_function kind;
     *          addr_uid = -1;
     *        }
     *      in
     *      Addr.add_builtin_function (addr, mk_py_empty range) ()
     *
     *   | S_block(block) ->
     *      List.iter (parse base) block
     *
     *   | _ -> Framework.Exceptions.fail "stmt %a not supported in builtin class definition" Framework.Ast.pp_stmt stmt
     *     in
     *     parse (Some name) cls.py_cls_body *)


    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
