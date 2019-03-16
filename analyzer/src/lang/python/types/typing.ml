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

(* TODO: move S_assume and eval of not into t_bool domain? *)
open Mopsa
open Ast
open Addr
open Data_model.Attribute
open MapExt
open Universal.Ast

type polytype =
  | Bot | Top

  | Class of class_address * py_object list (* class * mro *)
  | Function of function_address
  | Method of function_address * addr
  | Module of module_address

  | Instance of pytypeinst

  (* | Union of addr list *)
  | Typevar of int

and pytypeinst = {classn: polytype; uattrs: addr StringMap.t; oattrs: addr StringMap.t}

let rec compare_polytype t1 t2 =
  match t1, t2 with
  | Class (ca, objs), Class (ca', objs') ->
    compare_addr_kind (A_py_class (ca, objs)) (A_py_class (ca', objs'))
  | Function f1, Function f2 ->
    compare_addr_kind (A_py_function f1) (A_py_function f2)
  | Module m1, Module m2 ->
    compare_addr_kind (A_py_module m1) (A_py_module m2)
  | Instance i1, Instance i2 ->
    Compare.compose [
      (* (fun () -> compare_addr i1.classn i2.classn); *)
      (fun () -> compare_polytype i1.classn i2.classn);
      (fun () -> StringMap.compare compare_addr i1.uattrs i2.uattrs);
      (fun () -> StringMap.compare compare_addr i1.oattrs i2.oattrs)
    ]
  (* | Union l1, Union l2 ->
   *   ListExt.compare compare_addr l1 l2 *)
  | Typevar a1, Typevar a2 ->
    Pervasives.compare a1 a2
  | _ -> Pervasives.compare t1 t2

let map_printer = MapExtSig.{ print_empty = "∅";
                              print_begin = "{";
                              print_arrow = ":";
                              print_sep = ";";
                              print_end = "}"; }

let rec pp_polytype fmt t =
  match t with
  | Bot -> Format.fprintf fmt "⊥"
  | Top -> Format.fprintf fmt "⊤"
  | Class (C_user c, _) -> Format.fprintf fmt "Class {%a}" pp_var c.py_cls_var
  | Class (C_builtin c, _) | Class (C_unsupported c, _) -> Format.fprintf fmt "Class[%s]" c
  | Function (F_user f) -> Format.fprintf fmt "Function {%a}" pp_var f.py_func_var
  | Function (F_builtin f) | Function (F_unsupported f) -> Format.fprintf fmt "Function[%s]" f
  | Method (F_user f, a) -> Format.fprintf fmt "Method {%a}@%a" pp_var f.py_func_var pp_addr a
  | Method (F_builtin f, a) | Method (F_unsupported f, a) -> Format.fprintf fmt "Method {%s}@%a" f pp_addr a

  | Module (M_user (m, _) | M_builtin(m)) -> Format.fprintf fmt "Module[%s]" m

  | Instance {classn; uattrs; oattrs} ->
    if StringMap.is_empty uattrs && StringMap.is_empty oattrs then
      Format.fprintf fmt "Instance[%a]" pp_polytype classn
    else
      let pp_attrs = (StringMap.fprint map_printer Format.pp_print_string pp_addr) in
      Format.fprintf fmt "Instance[%a, %a, %a]" pp_polytype classn pp_attrs uattrs pp_attrs oattrs

  (* | Union l -> Format.fprintf fmt "Union[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_addr) l *)

  | Typevar t -> Format.fprintf fmt "α(%d)" t

type expr_kind +=
  | E_py_type of polytype

let () =
  register_expr_pp (fun default fmt exp ->
      match ekind exp with
      | E_py_type (Instance {classn = Class (C_builtin c, _)})
      | E_py_type (Instance {classn = Class (C_unsupported c, _)}) -> Format.fprintf fmt "%s" c
      | E_py_type (Instance {classn = Class (C_user c, _)}) -> Format.fprintf fmt "%a" pp_var c.py_cls_var
      | E_py_type p -> Format.fprintf fmt "E_py_type %a" pp_polytype p
      | _ -> default fmt exp);
  register_expr_compare (fun next e1 e2 ->
      match ekind e1, ekind e2 with
      | E_py_type p1, E_py_type p2 -> compare_polytype p1 p2
      | _ -> next e1 e2)


(* conventions: -1 represents True, -2 represents False, -3 is T:bool (Weak), -4 is None, -5 is NotImplemented, -6 is for all integers (weak),  *)
let addr_true = {addr_uid = -1; addr_kind = A_py_instance "bool"; addr_mode = STRONG}
let addr_false = {addr_uid = -2; addr_kind = A_py_instance "bool"; addr_mode = STRONG}
let addr_bool_top = {addr_uid = -3; addr_kind = A_py_instance "bool"; addr_mode = WEAK}
let addr_none = {addr_uid = -4; addr_kind = A_py_instance "NoneType"; addr_mode = STRONG}
let addr_notimplemented = {addr_uid = -5; addr_kind = A_py_instance "NotImplementedType"; addr_mode = STRONG}
let addr_integers = {addr_uid = -6; addr_kind = A_py_instance "int"; addr_mode = WEAK}


module Domain =
struct

  module Polytypeset = Framework.Lattices.Powerset.Make
      (struct
        type t = polytype
        let compare = compare_polytype
        let print = pp_polytype
      end)

  (* module PolytypesetJoin =
   * struct
   *   include Polytypeset
   *   let join _ a b = failwith "join"
   *   (\* join {instance[a]} {instance[a, empty, a]} = {instance[a, empty, a]} *\)
   * end *)

  module TMap = Framework.Lattices.Partial_map.Make
      (struct
        type t = addr
        let compare = compare_addr
        let print = pp_addr
      end)
      (Polytypeset)

  type typevar = int

  module TypeVarMap = Framework.Lattices.Partial_map.Make
      (struct
        type t = typevar
        let compare = compare
        let print fmt d = Format.fprintf fmt "%d@\n" d
      end)
      (Polytypeset)

  type t = {abs_heap: TMap.t;
            typevar_env: TypeVarMap.t}

  type _ domain += D_python_typing : t domain

  let id = D_python_typing
  let name = "python.types.typing"
  let identify : type a. a domain -> (t, a) eq option = function
    | D_python_typing -> Some Eq
    | _ -> None

  type _ Framework.Query.query +=
    | Q_exn_string : Framework.Ast.expr -> string Framework.Query.query

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Zone.Z_py_obj]; import = []}
  let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any; Universal.Zone.Z_u, Z_any]}

  let join annot d d' =
    {abs_heap = TMap.join annot d.abs_heap d'.abs_heap;
     typevar_env = TypeVarMap.join annot d.typevar_env d'.typevar_env}

  let polytype_leq (pty, env) (pty', env') =
    (* FIXME *)
    compare_polytype pty pty' = 0

  let pp_absheap = TMap.print

  let pp_typevar_env = TypeVarMap.print

  let print fmt {abs_heap; typevar_env} =
    Format.fprintf fmt "abs_heap = %a@\ntypevar_env = %a@\n"
      pp_absheap abs_heap
      pp_typevar_env typevar_env

  let subset d d' =
    let res = TMap.fold (fun absaddr ptys acc ->
        if TMap.mem absaddr d'.abs_heap then
          let ptys' = TMap.find absaddr d'.abs_heap in
          (* acc && polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env) *)
          acc && Polytypeset.for_all (fun pty -> Polytypeset.exists (fun pty' -> polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env)) ptys') ptys
        else false
      )
        d.abs_heap true
    in
    debug "subset %a %a = %b@\n" print d print d' res;
    res

  let meet _ _ =  Exceptions.panic "todo meet "

  let widen annot d d' =
    {abs_heap = TMap.widen annot d.abs_heap d'.abs_heap;
     typevar_env = TypeVarMap.widen annot d.typevar_env d'.typevar_env}

  let top = {abs_heap = TMap.top; typevar_env = TypeVarMap.top}
  let bottom = {abs_heap = TMap.bottom; typevar_env = TypeVarMap.bottom}
  let is_bottom {abs_heap; typevar_env} = TMap.is_bottom abs_heap && TypeVarMap.is_bottom typevar_env



  let init progr man flow =
    Flow.set_domain_env T_cur {abs_heap = TMap.empty; typevar_env = TypeVarMap.empty} man flow |> Flow.without_callbacks |> OptionExt.return

  let class_le (c, b: class_address * py_object list) (d, b': class_address * py_object list) : bool =
    let res = List.exists (fun x -> match akind @@ fst x with
        | A_py_class (x, _) -> x = d
        | _ -> false) b in
    debug "class_le %a %a = %b" pp_addr_kind (A_py_class (c, b)) pp_addr_kind (A_py_class (d, b')) res;
    res

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign ({ekind = E_addr ({addr_mode} as la)}, {ekind = E_py_object (a, _)}) ->
      (* si l'adresse est weak et pas dans le store, faire un assign strong *)
      let cur = Flow.get_domain_cur man flow in
      let annot = Flow.get_all_annot flow in
      if TMap.mem a cur.abs_heap then
        let tys = TMap.find a cur.abs_heap in
        let abs_heap =
          if addr_mode = STRONG || not (TMap.mem la cur.abs_heap) then
            TMap.add la tys cur.abs_heap
          else
            let old_tys = TMap.find la cur.abs_heap in
            TMap.add la (Polytypeset.union annot old_tys tys) cur.abs_heap
        in
        Flow.set_domain_cur {cur with abs_heap} man flow |> Post.return
      else
        let () = warn_at (srange stmt) "%a => addr %a not in cur.abs_heap, nothing done" pp_stmt stmt pp_addr a in
        Post.return flow
    | S_assign ({ekind = E_addr _}, _) ->
      debug "nothing to do@\n";
      Post.return flow

    | S_rename ({ekind = E_addr a}, {ekind = E_addr a'}) ->
      (* TODO: le faire autrepart (addr_env), /!\ zones *)
      let cur = Flow.get_domain_cur man flow in
      let abs_heap = TMap.rename a a' cur.abs_heap in
      debug "abs_heap = %a@\n" pp_absheap abs_heap;
      Flow.set_domain_cur {cur with abs_heap} man flow |> Post.return

    | S_add _ -> Post.return flow

    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
      begin match ekind lval, ekind rval with
        | E_py_object (alval, _), E_py_object (arval, _) when alval.addr_mode = STRONG ->
          (* FIXME: weak vs strong updates? *)
          let cur = Flow.get_domain_cur man flow in
          (* during strong updates, there should be just one sub element *)
          assert (Polytypeset.cardinal (TMap.find alval cur.abs_heap) <= 1);
          let ael = Polytypeset.map (fun old_inst ->
              let old_inst = match old_inst with
                | Instance i -> i
                | _ -> assert false in
              Instance {classn = old_inst.classn;
                        uattrs = StringMap.add attr arval old_inst.uattrs;
                        oattrs = old_inst.oattrs}) (TMap.find alval cur.abs_heap) in
          let abs_heap = TMap.add alval ael cur.abs_heap in
          let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
          Post.return flow
          (* Polytypeset.fold (fun old_inst acc ->
           *     let old_inst = match old_inst with
           *       | Instance i -> i
           *       | _ -> assert false in
           *     let new_inst = Instance {classn=old_inst.classn;
           *                              uattrs=StringMap.add attr arval old_inst.uattrs;
           *                              oattrs=old_inst.oattrs} in
           *     let abs_heap = TMap.add alval (Polytypeset.singleton new_inst) cur.abs_heap in
           *     Flow.set_domain_cur {cur with abs_heap} man flow :: acc) (TMap.find alval cur.abs_heap) []
           * |> Flow.join_list man |> Post.return *)

        | E_py_object (alval, _), E_py_object (arval, _) ->
          (* in case of weak updates, we can only add the attribute in the overapproximation *)
          let cur = Flow.get_domain_cur man flow in
          let ael = Polytypeset.map (fun old_inst ->
              let old_inst = match old_inst with
                | Instance i -> i
                | _ -> assert false in
              Instance {classn = old_inst.classn;
                        uattrs = old_inst.uattrs;
                        oattrs = StringMap.add attr arval old_inst.oattrs}) (TMap.find alval cur.abs_heap) in
          let abs_heap = TMap.add alval ael cur.abs_heap in
          let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
          Post.return flow


        | _ -> assert false
      end

    | _ -> None

  let get_builtin bltin =
    let obj = find_builtin bltin in
    match kind_of_object obj with
    | A_py_class (c, b) -> (c, b)
    | _ -> assert false

  let allocate_builtin man range flow bltin =
    (* allocate addr, and map this addr to inst bltin *)
    let range = tag_range range "alloc_%s" bltin in
    let bltin_cls, bltin_mro = get_builtin bltin in
    man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance bltin) range) flow |>
    Eval.bind (fun eaddr flow ->
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        let cur = Flow.get_domain_cur man flow in
        let bltin_inst = (Polytypeset.singleton (Instance {classn=Class (bltin_cls, bltin_mro); uattrs=StringMap.empty; oattrs=StringMap.empty})) in
        let abs_heap = TMap.add addr bltin_inst cur.abs_heap in
        let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
        (* Eval.singleton eaddr flow *)
        Eval.singleton (mk_py_object (addr, None) range) flow
      )

  let process_constant man flow range bltin addr =
    let cur = Flow.get_domain_cur man flow in
    let cls, mro = get_builtin bltin in
    let bltin_inst = Polytypeset.singleton (Instance {classn = Class (cls, mro); uattrs = StringMap.empty; oattrs = StringMap.empty}) in
    let abs_heap = TMap.add addr bltin_inst cur.abs_heap in
    let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
    Eval.singleton (mk_py_object (addr, None) range) flow

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    (* | E_py_object (addr, _) when TMap.mem addr (Flow.get_domain_cur man flow).abs_heap ->
     *   debug "this addr: %a@\n" pp_addr addr;
     *   let cur = Flow.get_domain_cur man flow in
     *   Polytypeset.fold (fun pty acc ->
     *       let abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap in
     *       let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
     *       match pty with
     *       | Class (c, b) ->
     *         debug "class is %a@\n" pp_addr addr;
     *         Eval.singleton (mk_py_object ({addr with addr_kind = (A_py_class (c, b))}, None) range) flow :: acc
     *
     *       | Instance _ ->
     *         Eval.singleton (mk_py_object ({addr with addr_kind = A_py_instance}, None) range) flow :: acc
     *
     *       (\* TODO: no need for modules and functions in types? *\)
     *       | Module m ->
     *         Eval.singleton (mk_py_object ({addr with addr_kind = A_py_module m}, None) range) flow :: acc
     *
     *       | Function f ->
     *         Eval.singleton (mk_py_object ({addr with addr_kind = A_py_function f}, None) range) flow :: acc
     *
     *       | _ -> Exceptions.panic_at range "E_py_object mem: %a@\n" pp_polytype pty)
     *     (TMap.find addr cur.abs_heap) []
     *   |> Eval.join_list |> OptionExt.return *)

    | E_py_object ({addr_kind = A_py_instance _}, _) ->
      Eval.singleton exp flow |> OptionExt.return


      (* let cur = Flow.get_domain_cur man flow in
       *
       * let ty = match akind addr with
       *   | A_py_class (c, b) -> Class (c, b)
       *   | A_py_function f -> Function f
       *   | A_py_module m -> Module m
       *   | _ -> Exceptions.panic_at range "E_py_object not mem: %a@\n" pp_addr addr in
       * let abs_heap = TMap.add addr (Polytypeset.singleton ty) cur.abs_heap in
       * let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
       * Eval.singleton (mk_py_object (addr, None) range) flow |> OptionExt.return *)

    | E_constant (C_top T_bool) ->
      process_constant man flow range "bool" addr_bool_top  |> OptionExt.return

    | E_constant (C_bool true) ->
      process_constant man flow range "bool" addr_true  |> OptionExt.return

    | E_constant (C_bool false) ->
      process_constant man flow range "bool" addr_false |> OptionExt.return

    | E_constant (C_top T_int)
    | E_constant (C_int _) ->
      process_constant man flow range "int" addr_integers |> OptionExt.return

    | E_constant C_py_none ->
      process_constant man flow range "NoneType" addr_none |> OptionExt.return

    | E_constant C_py_not_implemented ->
      process_constant man flow range "NotImplementedType" addr_notimplemented |> OptionExt.return

    | E_constant (C_top (T_float _))
    | E_constant (C_float _) ->
      allocate_builtin man range flow "float" |> OptionExt.return

    | E_constant (C_top T_string) ->
      allocate_builtin man range flow "str" |> OptionExt.return

    | E_constant (C_string s) ->
      let range = tag_range range "alloc_str" in
      let bltin_cls, bltin_mro = get_builtin "str" in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance "str" (*bltin_cls*)) range) flow |>
      Eval.bind (fun eaddr flow ->
          let addr = match ekind eaddr with
            | E_addr a -> a
            | _ -> assert false in
          let cur = Flow.get_domain_cur man flow in
          let bltin_inst = (Polytypeset.singleton (Instance {classn=Class (bltin_cls, bltin_mro); uattrs=StringMap.empty; oattrs=StringMap.empty})) in
          let abs_heap = TMap.add addr bltin_inst cur.abs_heap in
          let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
          (* Eval.singleton eaddr flow *)
          Eval.singleton (mk_py_object (addr, Some exp) range) flow
        )
      |> OptionExt.return

    | E_py_bytes _ ->
      allocate_builtin man range flow "bytes" |> OptionExt.return


    (* Je pense pas avoir besoin de ça finalement *)
    (* | E_py_object ({addr_kind = A_py_class (c, b)} as addr, expr) ->
     *   let cur = Flow.get_domain_cur man flow in
     *   let abs_heap = TMap.add addr (Polytypeset.singleton (Class (c, b))) cur.abs_heap in
     *   let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
     *   Eval.singleton (mk_addr addr range) flow |> OptionExt.return *)

    (* begin match akind with
     * | A_py_method (func, self) ->
     *    man.eval (mk_py_object ({addr_kind = akind; addr_uid = (-1); addr_mode=STRONG}, mk_py_empty range) range) flow
     * | _ ->
     *    let addr = {addr_kind = akind; addr_uid=(-1);addr_mode=STRONG} in
     *    Eval.singleton (mk_addr addr range) flow
     * end
     * |> OptionExt.return *)

    (* | E_unop(Framework.Ast.O_log_not, {ekind=E_constant (C_bool b)}) ->
     *   Eval.singleton (mk_py_bool (not b) range) flow
     *   |> OptionExt.return *)

    | E_unop(Framework.Ast.O_log_not, e') ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e' flow |>
      Eval.bind
        (fun exp flow ->
           (* FIXME: test if instance of bool and proceed accordingly *)
           match ekind exp with
           | E_constant (C_top T_bool) ->
             Eval.singleton exp flow
           | E_constant (C_bool true) ->
             Eval.singleton (mk_py_false range) flow
           | E_constant (C_bool false) ->
             Eval.singleton (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a addr_true = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
           | E_py_object (a, _) when compare_addr a addr_false = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a addr_bool_top = 0 ->
             Eval.singleton exp flow
           | _ -> failwith "not: ni"
        )
      |> OptionExt.return

    | E_py_ll_hasattr({ekind = E_py_object (addr, objexpr)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_class (C_builtin _, _)
        | A_py_module _ ->
          Eval.singleton (mk_py_bool (is_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> v.org_vname = attr) c.py_cls_static_attributes) range) flow

        | A_py_instance _ ->
          let cur = Flow.get_domain_cur man flow in
          let ptys = TMap.find addr cur.abs_heap in

          Polytypeset.fold (fun pty acc ->
              match pty with
              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
                let cur = Flow.get_domain_cur man flow in
                let flow = Flow.set_domain_cur {cur with abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap} man flow in
                Eval.singleton (mk_py_true range) flow :: acc

              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) oattrs ->
                let pty_u = Instance {classn; uattrs= StringMap.add attr (StringMap.find attr oattrs) uattrs; oattrs = StringMap.remove attr oattrs} in
                let pty_o = Instance {classn; uattrs; oattrs = StringMap.remove attr oattrs} in
                let cur = Flow.get_domain_cur man flow in
                let flowt = Flow.set_domain_cur {cur with abs_heap = TMap.add addr (Polytypeset.singleton pty_u) cur.abs_heap} man flow in
                let flowf = Flow.set_domain_cur {cur with abs_heap = TMap.add addr (Polytypeset.singleton pty_o) cur.abs_heap} man flow in
                Eval.singleton (mk_py_true range) flowt :: Eval.singleton (mk_py_false range) flowf :: acc

              | Instance _ ->
                let cur = Flow.get_domain_cur man flow in
                let flow = Flow.set_domain_cur {cur with abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap} man flow in
                Eval.singleton (mk_py_false range) flow :: acc

              | _ -> Exceptions.panic "ll_hasattr %a" pp_polytype pty) ptys [] |> Eval.join_list

        | Objects.Py_list.A_py_list _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_list.A_py_iterator _ ->
          Eval.singleton (mk_py_false range) flow

        | _ ->
          debug "%a@\n" pp_expr e; assert false
      end
      |> OptionExt.return

    | E_py_ll_getattr({ekind = E_py_object (addr, objexpr)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_builtin m) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_module (M_user (name, globals)) ->
          let v = List.find (fun x -> x.org_vname = attr) globals in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var v range) flow

        | A_py_class (C_builtin c, b) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          let f = List.find (fun x -> x.org_vname = attr) c.py_cls_static_attributes in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var f range) flow

        | A_py_instance _ ->
          let cur = Flow.get_domain_cur man flow in
          let ptys = TMap.find addr cur.abs_heap in

          Polytypeset.fold (fun pty acc ->
              match pty with
              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
                let attr_addr = StringMap.find attr uattrs in
                let cur = Flow.get_domain_cur man flow in
                let flow = Flow.set_domain_cur {cur with abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap} man flow in
                Eval.singleton (mk_py_object (attr_addr, None) range) flow :: acc

              | _ -> Exceptions.panic "ll_hasattr %a@\n"  pp_polytype pty)
            ptys [] |> Eval.join_list

        | _ -> Exceptions.panic_at range "ll_getattr: todo"
      end
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind
        (fun earg flow ->
           let cur = Flow.get_domain_cur man flow in
           let proceed addr (cl, mro, cur) =
             let flow = Flow.set_domain_cur cur man flow in
             let obj = mk_py_object ({addr with addr_kind = A_py_class (cl, mro)}, None) range in
             Eval.singleton obj flow in
           match ekind earg with
           | E_py_object ({addr_kind = A_py_instance _} as addr, _) ->
             let ptys = TMap.find addr cur.abs_heap in
             let types = Polytypeset.fold (fun pty acc ->
                 match pty with
                 | Instance {classn = Class (c, b) } ->
                   let cur = Flow.get_domain_cur man flow in
                   let abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap in
                   (c, b, {cur with abs_heap})::acc
                 | _ -> Exceptions.panic_at range "type : todo"
               ) ptys [] in
             List.map (proceed addr) types |> Eval.join_list

           | E_py_object ({addr_kind = Objects.Py_list.A_py_list _} as a, _) ->
             let lc, lb = get_builtin "list" in
             proceed a (lc, lb, cur)

           | E_py_object ({addr_kind = Objects.Py_list.A_py_iterator (s, _)} as a, _) ->
             let ic, ib = get_builtin s in
             proceed a (ic, ib, cur)

           | _ -> Exceptions.panic_at range "type: todo"


        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
      Eval.eval_list [cls; cls'] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      Eval.bind (fun evals flow ->
          let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_cls = match ekind cls with | E_py_object (a, _) -> a | _ -> assert false in
          let addr_cls' = match ekind cls' with | E_py_object (a, _) -> a | _ -> assert false in
          match akind addr_cls, akind addr_cls' with
          | A_py_class (c, mro), A_py_class (c', mro') ->
            Eval.singleton (mk_py_bool (class_le (c, mro) (c', mro')) range) flow
          | _ -> assert false)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
      Eval.eval_list [obj; attr] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      Eval.bind (fun evals flow ->
          let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          debug "now isinstance(%a, %a)@\n" pp_expr eobj pp_expr eattr;
          let addr_obj = match ekind eobj with
            | E_py_object (a, _) -> a
            | _ -> assert false in
          let addr_attr = match ekind eattr with
            | E_py_object (a, _) -> a
            | _ -> assert false in
          match akind addr_obj, akind addr_attr with
          (* FIXME: isinstance _, object *)
          | A_py_class _, A_py_class (C_builtin c, _) ->
            Eval.singleton (mk_py_bool (c = "type") range) flow

          | A_py_function _, A_py_class (C_builtin c, _) ->
            Eval.singleton (mk_py_bool (c = "function") range) flow

          | A_py_instance _, A_py_class (c, mro) ->
            let cur = Flow.get_domain_cur man flow in
            let ptys = TMap.find addr_obj cur.abs_heap in
            Polytypeset.fold (fun pty acc ->
                begin match pty with
                  | Instance {classn=Class (ci, mroi); uattrs; oattrs} ->
                    Eval.singleton (mk_py_bool (class_le (ci, mroi) (c, mro)) range) flow :: acc
                  | _ -> Exceptions.panic "todo@\n"
                end) ptys []
            |> Eval.join_list

          | Objects.Py_list.A_py_list _, A_py_class (C_builtin c, _) ->
            Eval.singleton (mk_py_bool (c = "list") range) flow

          | Objects.Py_list.A_py_iterator (s, _), A_py_class (C_builtin c, _) ->
            Eval.singleton (mk_py_bool (c = s) range) flow

          | _ -> assert false
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
      Eval.eval_list args (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      Eval.bind (fun args flow ->
          match args with
          | [] ->
            debug "Error during creation of a new instance@\n";
            man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton
          | cls :: tl ->
            let cls, mro = match akind @@ fst @@ object_of_expr cls with
              | A_py_class (c, mro) -> c, mro
              | _ -> assert false in
            let cls_str = match cls with
              | C_builtin s
              | C_unsupported s -> s
              | C_user c -> c.py_cls_var.org_vname
            in
            man.eval  ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance cls_str) range) flow |>
            Eval.bind (fun eaddr flow ->
                let addr = match ekind eaddr with
                  | E_addr a -> a
                  (* | E_py_object (a, _) -> a *)
                  | _ -> assert false in
                let cur = Flow.get_domain_cur man flow in
                let inst = Polytypeset.singleton (Instance {classn = Class (cls, mro); uattrs=StringMap.empty; oattrs=StringMap.empty}) in
                let abs_heap = TMap.add addr inst cur.abs_heap in
                let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
                Eval.singleton (mk_py_object (addr, None) range) flow
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range) flow |> OptionExt.return

    | E_py_sum_call (f, args) ->
      let func = match ekind f with
        | E_function (User_defined func) -> func
        | _ -> assert false in
      (* if !opt_pyty_summaries then
       *   Exceptions.panic_at range "todo@\n"
       * else *)
        man.eval ~zone:(Universal.Zone.Z_u, Z_any) (mk_call func args range) flow
        |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)}, cls :: [down; up; step], []) ->
      Utils.check_instances man flow range
        [down ; up   ; step]
        ["int"; "int"; "int"]
        (fun args flow -> allocate_builtin man range flow "range_iterator")
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__contains__")}, _)}, args, []) ->
      (* isinstance(arg1, range) && isinstance(arg2, int) ? *)
      Exceptions.panic "todo: %a@\n" pp_expr exp

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__len__")}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind (fun arg flow ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
          (* TODO: which one is better? *)
          (* process_constant man flow range "int" addr_integers *)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__iter__")}, _)}, [arg], []) ->
      Utils.check_instances man flow range
        [arg]
        ["range"]
        (fun r flow -> allocate_builtin man range flow "range_iterator")
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range_iterator.__next__")}, _)}, [arg], []) ->
      Utils.check_instances man flow range
        [arg]
        ["range_iterator"]
        (fun _ flow ->
           let res = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_annot stopiteration res :: stopiteration :: [])
        )
      |> OptionExt.return


    | E_py_undefined _ -> Eval.singleton exp flow |> OptionExt.return

    | E_py_object _ -> Eval.singleton exp flow |> OptionExt.return

    | _ ->
      Exceptions.panic_at range "Warning: no eval for %a" pp_expr exp (* ;
           None *)

  let ask : type r. r Framework.Query.query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_exn_string t ->
      let cur = Flow.get_domain_cur man flow in
      let addr = match ekind t with
        | E_py_object (a, _) -> a
        | _ -> assert false in
      let ptys = TMap.find addr cur.abs_heap in
      if Polytypeset.cardinal ptys = 1 then
        let r = Polytypeset.choose ptys in
        let str = match r with
          | Instance {classn} -> begin match classn with
              | Class (c, b) -> begin match c with
                  | C_builtin name | C_unsupported name -> name
                  | C_user c -> c.py_cls_var.org_vname
                end
              | _ -> assert false
            end
          | _ -> assert false in
        let () = debug "answer to query is %s@\n" str in
        Some str
      else
        assert false
    | _ -> None


end

let () = register_domain (module Domain)
