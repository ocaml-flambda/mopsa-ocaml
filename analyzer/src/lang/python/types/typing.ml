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
open Sig.Domain.Intermediate
open Ast
open Addr
open Data_model.Attribute
open MapExt
open SetExt
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



let addr_none = {addr_group = G_all; addr_kind = A_py_instance "NoneType"; addr_mode = STRONG}
let addr_notimplemented = {addr_group = G_all; addr_kind = A_py_instance "NotImplementedType"; addr_mode = STRONG}
let addr_integers = {addr_group = G_all; addr_kind = A_py_instance "int"; addr_mode = WEAK}
let addr_float = {addr_group = G_all; addr_kind = A_py_instance "float"; addr_mode = WEAK}

type addr_group +=
  | G_py_bool of bool option
  | G_group of addr * addr



let () =
  register_addr_group {
    print = (fun next fmt g ->
        match g with
        | G_py_bool (Some true) -> Format.fprintf fmt "true"
        | G_py_bool (Some false) -> Format.fprintf fmt "false"
        | G_py_bool None -> Format.fprintf fmt "⊤"
        | G_group (a1, a2) -> Format.fprintf fmt "(%a, %a)" pp_addr a1 pp_addr a2
        | _ -> next fmt g
      );
    compare = (fun next g1 g2 ->
        match g1, g2 with
        | G_py_bool b1, G_py_bool b2 -> Option.compare Pervasives.compare b1 b2
        | G_group (a1, b1), G_group (a2, b2) ->
          Compare.compose
            [(fun () -> compare_addr a1 a2);
             (fun () -> compare_addr b1 b2);
            ]
        | _ -> next g1 g2
      );
  }


let addr_true = {addr_group = G_py_bool (Some true); addr_kind = A_py_instance "bool"; addr_mode = STRONG}
let addr_false = {addr_group = G_py_bool (Some false); addr_kind = A_py_instance "bool"; addr_mode = STRONG}
let addr_bool_top = {addr_group = G_py_bool None; addr_kind = A_py_instance "bool"; addr_mode = WEAK}


let pyvarcounter = ref (-1)

let get_fresh_a_py_var () =
  incr pyvarcounter;
  !pyvarcounter

let greek_of_int =
  let list = ["α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο"; "π"; "ρ"; "σ"; "τ"; "υ"; "φ"; "χ"; "ψ"; "ω"] in
  let listn = List.length list in
  fun n ->
    let letter = List.nth list (n mod listn) in
    if n < listn then letter
    else Format.sprintf "%s(%d)" letter (n / listn)

type addr_kind +=
    A_py_var of int

let () =
  Format.(
    register_addr_kind {
      print =
        (fun default fmt a -> match a with
           | A_py_var a -> Format.fprintf fmt "%s" (greek_of_int a)
           | _ -> default fmt a
        );
      compare =
        (fun default a1 a2 ->
           match a1, a2 with
           | A_py_var v1, A_py_var v2 -> Pervasives.compare v1 v2
           | _ -> default a1 a2);
    }
  )

type _ query += Q_exn_string_query : expr -> (string * string) query
(** FIXME: register THAT QUERY *)

module Domain =
struct

  module Polytypeset =
  struct
    include Framework.Lattices.Powerset.Make(struct
        type t = polytype
        let compare = compare_polytype
        let print = pp_polytype
      end)


    exception JoinError

    let join t1 t2 =
      (* joins I[A, a, empty] and I[A, empty, a] into the last one *)
      let proceed i1 s2 =
        let s, r = Set.fold (fun e2 (sacc, ok) ->
            match e2 with
            | Instance i2 when compare_polytype i1.classn i2.classn = 0 &&
                               (StringMap.compare compare_addr i1.uattrs i2.uattrs <> 0 ||
                               StringMap.compare compare_addr i1.oattrs i2.oattrs <> 0) ->
              begin try
                let ru, o1, o2 = StringMap.fold2o
                    (fun k v1 (accr, acc1o, acc2o) -> (accr, StringMap.add k v1 acc1o, acc2o))
                    (fun k v2 (accr, acc1o, acc2o) -> (accr, acc1o, StringMap.add k v2 acc2o))
                    (fun k v1 v2 (accr, acc1o, acc2o) ->
                       if compare_addr v1 v2 = 0 then
                         (StringMap.add k v1 accr, acc1o, acc2o)
                       else
                         raise JoinError
                    ) i1.uattrs i2.uattrs (StringMap.empty, i1.oattrs, i2.oattrs) in
                let ro = StringMap.fold2o
                    StringMap.add
                    StringMap.add
                    (fun k v1 v2 acc ->
                       if compare_addr v1 v2 = 0 then
                         StringMap.add k v1 acc
                       else
                         raise JoinError
                    )
                    o1 o2 StringMap.empty
                in
                let ri = {classn = i1.classn; uattrs = ru; oattrs = ro} in
                (* Format.printf "join @[@\n%a@\n%a@\n%a@]@\n@\n" pp_polytype (Instance i1) pp_polytype (Instance i2) pp_polytype (Instance ri); *)
                Set.add (Instance ri) sacc, true
              with JoinError ->
                Set.add (Instance i2) sacc, ok
            end
            | _ -> Set.add e2 sacc, ok
          ) s2 (Set.empty, false) in
        if r then s else Set.add (Instance i1) s in
      Top.top_lift2 (fun s1 s2 ->
          if cardinal t1 = 1 then
            match choose t1 with
            | Instance i1 ->proceed i1 s2
            | _ -> Set.union s1 s2
          else if cardinal t2 = 1 then
            match choose t2 with
            | Instance i2 -> proceed i2 s1
            | _ -> Set.union s1 s2
          else
            Set.union s1 s2
        ) t1 t2

    (* let widen _ = join *)
  end

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

  include TMap

  let widen ctx = widen

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.typing"
    end)

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any; Universal.Zone.Z_u, Z_any]}
  }

  let polytype_leq pty pty' (* env env' *) =
    (* FIXME *)
    compare_polytype pty pty' = 0

  let print fmt d =
    Format.fprintf fmt "types: @[%a@]@\n"
    TMap.print d

  (* let subset d d' =
   * (* not supporting correctly top and bottom *)
   *   debug "subset %a %a..." print d print d';
   *   let res = TMap.fold (fun absaddr ptys acc ->
   *       if TMap.mem absaddr d' then
   *         let ptys' = TMap.find absaddr d' in
   *         debug "absaddr = %a, ptys' = %a@\n" pp_addr absaddr Polytypeset.print ptys';
   *         (\* acc && polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env) *\)
   *         acc && (Polytypeset.is_top ptys' || Polytypeset.for_all (fun pty -> Polytypeset.exists (fun pty' -> polytype_leq pty pty') ptys') ptys)
   *       else false
   *     )
   *       d true
   *   in
   *   debug "= %b@\n" res;
   *   res *)


  let merge _ _ _ = assert false


  let class_le (c, b: class_address * py_object list) (d, b': class_address * py_object list) : bool =
    let res = List.exists (fun x -> match akind @@ fst x with
        | A_py_class (x, _) -> x = d
        | _ -> false) b in
    debug "class_le %a %a = %b" pp_addr_kind (A_py_class (c, b)) pp_addr_kind (A_py_class (d, b')) res;
    res


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
        let cur = get_env T_cur man flow in
        let bltin_inst = (Polytypeset.singleton (Instance {classn=Class (bltin_cls, bltin_mro); uattrs=StringMap.empty; oattrs=StringMap.empty})) in
        let cur = TMap.add addr bltin_inst cur in
        let flow = set_env T_cur cur man flow in
        (* Eval.singleton eaddr flow *)
        Eval.singleton (mk_py_object (addr, None) range) flow
      )

  let bltin_inst bltin =
    let cls, mro = get_builtin bltin in
    Polytypeset.singleton (Instance {classn = Class (cls, mro); uattrs = StringMap.empty; oattrs = StringMap.empty})


  let process_constant ?(value=None) man flow range bltin addr =
    let cur = get_env T_cur man flow in
    let bltin_inst = bltin_inst bltin in
    let cur = TMap.add addr bltin_inst cur in
    let flow = set_env T_cur cur man flow in
    Eval.singleton (mk_py_object (addr, value) range) flow

  let init progr man flow =
    let tmap_init = List.fold_left (fun tmap static_addr ->
        let inst = match static_addr.addr_kind with
          | A_py_instance s -> bltin_inst s
          | _ -> assert false in
        TMap.add static_addr inst tmap) TMap.empty [addr_true; addr_false; addr_bool_top; addr_none; addr_notimplemented; addr_integers; addr_float] in
    set_env T_cur tmap_init man flow

  let exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_assign ({ekind = E_addr ({addr_mode} as la)}, {ekind = E_py_object (a, _)}) ->
      (* si l'adresse est weak et pas dans le store, faire un assign strong *)
      let cur = get_env T_cur man flow in
      if TMap.mem a cur then
        let tys = TMap.find a cur in
        let cur =
          if addr_mode = STRONG || not (TMap.mem la cur) then
            TMap.add la tys cur
          else
            let old_tys = TMap.find la cur in
            TMap.add la (Polytypeset.union old_tys tys) cur
        in
        set_env T_cur cur man flow |>
        Post.return |>
        Option.return
      else
        let () = warn_at (srange stmt) "%a => addr %a not in cur.abs_heap, nothing done" pp_stmt stmt pp_addr a in
        Post.return flow |>
        Option.return
    | S_assign ({ekind = E_addr _}, _) ->
      debug "nothing to do@\n";
      Post.return flow |>
      Option.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_instance _ } as a)}, {ekind = E_addr a'})
    | S_rename ({ekind = E_addr ({addr_kind = A_py_var _ } as a)}, {ekind = E_addr a'}) ->
      (* TODO: le faire autrepart (addr_env), /!\ zones *)
      let cur = get_env T_cur man flow in
      let abs_heap = TMap.rename a a' cur in
      set_env T_cur abs_heap man flow |>
      Post.return |>
      Option.return

    | S_add _ ->
      Post.return flow |>
      Option.return

    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
      begin match ekind lval, ekind rval with
        | E_py_object ({addr_kind = A_py_class (C_user c, b)} as alval, _ ), E_py_object (arval, _) when alval.addr_mode = STRONG ->
          if List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes then
            let var = List.find (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes in
            man.exec (mk_assign (mk_var var range) rval range) flow
            |> Post.return |> Option.return
          else
            Exceptions.panic_at range "Adding an attribute to a *class* is not supported yet"
        (* todo: enelver l'ancien c.py_cls_dec et ajouter le nouveau ave cla bonne variable, avant de faire la même assignation *)
        | E_py_object ({addr_kind = A_py_class (_)}, _ ), E_py_object (arval, _) ->
          Exceptions.panic_at range "Attr assignment on non user-defined classes not supported yet.@\n"
        | E_py_object (alval, _), E_py_object (arval, _) when alval.addr_mode = STRONG ->
          (* FIXME: weak vs strong updates? *)
          let cur = get_env T_cur man flow in
          (* during strong updates, there should be just one sub element *)
          (* if not (Polytypeset.cardinal (TMap.find alval cur.abs_heap) <= 1) then
           *   Exceptions.panic_at stmt.srange "In %a: TMap.find %a cur.abs_heap = %a" pp_stmt stmt pp_addr alval Polytypeset.print (TMap.find alval cur.abs_heap); *)
          let ael = Polytypeset.map (fun old_inst ->
              let old_inst = match old_inst with
                | Instance i -> i
                | _ -> assert false in
              Instance {classn = old_inst.classn;
                        uattrs = StringMap.add attr arval old_inst.uattrs;
                        oattrs = StringMap.remove attr old_inst.oattrs}) (TMap.find alval cur) in
          let abs_heap = TMap.add alval ael cur in
          let flow = set_env T_cur abs_heap man flow in
          Post.return flow |>
          Option.return
          (* Polytypeset.fold (fun old_inst acc ->
           *     let old_inst = match old_inst with
           *       | Instance i -> i
           *       | _ -> assert false in
           *     let new_inst = Instance {classn=old_inst.classn;
           *                              uattrs=StringMap.add attr arval old_inst.uattrs;
           *                              oattrs=old_inst.oattrs} in
           *     let abs_heap = TMap.add alval (Polytypeset.singleton new_inst) cur.abs_heap in
           *     set_env T_cur {cur with abs_heap} man flow :: acc) (TMap.find alval cur.abs_heap) []
           * |> Flow.join_list man |> Post.return *)

        | E_py_object (alval, _), E_py_object (arval, _) ->
          (* in case of weak updates, we can only add the attribute in the overapproximation *)
          let cur = get_env T_cur man flow in
          let ael = Polytypeset.map (fun old_inst ->
              let old_inst = match old_inst with
                | Instance i -> i
                | _ -> assert false in
              (* assert (not (StringMap.mem attr old_inst.uattrs)); *)
              Instance {classn = old_inst.classn;
                        uattrs = old_inst.uattrs;
                        oattrs = StringMap.add attr arval old_inst.oattrs}) (TMap.find alval cur) in
          let cur = TMap.add alval ael cur in
          let flow = set_env T_cur cur man flow in
          Post.return flow |>
          Option.return


        | _ -> assert false
      end

    | _ -> None

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    (* | E_py_object (addr, _) when TMap.mem addr (Flow.get_env T_cur man flow).abs_heap ->
     *   debug "this addr: %a@\n" pp_addr addr;
     *   let cur = Flow.get_env T_cur man flow in
     *   Polytypeset.fold (fun pty acc ->
     *       let abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap in
     *       let flow = set_env T_cur {cur with abs_heap} man flow in
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
      Eval.singleton exp flow |> Option.return


      (* let cur = Flow.get_env T_cur man flow in
       *
       * let ty = match akind addr with
       *   | A_py_class (c, b) -> Class (c, b)
       *   | A_py_function f -> Function f
       *   | A_py_module m -> Module m
       *   | _ -> Exceptions.panic_at range "E_py_object not mem: %a@\n" pp_addr addr in
       * let abs_heap = TMap.add addr (Polytypeset.singleton ty) cur.abs_heap in
       * let flow = set_env T_cur {cur with abs_heap} man flow in
       * Eval.singleton (mk_py_object (addr, None) range) flow |> OptionExt.return *)

    | E_constant (C_top T_bool) ->
      process_constant man flow range "bool" addr_bool_top  |> Option.return

    | E_constant (C_bool true) ->
      process_constant man flow range "bool" addr_true  |> Option.return

    | E_constant (C_bool false) ->
      process_constant man flow range "bool" addr_false |> Option.return

    | E_constant (C_top T_int)
    | E_constant (C_int _) ->
      process_constant man flow range "int" addr_integers |> Option.return

    | E_constant C_py_none ->
      process_constant man flow range "NoneType" addr_none |> Option.return

    | E_constant C_py_not_implemented ->
      process_constant man flow range "NotImplementedType" addr_notimplemented |> Option.return

    | E_constant (C_top (T_float _))
    | E_constant (C_float _) ->
      process_constant man flow range "float" addr_float |> Option.return

    | E_constant (C_top T_string) ->
      allocate_builtin man range flow "str" |> Option.return


    | E_constant (C_top T_py_complex) ->
      allocate_builtin man range flow "complex" |> Option.return

    | E_constant (C_string s) ->
      (* we keep s in the expression of the returned object *)
      let range = tag_range range "alloc_str" in
      let bltin_cls, bltin_mro = get_builtin "str" in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance "str" (*bltin_cls*)) range) flow |>
      Eval.bind (fun eaddr flow ->
          let addr = match ekind eaddr with
            | E_addr a -> a
            | _ -> assert false in
          let cur = get_env T_cur man flow in
          let bltin_inst = (Polytypeset.singleton (Instance {classn=Class (bltin_cls, bltin_mro); uattrs=StringMap.empty; oattrs=StringMap.empty})) in
          let cur = TMap.add addr bltin_inst cur in
          let flow = set_env T_cur cur man flow in
          (* Eval.singleton eaddr flow *)
          Eval.singleton (mk_py_object (addr, Some exp) range) flow
        )
      |> Option.return

    | E_constant (C_top T_py_bytes)
    | E_py_bytes _ ->
      allocate_builtin man range flow "bytes" |> Option.return


    (* Je pense pas avoir besoin de ça finalement *)
    (* | E_py_object ({addr_kind = A_py_class (c, b)} as addr, expr) ->
     *   let cur = get_env T_cur man flow in
     *   let abs_heap = TMap.add addr (Polytypeset.singleton (Class (c, b))) cur.abs_heap in
     *   let flow = set_env T_cur {cur with abs_heap} man flow in
     *   Eval.singleton (mk_addr addr range) flow |> Option.return *)

    (* begin match akind with
     * | A_py_method (func, self) ->
     *    man.eval (mk_py_object ({addr_kind = akind; addr_uid = (-1); addr_mode=STRONG}, mk_py_empty range) range) flow
     * | _ ->
     *    let addr = {addr_kind = akind; addr_uid=(-1);addr_mode=STRONG} in
     *    Eval.singleton (mk_addr addr range) flow
     * end
     * |> Option.return *)

    (* | E_unop(O_log_not, {ekind=E_constant (C_bool b)}) ->
     *   Eval.singleton (mk_py_bool (not b) range) flow
     *   |> Option.return *)

    | E_unop(O_log_not, e') ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e' (*(Utils.mk_builtin_call "bool" [e'] range)*) flow |>
      Eval.bind
        (fun exp flow ->
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
           | _ ->
             Exceptions.panic "not ni on %a@\n" pp_expr exp
        )
      |> Option.return


    | E_binop(O_py_is, e1, e2) ->
      bind_list [e1;e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let e1, e2 = match evals with [e1;e2] -> e1, e2 | _ -> assert false in
          begin match ekind e1, ekind e2 with
            | E_py_object (a1, _), E_py_object (a2, _) when
                compare_addr a1 a2 = 0 &&
                (compare_addr a1 addr_notimplemented = 0 || compare_addr a1 addr_none = 0) ->
              man.eval (mk_py_true range) flow
            | _ -> man.eval (mk_py_top T_bool range) flow
          end
        )
      |> Option.return

    | E_py_ll_hasattr({ekind = E_py_object (addr, objexpr)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_user(name, globals)) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> get_orig_vname v = attr) globals) range) flow
        | A_py_class (C_builtin _, _)
        | A_py_module _ ->
          Eval.singleton (mk_py_bool (is_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes) range) flow

        | A_py_var _
        | A_py_instance _ ->
          let cur = get_env T_cur man flow in
          let ptys = TMap.find addr cur in
          debug "%a %a" pp_addr addr (Flow.print man.lattice.print) flow;
          Polytypeset.fold (fun pty acc ->
              match pty with
              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
                let cur = get_env T_cur man flow in
                let flow = set_env T_cur (TMap.add addr (Polytypeset.singleton pty) cur) man flow in
                Eval.singleton (mk_py_true range) flow :: acc

              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) oattrs ->
                let pty_u = Instance {classn; uattrs= StringMap.add attr (StringMap.find attr oattrs) uattrs; oattrs = StringMap.remove attr oattrs} in
                let pty_o = Instance {classn; uattrs; oattrs = StringMap.remove attr oattrs} in
                let cur = get_env T_cur man flow in
                let flowt = set_env T_cur (TMap.add addr (Polytypeset.singleton pty_u) cur) man flow in
                let flowf = set_env T_cur (TMap.add addr (Polytypeset.singleton pty_o) cur) man flow in
                Eval.singleton (mk_py_true range) flowt :: Eval.singleton (mk_py_false range) flowf :: acc

              | Instance _ ->
                let cur = get_env T_cur man flow in
                let flow = set_env T_cur (TMap.add addr (Polytypeset.singleton pty) cur) man flow in
                Eval.singleton (mk_py_false range) flow :: acc

              | _ -> Exceptions.panic "ll_hasattr %a" pp_polytype pty) ptys [] |> Eval.join_list ~empty:(Eval.empty_singleton flow)

        | Objects.Py_list.A_py_list _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_set.A_py_set _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_list.A_py_iterator _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict_view _ ->
          Eval.singleton (mk_py_false range) flow

        | _ ->
          Exceptions.panic_at range "%a@\n" pp_expr e
      end
      |> Option.return

    | E_py_ll_getattr({ekind = E_py_object ((addr, objexpr) as obj)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_builtin m) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_module (M_user (name, globals)) ->
          let v = List.find (fun x -> get_orig_vname x = attr) globals in
          (* FIXME: is that normal?! used in stub module unittest with builtin unittest.TestCase... *)
          if is_builtin_name (name ^ "." ^ attr) then
            let () = warn_at range "using builtin rather than variable when performing %a" pp_expr exp in
            Eval.singleton (mk_py_object (find_builtin_attribute obj attr) range) flow
          else
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var v range) flow

        | A_py_class (C_builtin c, b) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          let f = List.find (fun x -> get_orig_vname x = attr) c.py_cls_static_attributes in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var f range) flow

        | A_py_var _
        | A_py_instance _ ->
          let cur = get_env T_cur man flow in
          let ptys = TMap.find addr cur in

          Polytypeset.fold (fun pty acc ->
              match pty with
              | Instance {classn; uattrs; oattrs} when StringMap.exists (fun k _ -> k = attr) uattrs ->
                let attr_addr = StringMap.find attr uattrs in
                let cur = get_env T_cur man flow in
                let flow = set_env T_cur (TMap.add addr (Polytypeset.singleton pty) cur) man flow in
                Eval.singleton (mk_py_object (attr_addr, None) range) flow :: acc

              | _ -> Exceptions.panic "ll_hasattr %a@\n"  pp_polytype pty)
            ptys [] |> Eval.join_list ~empty:(Eval.empty_singleton flow)

        | _ -> Exceptions.panic_at range "ll_getattr: todo %a, attr=%s in@\n%a" pp_addr addr attr (Flow.print man.lattice.print) flow
      end
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind
        (fun earg flow ->
           let cur = get_env T_cur man flow in
           let proceed addr (cl, mro, cur) =
             let flow = set_env T_cur cur man flow in
             let obj = mk_py_object ({addr_group = G_all; addr_kind = A_py_class (cl, mro); addr_mode = STRONG}, None) range in
             Eval.singleton obj flow in
           match ekind earg with
           | E_py_object ({addr_kind = A_py_instance _ | A_py_var _ } as addr, _) ->
             let ptys = TMap.find addr cur in
             (* weird case happening sometimes in bm_chaos. To investigate? commit bfecf234 *)
             if Polytypeset.is_empty ptys then
               Eval.empty_singleton flow
             else
               let types = Polytypeset.fold (fun pty acc ->
                   match pty with
                   | Instance {classn = Class (c, b) } ->
                     let cur = get_env T_cur man flow in
                     let cur = TMap.add addr (Polytypeset.singleton pty) cur in
                     (c, b, cur)::acc
                   | _ -> Exceptions.panic_at range "type : todo"
                 ) ptys [] in
               List.map (proceed addr) types |> Eval.join_list ~empty:(Eval.empty_singleton flow)

           | E_py_object ({addr_kind = Objects.Py_list.A_py_list _} as a, _) ->
             let lc, lb = get_builtin "list" in
             proceed a (lc, lb, cur)

           | E_py_object ({addr_kind = Objects.Py_set.A_py_set _} as a, _) ->
             let sc, sb = get_builtin "set" in
             proceed a (sc, sb, cur)

           | E_py_object ({addr_kind = Objects.Dict.A_py_dict _} as a, _) ->
             let dc, db = get_builtin "dict" in
             proceed a (dc, db, cur)

           | E_py_object ({addr_kind = Objects.Tuple.A_py_tuple _} as a, _) ->
             let tc, tb = get_builtin "tuple" in
             proceed a (tc, tb, cur)

           | E_py_object ({addr_kind = Objects.Py_list.A_py_iterator (s, _, _)} as a, _) ->
             let ic, ib = get_builtin s in
             proceed a (ic, ib, cur)

           | E_py_object ({addr_kind = Objects.Dict.A_py_dict_view (s, _)} as a, _) ->
             let ic, ib = get_builtin s in
             proceed a (ic, ib, cur)

           | E_py_object ({addr_kind = A_py_module m} as a, _) ->
             let mc, mb = get_builtin "module" in
             proceed a (mc, mb, cur)

           | E_py_object ({addr_kind = A_py_method _} as a, _) ->
             let mc, mb = get_builtin "module" in
             proceed a (mc, mb, cur)

           | _ -> Exceptions.panic_at range "type: todo: %a@\n" pp_expr arg


        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
      bind_list [cls; cls'] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let cls, cls' = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          let addr_cls = match ekind cls with | E_py_object (a, _) -> a | _ -> assert false in
          let addr_cls' = match ekind cls' with | E_py_object (a, _) -> a | _ -> assert false in
          match akind addr_cls, akind addr_cls' with
          | A_py_class (c, mro), A_py_class (c', mro') ->
            Eval.singleton (mk_py_bool (class_le (c, mro) (c', mro')) range) flow
          | _ -> assert false)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
      bind_list [obj; attr] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let eobj, eattr = match evals with [e1; e2] -> e1, e2 | _ -> assert false in
          debug "now isinstance(%a, %a) at range %a, flow %a@\n" pp_expr eobj pp_expr eattr pp_range range (Flow.print man.lattice.print) flow;
          let addr_obj = match ekind eobj with
            | E_py_object (a, _) -> a
            | _ -> assert false in
          let addr_attr = match ekind eattr with
            | E_py_object (a, _) -> a
            | _ -> assert false in
          match akind addr_obj, akind addr_attr with
          (* FIXME: isinstance _, object *)
          | A_py_class _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "type") range) flow

          | A_py_function _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "function") range) flow

          | A_py_var _, A_py_class (c, mro)
          | A_py_instance _, A_py_class (c, mro) ->
            let cur = get_env T_cur man flow in
            let ptys = if TMap.mem addr_obj cur then TMap.find addr_obj cur
              else Polytypeset.empty in
            let ptys =
              if not (Polytypeset.is_empty ptys) then ptys
              else
                let is_ao addr = compare_addr addr_obj addr = 0 in
                let process bltin =
                  let bltin_cls, bltin_mro = get_builtin bltin in
                  Polytypeset.singleton (Instance {classn = Class (bltin_cls, bltin_mro);
                                                   uattrs = StringMap.empty;
                                                   oattrs = StringMap.empty}) in
                if is_ao addr_true || is_ao addr_false || is_ao addr_bool_top then
                  process "bool"
                else if is_ao addr_none then
                  process "NoneType"
                else if is_ao addr_notimplemented then
                  process "NotImplementedType"
                else if is_ao addr_integers then
                  process "int"
                else if addr_obj.addr_group = G_all then
                  match akind addr_obj with
                  | A_py_instance i ->
                    assert (is_builtin_name i);
                    process i
                  | _ ->
                    Exceptions.panic_at range "wtf @ %a@\n" pp_addr addr_obj
                else
                  (* Exceptions.panic_at range "wtf @ %a %a@\n" pp_addr addr_obj (Flow.print man.lattice) flow *)
                  Polytypeset.empty
            in
            Polytypeset.fold (fun pty acc ->
                begin match pty with
                  | Instance {classn=Class (ci, mroi); uattrs; oattrs} ->
                    let flow =
                      if TMap.mem addr_obj cur then
                        let cur = TMap.add addr_obj (Polytypeset.singleton pty) cur in
                        set_env T_cur cur man flow
                      else flow in
                    man.eval (mk_py_bool (class_le (ci, mroi) (c, mro)) range) flow :: acc
                  | _ -> Exceptions.panic "todo@\n"
                end) ptys []
            |> (Eval.join_list ~empty:(Eval.empty_singleton flow))

          | Objects.Py_list.A_py_list _, A_py_class (C_builtin "list", _)
          | Objects.Py_set.A_py_set _, A_py_class (C_builtin "set", _)
          | Objects.Dict.A_py_dict _, A_py_class (C_builtin "dict", _)
          | Objects.Tuple.A_py_tuple _, A_py_class (C_builtin "tuple", _) ->
            man.eval (mk_py_true range) flow

          | Objects.Py_list.A_py_list _, A_py_class (c, b)
          | Objects.Py_set.A_py_set _, A_py_class (c, b)
          | Objects.Dict.A_py_dict _, A_py_class (c, b)
          | Objects.Tuple.A_py_tuple _, A_py_class (c, b) ->
            let str_addr = match akind addr_obj with
              | Objects.Py_list.A_py_list _ -> "list"
              | Objects.Py_set.A_py_set _ -> "set"
              | Objects.Dict.A_py_dict _ -> "dict"
              | Objects.Tuple.A_py_tuple _ -> "tuple"
              | _ -> assert false in
            assume (mk_py_issubclass_builtin_l str_addr eattr range) man flow
              ~fthen:(man.eval (mk_py_false range))
              ~felse:(man.eval (mk_py_false range))


          | Objects.Py_list.A_py_iterator (s, _, _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = s) range) flow

          | Objects.Dict.A_py_dict_view (s, _), A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = s) range) flow

          | A_py_module _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "module" || c = "object") range) flow

          | A_py_method _, A_py_class (C_builtin c, _) ->
            man.eval (mk_py_bool (c = "method" || c = "object") range) flow

          | _ -> assert false
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
      bind_list args (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun args flow ->
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
              | C_user c -> get_orig_vname c.py_cls_var
            in
            man.eval  ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance cls_str) range) flow |>
            Eval.bind (fun eaddr flow ->
                let addr = match ekind eaddr with
                  | E_addr a -> a
                  (* | E_py_object (a, _) -> a *)
                  | _ -> assert false in
                let cur = get_env T_cur man flow in
                let inst = Polytypeset.singleton (Instance {classn = Class (cls, mro); uattrs=StringMap.empty; oattrs=StringMap.empty}) in
                let cur = TMap.add addr inst cur in
                let flow = set_env T_cur cur man flow in
                Eval.singleton (mk_py_object (addr, None) range) flow
              )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range) flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)}, cls :: args, []) ->
      Utils.check_instances man flow range args
        ["int"; "int"; "int"]
        (fun args flow -> allocate_builtin man range flow "range")
      |> Option.return

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
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["range"]
        (fun r flow -> allocate_builtin man range flow "range_iterator")
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range_iterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["range_iterator"]
        (fun _ flow ->
           let res = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_ctx stopiteration res :: stopiteration :: []) ~empty:(Eval.empty_singleton flow)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "slice.__new__")}, _)}, cls :: args, []) ->
      let intornone = ["int"; "NoneType"] in
      Utils.check_instances_disj man flow range args
        [intornone; intornone; intornone]
        (fun _ flow ->
           allocate_builtin man range flow "slice")
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "abs")}, _)}, args, []) ->
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length eargs <> 1 then tyerror flow
          else
            let v = List.hd eargs in
            assume (mk_py_isinstance_builtin v "int" range) man flow
              ~fthen:(man.eval (mk_py_top T_int range))
              ~felse:(fun flow ->
                assume (mk_py_isinstance_builtin v "float" range) man flow
                  ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                  ~felse:tyerror
              )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "all")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["list"] (fun _ -> man.eval (mk_py_top T_bool range))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "divmod")}, _)}, args, []) ->
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length args <> 2 then tyerror flow else
            let argl, argr = match eargs with l::r::[] -> l, r | _ -> assert false in
            assume (mk_py_isinstance_builtin argl "int" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance_builtin argr "int" range) man flow
                    ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top T_int range; mk_py_top T_int range]) range))
                    ~felse:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "float" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:tyerror
                      )
              )
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin argl "float" range) man flow
                    ~fthen:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "int" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:(fun flow ->
                              assume (mk_py_isinstance_builtin argr "float" range) man flow
                                ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                                ~felse:tyerror
                            )

                      )
                    ~felse:tyerror
                )
        )
      |> Option.return


    | E_py_undefined _ -> Eval.singleton exp flow |> Option.return

    | E_py_object _ -> Eval.singleton exp flow |> Option.return

    | _ ->
      None

  let ask : type r. r query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Q_exn_string_query t ->
        let cur = get_env T_cur man flow in
        let addr = match ekind t with
          | E_py_object (a, _) -> a
          | _ -> assert false in
        let ptys = TMap.find addr cur in
        if Polytypeset.cardinal ptys = 1 then
          let r = Polytypeset.choose ptys in
          let exc, message = match r with
            | Instance {classn; uattrs} ->
              let name = match classn with
                | Class (c, b) -> begin match c with
                    | C_builtin name | C_unsupported name -> name
                    | C_user c -> get_orig_vname c.py_cls_var
                  end
                | _ -> assert false
              in
              let message =
                if StringMap.mem "args" uattrs then
                  let var = List.hd @@ Objects.Tuple.Domain.var_of_addr @@ StringMap.find "args" uattrs in
                  man.ask (Values.Py_string.Q_strings_of_var var) flow
                else
                  ""
              in
              name, message
            | _ -> assert false in
          let () = debug "answer to query is %s %s@\n" exc message in
                    Some (exc, message)
        else
          assert false

      | _ -> None


  let refine channel man flow = Channel.return flow

end

let () = Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
