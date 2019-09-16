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

(** Heap addresses of Python objects. *)

open Mopsa
open Ast
open Universal.Ast

let debug fmt = Debug.debug ~channel:"python.addr" fmt

(*==========================================================================*)
(**                           {2 Addresses}                                 *)
(*==========================================================================*)


(** Parameters of instances of some builtin-in types *)
type obj_param =
  | List of py_object (* the address of the iterated list used by listiter *)
  | Tuple of py_object
  | Dict of py_object
  | Range of py_object
  | Generator of py_fundec

(** Classes *)
type class_address =
  | C_builtin of string (* name of a built-in class *)
  | C_user of py_clsdec (* declaration of a user class *)
  | C_unsupported of string (** unsupported class *)

(** Functions *)
type function_address =
  | F_builtin of string (* name of a builtin function *)
  | F_user of py_fundec (* declaration of a user function *)
  | F_unsupported of string (** unsupported function *)
  | F_annot of py_func_annot (* function annotations *)

type module_address =
  | M_user of string (** name *) * var list (** globals *)
  | M_builtin of string (** name *)

(** Kinds of Python addresses *)
(** These addresses refer only to static objects *)
type addr_kind +=
  | A_py_class of class_address (** class *) * py_object list (** mro *)
  | A_py_function of function_address (** function *)
  | A_py_method of py_object (** address of the function to bind *) * expr (** method instance *)
  | A_py_module of module_address


(** Allocate an object on the heap and return its address as an evaluation *)
let eval_alloc man kind range flow =
  let exp = mk_alloc_addr kind range in
  let open Sig.Domain.Manager in
  man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
  Eval.bind (fun exp flow ->
      match ekind exp with
      | E_addr (addr) -> Result.singleton addr flow
      | _ -> panic "eval_alloc: allocation returned a non-address express %a" pp_expr exp
    )

(*==========================================================================*)
(**                           {2 Built-ins}                                 *)
(*==========================================================================*)


(** Lists of built-ins *)
let classes = Hashtbl.create 100
let functions = Hashtbl.create 100
let modules = Hashtbl.create 10
(* let all () = !classes @ !functions @ !modules *)
let type_aliases = Hashtbl.create 100
let typed_functions = Hashtbl.create 100

(** Name of a builtin with an optional dot notation in case of
   sub-objects (methods of classes, etc.) *)
let mk_dot_name base name =
  match base with
  | None -> name
  | Some base -> base ^ "." ^ name

(** Return the base and the attribute of a dot name *)
let split_dot_name x =
  let l = String.split_on_char '.' x in
  match l with
  | [cls; attr] -> Some (cls, attr)
  | [modul; cls; attr] -> Some (modul ^ "." ^ cls, attr)
  | _ -> None

(** Address of an object *)
let addr_of_object (obj: py_object) : addr = fst obj

let kind_of_object (obj: py_object) : addr_kind =
  let addr = addr_of_object obj in
  addr.addr_kind

(** Name of an object *)
let object_name obj =
  match kind_of_object obj with
  | A_py_class(C_builtin name, _) | A_py_class(C_unsupported name, _)
  | A_py_function(F_builtin name) | A_py_function(F_unsupported name)
  | A_py_module(M_builtin name) | A_py_module(M_user (name, _))
    -> name
  | A_py_function(F_user f) -> get_orig_vname f.py_func_var
  | A_py_function(F_annot f) -> get_orig_vname f.py_funca_var
  | A_py_class(C_user c, _) -> get_orig_vname c.py_cls_var
  | _ -> panic "builtin_name: %a is not a builtin" pp_addr (addr_of_object obj)

let add_type_alias (v: var) (e: expr) =
  Hashtbl.replace type_aliases v e

let find_type_alias_by_name (s: string) : expr =
  let exception Foundit of expr in
  try
    Hashtbl.iter (fun v e -> if get_orig_vname v = s then raise (Foundit e)) type_aliases;
    raise Not_found
  with Foundit e -> e

let add_builtin_class obj () =
  Hashtbl.add classes (object_name obj) obj

let print_classes fmt _ =
  Hashtbl.iter (fun cl _ ->
      Format.fprintf fmt "%s\n" cl
    ) classes

let print_functions fmt _ =
  Hashtbl.iter (fun cl _ ->
      Format.fprintf fmt "%s\n" cl
    ) functions


let add_builtin_function obj () =
  debug "added builtin function %s" (object_name obj);
  Hashtbl.add functions (object_name obj) obj

let add_typed_function obj =
  Hashtbl.add typed_functions (object_name obj) obj

let add_typed_function_overload obj =
  match Hashtbl.find_opt typed_functions (object_name obj) with
  | None -> add_typed_function obj
  | Some ({addr_kind = A_py_function (F_annot oldf)} as a, _) ->
    let obj_sig = match akind @@ fst obj with
      | A_py_function (F_annot f) -> f.py_funca_sig
      | _ -> assert false in
    let newf = {oldf with py_funca_sig=(oldf.py_funca_sig @ obj_sig)} in
    Hashtbl.remove typed_functions (object_name obj);
    Hashtbl.add typed_functions (object_name obj) ({a with addr_kind = A_py_function (F_annot newf)}, snd obj);
  | _ -> assert false


let add_builtin_module obj () =
  Hashtbl.add modules (object_name obj) obj

(** Search for the address of a builtin given its name *)
let find_builtin name =
  let search = fun tbl -> Hashtbl.find tbl name in
  try search typed_functions
  with Not_found ->
  try search classes
  with Not_found ->
  try search functions
  with Not_found ->
    search modules


let is_object_unsupported obj =
  match kind_of_object obj with
  | A_py_class(C_unsupported _, _)
  | A_py_function (F_unsupported _) -> true
  | _ -> false

(** Check whether a built-in exists given its name *)
let is_builtin_name name =
  let exists = fun tbl -> Hashtbl.mem tbl name in
  exists classes || exists functions || exists modules || exists typed_functions

(** Check whether an attribute of a built-in object exists, given its name *)
let is_builtin_attribute base attr =
  let name = object_name base in
  if is_object_unsupported base then
    panic "Unsupported builtin %s" name
  else
    match kind_of_object base with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      is_builtin_name (mk_dot_name (Some name) attr)
    | _ -> false

(** Search for the address of a builtin attribute *)
let find_builtin_attribute base attr =
  let name = object_name base in
  if is_object_unsupported base then
    panic "Unsupported builtin %s" name
  else
    match kind_of_object base with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) | A_py_module(M_user (name, _)) ->
      find_builtin (mk_dot_name (Some name) attr)
    | A_py_class(C_user cls, _) ->
      let name = get_orig_vname cls.py_cls_var in
      find_builtin (mk_dot_name (Some name) attr)
    | _ -> assert false


(** Check whether a dot-named function [f] is a member of the class [cls] *)
let is_builtin_class_function cls f =
  match split_dot_name f with
  | None -> false
  | Some (cls', _) -> cls = cls' && is_builtin_name f


(** Class name of an atomic type *)
let atomic_type_to_class_name (t: typ) : string=
  let open Universal.Ast in
  match t with
  | T_int -> "int"
  | T_float F_DOUBLE -> "float"
  | T_bool -> "bool"
  | T_string -> "str"
  | T_py_none -> "NoneType"
  | T_py_complex -> "complex"
  | T_py_not_implemented -> "NotImplementedType"
  | _ -> assert false



(*==========================================================================*)
(**                      {2 Utility functions}                              *)
(*==========================================================================*)


(** Address of the type class of an object *)
let class_of_object (obj: py_object) : py_object =
  match kind_of_object obj with
  | A_py_class _ -> find_builtin "type"
  | A_py_function _ -> find_builtin "function"
  (* | A_py_method _ -> find_builtin "method" *)
  | A_py_module _ -> find_builtin "module"
  | _ -> assert false

let mro (obj: py_object) : py_object list =
  match kind_of_object obj with
  | A_py_class (c, b) -> b
  | _ -> assert false


(** Check class membership of an instance *)
let isinstance obj cls =
  match kind_of_object obj, kind_of_object cls with
  | A_py_class _, A_py_class (C_builtin "type", _)-> true
  | A_py_class _, _ -> false
  | _ -> (* FIXME *) assert false

let isclass obj =
  match kind_of_object obj with
  | A_py_class _ -> true
  | _ -> false

let is_not_implemented r =
  let o = object_of_expr r in
  isinstance o (find_builtin "NotImplementedType")

let is_none r =
  let o = object_of_expr r in
  isinstance o (find_builtin "NoneType")

let mk_py_z_interval l u range =
  mk_z_interval l u range

let mk_py_float_interval l u range =
  mk_float_interval l u range


let mk_py_issubclass e1 e2 range =
  mk_py_call (mk_py_object (find_builtin "issubclass") range) [e1; e2] range

let mk_py_issubclass_builtin_r e builtin range =
  let obj = find_builtin builtin in
  mk_py_issubclass e (mk_py_object obj range) range

let mk_py_issubclass_builtin_l builtin e range =
  let obj = find_builtin builtin in
  mk_py_issubclass (mk_py_object obj range) e range


let mk_py_issubclass_builtin2 blt1 blt2 range =
  let obj1 = find_builtin blt1 in
  let obj2 = find_builtin blt2 in
  mk_py_issubclass (mk_py_object obj1 range) (mk_py_object obj2 range) range

let mk_py_hasattr e attr range =
  mk_py_call (mk_py_object (find_builtin "hasattr") range) [e; mk_constant ~etyp:T_string (C_string attr) range] range

let mk_py_isinstance e1 e2 range =
  mk_py_call (mk_py_object (find_builtin "isinstance") range) [e1; e2] range

let mk_py_isinstance_builtin e builtin range =
  let obj = find_builtin builtin in
  mk_py_isinstance e (mk_py_object obj range) range

let mk_py_type e range =
  let obj = find_builtin "type" in
  mk_py_call (mk_py_object obj range) [e] range



exception C3_lin_failure

(** Computes the c3 linearization of an object. This is Python's
       approach to deal with redundant parents in the inheritance *)
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
                (List.map (fun li -> List.filter (fun x -> compare_addr_kind (akind @@ fst c) (akind @@ fst x) <> 0) li)
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
                    i = k || lk = [] || not (List.exists (fun x -> compare_addr (fst c) (fst x) = 0) (List.tl lk))) indexed_l
        in
        if a then Some c else acc
    )
    None indexed_l

let create_builtin_class kind name cls bases range =
  let mro = c3_lin ({
      addr_kind= (A_py_class (kind, bases));
      addr_group = G_all;
      addr_mode = STRONG
    }, None)
  in
  let addr = {
      addr_kind = A_py_class(kind, mro);
      addr_group = G_all;
      addr_mode = STRONG
    }
  in
  add_builtin_class (addr, None) ()


let () =
  Format.(
    register_addr_kind {
      print =
        (fun default fmt a ->
           match a with
           | A_py_class(C_user c, _) -> fprintf fmt "u{%a}" pp_var c.py_cls_var
           | A_py_class((C_builtin c | C_unsupported c), _) -> fprintf fmt "cb{%s}" c
           | A_py_function(F_user f) -> fprintf fmt "function %a" pp_var f.py_func_var
           | A_py_function(F_annot f) -> fprintf fmt "f-annot %a@\n%a" pp_var f.py_funca_var Ast.pp_py_func_annot f
           | A_py_function((F_builtin f | F_unsupported f)) -> fprintf fmt "builtin-function %s" f
           | A_py_method(f, e) -> fprintf fmt "method %a of %a" pp_addr (addr_of_object f) pp_expr e
           | A_py_module(M_builtin(m)) -> fprintf fmt "module %s" m
           | A_py_module(M_user(m, globals)) -> fprintf fmt "module %s[defined globals = %a]" m
                           (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_var) globals
           | _ -> default fmt a
        );
      compare =
        (fun default a1 a2 ->
           match a1, a2 with
           | A_py_class (c1, _), A_py_class (c2, _) ->
             begin match c1, c2 with
               | C_builtin s1, C_builtin s2
               | C_unsupported s1, C_unsupported s2 -> Pervasives.compare s1 s2
               | C_user c1, C_user c2 -> compare_var c1.py_cls_var c2.py_cls_var
               | _, _ -> default a1 a2
             end
           | A_py_function f1, A_py_function f2 ->
             begin match f1, f2 with
               | F_builtin s1, F_builtin s2
               | F_unsupported s1, F_unsupported s2 -> Pervasives.compare s1 s2
               | F_user u1, F_user u2 -> compare_var u1.py_func_var u2.py_func_var
               | F_annot f1, F_annot f2 -> compare_var f1.py_funca_var f2.py_funca_var
               | _, _ -> default a1 a2
             end
           | A_py_module m1, A_py_module m2 ->
             begin match m1, m2 with
               | M_user (s1, _), M_user (s2, _)
               | M_builtin s1, M_builtin s2 -> Pervasives.compare s1 s2
               | _, _ -> default a1 a2
             end
           | A_py_method ((addr1, oexpr1), expr1), A_py_method ((addr2, oexpr2), expr2) ->
             Compare.compose
               [ (fun () -> compare_addr addr1 addr2);
                 (fun () -> Compare.option compare_expr oexpr1 oexpr2);
                 (fun () -> compare_expr expr1 expr2); ]
           | _ -> default a1 a2)
    }
  )


let builtin_cl_and_mro s =
  let tyo = kind_of_object (find_builtin s) in
  match tyo with
  | A_py_class (c, b) -> c, b
  | _ -> assert false



type addr_kind +=
  | A_py_instance of addr
  (* it's the class from which the object is instantiated *)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_instance c -> fprintf fmt "Inst{%a}" pp_addr_kind (akind c)
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_instance c1, A_py_instance c2 ->
            compare_addr c1 c2
          | _ -> default a1 a2);})
