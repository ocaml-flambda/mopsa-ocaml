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

(** Classes *)
type class_address =
  | C_builtin of string (* name of a built-in class *)
  | C_user of py_clsdec (* declaration of a user class *)
  | C_unsupported of string (** unsupported class *)
  | C_annot of py_cls_annot (** class annotations *)

(** Functions *)
type function_address =
  | F_builtin of string * string (* name of a builtin function, and its type (among builtin_function_or_method, wrapper_descriptor, method_descriptor usually) *)
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
  | A_py_method of py_object (** address of the function to bind *) * py_object (** method instance *) * string  (* type. method or method-wrapper or ... *)
  | A_py_module of module_address


(** Allocate an object on the heap and return its address as an evaluation *)
let eval_alloc ?(mode=STRONG) man kind range flow =
  let exp = mk_alloc_addr ~mode:mode kind range in
  man.eval exp flow >>$
    fun exp flow ->
    match ekind exp with
    | E_addr (addr, _) -> Cases.singleton addr flow
    | _ -> panic "eval_alloc: allocation returned a non-address express %a" pp_expr exp

(*==========================================================================*)
(**                           {2 Built-ins}                                 *)
(*==========================================================================*)


(** Lists of built-ins *)
let classes = Hashtbl.create 100
let functions = Hashtbl.create 100
let modules = Hashtbl.create 10
let type_aliases = Hashtbl.create 100
let typed_functions = Hashtbl.create 100 (* and typed classes *)

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
let oobject_name obj =
  let some = fun x -> Some x in
  match kind_of_object obj with
  | A_py_class(C_builtin name, _) | A_py_class(C_unsupported name, _)
  | A_py_function(F_builtin (name, _)) | A_py_function(F_unsupported name)
  | A_py_module(M_builtin name) | A_py_module(M_user (name, _))
    -> some name
  | A_py_function(F_user f) -> some @@ get_orig_vname f.py_func_var
  | A_py_function(F_annot f) -> some @@ get_orig_vname f.py_funca_var
  | A_py_class(C_user c, _) -> some @@ get_orig_vname c.py_cls_var
  | A_py_class(C_annot c, _) -> some @@ get_orig_vname c.py_cls_a_var
  | _ -> None

let object_name obj =
  match oobject_name obj with
  | Some o -> o
  | None -> panic "builtin_name: %a is not a builtin" pp_addr (addr_of_object obj)

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
  debug "added builtin function %a" pp_addr (fst obj);
  Hashtbl.add functions (object_name obj) obj

let add_typed obj =
  let name = object_name obj in
  debug "adds %s -> %a" name pp_expr (mk_py_object obj (Location.R_fresh (-1)));
  Hashtbl.add typed_functions name obj

let add_typed_overload obj =
  match Hashtbl.find_opt typed_functions (object_name obj) with
  | None -> add_typed obj
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

let find_builtin_function = Hashtbl.find functions


let is_object_unsupported obj =
  match kind_of_object obj with
  | A_py_class(C_unsupported _, _)
  | A_py_function (F_unsupported _) -> true
  | _ -> false

(** Check whether a built-in exists given its name *)
let is_builtin_name name =
  let exists = fun tbl -> Hashtbl.mem tbl name in
  exists classes || exists functions || exists modules || exists typed_functions

let is_builtin_var v =
  match vkind v with
  | V_uniq _ -> is_builtin_name @@ get_orig_vname v
  | _ -> false

let is_builtin_module name = Hashtbl.mem modules name
let find_builtin_module name = Hashtbl.find modules name

(** Check whether an attribute of a built-in object exists, given its name *)
let is_builtin_attribute base attr =
  let name = object_name base in
  if is_object_unsupported base then
    panic "Unsupported builtin %s" name
  else
    match kind_of_object base with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      is_builtin_name (mk_dot_name (Some name) attr)
    | A_py_class (C_annot c, _) ->
      debug "searching for %s" (mk_dot_name (Some (get_orig_vname c.py_cls_a_var)) attr);
      is_builtin_name (mk_dot_name (Some (get_orig_vname c.py_cls_a_var)) attr)
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
    | A_py_class (C_annot cls, _) ->
      let name = get_orig_vname cls.py_cls_a_var in
      find_builtin (mk_dot_name (Some name) attr)
    | _ -> assert false


(** Check whether a dot-named function [f] is a member of the class [cls] *)
let is_builtin_class_function cls f =
  match split_dot_name f with
  | None -> false
  | Some (cls', _) -> cls = cls' && is_builtin_name f


(*==========================================================================*)
(**                      {2 Utility functions}                              *)
(*==========================================================================*)


let mk_py_z_interval l u range =
  mk_z_interval l u range

let mk_py_float_interval l u range =
  {(mk_float_interval l u range) with etyp=(T_py (Some (Float F_DOUBLE)))}

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
      addr_partitioning = G_all;
      addr_mode = STRONG
    }, None)
  in
  let addr = {
      addr_kind = A_py_class(kind, mro);
      addr_partitioning = G_all;
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
           | A_py_class(C_annot c, _) -> fprintf fmt "ua{%a}" pp_var c.py_cls_a_var;
           | A_py_function(F_user f) -> fprintf fmt "function %a" pp_var f.py_func_var
           | A_py_function(F_annot f) -> fprintf fmt "f-annot %a" pp_var f.py_funca_var (* Ast.pp_py_func_annot f*)
           | A_py_function(F_builtin (f, t)) -> fprintf fmt "%s %s" t f
           | A_py_function(F_unsupported f) -> fprintf fmt "unsupported-builtin %s" f
           | A_py_method(f, e, t) -> fprintf fmt "%s %a of %a" t pp_addr (addr_of_object f) Pp.pp_py_object e
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
               | C_unsupported s1, C_unsupported s2 -> Stdlib.compare s1 s2
               | C_user c1, C_user c2 -> compare_var c1.py_cls_var c2.py_cls_var
               | C_annot c1, C_annot c2 -> compare_var c1.py_cls_a_var c2.py_cls_a_var;
               | _, _ -> default a1 a2
             end
           | A_py_function f1, A_py_function f2 ->
             begin match f1, f2 with
               | F_builtin (s1, t1), F_builtin (s2, t2) ->
                 Compare.compose
                   [(fun () -> Stdlib.compare s1 s2);
                    (fun () -> Stdlib.compare t1 t2)]
               | F_unsupported s1, F_unsupported s2 -> Stdlib.compare s1 s2
               | F_user u1, F_user u2 -> compare_var u1.py_func_var u2.py_func_var
               | F_annot f1, F_annot f2 -> compare_var f1.py_funca_var f2.py_funca_var
               | _, _ -> default a1 a2
             end
           | A_py_module m1, A_py_module m2 ->
             begin match m1, m2 with
               | M_user (s1, _), M_user (s2, _)
               | M_builtin s1, M_builtin s2 -> Stdlib.compare s1 s2
               | _, _ -> default a1 a2
             end
           | A_py_method ((addr1, oexpr1), expr1, t1), A_py_method ((addr2, oexpr2), expr2, t2) ->
             Compare.compose
               [ (fun () -> compare_addr addr1 addr2);
                 (fun () -> Compare.option compare_expr oexpr1 oexpr2);
                 (fun () -> compare_py_object expr1 expr2);
                 (fun () -> Stdlib.compare t1 t2);]
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


let () = Universal.Heap.Policies.register_mk_addr
           (fun default ak -> match ak with
                              | A_py_class _ | A_py_module _ | A_py_function _ -> Universal.Heap.Policies.mk_addr_all ak
                              | _ -> default ak)

let nominal_type_of_addr_kind : (addr_kind -> string) ref = ref (fun ak -> panic "unknown nominal type for addr_kind %a" pp_addr_kind ak)

let structural_type_of_addr_kind : (addr_kind -> string -> bool) ref = ref (fun ak s -> panic "unknown structural type for addr_kind %a" pp_addr_kind ak)

let register_addr_kind_nominal_type f  = nominal_type_of_addr_kind := f !nominal_type_of_addr_kind
let register_addr_kind_structural_type f  = structural_type_of_addr_kind := f !structural_type_of_addr_kind
let addr_kind_find_nominal_type ak = !nominal_type_of_addr_kind ak
let addr_kind_find_structural_type ak s  = !structural_type_of_addr_kind ak s

type py_c_function_kind =
  | Builtin_function_or_method
  | Wrapper_descriptor of string option (* optional wrapper: in that case name *)
  | Method_descriptor

let str_of_py_c_function_kind =
  function
  | Builtin_function_or_method -> "builtin_function_or_method"
  | Wrapper_descriptor _ -> "wrapper_descriptor"
  | Method_descriptor -> "method_descriptor"

(* multilanguage *)
type addr_kind +=
   | A_py_c_module of string (** name *) (** Mopsa.program (* C program *)*)
   | A_py_c_function of
       string (** name *) *
       int (** function uid *) *
       py_c_function_kind *
       int option (* ml_flags *) *
       py_object (** self *)
   | A_py_c_class of var (** static variable used in the C stack to define the class *)


let mro (obj: py_object) : py_object list =
  match kind_of_object obj with
  | A_py_class (c, b) -> b
  | A_py_c_class _ ->  (* FIXME *) obj :: (find_builtin "object") :: []
  | _ -> assert false

let () =
  Format.(
    register_addr_kind {
        print =
          (fun default fmt a ->
            match a with
            | A_py_c_module(c(*, p*)) -> fprintf fmt "c module %s" c
            | A_py_c_function (f, _, _, _, _) -> fprintf fmt "c function %s" f
            | A_py_c_class v -> fprintf fmt "c class %a" pp_var v
            | _ -> default fmt a);
        compare =
          (fun default a1 a2 ->
            match a1, a2 with
            | A_py_c_function (f1, i1, k1, of1, o1), A_py_c_function(f2, i2, k2, of2, o2) ->
               Compare.compose
                 [
                   (fun () -> Stdlib.compare f1 f2);
                   (fun () -> Stdlib.compare i1 i2);
                   (fun () -> Stdlib.compare k1 k2);
                   (fun () -> Stdlib.compare of1 of2);
                   (fun () -> compare_py_object o1 o2);
                 ]
            | _ -> default a1 a2);
      }
  );
  register_addr_kind_nominal_type (fun default ak ->
      match ak with
      | A_py_c_module _ -> "module"
      | A_py_c_function (_, _, k, _, _) -> str_of_py_c_function_kind k
      | A_py_c_class _ -> "type"
      (* FIXME: we should/could call C's Py_TYPE? which currently assigns PyType_Type,  ~ ok up to reduction... *)
      | _ -> default ak);
  register_addr_kind_structural_type (fun default ak s ->
      match ak with
      | A_py_c_class _
      | A_py_c_function _ -> false
      | _ -> default ak s)


let () = Universal.Heap.Policies.register_mk_addr
           (fun default ak -> match ak with
                              | A_py_instance {addr_kind = A_py_class (C_builtin "member_descriptor", _)} -> Universal.Heap.Policies.mk_addr_range ak
                              (* FIXME: only if cpython analysis. A bit expensive too... *)
                              | A_py_instance {addr_kind = A_py_c_class _} -> Universal.Heap.Policies.mk_addr_stack_range ak
                              | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} -> Universal.Heap.Policies.mk_addr_stack_range ak
                              | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} -> Universal.Heap.Policies.mk_addr_stack_range ak
                              | _ -> default ak)
