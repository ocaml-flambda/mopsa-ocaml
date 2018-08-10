(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Class loading.
    Based on Javalib [http://sawja.inria.fr/].
 *)

open Javalib_pack
open Javalib
open JBasics
open JCode
open Framework.Ast
open Framework.Flow
open Universal.Ast
open Cfg.Ast
open Ast


let name = "jvm.frontend"
let debug fmt = Debug.debug ~channel:name fmt



   
let class_path =
  class_path
    (String.concat
       ":"
       ["/opt/oracle-jdk-bin-1.8.0.172/jre/lib/resources.jar";
        "/opt/oracle-jdk-bin-1.8.0.172/jre/lib/rt.jar";
        "/opt/oracle-jdk-bin-1.8.0.172/jre/lib/jsse.jar";
        "/opt/oracle-jdk-bin-1.8.0.172/jre/lib/jce.jar";
        "/opt/oracle-jdk-bin-1.8.0.172/jre/lib/charsets.jar"
       ]
    )

let java_lang_string = make_cn "java.lang.String"

let java_lang_string_class = get_class class_path java_lang_string


(*========================================================================*)
                   (** {2 Utilities} *)
(*========================================================================*)


(** JNI provides a useful naming scheme for types, signatures, and thus
    to uniquely identify methods.
 *)

let rec jni_of_basic_type = function
  | `Bool -> "Z"
  | `Byte -> "B"
  | `Char -> "C"
  | `Double -> "D"
  | `Float -> "F"
  | `Int -> "I"
  | `Long -> "J"
  | `Short -> "S"

and jni_of_object_type = function
  | TClass c -> "L" ^ (jni_of_class_name c) ^ ";"
  | TArray a -> "[" ^ (jni_of_value_type a)
              
and jni_of_class_name c =
  String.concat "/" ((cn_package c)@[cn_simple_name c])

and jni_of_value_type = function
  | TBasic b -> jni_of_basic_type b
  | TObject o -> jni_of_object_type o

and jni_of_value_type_or_void = function
  | None -> "V"
  | Some t -> jni_of_value_type t
    
and jni_of_method_signature c =
  let args = List.map jni_of_value_type (ms_args c) in
  let ret = jni_of_value_type_or_void (ms_rtype c) in
  (ms_name c) ^ "(" ^ (String.concat "" args) ^ ")" ^ ret

let uid_of_class_method_signature c =
  let c, m = cms_split c in
  (jni_of_class_name c) ^ "." ^ (jni_of_method_signature m)

  
               
(*========================================================================*)
                   (** {2 CFG construction} *)
(*========================================================================*)


(** JCode CFG.
    Uses opcode pc as node and vertex identifier.
 *)
module Jcfg =
  Graph.Make
    (struct
      module NodeId = Graph.IdInt
      module EdgeId = Graph.IdInt
      module Tag = Graph.IdString
    end)
  

let mk_jcode_graph jcode =
  let code = jcode.c_code in
  (* guess the end of the opcode starting at location i *)
  let rec opcode_end i =
    if i+1 < Array.length code && code.(i+1) = OpInvalid
    then opcode_end (i+1)
    else i
  in
  let g = Jcfg.create () in
  (* add locations (nodes) *)
  Array.iteri
    (fun i op ->
      if op <> OpInvalid then ignore (Jcfg.add_node g i ())
    ) code;
  ignore (Jcfg.add_node g (Array.length code) ());
  (* add opcodes (edges) *)
  Array.iteri
    (fun i op ->
      if op <> OpInvalid then
        let src = Jcfg.get_node g i
        and dst = Jcfg.get_node g ((opcode_end i) + 1) in
        ignore (Jcfg.add_edge g ~src:["",src] ~dst:["",dst] i op)
    (* TODO: flows, alternate targets *)
    ) code;
  ()

  
let handle_class class_name =
  let cls = get_class class_path class_name in  

  let fields = get_fields cls in
  let methods = get_concrete_methods cls in
  Printf.printf "class %s\n" (cn_name (get_name cls));
  FieldMap.iter
    (fun sign field ->
      Printf.printf "  field %s\n" (JPrint.field_signature sign)
    ) fields;
  MethodMap.iter
    (fun sign meth ->
      Printf.printf "  method %s\n" (JPrint.method_signature sign);
      match meth.cm_implementation with
      | Native -> Printf.printf "    [native]\n"
      | Java j ->
         let jcode = Lazy.force j in
         mk_jcode_graph jcode;         
         Array.iteri
           (fun i op ->
             if op <> OpInvalid then
               Printf.printf "    %3i: %s\n" i (JPrint.jopcode op)
           ) jcode.c_code;
    ) methods;
  ()
  


let load_method
      (cls: j_class)
      (jmethod: jcode concrete_method)
    : j_method =
  let g = CFG.create () in
  let meth = {
      m_jmethod = jmethod;
      m_class = cls;
      m_uid = uid_of_class_method_signature jmethod.cm_class_method_signature;
      m_name = ms_name jmethod.cm_signature;
      m_args = ms_args jmethod.cm_signature;
      m_ret = ms_rtype jmethod.cm_signature;
      m_cfg = g;
    }
  in
  meth

let load_class
      (class_path: class_path)
      (class_name: class_name)
    : j_class =
  let jclass = get_class class_path class_name in
  let meths = get_concrete_methods jclass in
  
  let cls = {
      c_jclass = jclass;
      c_uid = cn_name (get_name jclass);
      c_name = cn_simple_name (get_name jclass);
      c_package = cn_package (get_name jclass);
      c_methods = MapExt.StringMap.empty;
    }
  in
  cls
  
                           
let () =
  Printf.printf "JVM test\n";
  handle_class java_lang_string
  

  

  
    
