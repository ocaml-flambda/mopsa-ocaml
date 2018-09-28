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
open Framework.Manager
open Framework.Location
open Universal.Ast
open Cfg.Ast
open Ast


let name = "jvm.frontend"
let debug fmt = Debug.debug ~channel:name fmt

let dump_dot = false (** dump CFG in dot file *)
let dump_exn_in_dot = false (** exception can clutter the dot file *)
let dump_text = false (** dump CFG in text format *)
let dump_field_list = false
              

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


(** Guess the end of the opcode starting at location i. *)
let rec opcode_end (code:jopcode array) (i:op_loc) : op_loc =
  if i+1 < Array.length code && code.(i+1) = OpInvalid
  then opcode_end code (i+1)
  else i


(** Get the possible successors of the opcode at location i. *)
let opcode_succ (code:jopcode array) (i:op_loc) : (token * op_loc) list =
  (* opcode immediately following *)
  let succ = (opcode_end code i) + 1 in
  match code.(i) with
  | OpLoad _ | OpStore _ | OpIInc _ | OpPop | OpPop2 | OpDup | OpDupX1
  | OpDupX2 | OpDup2 | OpDup2X1 | OpDup2X2 | OpSwap | OpConst _
  | OpAdd _ | OpSub _ | OpMult _ | OpDiv _ | OpRem _ | OpNeg _
  | OpIShl | OpLShl | OpIShr | OpLShr | OpIUShr | OpLUShr
  | OpIAnd | OpLAnd | OpIOr | OpLOr | OpIXor | OpLXor
  | OpI2L | OpI2F | OpI2D | OpL2I | OpL2F | OpL2D | OpF2I | OpF2L
  | OpF2D | OpD2I | OpD2L | OpD2F | OpI2B | OpI2C | OpI2S | OpCmp _
  | OpNew _ | OpNewArray _ | OpAMultiNewArray _
  | OpCheckCast _ | OpInstanceOf _
  | OpGetStatic _ | OpPutStatic _ | OpGetField _ | OpPutField _
  | OpArrayLength | OpArrayLoad _ | OpArrayStore _
  | OpMonitorEnter | OpMonitorExit | OpNop | OpBreakpoint ->
     (* flow directly to next instruction *)
     [T_cur, succ]
     
  | OpIf (_,dst) | OpIfCmp (_,dst) ->
     (* flow to next instruction if false, jump of true *)
     [(T_cur, succ); (F_java_if_true, i+dst)]

  | OpGoto dst ->
     (* flow unconditionally to goto target *)
     [T_cur, i+dst]

  | OpJsr dst ->
     (* jump to subroutine, indirect return to next instruction *)
     [(F_java_jsr, i+dst); (F_java_ret_site, succ)]

  | OpRet _
  | OpThrow 
  | OpReturn _ ->
     (* no target until resolved *)
     []
           
  | OpTableSwitch (def,low,_,offs) ->
     (* switch cases & default targets *)
     let ar =
       Array.mapi
         (fun x off ->
           let v = Int32.add low (Int32.of_int x) in
           F_java_switch (Some v),  i + off
         ) offs
     in
     (F_java_switch None, i + def)::(Array.to_list ar)

  | OpLookupSwitch (def,pairs) ->
     (* switch cases & default targets *)
     let l =
       List.map
         (fun (v,off) ->
           F_java_switch (Some v),  i + off
         ) pairs
     in
     (F_java_switch None, i + def)::l
     
  | OpInvoke _ ->
     (* indirect jump, indirect return to next instruction *)
     [F_java_return_site, succ]

  | OpInvalid ->
     (* no successor *)
     []
    

(** Whether the opcode can raise an exception. *)    
let raises_exn (op:jopcode) : bool =
  match op with
  | OpArrayLength | OpArrayLoad _ | OpArrayStore _
  | OpNew _ | OpNewArray _ | OpAMultiNewArray _
  | OpCheckCast _ 
  | OpThrow 
  | OpReturn _
  | OpGetStatic _ | OpPutStatic _ | OpGetField _ | OpPutField _
  | OpDiv (`Int2Bool | `Long) | OpRem (`Int2Bool | `Long)
  | OpInvoke _ 
  | OpMonitorEnter | OpMonitorExit
  | OpInvalid
    -> true

  | OpInstanceOf _
  | OpLoad _ | OpStore _ | OpIInc _ | OpPop | OpPop2 | OpDup | OpDupX1
  | OpDupX2 | OpDup2 | OpDup2X1 | OpDup2X2 | OpSwap | OpConst _
  | OpAdd _ | OpSub _ | OpMult _ | OpNeg _
  | OpDiv (`Float | `Double) | OpRem (`Float | `Double)
  | OpIShl | OpLShl | OpIShr | OpLShr | OpIUShr | OpLUShr
  | OpIAnd | OpLAnd | OpIOr | OpLOr | OpIXor | OpLXor
  | OpI2L | OpI2F | OpI2D | OpL2I | OpL2F | OpL2D | OpF2I | OpF2L
  | OpF2D | OpD2I | OpD2L | OpD2F | OpI2B | OpI2C | OpI2S | OpCmp _
  | OpNop | OpBreakpoint | OpIf _| OpIfCmp _
  | OpGoto _ | OpJsr _ | OpRet _
  | OpTableSwitch _ | OpLookupSwitch _
     -> false

    
(** Use bytecode position as locations *)
let mk_jvm_loc (meth_uid:string) (pos:op_loc) =
  mk_loc meth_uid pos 0

  
(** Bytecode ranges *)
let mk_jvm_range (meth_uid:string) (pos1:op_loc) (pos2:op_loc) =
  Range_origin
    (mk_range (mk_jvm_loc meth_uid pos1) (mk_jvm_loc meth_uid pos2))

  
(** Fill-in [g] with a CFG for code [jcode].
    Uses the unique identifier [meth_uid] as location filename.
 *)
let fill_cfg (meth_uid:string) (g:cfg) (jcode:jcode) =
  let code = jcode.c_code in

  (* add locations (nodes) *)
  Array.iteri
    (fun i op ->
      let loc = mk_jvm_loc meth_uid i in 
      if op <> OpInvalid then ignore (CFG.add_node g loc ())
    ) code;
  let endloc = mk_jvm_loc meth_uid (Array.length code) in
  ignore (CFG.add_node g endloc ());

  (* set entry *)
  CFG.node_set_entry g (CFG.get_node g (mk_jvm_loc meth_uid 0)) (Some T_cur);

  (* utility to get possible exception targets *)
  let exn i =
    List.fold_left
      (fun acc e ->
        if i < e.e_start || i >= e.e_end then acc
        else
          (F_java_exn,
           CFG.get_node g (mk_jvm_loc meth_uid e.e_handler)
          )::acc
      )
      [] jcode.c_exc_tbl
  in

  (* add opcodes (edges) *)
  Array.iteri
    (fun i op ->
      if op <> OpInvalid then
        let lend = opcode_end code i in
        let range = mk_jvm_range meth_uid i lend in
        let src_node = CFG.get_node g (mk_jvm_loc meth_uid i) in
        let src = [T_cur, src_node] in

        (* get successors *)
        let dst =
          List.map
            (fun (port,p) -> port, CFG.get_node g (mk_jvm_loc meth_uid p))
            (opcode_succ code i)
        in
        
        (* connects exceptions *)
        let dst = if raises_exn op then (exn i)@dst else dst in

        (* add edge *)
        let stmt = mk_stmt (S_java_opcode [op,(i,lend)]) range in
        ignore (CFG.add_edge g range ~src ~dst stmt);

    ) code


(** Simplifies a CFG by merging edges connected simply through
    a single node with TCut flows.
    At the end, edges correspond to basic blocs instead of single
    opcodes.
 *)
let coalesce_cfg g =
  CFG.iter_nodes
    (fun _ n ->
      match CFG.node_in n, CFG.node_out n with
      | [T_cur, e1], [T_cur, e2]
           when CFG.edge_dst_size e1 = 1 && CFG.edge_src_size e2 = 1
        ->
         (match CFG.edge_data e1, CFG.edge_data e2 with
          | { skind = S_java_opcode o1; srange = Range_origin r1; },
            { skind = S_java_opcode o2; srange = Range_origin r2; }
            ->
             (* merge ranges *)
             let r = Range_origin { r1 with range_end = r2.range_end } in
             (* merge opcode lists *)
             let s = mk_stmt (S_java_opcode (o1@o2)) r in
             (* update graph *)
             CFG.edge_set_data e1 s;
             CFG.edge_set_dst e1 (CFG.edge_dst e2);
             CFG.remove_edge g e2
             (* n should be orphan here *)
          | _ -> ())
      | _ -> ()
    ) g;
  CFG.remove_orphan g
  
  
  

(*========================================================================*)
                     (** {2 Loading} *)
(*========================================================================*)


let nb_loaded_methods = ref 0
  
let load_method
      (cls: j_class)
      (jmethod: jcode concrete_method)
    : j_method =
  let g = CFG.create () in
  (* method information *)
  let meth = {
      m_jmethod = jmethod;
      m_class = cls;
      m_uid = uid_of_class_method_signature jmethod.cm_class_method_signature;
      m_name = ms_name jmethod.cm_signature;
      m_args = ms_args jmethod.cm_signature;
      m_ret = ms_rtype jmethod.cm_signature;
      m_cfg = g;
      m_native = (jmethod.cm_implementation = Native);
      m_static = jmethod.cm_static;
    }
  in
  (* cfg construction *)
  (match jmethod.cm_implementation with
   | Native ->
      if dump_text then Format.printf "    [native]@\n"
   | Java j ->
      incr nb_loaded_methods;
      let jcode = Lazy.force j in
      fill_cfg meth.m_uid g jcode;
      coalesce_cfg g;
      Precheck.analyze meth;
      if dump_dot then (
        let f = open_out (Printf.sprintf "tmp/%s.dot" meth.m_name) in
        let fmt = Format.formatter_of_out_channel f in
        CFG.print_dot
          { CFG.dot_pp_node =
              (fun fmt n -> Format.fprintf fmt "%i:" (CFG.node_id n).loc_line);
            CFG.dot_pp_edge =
              (fun fmt e -> pp_stmt fmt (CFG.edge_data e));
            CFG.dot_pp_port =
              (fun fmt t -> pp_token fmt t);
            CFG.dot_filter_node = (fun x -> true);
            CFG.dot_filter_edge = (fun x -> true);
            CFG.dot_filter_port =
              (fun x -> dump_exn_in_dot || x <> F_java_exn);
          }
          meth.m_name fmt g ;
        Format.pp_print_flush fmt ();
        close_out f
      );
      if dump_text then
        Format.printf "    %a@\n" pp_stmt (mk_stmt (S_CFG g) (mk_fresh_range ()))
  );
  meth


let nb_loaded_classes = ref 0

let load_class jclass : j_class =
  Format.printf "class %s@." (cn_name (get_name jclass));
  incr nb_loaded_classes;
  let fields = get_fields jclass in
  FieldMap.iter
    (fun sign field ->
      if dump_field_list then
        Format.printf "  field %s@\n" (JPrint.field_signature sign)
    ) fields;
  (* global class information *)
  let cls = {
      c_jclass = jclass;
      c_uid = cn_name (get_name jclass);
      c_name = cn_simple_name (get_name jclass);
      c_package = cn_package (get_name jclass);
      c_methods = MapExt.StringMap.empty;
    }
  in
  (* translate each method *)
  MethodMap.iter
    (fun sign m ->
      if dump_field_list then
        Format.printf "  method %s@\n" (JPrint.method_signature sign);
      let mm = load_method cls m in
      cls.c_methods <- MapExt.StringMap.add mm.m_uid mm cls.c_methods
    )
    (get_concrete_methods jclass);
  
  cls
  

  
(*========================================================================*)
                       (** {2 Test} *)
(*========================================================================*)


let jdk_path = "/opt/oracle-jdk-bin-1.8.0.181/"
let jdk_lib_path = jdk_path ^ "jre/lib/"
             
let class_path =
  class_path
    (String.concat
       ":"
       [jdk_lib_path ^ "resources.jar";
        jdk_lib_path ^ "rt.jar";
        jdk_lib_path ^ "jsse.jar";
        jdk_lib_path ^ "jce.jar";
        jdk_lib_path ^ "charsets.jar"
       ]
    )

let nb_jar = ref 0

let test () =
  Format.printf "JVM test@\n";
  List.iter
    (fun x ->
      incr nb_jar;
      Format.printf "loading %s@\n" x;
      iter (fun c -> ignore (load_class c)) x
    )
    [jdk_lib_path ^ "charsets.jar";
     jdk_lib_path ^ "deploy.jar";
     jdk_lib_path ^ "javaws.jar";
     jdk_lib_path ^ "jce.jar";
     jdk_lib_path ^ "jfxswt.jar";
     jdk_lib_path ^ "jsse.jar";
     jdk_lib_path ^ "plugin.jar";
     jdk_lib_path ^ "rt.jar";
     jdk_lib_path ^ "ext/cldrdata.jar";
     jdk_lib_path ^ "ext/dnsns.jar";
     jdk_lib_path ^ "ext/jaccess.jar";
     jdk_lib_path ^ "ext/jfxrt.jar";
     jdk_lib_path ^ "ext/localedata.jar";
     jdk_lib_path ^ "ext/nashorn.jar";
     jdk_lib_path ^ "ext/sunec.jar";
     jdk_lib_path ^ "ext/sunjce_provider.jar";
     jdk_lib_path ^ "ext/sunpkcs11.jar";
     jdk_lib_path ^ "ext/zipfs.jar";     
    ];

    (*load_class (get_class class_path (make_cn "org.omg.stub.javax.management.remote.rmi._RMIConnection_Stub"))*)

  Format.printf
    "%i jar(s) loaded@\n%i class(es) loaded@\n%i method(s) loaded@\n%i error(s) found@\n"
    !nb_jar !nb_loaded_classes !nb_loaded_methods !Precheck.nb_errors
  

let _ = test ()
      
