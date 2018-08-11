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
open Framework.Pp
open Universal.Ast
open Cfg.Ast
open Ast


let name = "jvm.frontend"
let debug fmt = Debug.debug ~channel:name fmt


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
let rec opcode_end (code:jopcode array) (i:int) : int =
  if i+1 < Array.length code && code.(i+1) = OpInvalid
  then opcode_end code (i+1)
  else i


(** Get the possible successors of the opcode at location i. *)  
let opcode_succ (code:jopcode array) (i:int) : (token * int) list =
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
     [TCur, succ]
     
  | OpIf (_,dst) | OpIfCmp (_,dst) ->
     (* flow to next instruction if false, jump of true *)
     [(TCur, succ); (T_java_iftrue, i+dst)]

  | OpGoto dst ->
     (* flow unconditionally to goto target *)
     [TCur, i+dst]

  | OpJsr dst ->
     (* jump to subroutine, indirect return to next instruction *)
     [(T_java_jsr, i+dst); (T_java_ret, succ)]

  | OpRet _
  | OpThrow
  | OpReturn _ ->
     (* indirect target, none until resolved *)
     []
           
  | OpTableSwitch (def,low,_,offs) ->
     (* switch cases & default targets *)
     let ar =
       Array.mapi
         (fun x off ->
           let v = Int32.add low (Int32.of_int x) in
           T_java_switch (Some v),  i + off
         ) offs
     in
     (T_java_switch None, i + def)::(Array.to_list ar)

  | OpLookupSwitch (def,pairs) ->
     (* switch cases & default targets *)
     let l =
       List.map
         (fun (v,off) ->
           T_java_switch (Some v),  i + off
         ) pairs
     in
     (T_java_switch None, i + def)::l
     
  | OpInvoke _ ->
     (* indirect jump, indirect return to next instruction *)
     [T_java_ret, succ]

  | OpInvalid ->
     (* no successor *)
     []
    

(** Use bytecode position as locations *)
let mk_jvm_loc (meth_uid:string) (pos:int) =
  mk_loc meth_uid pos 0

let mk_jvm_range (meth_uid:string) (pos1:int) (pos2:int) =
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
  (* add opcodes (edges) *)
  Array.iteri
    (fun i op ->
      if op <> OpInvalid then
        let range = mk_jvm_range meth_uid i ((opcode_end code i) + 1) in
        let src = [TCur, CFG.get_node g (mk_jvm_loc meth_uid i)] in
        let dst =
          List.map
            (fun (tag,p) -> tag, CFG.get_node g (mk_jvm_loc meth_uid p))
            (opcode_succ code i)
        in
        let stmt = mk_stmt (S_java_opcode op) range in
        ignore (CFG.add_edge g range ~src ~dst stmt)
    ) code;
  ()



(*========================================================================*)
                     (** {2 Loading} *)
(*========================================================================*)


let load_method
      (cls: j_class)
      (jmethod: jcode concrete_method)
    : j_method =
  (* method information *)
  let meth = {
      m_jmethod = jmethod;
      m_class = cls;
      m_uid = uid_of_class_method_signature jmethod.cm_class_method_signature;
      m_name = ms_name jmethod.cm_signature;
      m_args = ms_args jmethod.cm_signature;
      m_ret = ms_rtype jmethod.cm_signature;
      m_cfg = None;
    }
  in
  (* cfg construction *)
  (match jmethod.cm_implementation with
   | Native ->
      Printf.printf "    [native]\n"
   | Java j ->
      let jcode = Lazy.force j in
      let g = CFG.create () in
      fill_cfg meth.m_uid g jcode;
      meth.m_cfg <- Some g;

      let f = open_out (Printf.sprintf "tmp/%s.dot" meth.m_name) in
      let fmt = Format.formatter_of_out_channel f in
      CFG.print_dot
        fmt g meth.m_name
        (fun fmt n -> Format.fprintf fmt "%i:" (CFG.node_id n).loc_line)
        (fun fmt e -> pp_stmt fmt (CFG.edge_data e))
        (fun fmt t -> pp_token fmt t);
      Format.pp_print_flush fmt ();
      close_out f
  );
  meth

  
let load_class
      (class_path: class_path)
      (class_name: class_name)
    : j_class =
  let jclass = get_class class_path class_name in
  Printf.printf "class %s\n" (cn_name (get_name jclass));
  let fields = get_fields jclass in
  FieldMap.iter
    (fun sign field ->
      Printf.printf "  field %s\n" (JPrint.field_signature sign)
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
      Printf.printf "  method %s\n" (JPrint.method_signature sign);
      let mm = load_method cls m in
      cls.c_methods <- MapExt.StringMap.add mm.m_uid mm cls.c_methods
    )
    (get_concrete_methods jclass);
  
  cls
  

  
(*========================================================================*)
                       (** {2 Test} *)
(*========================================================================*)

   
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

let _ =
  Printf.printf "JVM test\n";
  load_class class_path java_lang_string
  

  

  
    
