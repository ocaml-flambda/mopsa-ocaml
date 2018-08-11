(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Bytecode representation.
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

   
(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Use Javalib types. *)
type typ +=
   | T_java of value_type



             
(*==========================================================================*)
                           (** {2 Flows} *)
(*==========================================================================*)

             
(** Non-direct control *)
type token +=
   | T_java_iftrue                  (** jump if test is true *)
   | T_java_jsr                     (** subroutine jump *)
   | T_java_ret                     (** return point *)
   | T_java_exn                     (** exception *)
   | T_java_switch of int32 option  (** switch (None for default) *)

             

(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


(** Method code is specified as generic CFG with opcodes on edges.
    We use Javalib jopcodes as opcodes.
 *)
type stmt_kind +=
   | S_java_opcode of jopcode



(*==========================================================================*)
                           (** {2 Programs} *)
(*==========================================================================*)


type j_method = {
    m_jmethod: jcode concrete_method; (** Javalib data *)
    m_class: j_class; (** class defining the method *)
    m_uid: string; (** unique name: contains the class name, method name and signature *)
    m_name: string; (** name *)
    m_args: value_type list; (** type of arguments *)
    m_ret: value_type option; (** returned type (or None for void) *)
    mutable m_cfg: cfg option; (** control-flow graph, None for native methods *)
  }

and j_class = {
    c_jclass: jcode interface_or_class; (** Javalib data *)
    c_uid: string; (** unique name: fully qualified class name *)
    c_name: string; (** class name without package *)
    c_package: string list; (** package *)    
    mutable c_methods: j_method MapExt.StringMap.t; (** concrete methods *)
  }

type j_program = {
    mutable p_classes: j_class MapExt.StringMap.t;
  }
            
type program_kind +=
   | Java_program of j_program

   
                   
(*==========================================================================*)
                       (** {2 Register new types} *)
(*==========================================================================*)


let () =
  register_pp_stmt
    (fun next fmt stmt ->
      match stmt.skind with
      | S_java_opcode op -> Format.pp_print_string fmt (JPrint.jopcode op)
      | _ -> next fmt stmt
    );
  register_pp_typ
    (fun next fmt typ ->
      match typ with
      | T_java t -> Format.pp_print_string fmt (JPrint.value_type t)
      | _ -> next fmt typ
    );
  register_pp_token
    (fun next fmt token ->
      match token with
      | T_java_iftrue -> Format.pp_print_string fmt "true"
      | T_java_jsr -> Format.pp_print_string fmt "jsr"
      | T_java_ret -> Format.pp_print_string fmt "ret"
      | T_java_exn -> Format.pp_print_string fmt "exn"
      | T_java_switch None -> Format.pp_print_string fmt "default"
      | T_java_switch (Some i) -> Format.fprintf fmt "case %li" i
      | _ -> next fmt token
    );
  (* TODO: register_typ_compare, register_token_compare *)

           
