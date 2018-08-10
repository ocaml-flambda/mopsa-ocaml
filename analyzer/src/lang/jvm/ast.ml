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
   | T_java_true  (* jump if test is true *)
   | T_java_jsr   (* subroutine jump *)
   | T_java_exn   (* exception *)

             

(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


(** Method code is specified as generic CFG with opcodes on edges.
    We use Javalib jopcodes as opcodes.
 *)
type stmt_kind +=
   | S_java of jopcode



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
    m_cfg: cfg; (** control-flow graph *)
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

   



