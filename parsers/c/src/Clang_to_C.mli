(**
  Clang_to_C - Translates Clang AST to C AST and link C AST.

 
  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.
  @author Antoine Miné
*)

type context
(** Abstract structure used internally during project parsing & linking. *)
    
val create_context: string -> Clang_AST.target_info -> context
(** [create_context project_name target] creates a new project (i.e., a program
    made of several translation units accumulated) with the given name and  
    target.
    Returns a context to manipulate the project.
 *)
                                                                          
val add_translation_unit: context -> string -> Clang_AST.decl -> Clang_AST.comment list -> Clang_AST.macro list -> unit
(** [add_translation_unit context name decl coms macros] converts a Clang definition
    of a translation unit with the given name (generally, the source C file)
    into a cAST and accumulates the definition to the projet.
 *)

val link_project: context -> C_AST.project
(** [link_project context] links all the translation units accumulated in the
    project and returns the consolidated definitions.
 *)
                                  

val dump_decls: bool ref
(** dump each C declarations found, for debugging *)

val log_rename: bool ref
(** log when renaming (or assign a name to an anonymous) *)

val log_merge: bool ref
(* log when merging declarations *)

val dump_dir: string ref
(** Log destination directory. *)

val simplify: bool ref
(* Whether to apply simplification *)                   

val new_uid: context -> C_AST.uid
                        
                     
