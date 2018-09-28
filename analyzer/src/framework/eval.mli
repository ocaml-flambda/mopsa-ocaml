(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Manager

val empty : ('a, 'e) evl

val case : ('a, 'e) evl_case -> ('a, 'e) evl

val singleton : 'e -> ?cleaners:Ast.stmt list -> 'a flow -> ('a, 'e) evl

val empty_singleton : 'a flow -> ('a, 'e) evl

val join : ('a, 'e) evl  -> ('a, 'e) evl  -> ('a, 'e) evl

val join_list : ('a, 'e) evl list -> ('a, 'e) evl

val meet : ('a, 'e) evl  -> ('a, 'e) evl  -> ('a, 'e) evl

val meet_list : ('a, 'e) evl list -> ('a, 'e) evl

val add_cleaners : Ast.stmt list -> ('a, 'e) evl  -> ('a, 'e) evl

val fold :
    ('b -> ('a, 'e) evl_case -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b -> 'b -> 'b) ->
    'b -> ('a, 'e) evl -> 'b

val iter : (('a, 'e) evl_case -> unit) -> ('a, 'e) evl -> unit

val bind : ('e -> 'a flow -> ('a, 'f) evl ) -> ('a, 'e) evl -> ('a, 'f) evl

val bind_opt : ('e -> 'a flow -> ('a, 'f) evl option) -> ('a, 'e) evl -> ('a, 'f) evl option

val eval_list : 'e list -> ('e -> 'a flow -> ('a, 'c) evl) -> 'a flow ->  ('a, 'c list) evl

val eval_list_opt : 'e list -> ('e -> 'a flow -> ('a, 'c) evl option) -> 'a flow ->  ('a, 'c list) evl option

val assume :
  Ast.expr -> ?zone:Zone.zone ->
  fthen:('a flow -> ('a, 'e) evl ) ->
  felse:('a flow -> ('a, 'e) evl ) ->
  ?fboth:('a flow -> 'a flow -> ('a, 'e) evl ) ->
  ?fnone:('a flow -> ('a, 'e) evl) ->
  ('a, 'b) man -> 'a flow ->
  ('a, 'e) evl

val switch :
  ((Ast.expr * bool) list * ('a flow -> ('a, 'e) evl )) list ->
  ?zone:Zone.zone ->
  ('a, 'b) Manager.man -> 'a flow ->
  ('a, 'e) evl

val print: pp:(Format.formatter -> 'e -> unit) -> Format.formatter -> ('a, 'e) evl -> unit

val map:
  (('a, 'e) evl_case -> ('a, 'f) evl_case) ->
  ('a, 'e) evl -> ('a, 'f) evl

val choose : ('a, 'e) evl -> ('e option * 'a flow) option

val to_dnf : ('a, 'e) evl -> ('a, 'e) evl_case Dnf.t
