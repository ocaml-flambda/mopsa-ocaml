(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Utility functions for decorating values with reduction channels. *)

open Manager

(** Decorator type of values with a list of reduction channels *)
type 'a with_channel = {
  value : 'a;
  channels : Post.channel list;
}

(** Decorate a value with a list of channels *)
let return ?(channels = []) a = {
  value = a;
  channels;
}

let without_channel (a: 'a with_channel) : 'a =
  a.value

(** Apply a decorated function on a value *)
let bind (f: 'a -> 'b with_channel) (a: 'a with_channel) : 'b with_channel =
  let b = f a.value in
  {b with channels = a.channels @ b.channels}

(** Apply a decorated function on a token flow and return the modified flow and the resulting channels *)
let map_domain_env (tk:token) (f:'t -> 't with_channel) (man:('a, 't) man) (flow:'a flow) : 'a flow * Post.channel list=
  let a = f (Flow.get_domain_env tk man flow) in
  Flow.set_domain_env tk a.value man flow, a.channels
