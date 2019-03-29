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

(** The [Compose] operator implements the classic function composition between
    two stack domains *)

open Ast.All
open Core.All
open Sig.Abstraction
open Log


module Make (S1:STACK) (S2:STACK) : STACK =
struct

  type t = S1.t * S2.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.combiners.compose"
    end
    )

  let interface = Core.Interface.concat S1.interface S2.interface

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" S1.print a S2.print b

  let merge = assert false

  let bottom = S1.bottom, S2.bottom

  let top = S1.top, S2.top

  let is_bottom (a, b) =
    S1.is_bottom a || S2.is_bottom b


  module Make(Sub:ABSTRACTION) =
  struct
    module D = Apply.Make(S2)(Sub)
    module Sub1 = Sig.Abstraction.Make(D)
    module SI1 = S1.Make(Sub1)

    let subset ((a1,a2),s) ((a1',a2'),s') =
      let b, (_, s), (_,s') = SI1.subset (a1,(a2,s)) (a1',(a2',s')) in
      b,s,s'

    let join ((a1,a2),s) ((a1',a2'),s') =
      let a1, (a2, s), (a2',s') = SI1.join (a1,(a2,s)) (a1',(a2',s')) in
      (a1,a2),s,s'

    let meet ((a1,a2),s) ((a1',a2'),s') =
      let a1, (a2, s), (a2',s') = SI1.meet (a1,(a2,s)) (a1',(a2',s')) in
      (a1,a2),s,s'

    let widen ctx ((a1,a2),s) ((a1',a2'),s') =
      let a1, stable, (a2, s), (a2',s') = SI1.widen ctx (a1,(a2,s)) (a1',(a2',s')) in
      (a1,a2),stable,s,s'

    let s1_man (man:('a,S1.t*S2.t) man) : ('a,S1.t) man = {
      man with
      get = (fun a -> man.get a |> fst);
      set = (fun a1 a -> man.set (a, man.get a |> snd) a);
    }

    let s1_sman (man:('a,S1.t*S2.t) man) (sman:('a,Sub.t) stack_man) : ('a,S2.t*Sub.t) stack_man = {
      get_sub = (fun a -> (man.get a |> snd, sman.get_sub a));
      set_sub = (fun (a2,s) a -> sman.set_sub s a |> man.set (man.get a |> fst, a2));
      sub_exec = (fun ?(zone=any_zone) stmt flow ->
          Sub1.exec 
        );
    }

    let init prog man sman flow =
      SI1.init prog (s1_man man) (sman sman)

  end

end
