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

(** Extended domains signatures used by combiners *)

open Ast.All
open Core.All

type combiner =
  | Sequence
  | Compose
  | Product

type _ id +=
  | I_empty : unit id
  | I_binary : combiner * 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | I_empty, I_empty -> Some Eq
        | I_binary(c1,x1,y1), I_binary(c2,x2,y2) ->
          if c1 = c2 then
            match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          else
            None
        | _ -> next.eq id1 id2
      in
      f
    );
  }


module type STACKED =
sig
  include Sig.Domain.Stacked.STACKED
  val leaves : string list
  val exec : string list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : string list -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : string list -> 'r query -> ('a,t) man -> 'a flow -> 'r option
end

module type DOMAIN =
sig
  include Sig.Domain.Standard.DOMAIN
  val leaves : string list
  val exec : string list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : string list -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : string list -> 'r query -> ('a,t) man -> 'a flow -> 'r option
end

module type SIMPLIFIED =
sig
  include Sig.Domain.Simplified.SIMPLIFIED
  val leaves : string list
  val exec : string list -> stmt -> t Sig.Domain.Simplified.simplified_man -> uctx -> t -> t option
  val ask  : string list -> 'r query -> t Sig.Domain.Simplified.simplified_man -> uctx -> t -> 'r option
end

module type STATELESS =
sig
  include Sig.Domain.Stateless.STATELESS
  val leaves : string list
  val exec : string list -> stmt -> ('a,unit) man -> 'a flow -> 'a post option
  val eval : string list -> expr -> ('a,unit) man -> 'a flow -> 'a eval option
  val ask  : string list -> 'r query -> ('a,unit) man -> 'a flow -> 'r option
end



module FromStacked(D:Sig.Domain.Stacked.STACKED) : STACKED with type t = D.t =
struct
  include D
  let leaves = [D.name]
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
end

module ToStacked(T:STACKED) : Sig.Domain.Stacked.STACKED with type t = T.t =
struct
  include T
  let name = ""
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
end


module FromDomain(D:Sig.Domain.Standard.DOMAIN) : DOMAIN with type t = D.t =
struct
  include D
  let leaves = [D.name]
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
end

module ToDomain(T:DOMAIN) : Sig.Domain.Standard.DOMAIN with type t = T.t =
struct
  include T
  let name = ""
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
end


module FromStateless(D:Sig.Domain.Stateless.STATELESS) =
struct
  include D
  let leaves = [D.name]
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
end

module ToStateless(T:STATELESS) : Sig.Domain.Stateless.STATELESS =
struct
  include T
  let name = ""
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
end


module FromSimplified(D:Sig.Domain.Simplified.SIMPLIFIED) : SIMPLIFIED with type t = D.t =
struct
  include D
  let leaves = [D.name]
  let exec targets = D.exec
  let ask targets  = D.ask
end

module ToSimplified(T:SIMPLIFIED) : Sig.Domain.Simplified.SIMPLIFIED with type t = T.t =
struct
  include T
  let name = ""
  let exec stmt man ctx a = T.exec [] stmt man ctx a
  let ask query man ctx a = T.ask [] query man ctx a
end

module StackedFunctor(F:Sig.Functor.Stacked.STACKED_FUNCTOR)(D:STACKED) : STACKED =
struct
  include FromStacked(F.Functor(ToStacked(D)))
end


module StandardFunctor(F:Sig.Functor.Standard.DOMAIN_FUNCTOR)(D:DOMAIN) : DOMAIN =
struct
  include FromDomain(F.Functor(ToDomain(D)))
end


module SimplifiedFunctor(F:Sig.Functor.Simplified.SIMPLIFIED_FUNCTOR)(D:SIMPLIFIED) : SIMPLIFIED =
struct
  include FromSimplified(F.Functor(ToSimplified(D)))
end


module StandardToStacked(D:DOMAIN) : STACKED with type t = D.t =
struct

  include D

  let subset man sman ctx (a,s) (a',s') =
    if a == a' then true, s, s' else
    D.subset a a', s, s'

  let join man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.join a a', s, s'

  let meet man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.meet a a', s, s'

  let widen man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s', true else
    D.widen ctx a a', s, s', true

end



module StatelessToDomain(S:STATELESS) : DOMAIN with type t = unit =
struct

  include S

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom () = false
  let merge _ _ _ = ()
  let print _ _ = ()

  let subset () () = true
  let join () () = ()
  let meet () () = ()
  let widen _ () () = ()

end



module SimplifiedToStandard(D: SIMPLIFIED) : DOMAIN with type t = D.t =
struct

  include D

  let merge pre (post1, log1) (post2, log2) =
    let stmts1 = Log.get_log_stmts log1
    and stmts2 = Log.get_log_stmts log2 in
    D.merge pre (post1, stmts1) (post2, stmts2)


  let init prog man flow =
    let a' = D.init prog in
    set_env T_cur a' man flow


  let dependencies = [] (* Leaf domains have no dependency *)

  let alarms = []

  let simplified_man man flow : t Sig.Domain.Simplified.simplified_man ={
    exec = (fun stmt -> man.Core.Manager.exec stmt flow |>
                        get_env T_cur man
           );
    ask = (fun query -> man.Core.Manager.ask query flow);
  }

  let exec targets stmt man flow =
    let a = get_env T_cur man flow in
    if D.is_bottom a
    then
      Post.return flow |>
      OptionExt.return
    else
      D.exec targets stmt (simplified_man man flow) (Flow.get_unit_ctx flow) a |>
      OptionExt.lift @@ fun a' ->
      set_env T_cur a' man flow |>
      Post.return |>
      Cases.map_log (fun log ->
          man.set_log (
            man.get_log log |> Log.add_stmt_to_log stmt
          ) log
        )

  let eval targets exp man flow = None

  let ask targets query man flow =
    D.ask targets query (simplified_man man flow) (Flow.get_unit_ctx flow) (get_env T_cur man flow)

end


let sat_targets ~targets ~leaves =
  targets = [] || List.exists (fun t -> List.mem t leaves) targets



(**************************************************************************)
(**                           {2 Managers}                                *)
(**************************************************************************)

(** Manager of the left argument in a compose topology *)
let fst_pair_man (man:('a, 'b * 'c) man) : ('a, 'b) man = {
  man with
  get = get_pair_fst man;
  set = set_pair_fst man;
  get_log = (fun glog -> man.get_log glog |> Log.get_left_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] log (man.get_log glog |> Log.get_right_log)
    ) glog);
}

(** Manager of the right argument in a compose topology *)
let snd_pair_man (man:('a, 'b * 'c) man) : ('a, 'c) man = {
  man with
  get = get_pair_snd man;
  set = set_pair_snd man;
  get_log = (fun glog -> man.get_log glog |> Log.get_right_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] (man.get_log glog |> Log.get_left_log) log
    ) glog);
}


let rec find_domain_man : type b c. target:b id -> tree:c id -> ('a,c) man -> ('a,b) man = fun ~target ~tree man ->
  match tree with
  | I_empty -> raise Not_found
  | I_binary(_,left,right) ->
    begin
      try find_domain_man target left (fst_pair_man man)
      with Not_found -> find_domain_man target right (snd_pair_man man)
    end
  | _ ->
    match equal_id target tree with
    | Some Eq -> man
    | None -> raise Not_found

let rec mem_domain : type b c. target:b id -> tree:c id -> bool = fun ~target ~tree ->
  match tree with
  | I_empty -> false
  | I_binary(_,left,right) -> mem_domain target left || mem_domain target right
  | _ ->
    match equal_id target tree with
    | Some Eq -> true
    | None -> false
