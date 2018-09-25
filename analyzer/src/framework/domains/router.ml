(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Routers compose transfer functions for creating new zoning
    paths not provided by abstract domains. *)

open Manager
open Domain
open Zone

type entry = {
  src : zone;
  dst : zone;
  path : zone list;
}

let pp_entry fmt e =
  Format.fprintf fmt "src: %a, dst: %a, path: %a"
    pp_zone e.src
    pp_zone e.dst
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " -> ") pp_zone) e.path

module type S =
sig
  val name : string
  val table : entry list
end

module Make(Router: S) : Domain.DOMAIN =
struct

  include Empty

  let name = Router.name

  let get_hops e =
    let last, hops = List.fold_left (fun (pred, hops) next ->
        next, (pred, next) :: hops
      ) (e.src, []) e.path
    in
    List.rev ((last, e.dst) :: hops)

  let eval_interface =
    let export, import =
      List.fold_left (fun (export, import) e ->
          (e.src, e.dst) :: export, (get_hops e) @ import
        ) ([], []) Router.table
    in
    {export; import}

  let eval (z1, z2) exp man flow =
    let entries = List.find_all (function {src; dst} -> sat_zone2 (src, dst) (z1, z2)) Router.table in
    let rec aux = function
      | [] -> None
      | entry :: tl ->
        let hops = get_hops entry in
        let rec aux2 hops exp flow =
          match hops with
          | [] -> Some (Eval.singleton exp flow)
          | zone :: tl ->
            man.eval_opt ~zone exp flow |>
            Option.bind @@
            Eval.bind_opt @@ aux2 tl
        in
        match aux2 hops exp flow with
        | None -> aux tl
        | Some ret -> Some ret
    in
    aux entries


end

let register_router r =
  let module R = (val r : S) in
  let module D = Make(R) in
  Domain.register_domain (module D : DOMAIN)
