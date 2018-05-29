(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Statement memoisation *)

open Framework.Lattice
open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateful
open Framework.Domains.Stateless
open Framework.Exec
open Framework.Ast
open Framework.Pp
open Ast


let name = "universal.flows.memoisation"

let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                      {2 New statement definition}                       *)
(*==========================================================================*)

type stmt_kind +=
  | S_memoisation of stmt

let () =
  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_memoisation s ->
        Format.fprintf fmt "memo(%a)" pp_stmt s
      | _ -> default fmt stmt
    )

let max_memo = ref 10

(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain(S : Framework.Domains.Stateful.DOMAIN) =
struct

  type io = S.t Framework.Flow.flow * S.t Framework.Flow.flow

  let print_io fmt (i, o) =
    Format.fprintf fmt "@[<v>I: %a@,O: %a@]"
      (Framework.Flow.print S.print) i
      (Framework.Flow.print S.print) o

  type value = io list

  let print_value fmt v =
    Format.fprintf fmt "@[<v>[@,%a@,]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
         print_io
      ) v

  module K =
  struct
    type t = Framework.Context.context * Framework.Ast.stmt
    let compare (c,s) (c',s') =
      compare_composer
        [
          (fun () -> compare c c');
          (fun () -> compare s s')
        ]
    let print fmt (c,s) =
      Format.fprintf fmt "@[ctx:@\nstmt: %a@\n@]"
        Framework.Pp.pp_stmt s
  end

  module M = MapExt.Make(K)

  let print_m fmt m =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
         (fun fmt ((ctx, stmt), iol) -> Format.fprintf fmt "%a ↦ %a" Framework.Pp.pp_stmt stmt print_value iol)
      ) (M.bindings m)

  type t =
    { a : S.t;
      m : value M.t
    }
  let print fmt x =
    Format.fprintf fmt "@[<v>%a@,@,{memo: %a}@]"
      S.print x.a
      print_m x.m

  let loc_flow_leq f f' =
    Top.top_included ( fun f f' ->
        Framework.Flow.Map.for_all (fun k a ->
            try
              S.leq a (Framework.Flow.Map.find k f')
            with
            | Not_found -> false
          ) f
      ) f f'

  let merger _ v v' = match v, v' with
    | None, None -> v
    | Some _, None -> v
    | None, Some _ -> v'
    | Some v, Some v' ->
      Some
        (
          List.fold_left (fun (asize,acc) (i,o) ->
              if ((asize > !max_memo) || List.exists (fun (i',_) ->
                  loc_flow_leq i i'
                ) v') then (asize, acc) else (asize+1, (i,o)::acc)
            ) (List.length v', v') v |> snd
        )

  let merge_memo a b =
    M.merge merger a b

  let build_subman (man : ('a, t) Framework.Manager.manager) =
    {man with
     ax =
       {
         get = (fun cur -> (man.ax.get cur).a);
         set = (fun a cur ->
             let l' = man.ax.get cur in
             man.ax.set {l' with a = a} cur)
       }
    }

  let bottom    = {a = S.bottom; m = M.empty}
  let top       = {a = S.top; m = M.empty}
  let is_bottom x = S.is_bottom x.a
  let is_top    x = S.is_top x.a
  let leq x y = S.leq x.a y.a
  let join x y  = {a = S.join x.a y.a; m = M.merge merger x.m y.m}
  let meet x y  = {a = S.meet x.a y.a; m = M.merge merger x.m y.m}
  let widening u x y  = {a = S.widening u x.a y.a; m = M.merge merger x.m y.m}
  let init man ctx prg flow =
    S.init (build_subman man) ctx prg flow

  let get_cur_f man flow =
    Top.top_lift1 (fun f ->
        Framework.Flow.Map.map (fun v ->
            (man.ax.get v).a
          ) f
      ) flow

  let set_cur_f man memo lflow flow =
    Top.top_lift1 (fun lf ->
        Framework.Flow.Map.mapi (fun k v ->
            let extension =
              match flow with
              | Top.TOP -> man.env.top
              | Top.Nt f ->
                try
                  Framework.Flow.Map.find k f
                with
                | Not_found -> man.env.bottom
            in
            man.ax.set {a = v ; m = memo} extension
          ) lf
      ) lflow

  let get_memo man flow =
    (man.flow.get TCur flow
     |> man.ax.get).m

  let find_applicable ctx stmt (m : value M.t) a : io option =
    try
      let l = M.find (ctx, stmt) m in
      Some (List.find (fun (i, o) ->
          loc_flow_leq a i
        ) l)
    with
    | Not_found -> None

  let memo_size ctx stmt m =
    try
      let l = M.find (ctx, stmt) m in
      List.length l
    with Not_found -> 0

  let add_to_memo ctx stmt (io : io) (m : value M.t) =
    let old =
      try M.find (ctx, stmt) m
      with | Not_found -> [] in
    M.add (ctx, stmt) (io::old) m

  let recover_memo_from_current man f =
    match f with
    | Top.TOP -> M.empty
    | Top.Nt f -> Framework.Flow.Map.fold (fun _ x acc ->
        merge_memo ((man.ax.get x).m) acc
      ) f M.empty

  let remove_all_memo_from_flow man f =
    man.flow.map (fun e _ ->
        let l = man.ax.get e in
        man.ax.set {l with m = M.empty} e
      ) f

  let set_memo_to_all_flow man f m =
    man.flow.map
      (fun e _ ->
         let l = man.ax.get e in
         let e' = man.ax.set {l with m = m} e in
         let () = debug "resulting env: %a" man.env.print e' in
         e'
      ) f

  let exec man ctx stmt flow =
    match skind stmt with
    | S_memoisation stmt ->
      begin
        let m = get_memo man flow in
        let i = get_cur_f man flow in
        match find_applicable ctx stmt m i with
        | None ->
          let () = debug "could not find an applicable IO" in
          if memo_size ctx stmt m < (!max_memo) then

            S.exec (build_subman man) ctx stmt flow
            |> oflow_compose (fun f' ->
                let ()  = debug "sub exec done: %a" man.flow.print f' in
                let m'  = recover_memo_from_current man f' in
                let o   = get_cur_f man f' in
                let m'' = merge_memo m' (M.singleton (ctx, stmt) [(i,o)]) in
                let ()  = debug "resulting memo: %a" print_m m'' in
                let f'' = remove_all_memo_from_flow man f' in
                let rep = set_memo_to_all_flow man f'' m'' in
                let ()  = debug "resulting: %a" man.flow.print rep in
                rep
              )
          else
            S.exec (build_subman man) ctx stmt flow
        | Some (i,o) ->
          let () = debug "found an applicable IO : %a" print_io (i,o) in
          return (set_cur_f man m o flow)
      end
    | _ -> S.exec (build_subman man) ctx stmt flow

  let eval man ctx exp flow =
    S.eval (build_subman man) ctx exp flow

  let ask man ctx query flow =
    S.ask (build_subman man) ctx query flow
end

let setup () =
  Framework.Domains.Fun.register_domain name (module Domain);
  Framework.Options.register (
    "-memoization-size", Arg.Set_int max_memo , " number of I/O relations kept per context and statement: 1)"
  )
