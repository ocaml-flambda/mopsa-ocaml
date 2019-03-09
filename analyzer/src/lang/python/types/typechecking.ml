(* TODO: using union-find is probably useless, maybe arrays instead? *)
open Mopsa
open Addr
open Typing
open Addr_env
open Ast
open Universal.Ast
open Unionfind

type addr_kind +=
    A_py_var of int

let pyvarcounter = ref (-1)

let get_fresh_a_py_var () =
  incr pyvarcounter;
  !pyvarcounter

let greek_of_int =
  let list = ["α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο"; "π"; "ρ"; "σ"; "τ"; "υ"; "φ"; "χ"; "ψ"; "ω"] in
  let listn = List.length list in
  fun n ->
    let letter = List.nth list (n mod listn) in
    if n < listn then letter
    else Format.sprintf "%s(%d)" letter (n / listn)

let () =
  Format.(
    register_addr {
      print =
        (fun default fmt a -> match a with
           | A_py_var a -> Format.fprintf fmt "%s" (greek_of_int a)
           | _ -> default fmt a
        );
      compare =
        (fun default a1 a2 ->
           match a1, a2 with
           | A_py_var v1, A_py_var v2 -> Pervasives.compare v1 v2
           | _ -> default a1 a2);
    }
  )



module Domain =
struct

  module Iter = Framework.Domains.Iter.Make(Addr_env.Domain)(Typing.Domain)
  include Iter

  let name = "python.types.typechecking"
  let debug fmt = Debug.debug ~channel:name fmt

  module VarEls =
  struct
    type t = var
    let compare = compare_var
    let print = pp_var
  end

  module Uf = Unionfind(VarEls)

  let extract_type_aset (a: Addr_env.Domain.t) (t: Typing.Domain.t) (aset: Addr_env.Domain.ASet.t) : Typing.polytype option =
    if Addr_env.Domain.ASet.cardinal aset = 1 then
      let addr = Addr_env.Domain.ASet.choose aset in
      let addr = match addr with | Def a -> a | _ -> assert false in
      match addr.addr_kind with
        | A_py_instance _ ->
          let tys = Typing.Domain.TMap.find addr t.abs_heap in
          if Typing.Domain.Polytypeset.cardinal tys = 1 then
            Some (Typing.Domain.Polytypeset.choose tys)
          else
            None
        | A_py_class (c, b) ->
          let ty = Class (c, b) in Some ty
        | A_py_module m ->
          Some (Module m)
        | A_py_function f ->
          Some (Function f)
        | _ -> Exceptions.panic "%a@\n" pp_addr addr
    else
      None

  let extract_type (a: Addr_env.Domain.t) (t: Typing.Domain.t) (v: var) : Typing.polytype option =
    (* debug "extracting type of var %a in %a %a@\n" pp_var v Addr_env.Domain.print a Typing.Domain.print t; *)
    extract_type_aset a t (Addr_env.Domain.AMap.find v a)

  let sametype_vars (a: Addr_env.Domain.t) (t: Typing.Domain.t) : Uf.t =
    let module TypeMap = MapExt.Make(struct
        type t = Typing.polytype
        let compare = Typing.compare_polytype
        let print = Typing.pp_polytype
      end) in
    let map = TypeMap.empty in
    let map = Addr_env.Domain.AMap.fold (fun var aset acc ->
        let ty = extract_type_aset a t aset in
        match ty with
        | None -> acc
        | Some ty ->
          let old = if TypeMap.mem ty acc then TypeMap.find ty acc else [] in
          TypeMap.add ty (var::old) acc) a map in
    let domain = Addr_env.Domain.AMap.fold (fun var _ acc -> var::acc) a [] in
    let equivs = TypeMap.fold (fun ty vars acc -> vars :: acc) map [] in
    debug "uf = %a@\n" Uf.print_llist equivs;
    Uf.from_llist domain equivs


  let join annot ((hd, tl): t) ((hd', tl'): t) =
    debug "join %a %a and join %a %a@\n" Addr_env.Domain.print hd Addr_env.Domain.print hd' Typing.Domain.print tl Typing.Domain.print tl';
    let u1 = sametype_vars hd tl in
    let u2 = sametype_vars hd' tl' in
    let u = Uf.intersection u1 u2 in
    debug "u1 = %a@\nu2 = %a@\nu = %a@\n" Uf.print u1 Uf.print u2 Uf.print u;
    let toworkon = List.filter (fun l -> List.length l > 1) (Uf.to_llist u) in
    let jhd, jtl = Iter.join annot (hd, tl) (hd', tl') in
    let rhd, rabsheap = List.fold_left (fun (rhd, rabsheap) partition ->
        debug "partition = %a@\n" Uf.print_class partition;
        let v = get_fresh_a_py_var () in
        let addr = {addr_uid = v; addr_kind = A_py_var v; addr_mode = WEAK} in
        (* TODO: optimise types, we don't need that many add *)
        let types = List.fold_left (fun ptys var ->
            let ty1 = extract_type hd tl var in
            let ty2 = extract_type hd' tl' var in
            let ptys = match ty1 with
              | None -> ptys
              | Some ty -> Typing.Domain.Polytypeset.add ty ptys in
            match ty2 with
            | None -> ptys
            | Some ty -> Typing.Domain.Polytypeset.add ty ptys
          ) Typing.Domain.Polytypeset.empty partition in
        let rhd = List.fold_left (fun rhd var ->
            Addr_env.Domain.AMap.add var (Addr_env.Domain.ASet.singleton (Def addr)) rhd
          ) rhd partition in
        debug "types = %a@\n" Typing.Domain.Polytypeset.print types;
        let rtl = Typing.Domain.TMap.add addr types rabsheap in
        (rhd, rtl)
      ) (jhd, jtl.abs_heap) toworkon in
    let rtl = {Typing.Domain.abs_heap=rabsheap; Typing.Domain.typevar_env=jtl.typevar_env} in
    debug "result is %a@\n%a@\n" Addr_env.Domain.print rhd Typing.Domain.print rtl;
    rhd, rtl

end

let () =
  register_domain (module Domain)
