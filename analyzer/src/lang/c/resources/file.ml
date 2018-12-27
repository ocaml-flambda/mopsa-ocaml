(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Manager of the `FileDescriptor` and `File` resources *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast
open Zone
open Mm.Common.Points_to
module Itv = Universal.Numeric.Values.Intervals.Value

module Domain =
struct

  (** Lattice definition *)
  (** ****************** *)

  (** Lattice definition *)
  (** ****************** *)

  (* UNIX file descriptors are associated to integer
     identifiers. These ids are generated in a particular way. As
     described in Single UNIX specification (Sec. 2.14), allocation
     should return the lowest number available.

     The File abstract domain maintains information about previously
     allocated file descriptors. Since the ids 0, 1 and 2 are used
     implicitly by many library functions, the domain tries to be more
     precise for those cases.

     More particularly, we associate to 0, 1 and 2 three possible
     states: Free, NotFree and MaybeFree.  For the two last states, we
     add the set of allocated heap addresses.

     For the remaining ids, the domains keeps a set of allocated
     addresses. Each address is annotated with the interval of
     possible ids. *)


  module Addr =
  struct
    type t = addr
    let compare = compare_addr
    let print = pp_addr
  end

  (** Set of addresses *)
  module AddrSet = SetExt.Make(Addr)

  let pp_set = AddrSet.fprint SetExt.printer_default pp_addr

  (** Map from addresses to intervals *)
  module AddrItvMap = Framework.Lattices.Partial_map.Make(Addr)(Itv)


  (* Precise file state *)
  module PreciseState =
  struct

    type t =
      | Bot
      | Free
      | NotFree of AddrSet.t
      | MaybeFree of AddrSet.t

    let subset s1 s2 =
      match s1, s2 with
      | Bot, _ -> true
      | _, Bot -> false

      | Free, Free
      | Free, MaybeFree _ -> true

      | NotFree a1, NotFree a2
      | NotFree a1, MaybeFree a2 -> AddrSet.subset a1 a2

      | MaybeFree a1, MaybeFree a2 -> AddrSet.subset a1 a2

      | _ -> false

    let join s1 s2 =
      match s1, s2 with
      | Bot, s
      | s, Bot -> s

      | Free, Free -> Free

      | Free, MaybeFree a
      | MaybeFree a, Free -> MaybeFree a

      | NotFree a1, NotFree a2 -> NotFree (AddrSet.union a1 a2)

      | NotFree a, Free
      | Free, NotFree a -> MaybeFree a

      | NotFree a1, MaybeFree a2
      | MaybeFree a1, NotFree a2 -> MaybeFree (AddrSet.union a1 a2)

      | MaybeFree a1, MaybeFree a2 -> MaybeFree (AddrSet.union a1 a2)

    let canonize s =
      match s with
      | MaybeFree a
      | NotFree a ->
        if AddrSet.is_empty a then Bot
        else s
      | _ -> s

    let meet s1 s2 =
      (
        match s1, s2 with
        | Bot, _
        | _, Bot -> Bot

        | Free, Free -> Free

        | Free, MaybeFree _
        | MaybeFree _, Free -> Free

        | NotFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2)

        | NotFree _, Free
        | Free, NotFree _ -> Bot

        | NotFree a1, MaybeFree a2
        | MaybeFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2)

        | MaybeFree a1, MaybeFree a2 -> MaybeFree (AddrSet.inter a1 a2)
      )
      |> canonize

    let print fmt s =
      match s with
      | Bot -> Format.fprintf fmt "⊥"
      | Free -> Format.fprintf fmt "⚪"
      | NotFree a -> Format.fprintf fmt "⚫:%a" pp_set a
      | MaybeFree a -> Format.fprintf fmt "◐:%a" pp_set a


  end

  (* Approximate file state *)
  module ApproxState =
  struct

    type t = {
      map: AddrItvMap.t; (* map from addresses to intervals *)
      support: AddrSet.t; (* under-approximation of the support of the map *)
    }

    let empty : t = {
      map = AddrItvMap.empty;
      support = AddrSet.empty;
    }

    let subset a1 a2 =
      AddrItvMap.subset a1.map a2.map &&
      AddrSet.subset a2.support a1.support

    let join annot a1 a2 = {
      map = AddrItvMap.join annot a1.map a2.map;
      support = AddrSet.inter a1.support a2.support;
    }

    let meet annot a1 a2 = {
      map = AddrItvMap.meet annot a1.map a2.map;
      support = AddrSet.union a1.support a2.support;
    }

    let widen annot a1 a2 = {
      map = AddrItvMap.widen annot a1.map a2.map;
      support = AddrSet.inter a1.support a2.support;
    }

    let print fmt a =
      Format.fprintf fmt "map: @[%a@]@\nsupport: @[%a@]"
        AddrItvMap.print a.map
        pp_set a.support

  end

  type precise = PreciseState.t
  type approx = ApproxState.t

  (* Precision window *)
  let window = 3

  type t = {
    first : precise list;
    others: approx;
  }

  let init = {
    first = List.init window (fun _ -> PreciseState.Free);
    others = ApproxState.empty;
  }

  let bottom = init

  let top = init

  let is_bottom _ = false


  let subset a1 a2 =
    List.for_all2 PreciseState.subset a1.first a2.first &&
    ApproxState.subset a2.others a1.others

  let join annot a1 a2 = {
    first = List.map2 PreciseState.join a1.first a2.first;
    others = ApproxState.join annot a1.others a2.others;
  }

  let meet annot a1 a2 = {
    first = List.map2 PreciseState.meet a1.first a2.first;
    others = ApproxState.meet annot a1.others a2.others;
  }

  let widen annot a1 a2 = {
    first = List.map2 PreciseState.join a1.first a2.first;
    others = ApproxState.widen annot a1.others a2.others;
  }


  let print fmt a =
    let rec pp_first i fmt l =
      match l with
      | [] -> ()
      | hd :: tl ->
        Format.fprintf fmt "%d ↦ %a,@\n%a"
          i PreciseState.print hd (pp_first (i + 1)) tl
    in
    Format.fprintf fmt "files: @[%a@\n_ ↦ @[%a@]@]@\n"
      (pp_first 0) a.first
      ApproxState.print a.others


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_resources_file : t domain

  let id = D_c_resources_file
  let name = "c.resources.file"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_resources_file -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [Z_c];
    import = []
  }

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [
      Z_c, Z_c_points_to;
      Universal.Zone.Z_u_heap, Z_any
    ]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =
    Some (Flow.set_domain_env T_cur init man flow)



  (** File management functions *)
  (** ========================= *)

  (* Insert an address in a precise location. Returns the new state
     after insertion and a boolean for stopping insertion *)
  let insert_precise_at addr (s:precise) : precise option * precise option =
    match s with
    | Bot         -> None, None
    | Free        -> Some (NotFree (AddrSet.singleton addr)), None
    | NotFree a   -> None, Some s
    | MaybeFree a ->
      Some (NotFree (AddrSet.add addr a)), Some (NotFree a)

  (* Iterate on precise locations and try to insert the address *)
  let rec iter_insert_precise addr i (s:precise list) =
    match s with
    | [] -> [], false
    | hd :: tl ->
      match insert_precise_at addr hd with
      | Some hd', None ->
        [hd' :: tl, Itv.of_int i i], true

      | None, None ->
        [], true

      | None, Some hd' ->
        let after, stop = iter_insert_precise addr (i + 1) tl in
        let l = List.map (fun (tl', itv) -> hd' :: tl', itv) after in
        l, stop

      | Some hd', Some hd'' ->
        let after, stop = iter_insert_precise addr (i + 1) tl in
        let l = List.map (fun (tl', itv) -> hd'' :: tl', itv) after in
        (hd' :: tl, Itv.of_int i i) :: l, stop

  let insert_approx addr (s:approx) : approx =
    assert false

  let insert addr range man flow = assert false
    (* let a = Flow.get_domain_env T_cur man flow in
     * let cases, stop = iter_insert_precise addr 0 a.first in
     * 
     * let evl = List.map (fun (first, itv) ->
     *     let a' = { a with first } in
     *     let flow' = Flow.set_domain_env T_cur a' man flow in
     *     Eval.singleton (mk_interval itv range) flow'
     *   ) cases
     * in
     * 
     * if stop then
     *   Eval.join_list evl
     * else
     *   let cases = insert_approx addr a.approx in
     *   let evl = List.map (fun (approx, itv) ->
     *       let a' = { a with approx } in
     *       let flow' = Flow.set_domain_env T_cur a' man flow in
     *       Eval.singleton (mk_interval itv range) flow'
     *     ) cases
     *   in *)





  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | _ -> None

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new FileDescriptor ⟧ *)
    | E_alloc_addr(A_stub_resource "FileDescriptor") ->
      panic_at exp.erange "new FileDescriptor not implemeneted"

    (* 𝔼⟦ _mopsa_fd_to_int(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_fd_to_int", [fd]) ->
      panic_at exp.erange "_mopsa_fd_to_int not implemented"

    (* 𝔼⟦ _mopsa_int_to_fd(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_int_to_fd", [fd]) ->
      panic_at exp.erange "_mopsa_int_to_fd not implemented"

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Domain.register_domain (module Domain)
