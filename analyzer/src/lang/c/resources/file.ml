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

     For the remaining ids, the domains keeps a partial map from
     allocated addresses to an interval over-approximating file
     ids. To improve precision, an under-approximation of the support
     of the map (i.e. set of addresses) is used.
  *)


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

  (* Availability info of a slot in the file table *)
  module FileSlot =
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
      | NotFree a -> Format.fprintf fmt "⚫ : %a" pp_set a
      | MaybeFree a -> Format.fprintf fmt "◐ : %a" pp_set a


  end

  (* Approximation of the file table *)
  module FileTable =
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

    let add addr itv a = {
      map = AddrItvMap.add addr itv a.map;
      support = AddrSet.add addr a.support;
    }

  end

  type slot = FileSlot.t
  type table = FileTable.t

  (* Precision window *)
  let window = 3

  type t = {
    first : slot list;
    others: table;
  }

  let init = {
    first = List.init window (fun _ -> FileSlot.Free);
    others = FileTable.empty;
  }

  let bottom = init

  let top = init

  let is_bottom _ = false


  let subset a1 a2 =
    List.for_all2 FileSlot.subset a1.first a2.first &&
    FileTable.subset a2.others a1.others

  let join annot a1 a2 = {
    first = List.map2 FileSlot.join a1.first a2.first;
    others = FileTable.join annot a1.others a2.others;
  }

  let meet annot a1 a2 = {
    first = List.map2 FileSlot.meet a1.first a2.first;
    others = FileTable.meet annot a1.others a2.others;
  }

  let widen annot a1 a2 = {
    first = List.map2 FileSlot.join a1.first a2.first;
    others = FileTable.widen annot a1.others a2.others;
  }


  let print fmt a =
    let rec pp_first i fmt l =
      match l with
      | [] -> ()
      | hd :: tl ->
        Format.fprintf fmt "%d ↦ %a,@\n%a"
          i FileSlot.print hd (pp_first (i + 1)) tl
    in
    Format.fprintf fmt "files: @[@[%a@]_ ↦ @[%a@]@]@\n"
      (pp_first 0) a.first
      FileTable.print a.others


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
      Z_c, Universal.Zone.Z_u_num;
      Universal.Zone.Z_u_heap, Z_any
    ]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =
    Some (Flow.set_domain_env T_cur init man flow)



  (** File management functions *)
  (** ========================= *)

  (** Insert an address in a slot. Returns the new state of the 
      slot after insertion, or its state when the insertion is 
      not possible. 
  *)
  let insert_slot addr (s:slot) : slot option * slot option =
    match s with
    | Bot         -> None, None
    | Free        -> Some (NotFree (AddrSet.singleton addr)), None
    | NotFree a   -> None, Some s
    | MaybeFree a ->
      Some (NotFree (AddrSet.add addr a)), Some (NotFree a)

  (* Iterate on first slot and try to insert the address *)
  let rec insert_first addr i (first:slot list) =
    match first with
    | [] -> [], []
    | hd :: tl ->
      match insert_slot addr hd with
      | Some hd', None ->
        [hd' :: tl], []

      | None, None ->
        [], []

      | None, Some hd' ->
        let after_inserted, after_no_place = insert_first addr (i + 1) tl in
        let no_place = List.map (fun tl' -> hd' :: tl') after_no_place in
        let inserted = List.map (fun tl' -> hd' :: tl') after_inserted in
        inserted, no_place

      | Some hd', Some hd'' ->
        let after_inserted, after_no_place = insert_first addr (i + 1) tl in
        let no_place = List.map (fun tl' -> hd'' :: tl') after_no_place in
        let inserted = List.map (fun tl' -> hd'' :: tl') after_inserted in
        (hd' :: tl) :: inserted, no_place

  (** Insert an address in the remaining part of the file table *)
  let insert_others addr (t:table) =
    (* Compute the interval of allocated ids *)
    let allocated = AddrItvMap.fold (fun _ -> Itv.join ()) t.map Itv.bottom in

    (* A sound solution is [window, b + 1], where [a, b] = allocated *)
    let l = Z.of_int window
    and u = Itv.bounds allocated |> snd
    in

    let sol0 = Itv.of_z l u in

    (* We can refine this solution by removing the ids of the minimal support *)
    let m = AddrItvMap.filter (fun addr _ -> AddrSet.mem addr t.support) t.map in
    let support = AddrItvMap.fold (fun _ itv acc -> itv :: acc) m [] in
    (* Sort intervals using the lowest bound *)
    let sorted = List.sort (fun itv1 itv2 ->
        let a1 = Itv.bounds itv2 |> fst in
        let a2 = Itv.bounds itv2 |> fst in
        Z.compare a1 a2
      ) support
    in
    let sol = List.fold_left (fun itv acc ->
        Itv.binop () O_minus acc itv |>
        Channel.without_channel
      ) sol0 sorted
    in
    FileTable.add addr sol t

  (** Insert an address in the abstract state and compute the interval of its ids *)
  let insert addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let inserted, no_place = insert_first addr 0 a.first in
    let annot = Flow.get_all_annot flow in
    let case1 =
      match inserted with
      | hd :: tl ->
        let first = List.fold_left (List.map2 FileSlot.join) hd tl in
        let a' = { a with first } in
        Flow.set_domain_env T_cur a' man flow

      | [] -> Flow.bottom annot
    in
    
    let case2 =
      match no_place with
      | hd :: tl ->
        let first = List.fold_left (List.map2 FileSlot.join) hd tl in
        let others = insert_others addr a.others in
        let a' = { first; others } in
        Flow.set_domain_env T_cur a' man flow

      | _ -> Flow.bottom annot
    in

    Flow.join man case1 case2

  let find_itv_first addr first =
    let rec iter i l =
      match l with
      | [] -> Itv.bottom
      | hd :: tl ->
        match hd with
        | FileSlot.Bot | Free -> iter (i + 1) tl
        | NotFree a | MaybeFree a ->
          let after = iter (i + 1) tl in
          if AddrSet.mem addr a then Itv.join () (Itv.of_int i i) after
          else after
    in
    iter 0 first

  let find_itv_others addr (others:table) = AddrItvMap.find addr others.map

  let find_itv addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let itv1 = find_itv_first addr a.first in
    let itv2 = find_itv_others addr a.others in
    Itv.join () itv1 itv2

  let find_addr_first itv first =
    let rec iter i l =
      match l with
      | [] -> AddrSet.empty
      | hd :: tl ->
        let after = iter (i + 1) tl in
        match hd with
        | FileSlot.Bot | Free -> after
        | NotFree a | MaybeFree a ->
          if Itv.mem (Z.of_int i) itv then
            AddrSet.union a after
          else
            after
    in
    iter 0 first

  let find_addr_others itv (others:table) =
    AddrItvMap.fold (fun addr itv' acc ->
        if Itv.is_bottom (Itv.meet () itv itv') then acc
        else AddrSet.add addr acc
      ) others.map AddrSet.empty

  let find_addr itv man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let a1 = find_addr_first itv a.first in
    let a2 = find_addr_others itv a.others in
    AddrSet.union a1 a2

  let remove_addr_first addr first =
    List.map (fun s ->
        match s with
        | FileSlot.Bot | Free -> s
        | NotFree a | MaybeFree a ->
          let a' = AddrSet.remove addr a in
          if AddrSet.is_empty a' then
            Free
          else
            MaybeFree a'
      ) first

  let remove_addr_others addr (others:table) : table = {
      map = AddrItvMap.remove addr others.map;
      support = AddrSet.remove addr others.support;
    }

  let remove_addr addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let first = remove_addr_first addr a.first in
    let others = remove_addr_others addr a.others in
    let a' = { first; others } in
    Flow.set_domain_env T_cur a' man flow

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_remove({ekind = E_addr ({ addr_kind = A_stub_resource "FileDescriptor"} as addr, _)}) ->
      remove_addr addr man flow |>
      Post.return

    | _ -> None

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new FileDescriptor ⟧ *)
    | E_alloc_addr(A_stub_resource "FileDescriptor") ->
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
      Eval.bind_return @@ fun exp' flow ->

      let addr, mode =
        match ekind exp' with
        | E_addr(addr, mode) -> addr, mode
        | _ -> assert false
      in

      let flow' = insert addr man flow in
      Eval.singleton exp' flow'

    (* 𝔼⟦ _mopsa_fd_to_int(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_fd_to_int", [fd]) ->
      man.eval ~zone:(Z_c, Z_c_points_to) fd flow |>
      Eval.bind_return @@ fun pt flow ->

      let addr =
        match ekind pt with
        | E_c_points_to (P_block (A (addr, mode), _)) -> addr
        | _ -> assert false
      in

      let itv = find_itv addr man flow in
      let l, u = Itv.bounds itv in
      let exp' = mk_z_interval l u exp.erange in

      Eval.singleton exp' flow

    (* 𝔼⟦ _mopsa_int_to_fd(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_int_to_fd", [id]) ->
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) id flow |>
      Eval.bind_return @@ fun id flow ->

      let itv = man.ask (Itv.Q_interval id) flow in
      let addrs = find_addr itv man flow in

      let evl = AddrSet.elements addrs |>
                List.map (fun addr ->
                    Eval.singleton (mk_addr addr exp.erange) flow
                  )
      in
      Eval.join_list evl

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Domain.register_domain (module Domain)
