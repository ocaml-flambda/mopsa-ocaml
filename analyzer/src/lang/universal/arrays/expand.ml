(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Expansion abstraction of arrays with threshold.

    When an array is accessed, a limited number of cells are
    realized. Each realized cell is encoded with a distinct variable.
*)

open Framework.Essentials
open Ast


(** Array cell *)
(** ********** *)

module Cell =
struct

  type t = {
    array : var; (* base array variable *)
    index : Z.t list; (* index path *)
  }

  let compare c1 c2 =
    Compare.compose [
      (fun () -> compare_var c1.array c2.array);
      (fun () -> Compare.list_compare Z.compare c1.index c2.index)
    ]

  let print fmt c =
    Format.fprintf fmt "%a%a"
      pp_var c.array
      (Format.pp_print_list ~pp_sep:(fun fmt () -> ()) (fun fmt i -> Format.fprintf fmt "⟬%a⟭" Z.pp_print i)) c.index
end


(** Cell lval expression *)
(** ******************** *)

type expr_kind +=
  | E_cell_lval of var * mode * Z.t list option

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_cell_lval(a1, i1), E_cell_lval(a2, i2) ->
          Compare.compose [
            (fun () -> compare_var a1 a2);
            (fun () -> Compare.option_compare (Compare.list_compare Z.compare) i1 i2)
          ]
        | _ -> next e1 e2
      );
    print = (fun next fmt e ->
        match ekind e with
        | E_cell_lval(a, Some i) ->
          let c = Cell.{array = a; index = i} in
          Cell.print fmt c
        | E_cell_lval(a, None) ->
          Format.fprintf fmt "%a⦋?⦌" pp_var a
        | _ -> next fmt e
      );
    visit = (fun next e ->
        match ekind e with
        | E_cell_lval _ -> Visitor.leaf e
        | _ -> next e
      );
  }


(** Cell lval zone *)
(** ************** *)

type Framework.Zone.zone +=
  | Z_expand_cell_lval

let () =
  Framework.Zone.(register_zone {
      subset = (fun next z1 z2 ->
          match z1, z2 with
          | Z_expand_cell_lval, Zone.Z_universal -> true
          | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_expand_cell_lval -> Format.fprintf fmt "universal/expand-cell-lval"
          | _ -> next fmt z
        );
    })


(** Command line options *)
(** ******************** *)

let opt_threshold = ref 1
let () =
  register_option (
    "-array-expand-threshold",
    Arg.Set_int opt_threshold,
    " maximal number of array cells to expand"
  )


(** Abstract domain *)
(** *************** *)

module Domain : Framework.Domain.DOMAIN =
struct

  (** Lattice structure *)
  (** ***************** *)

  (* We keep the set of realized cells. Useful when an array
     modification affects a large number of cells; we just remove the
     previously realized cells of that array *)
  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom _ = false
  let widen = join
  let print fmt a =
    Format.fprintf fmt "expand:@\n  @[%a@]@\n" print a


  (** Domain identification *)
  (** ********************* *)

  type _ domain += D_universal_arrays_expand : t domain
  let id = D_universal_arrays_expand
  let name = "universal.arrays.expand"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_arrays_expand -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Initialization *)
  (** ************** *)

  let init prog man flow =
    Some (
      Flow.set_domain_env T_cur empty man flow
    )


  (** Transfer functions interface *)
  (** **************************** *)

  let exec_interface = {
    export = [Zone.Z_universal];
    import = [];
  }

  let eval_interface = {
    export = [
      (Framework.Zone.top, Zone.Z_universal_num);
      (Framework.Zone.top, Z_expand_cell_lval);
    ];
    import = [Framework.Zone.top, Z_expand_cell_lval];
  }


  (** Post-conditions *)
  (** *************** *)

  (* Iterate over the interval of the index [i]. *)
  let fold_cells i ~expand ~threshold init man flow =
    let open Numeric.Values.Intervals.Value in
    let itv = man.ask (Q_interval i) flow in
    if is_bounded itv then
      let a, b = bounds itv in
      let rec aux j a' acc =
        let k = Z.(a + (of_int j)) in
        if Z.(gt k b) then
          acc
        else
        if j = !opt_threshold then
          threshold acc (a', Some b)
        else
          expand acc k |>
          aux (j + 1) (Z.(a' + one))
      in
      aux 0 a init
    else
      threshold init (Z.zero, None)

  let mk_cell (v:var) (i:Z.t list): Cell.t =
    Cell.{array = v; index = i}

  let mk_cell_var (c:Cell.t) typ : var =
    let name =
      let () = Cell.print Format.str_formatter c in
      Format.flush_str_formatter ()
    in
    {vname = name; vuid = c.Cell.array.vuid; vtyp = typ}

  (* Check whether the last index in an index path is covered by a given range *)
  let index_in_interval (i:Z.t list) range : bool =
    match range with
    | None -> true
    | Some (i0, min, max) ->
      let rec aux i1 i2 =
        match i1, i2 with
        | [], b :: _ ->
          Z.geq b min &&
          (match max with
           | None -> true
           | Some max -> Z.leq b max)
        | a :: tl1, b :: tl2 ->
          Z.equal a b && (aux tl1 tl2)
        | _ -> assert false
      in
      aux i0 i

  (* Remove previously realized cells of array [a] *)
  let ignore_old_cells a i e range man flow =
    let env = Flow.get_domain_env T_cur man flow in
    fold (fun c acc ->
        if
          compare_var c.Cell.array a = 0 &&
          index_in_interval c.Cell.index i
        then
          let v = mk_cell_var c e.etyp in
          man.exec (mk_assign (mk_var v ~mode:WEAK range) e range) flow |>
          Flow.join man acc
        else
          acc
      ) env flow

  (* Compute the post-condition of <a:i0>[i] = e, where <a:i0> is the
     cell of array a indexed by path i0 *)
  let assign_array_cell a i0 i e range mode man flow =
    man.eval i flow |> Post.bind man @@ fun i flow ->
    let flow', cells = fold_cells i
        ~expand:(fun (acc, cells) i ->
            let c = mk_cell a (i0 @ [i]) in
            let v = mk_cell_var c e.etyp in
            let flow' = man.exec (mk_assign (mk_var v ~mode:(mode) range) e range) flow |>
                        Flow.map_domain_env T_cur (add c) man
            in
            Flow.join man acc flow', v :: cells
          )
        ~threshold:(fun (acc, cells) (min,max) ->
            ignore_old_cells a (Some (i0, min, max)) e range man acc, cells
          ) (Flow.bottom (Flow.get_all_annot flow), []) man flow
    in
    Post.of_flow flow' |>
    Post.add_mergers (List.map (fun c -> mk_remove_var c range) cells)

  (* Post-condition transfer function *)
  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_subscript(a, i)}, e) ->
      Some (
        man.eval e flow |> Post.bind man @@ fun e flow ->
        (* Evaluate a as an lval *)
        man.eval a flow ~zone:(Framework.Zone.top, Z_expand_cell_lval) |> Post.bind man @@ fun a flow ->
        match ekind a with
        | E_var(a, mode) ->
          (* a evaluated into a *)
          assign_array_cell a [] i e range mode man flow

        | E_cell_lval (a, Some i0) ->
          (* a evaluated into <a:i0> *)
          assign_array_cell a i0 i e range mode man flow

        | E_cell_lval (a, None) ->
          (* the number of cells when evaluating a is too large *)
          ignore_old_cells a None e range man flow |>
          Post.of_flow

        | _ -> panic_at (srange stmt) "Array evaluated to a non-lval expression"
      )

    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let mk_cell_lval a i typ range =
    mk_expr (E_cell_lval (a, i)) ~etyp:typ range

  let eval_array_lval_cell a i0 i typ range man flow =
    man.eval i flow |> Eval.bind @@ fun i flow ->
    let l = fold_cells i
        ~expand:(fun acc j ->
            let e = mk_cell_lval a (Some (i0 @ [j])) typ range in
            let flow' = man.exec (mk_assume (mk_binop i O_eq (mk_z j range) range) range) flow in
            Eval.singleton e flow' :: acc
          )
        ~threshold:(fun acc (min,max) ->
            let e = mk_cell_lval a None typ range in
            let flow' = man.exec (mk_assume (mk_binop i O_ge (mk_z min range) range) range) flow |>
                        (fun flow ->
                           match max with
                           | None -> flow
                           | Some max -> man.exec (mk_assume (mk_binop i O_le (mk_z max range) range) range) flow
                        )
            in
            Eval.singleton e flow' :: acc
          )
        [] man flow
    in
    Eval.join_list l

  let eval_array_cell a i0 i typ range man flow =
    eval_array_lval_cell a i0 i typ range man flow |> Eval.bind @@ fun e flow ->
    match ekind e with
    | E_cell_lval(a, Some i) ->
      let c = mk_cell a i in
      let v = mk_cell_var c typ in
      let e = mk_var v range in
      let flow' = Flow.map_domain_env T_cur (add c) man flow in
      Eval.singleton e flow'

    | E_cell_lval(a, None) ->
      Eval.singleton (mk_top typ range) flow

    | _ -> assert false

  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp, (snd zone) with
    | E_subscript(a, i), Z_expand_cell_lval ->
      Some (
        man.eval a flow ~zone:(Framework.Zone.top, Z_expand_cell_lval) |> Eval.bind @@ fun a flow ->
        match ekind a with
        | E_var a -> eval_array_lval_cell a [] i exp.etyp range man flow
        | E_cell_lval(a, Some i0) -> eval_array_lval_cell a i0 i exp.etyp range man flow
        | E_cell_lval(a, None) ->
          let e' = mk_cell_lval a None exp.etyp range in
          Eval.singleton e' flow
        | _ -> assert false
      )

    | E_subscript(a, i), Zone.Z_universal_num ->
      Some (
        man.eval a flow ~zone:(Framework.Zone.top, Z_expand_cell_lval) |> Eval.bind @@ fun a flow ->
        match ekind a with
        | E_cell_lval(a, Some i0) -> eval_array_cell a i0 i exp.etyp range man flow
        | E_cell_lval(a, None) -> Eval.singleton (mk_top exp.etyp range) flow
        | E_var a -> eval_array_cell a [] i exp.etyp range man flow
        | _ -> assert false
      )


    | _ -> None


  (** Queries *)
  (** ******* *)

  let ask query man flow = None

end

let () =
  Framework.Domain.register_domain (module Domain)
