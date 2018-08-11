(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural iterator handles blocks, assignments and tests *)

open Framework.Essentials
open Ast


(** Array cell *)
(** ********** *)

module Cell =
struct
  type t = {
    array : var;
    index : Z.t;
  }

  let compare c1 c2 =
    Compare.compose [
      (fun () -> compare_var c1.array c2.array);
      (fun () -> Z.compare c1.index c2.index)
    ]

  let print fmt c =
    Format.fprintf fmt "%a⦋%a⦌" pp_var c.array Z.pp_print c.index
end


(** Command line options *)
(** ******************** *)

let opt_threshold = ref 1
let () =
  register_option (
    "-expand-threshold",
    Arg.Set_int opt_threshold,
    " maximal number of array cells to expand"
  )


(** Abstract domain *)
(** *************** *)

module Domain : Framework.Domain.DOMAIN =
struct

  (** Lattice structure *)
  (** ***************** *)

  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom a = false
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
      Flow.set_env T_cur empty man flow
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
      (Framework.Zone.top, Zone.Z_universal_lval);
    ];
    import = [Framework.Zone.top, Zone.Z_universal_lval];
  }


  (** Post-conditions *)
  (** *************** *)

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

  let mk_cell (v:var) (i:Z.t) : Cell.t =
    Cell.{array = v; index = i}
  
  let mk_cell_var (c:Cell.t) : var =
    let name =
      let () = Cell.print Format.str_formatter c in
      Format.flush_str_formatter ()
    in
    let typ =
      match c.Cell.array.vtyp with
      | T_array t -> t
      | _ -> assert false
    in
    {vname = name; vuid = c.Cell.array.vuid; vtyp = typ}
  
  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_subscript(a, i)}, e, mode) ->
      Some (
        man.eval e flow |> Post.bind man @@ fun e flow ->
        man.eval i flow |> Post.bind man @@ fun i flow ->
        man.eval a flow ~zone:(Framework.Zone.top, Zone.Z_universal_lval) |> Post.bind man @@ fun a flow ->
        match ekind a with
        | E_var a ->
          let flow' = fold_cells i
              ~expand:(fun acc i ->
                  let c = mk_cell a i in
                  let v = mk_cell_var c in
                  let flow' = man.exec (mk_assign (mk_var v range) e range) flow |>
                              Flow.map_env T_cur (add c) man
                  in
                  Flow.join man acc flow'
                )
              ~threshold:(fun acc (min, max) ->
                  let env = Flow.get_env T_cur man flow in
                  let in_interval i =
                    Z.geq i min &&
                    (match max with
                     | None -> true
                     | Some max -> Z.leq i max)
                  in
                  fold (fun c acc ->
                      if compare_var c.Cell.array a = 0 && in_interval c.Cell.index
                      then
                        let v = mk_cell_var c in
                        debug "weak update on %a, (%a, %a)" Cell.print c Z.pp_print min (Option.print (Z.pp_print)) max;
                        man.exec (mk_assign (mk_var v range) e ~mode:WEAK range) flow |>
                        Flow.join man acc
                      else
                        acc
                    ) env acc
                ) (Flow.bottom (get_annot flow)) man flow
          in
          Post.of_flow flow'
          
        | _ -> panic_at (srange stmt) "Array evaluated to a non-lval expression"
      )
        
    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp, (snd zone) with
    | E_subscript(a, i), Zone.Z_universal_num -> assert false
    | E_subscript(a, i), Zone.Z_universal_lval -> assert false
    | _ -> None


  (** Queries *)
  (** ******* *)

  let ask query man flow = None

end

let () =
  Framework.Domain.register_domain (module Domain)
