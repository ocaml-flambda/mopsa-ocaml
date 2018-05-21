(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python mutable sets. *)

open Framework.Domain
open Framework.Ast
open Framework.Query
open Framework.Manager
open Framework.Accessor
open Universal.Ast
open Ast
open Addr
open XAst
open Utils
    
let name = "python.objects.containers.sets"

module Make(SubLayer : Framework.Layer.S) = struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt
  
  module Abstract = Framework.Lattice.EmptyLattice
  module Flow = Framework.Lattice.EmptyLattice
  module SubAbstract = SubLayer.Abstract

  let has_abstract = false
  let has_flow = false
  
  type t = Abstract.t * Flow.t
  type st = SubLayer.t

  let mk_sv addr = mk_exp (PyAttribute(mk_addr addr, "$sv"))
  let mk_se addr = mk_exp ~etyp:TBool (PyAttribute (mk_addr addr, "$se"))

          
  let eval (exp : exp) manager ctx ax_ subax_ gabs =
    let subax = mk_domain_ax subax_ in
    match ekind exp with
    | PyCall(
        {ekind = Constant (Addr {akind = B_function "set.__init__"})},
        [{ekind = Constant (Addr set)}],
        []
      )
      ->
      let se = mk_se set in
      let sv = mk_sv set in

      let gabs = manager.exec
          (mk_assign se mk_true)
          ctx gabs
      in

      let gabs = manager.exec
          (mk_assign sv (mk_constant PyEmptyValue ~etyp:TEmptyValue))
          ctx gabs
      in


      [mk_constant PyNone ~etyp:TNone, (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr {akind = B_function "set.__init__"})},
        [
          {ekind = Constant (Addr set)};
          iterable
        ],
        []
      )
      ->

      let se = mk_se set in

      let gabs = manager.exec (mk_assign se mk_true) ctx gabs in


      let iterv = mktmp () in
      let iter = mk_var iterv in
      let gabs = manager.exec (
          mk_assign
            iter
            (mk_exp ~erange:iterable.erange(PyCall (
                 (mk_exp (PyAttribute (iterable, "__iter__")),
                  [],
                  []
                 )
               )))
        ) ctx gabs in

      let next = mk_exp (PyCall(
          mk_addr (Builtins.builtin_address "next"),
          [iter],
          []
        ))
      in

      let gabs = manager.exec
          (mk_try_stopiteration
            (mk_stmt (While (
                 mk_true,
                 (mk_stmt (Expression (mk_exp (PyCall(
                     (mk_addr (Builtins.builtin_address "set.add")),
                     [mk_addr set; next],
                     []
                   )))))
               )))
            (mk_stmt nop)
          )
          ctx gabs
      in

      let gabs = manager.exec (mk_stmt (RemoveAddrExp iter)) ctx gabs in

      let exp' = mk_constant PyNone ~etyp:TNone in
      [(exp', (gabs, [mk_stmt (RemoveAll iterv)]))]

    | PySet(el) ->
      panic "set not supported"

    | PySetComprehension(e, comprhs) ->
      panic "set comprehension not supported"


    | PyCall(
        {ekind = Constant (Addr {akind = B_function "set.add"})},
        [
          {ekind = Constant (Addr set)};
          value
        ],
        []
      )
      ->

      let sv = mk_sv set in
      let se = mk_se set in


      let empty_gabs = manager.exec (mk_stmt (Assume se)) ctx gabs in
      let empty_case =
        if SubAbstract.is_bottom (subax.get_abs empty_gabs) then
          []
        else
          let gabs = manager.exec (mk_assign sv value) ctx empty_gabs |>
                     manager.exec (mk_assign se mk_false) ctx |>
                     (fun gabs ->
                        debug "empty gabs =@\n@[  %a@]" manager.print gabs;
                        gabs
                     )
          in
          [mk_constant PyNone ~etyp:TNone, (gabs, [])]
      in

      let non_empty_gabs = manager.exec (mk_stmt (Assume (negate se))) ctx gabs in
      let non_empty_case =
        if SubAbstract.is_bottom (subax.get_abs non_empty_gabs) then
          []
        else
          let gabs = manager.exec (mk_assign sv value) ctx non_empty_gabs |>
                     manager.join non_empty_gabs |>
                     (fun gabs ->
                        debug "non empty gabs =@\n@[  %a@]" manager.print gabs;
                        gabs
                     )
          in
          [mk_constant PyNone ~etyp:TNone, (gabs, [])]
      in

      empty_case @ non_empty_case

    | PyCall(
        {ekind = Constant (Addr {akind = B_function "set.clear"})},
        [{ekind = Constant (Addr set)}],
        []
      )
      ->

      let sv = mk_sv set in
      let se = mk_se set in

      let gabs = manager.exec (mk_assign sv (mk_constant PyEmptyValue ~etyp:TEmptyValue)) ctx gabs |>
                 manager.exec (mk_assign se mk_true) ctx
      in
      [mk_constant PyNone ~etyp:TNone, (gabs, [])]

    | PyCall({ekind = Constant (Addr ({akind = B_function f}))}, _, _)
      when Builtins.is_class_dot_method "set" f ->

      panic "Set function %s not implemented" f


        
    | _ -> []

  let exec _ _ _ _ _ gabs = continue gabs
  let ask _ _ = Framework.Query.top


end

let setup () =
  register name (module Make)
