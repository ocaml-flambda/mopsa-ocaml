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

(**
   Universal frontend translates the parser's AST into Framework's AST.
*)

open Mopsa
open Mopsa_universal_parser

module NameG =
struct
  let compt = ref 0
  let fresh () =
    let rep = !compt in
    incr compt;
    rep
end

open Lexing
open Ast
open Mopsa

module T = Ast
module U = U_ast

module Float = ItvUtils.Float

(* vars to their unique identifier and declared types *)
module MS = MapExt.StringMap
type var_context = (int * typ) MS.t
type fun_context = (T.fundec) MS.t

let builtin_functions =
  [
    {name = "mopsa_assume"; args = [None]; output = T_unit};
    {name = "append"; args = [Some (T_array T_int); Some T_int]; output = T_unit};
  ]

let from_extent (e: U.extent) : Location.range = e

type uvar = {
  uvar_range: range;
  uvar_uid: int;
  uvar_orig_name: string;
  uvar_uniq_name: string;
}

type var_kind +=
  | V_uvar of uvar

let () = register_var {
    print = (fun next fmt v ->
        match vkind v with
        | V_uvar var ->
          if !Framework.Core.Ast.Var.print_uniq_with_uid then
            Format.fprintf fmt "%s:%a" var.uvar_orig_name pp_relative_range var.uvar_range
          else Format.fprintf fmt "%s" var.uvar_orig_name
        | _ -> next fmt v
      );

    compare = (fun next v1 v2 ->
        match vkind v1, vkind v2 with
        | V_uvar var1, V_uvar var2 ->
          Compare.compose [
            (fun () -> Stdlib.compare var1.uvar_uid var2.uvar_uid);
            (fun () -> Stdlib.compare var1.uvar_uniq_name var2.uvar_uniq_name)
          ]

        | _ -> next v1 v2
      );
  }
       

let from_var (v: string) (ext: U.extent) (var_ctx: var_context) =
  try
    let (id, typ) = MS.find v var_ctx in
    let uniq_name =  (v ^ ":" ^ string_of_int id) in 
    mkv uniq_name
      (V_uvar {
          uvar_range = ext;
          uvar_uid = id;
          uvar_orig_name = v;
          uvar_uniq_name = uniq_name
        })
      typ
  with
  | Not_found ->
    Exceptions.panic_at ext
      "%s was not found in typing/naming context"
      v

let rec from_typ (typ: U_ast.typ) : typ =
  match typ with
  | AST_INT     -> T_int
  | AST_REAL    -> T_float F_DOUBLE
  | AST_ARRAY t -> T_array (from_typ t)
  | AST_STRING  -> T_string
  | AST_CHAR    -> T_char

(* find a common type for the arguments of binary operations *)
let unify_typ (x:typ) (y:typ) : typ =
  match x,y with
  | T_int, T_float _ -> y
  | T_float _, T_int -> x
  | _ ->
     if compare_typ x y = 0 then x
     else Exceptions.panic "cannot unify types %a and %a" pp_typ x pp_typ y

(* cast expression to the given type (if needed) *)
let to_typ (t:typ) (e:expr) : expr =
  let range = erange e in
  let orgt = etyp e in
  if compare_typ orgt t = 0 then e
  else
    match ekind e, orgt, t with
    | _, (T_int | T_float _), (T_int | T_float _) ->
      mk_unop O_cast e ~etyp:t range
    | E_constant (C_top T_any), T_any, t ->
      {e with ekind = E_constant (C_top t); etyp = t}
    | _ -> 
      Exceptions.panic "cannot convert expression %a of type %a to type %a" pp_expr e pp_typ orgt pp_typ t

let from_binop (t: typ) (b: U.binary_op) : operator =
  match t, b with
  | T_int, AST_PLUS          -> O_plus
  | T_int, AST_MINUS         -> O_minus
  | T_int, AST_MULTIPLY      -> O_mult
  | T_int, AST_DIVIDE        -> O_div
  | T_int, AST_EQUAL         -> O_eq
  | T_int, AST_NOT_EQUAL     -> O_ne
  | T_int, AST_LESS          -> O_lt
  | T_int, AST_LESS_EQUAL    -> O_le
  | T_int, AST_GREATER       -> O_gt
  | T_int, AST_GREATER_EQUAL -> O_ge
  | T_int, AST_AND           -> O_log_and
  | T_int, AST_OR            -> O_log_or
  | T_string, AST_CONCAT        -> O_concat
  | T_string, AST_PLUS        -> O_concat
  | T_string, AST_EQUAL         -> O_eq
  | T_float _, AST_PLUS          -> O_plus
  | T_float _, AST_MINUS         -> O_minus
  | T_float _, AST_MULTIPLY      -> O_mult
  | T_float _, AST_DIVIDE        -> O_div
  | T_float _, AST_EQUAL         -> O_eq
  | T_float _, AST_NOT_EQUAL     -> O_ne
  | T_float _, AST_LESS          -> O_lt
  | T_float _, AST_LESS_EQUAL    -> O_le
  | T_float _, AST_GREATER       -> O_gt
  | T_float _, AST_GREATER_EQUAL -> O_ge
  | T_array _, AST_CONCAT -> O_concat
  | _ -> Exceptions.panic "operator %a cannot be used with type %a" U_ast_printer.print_binary_op b pp_typ t

let from_unop (t: typ) (b: U.unary_op) : operator =
  match t, b with
  | T_int, AST_UNARY_PLUS    -> O_plus
  | T_int, AST_UNARY_MINUS   -> O_minus
  | T_int, AST_NOT           -> O_log_not
  | T_float f, AST_UNARY_PLUS  -> O_plus
  | T_float f, AST_UNARY_MINUS -> O_minus
  | T_float f, AST_ROUND -> O_cast
  | _ -> Exceptions.panic "operator %a cannot be used with type %a" U_ast_printer.print_unary_op b pp_typ t

let rec from_expr (e: U.expr) (ext : U.extent) (var_ctx: var_context) (fun_ctx: fun_context option): expr =
  let range = from_extent ext in
  match e with
  | AST_unit_const -> mk_expr ~etyp:T_unit (E_constant (C_unit)) range
  | AST_fun_call((f, f_ext), args) ->
    begin
      let look_in_builtins (fun_ctx) =
        let exception Match of (expr list * fun_builtin) in
        try
          List.iter (fun (bi:fun_builtin) ->
              let () = Debug.debug ~channel:("remove_me") "builtin: %s, fun: %s, b: %b" bi.name f (bi.name = f) in
              if bi.name = f && List.length bi.args = List.length args then
                let exception NoMatch in
                try
                  let el = List.map2 (fun (e, ext) x ->
                      match x with
                      | Some x ->
                        let e' = from_expr e ext var_ctx (fun_ctx) in
                        let typ = etyp e' in
                        let () = Debug.debug ~channel:("remove_me") "x: %a, typ: %a" pp_typ x pp_typ typ in
                        if compare_typ typ x = 0 then
                          e'
                        else
                          raise NoMatch
                      | None -> from_expr e ext var_ctx (fun_ctx)
                    ) args bi.args
                  in
                  raise (Match (el, bi))
                with
                | NoMatch -> ()
            ) builtin_functions;
          Exceptions.panic_at ext
            "%s was not found in naming context nor in builtin functions"
            f
        with
        | Match(el, bi) ->
          (mk_expr ~etyp:(bi.output) (E_call(mk_expr (E_function (Builtin bi)) range, el)) range)
      in
      match fun_ctx with
      | None -> look_in_builtins (None)
      | Some fun_ctx ->
        begin
          try
            let fundec = MS.find f fun_ctx in
            if List.length fundec.fun_parameters = List.length args then
              let el = List.map2 (fun (e, ext) x ->
                  let e' = from_expr e ext var_ctx (Some fun_ctx) in
                  let typ = etyp e' in
                  if compare_typ x.vtyp typ = 0 then
                    e'
                  else
                    Exceptions.panic_at ext
                      "type of %a incompatible with declared function"
                      U_ast_printer.print_expr e
                ) args fundec.fun_parameters in
              let rettyp =
                match fundec.fun_return_type with
                | None -> T_int
                | Some t -> t
              in
              (mk_expr ~etyp:rettyp (E_call(mk_expr (E_function (User_defined fundec)) range, el)) range)
            else
              Exceptions.panic_at ext
                "%s number of arguments incompatible with call"
                f
          with
          | Not_found ->
            begin
              look_in_builtins (Some fun_ctx)
            end
        end
    end
  | AST_unary (op, (e, ext)) ->
    begin
      let e = from_expr e ext var_ctx fun_ctx in
      let typ = etyp e in
      let op = from_unop typ op in
      mk_unop op e ~etyp:typ range
    end
  | AST_binary (op, (e1, ext1), (e2, ext2)) ->
    begin
      let e1 = from_expr e1 ext1 var_ctx fun_ctx in
      let typ1 = etyp e1 in
      let e2 = from_expr e2 ext2 var_ctx fun_ctx in
      let typ2 = etyp e2 in
      let typ = unify_typ typ1 typ2 in
      let e1,e2 = to_typ typ e1, to_typ typ e2 in
      let op = from_binop typ op in
      mk_binop e1 op e2 ~etyp:typ range
    end

  | AST_identifier (v, ext) ->
    mk_var (from_var v ext var_ctx) range

  | AST_int_const (s, _) ->
    mk_z (Z.of_string s) range

  | AST_bool_const (b, _) ->
    mk_int (if b then 1 else 0) range

  | AST_real_const (s, _) ->
     (* double interval enclosing the real value *)
     let lo = Float.of_string `DOUBLE `DOWN s
     and up = Float.of_string `DOUBLE `UP s
     in
     mk_float_interval ~prec:F_DOUBLE lo up range

  | AST_string_const (s, _) ->
    mk_string s range

  | AST_char_const(c, _) ->
    mk_int ~typ:T_int (int_of_char c) range

  | AST_array_const(a, _) ->
     mk_expr (E_array (List.map (fun (e, ext) -> from_expr e ext var_ctx fun_ctx) (Array.to_list a))) range

  | AST_rand((l, _), (u, _)) ->
    mk_z_interval (Z.of_string l) (Z.of_string u) range

  | AST_randf((l, _), (u, _)) ->
    mk_float_interval (float_of_string l) (float_of_string u) range

  | AST_rand_string ->
    mk_top T_any range

  | AST_array_access((e1, ext1), (e2, ext2)) ->
    begin
      let e1o = e1 in
      let e1 = from_expr e1 ext1 var_ctx fun_ctx in
      let e2 = from_expr e2 ext2 var_ctx fun_ctx in
      let e2 = to_typ T_int e2 in
      match etyp e1 with
      | T_string -> mk_expr (E_subscript(e1, e2)) ~etyp:T_int range
      | T_array t -> mk_expr (E_subscript(e1, e2)) ~etyp:t range
      | _ -> Exceptions.panic_at ext
               "%a is of type %a and can not be subscripted"
               U_ast_printer.print_expr e1o
               (pp_typ) (etyp e1)
    end

  | AST_len (e, ext) ->
    begin
      let e1 = from_expr e ext var_ctx fun_ctx in
      match etyp e1 with
      | T_string
      | T_array _ -> mk_expr (E_len e1) ~etyp:T_int range
      | _ -> Exceptions.panic_at ext "%a is of type %a and can not be lengthed"
               U_ast_printer.print_expr e
               (pp_typ) (etyp e1)
    end

let rec from_stmt (s: U.stat) (ext: U.extent) (var_ctx: var_context) (fun_ctx: fun_context option): stmt =
  let range = from_extent ext in
  match s with
  | AST_block l ->
    mk_block (List.map (fun (x, ext) -> from_stmt x ext var_ctx fun_ctx) l) range

  | AST_assign((e1, ext1), (e2, ext2)) ->
    begin
      let e1o = e1 in
      match e1 with
      | AST_array_access(_, _)
      | AST_identifier _ ->
        let e1 = from_expr e1 ext1 var_ctx fun_ctx in
        let e2 = from_expr e2 ext2 var_ctx fun_ctx in
        let e2 = to_typ (etyp e1) e2 in
        mk_assign e1 e2 range
      | _ ->
        Exceptions.panic_at ext "%a not considered a left-value for now "
          U_ast_printer.print_expr e1o
    end

  | AST_if((e1, ext_e1), (s1, ext_s1), Some (s2, ext_s2)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    let s2 = from_stmt s2 ext_s2 var_ctx fun_ctx in
    mk_if e1 s1 s2 range

  | AST_if((e1, ext_e1), (s1, ext_s1), None) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_if e1 s1 (mk_nop range) range

  | AST_while((e1, ext_e1), (s1, ext_s1)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_while e1 s1 range

  | AST_for((v1, ext_v1), (e1, ext_e1), (e2, ext_e2), (s1, ext_s1)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let e2 = from_expr e2 ext_e2 var_ctx fun_ctx in
    let v  = from_var v1 ext_v1 var_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_block
      [
        mk_assign
          (mk_var v (tag_range range "var_init_for_variable"))
          e1
          (tag_range range "expr_init_for_variable");
        mk_while
          (mk_binop
             (mk_var v (tag_range range "var_comp_for"))
             O_le
             e2
             ~etyp:(T_int)
             (tag_range range "comp_for")
          )
          (mk_block (
              [
                s1;
                mk_assign
                  (mk_var v (tag_range range "var_incr_for"))
                  (mk_binop
                     (mk_var v (tag_range range "var_incr_for"))
                     O_plus
                     (mk_z Z.one (tag_range range "one_for"))
                     ~etyp:(T_int)
                     (tag_range range "incr_for")
                  )
                  (tag_range range "assign_for")
              ])
              (tag_range range "body_for")
          )
          (tag_range range "total_for")
      ]
      range

  | AST_return (Some (e, ext)) ->
    let e = from_expr e ext var_ctx fun_ctx in
    {skind = S_return (Some e);
     srange = range
    }

  | AST_return None ->
    {skind = S_return None;
     srange = range
    }

  | AST_break ->
    {skind = S_break; srange = range}

  | AST_continue ->
    {skind = S_continue; srange = range}

  | AST_assert (e, ext) ->
    let e = from_expr e ext var_ctx fun_ctx in
    mk_assert e range

  | AST_assume (e, ext) ->
    let e = from_expr e ext var_ctx fun_ctx in
    mk_assume e range


  | AST_print ->
    mk_stmt S_print_state range

  | AST_expr(e, ext) ->
    let e' = from_expr e ext var_ctx fun_ctx in
    mk_expr_stmt e' range

let rec check_declaration_list (dl : U.declaration U.ext list) =
  match dl with
  | p::q -> aux p q; check_declaration_list q
  | [] -> ()
and aux (((((_,v),e),_),_) as p : U.declaration U.ext) (dl: U.declaration U.ext list) =
  match dl with
  | ((((_,v'),e'),_),_)::q when v = v' -> Exceptions.panic_at e "%s has already been declared at %a"
                                            v'
                                            pp_range e'
  | p':: q -> aux p q
  | [] -> ()

let var_ctx_of_declaration (dl : U_ast.declaration U.ext list) (var_ctx: var_context) =
  let () = check_declaration_list dl in
  let add_var var_ctx v t =
    try
      MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      MS.add v (NameG.fresh (), t) var_ctx
  in
  let var_ctx, gvars = List.fold_left (fun (var_ctx, gvars) ((((t, v), extv ), o), e) ->
      let new_var_ctx = add_var var_ctx v (from_typ t) in
      let vv = from_var v extv new_var_ctx in
      (new_var_ctx, vv :: gvars)
    ) (var_ctx, []) dl in
  var_ctx, gvars

let var_ctx_init_of_declaration (dl : U_ast.declaration U.ext list) (var_ctx: var_context) (fun_ctx: fun_context option) (nvar_ctx)=
  let add_var var_ctx v t =
    try
      match nvar_ctx with
      | Some nvar_ctx -> MS.add v (MS.find v nvar_ctx) var_ctx
      | None -> MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      assert false
  in
  let var_ctx, init, gvars = List.fold_left (fun (var_ctx, init, gvars) ((((t, v), extv ), o), e) ->
      let new_var_ctx = add_var var_ctx v (from_typ t) in
      let vv = from_var v extv new_var_ctx in
      let range = from_extent extv in 
      let stmt_add = mk_add
          (mk_var vv (tag_range range "initializer_var"))
          (tag_range range "initializer") in
      let init = stmt_add :: init in 
      match o with
      | Some (e, ext) ->
        let e = from_expr e ext var_ctx fun_ctx in
        let e = to_typ (from_typ t) e in
        let range = from_extent ext in
        let stmt_init =
          mk_assign
            (mk_var vv (tag_range range "initializer_var"))
            e
            (tag_range range "initializer")
        in
        (new_var_ctx, stmt_init :: init, vv :: gvars)
      | None   ->
        (new_var_ctx, init, vv :: gvars)
    ) (var_ctx, [], []) dl in
  var_ctx, List.rev init, gvars

let var_ctx_of_function (var_ctx: var_context) (fundec: U.fundec) =
  let add_var var_ctx v t =
    try
      MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      MS.add v (NameG.fresh (), t) var_ctx
  in
  let var_ctx = List.fold_left (fun acc ((t, v), _) ->
      add_var acc v (from_typ t)
    ) var_ctx fundec.parameters in
  let var_ctx, _ =
    var_ctx_of_declaration fundec.locvars var_ctx
  in
  var_ctx

let var_init_of_function (var_ctx: var_context) var_ctx_map (fun_ctx: fun_context) (fundec: U.fundec) =
  let nvar_ctx = MS.find fundec.funname var_ctx_map in
  let add_var var_ctx n_var_ctx v t =
    try
      MS.add v (MS.find v n_var_ctx) var_ctx
    with
    | Not_found ->
      assert false
  in
  let var_ctx = List.fold_left (fun acc ((t, v), _) ->
      add_var acc nvar_ctx v (from_typ t)
    ) var_ctx fundec.parameters in
  let var_ctx, init, _ =
    var_ctx_init_of_declaration fundec.locvars var_ctx (Some (fun_ctx)) (Some nvar_ctx)
  in
  var_ctx, init


let from_fundec (f: U.fundec) (var_ctx: var_context): T.fundec =
  let typ = OptionExt.lift from_typ f.return_type in
  {
    fun_orig_name = f.funname;
    fun_uniq_name = f.funname;
    fun_range = from_extent f.range;
    fun_parameters = List.map (fun ((_, v), ext) -> from_var v ext var_ctx) f.parameters;
    fun_locvars = List.map (fun ((((_, v), _), _), ext) -> from_var v ext var_ctx) f.locvars;
    fun_body = mk_nop (from_extent (snd f.body));
    fun_return_type = typ;
    fun_return_var = None;
  }

let fun_ctx_of_global (fl: U_ast.fundec U.ext list) (var_ctx: var_context) =
  List.fold_left (fun (acc, var_ctx_map) (fundec, _) ->
      let var_ctx = var_ctx_of_function var_ctx fundec in
      (MS.add fundec.funname (from_fundec fundec var_ctx) acc, MS.add fundec.funname var_ctx var_ctx_map)
    ) (MS.empty, MS.empty) fl

let add_body (fl: fun_context) (f: string) (b: stmt): unit =
  try
    let fundec = MS.find f fl in
    fundec.fun_body <- b;
  with
  | Not_found -> Exceptions.panic "[Universal.frontend] should not happen"

let from_prog (p: U_ast.prog) : prog_kind =
  let ext = snd (p.main) in
  let var_ctx, init, gvars = var_ctx_init_of_declaration p.gvars MS.empty None None in
  let fun_ctx, var_ctx_map = fun_ctx_of_global p.funs var_ctx in
  List.iter (fun (fundec, ext) ->
      let var_ctx, init = var_init_of_function var_ctx var_ctx_map fun_ctx fundec in
      let body = from_stmt (fst fundec.body) (snd fundec.body) var_ctx (Some fun_ctx) in
      let total = mk_block (init @ [body]) (from_extent ext) in
      add_body fun_ctx fundec.funname total
    ) p.funs;
  let total = from_stmt (fst p.main) (snd p.main) var_ctx (Some fun_ctx) in
  let with_init = mk_block (init @ [total]) (from_extent ext) in
  P_universal
    {
      universal_gvars   = gvars;
      universal_fundecs = (MS.bindings fun_ctx) |> List.map (snd);
      universal_main    = with_init
    }

let rec parse_program (files: string list): program =
  match files with
  | [filename] ->
    let ast = U_file_parser.parse_file filename in
    {
      prog_kind = from_prog ast;
      prog_range = mk_program_range [filename];
    }

  | [] -> panic "no input file"

  | _ -> panic "analysis of multiple files not supported"


(* Front-end registration *)
let () =
  register_frontend {
    lang = "universal";
    parse = parse_program;
    on_panic = fun _ _ _ -> ();
  }
