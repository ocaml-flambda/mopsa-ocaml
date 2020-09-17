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
   Read-eval-print-loop interactive frontend.
   Borrows the syntax and semantics from universal.
*)

open Mopsa
open Sig.Abstraction.Stateless
open Lexing
open Universal.Ast
open Universal.Frontend
open Debug



(** {2 Interpreter state} *)
(** ********************* *)


type ctx = {
    ctx_var: var_context;
    ctx_fun: fun_context;
  }


let init_ctx () = {
    ctx_var = MS.empty;
    ctx_fun = MS.empty;
  }



(** {2 Parsing} *)
(** *********** *)


let range_of_string ?(org=0) (str:string) : range =
  mk_orig_range
    (mk_pos "<input>" 0 org)
    (mk_pos "<input>" 0 (org + String.length str))


(** Helper to parse a string using a menhir parser entry point. *)
let parse_string ?(org=0) parser str =
  let lex = from_string str in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_cnum = org; };
    parser U_lexer.token lex
  with
  | U_parser.Error ->
     let range = from_lexing_range (Lexing.lexeme_start_p lex) (Lexing.lexeme_end_p lex) in
     Exceptions.syntax_error range "Syntax error" str
  | Failure s ->
     let range = from_lexing_range (Lexing.lexeme_start_p lex) (Lexing.lexeme_end_p lex) in
     Exceptions.syntax_error range "%s" str

(** Parse an expression. *)
let parse_expr ?org (ctx:ctx) (str:string) : expr =
  let ast = parse_string ?org U_parser.expr_eof str in
  from_expr ast (range_of_string ?org str) ctx.ctx_var (Some ctx.ctx_fun)

(** Parse a statement. *)
let parse_stmt ?org (ctx:ctx) (str:string) : stmt =
  let ast = parse_string ?org U_parser.stat_eof str in
  from_stmt ast (range_of_string ?org str) ctx.ctx_var (Some ctx.ctx_fun)

(** Parse a variable declaration. *)
let parse_vardec ?org (ctx:ctx) (str:string) : ctx * stmt list * var list =
  let ast = parse_string ?org U_parser.declaration_eof str in
  let range = range_of_string ?org str in
  let var_ctx, init, gvar =
    var_ctx_init_of_declaration [ast,range] ctx.ctx_var (Some ctx.ctx_fun) None
  in
  { ctx with ctx_var = var_ctx; }, init, gvar

(** Parse a function declaration. *)
let parse_fundec ?org (ctx:ctx) (str:string) : ctx * fundec =
  let ast = parse_string ?org U_parser.fundec_eof str in
  let range = range_of_string ?org str in
  let fun_ctx, var_ctx_map = fun_ctx_of_global [ast,range] ctx.ctx_var in
  let f = MS.find ast.funname fun_ctx in
  let var_ctx2, init = var_init_of_function ctx.ctx_var var_ctx_map fun_ctx ast in
  let body = from_stmt (fst ast.body) (snd ast.body) var_ctx2 (Some fun_ctx) in
  f.fun_body <- mk_block (init @ [body]) range;
  { ctx with ctx_fun = fun_ctx }, f

(** Parse a variable. *)
let parse_var ?org (ctx:ctx) (str:string) : var =
  from_var str (range_of_string ?org str) ctx.ctx_var


type input_class =
  | VarDecl | FunDecl | Stmt

let vardecl_str =
  Str.regexp "\\(int\\|real\\|string\\|char\\)[ \t\r\n]+[a-zA-Z0-9]+[ \t\r\n]*[,;=].*"

let fundecl_str =
  Str.regexp "\\(int\\|real\\|string\\|char\\|void\\)[ \t\r\n]+[a-zA-Z0-9]+[ \t\r\n]*(.*"

(** Try to guess the nature of the input. *)
let classify_input str =
  if Str.string_match vardecl_str str 0 then VarDecl
  else if Str.string_match fundecl_str str 0 then FunDecl
  else Stmt



(** {2 Printing} *)
(** ************ *)


let eol = Str.regexp "\n\\|\r\\|\r\n"

(** Prints a string with some locations highlighted. *)
let print_highlight (str:string) (range:range) =
  (* coloring *)
  let reset () = print_string "\027[0m"
  and highlight () = print_string "\027[1;41m" (* bright with red background *)
  in
  (* range *)
  let r1 = get_range_start range
  and r2 = get_range_end range in
  let l1,l2 = get_pos_line r1, get_pos_line r2
  and c1,c2 = get_pos_column r1, get_pos_column r2
  in
  (* cut into lines *)
  let lines = Str.split eol str in
  (* for each line *)
  Printf.printf "\027[0m";
  let rec doit i = function
    | [] -> ()
    | line::rest ->
       if i > l1 && i <= l2 then highlight ();
       for j = 0 to String.length line - 1 do
         if i = l1 && j = c1 then highlight ()
         else if i = l2 && j = c2 + 1 then reset ();
         print_char line.[j]
       done;
       reset ();
       print_char '\n';
       doit (i+1) rest
  in
  doit 1 lines



(** {2 Main loop} *)
(** ************* *)


let spaces = Str.regexp "[ \t\r\n]+"

let repl_ctx = LineEdit.create_ctx ()

let prompt    = "\027[1;32mMOPSA > \027[0m"
let col_reset = "\027[0m"
let col_error = "\027[1;31m" (* bright red *)
let col_out   = "\027[1;33m" (* bright yellow *)

let pf = Format.printf

let print_usage () =
  pf "Commands:@.";
  pf "  h[elp]@.";
  pf "  q[uit]@.";
  pf "  p[rint] <variable>@.";
  pf "  <statement>;@.";
  pf "  <declaration>;@.";
  ()


let rec repl_loop ctx man flow =
  pf "%s@?" prompt;
  let str = LineEdit.read_line repl_ctx in
  let quit = ref false in
  let ctx,flow =
    try
      match Str.split spaces str with

      | [] | ["h"] | ["help"] | ["?"] ->
         print_usage ();
         ctx, flow

      | ["quit"] | ["q"] ->
         quit := true;
         ctx, flow

      | ("print" | "p")::vars ->
        Exceptions.panic "print not supported"

      | _ ->
         (* autodetect the command *)
         match classify_input str with

         | Stmt ->
            let stmt = parse_stmt ctx str in
            pf "%s@[<v 4>X♯ ≜ 𝕊⟦%a@]⟧ =@]%s@." col_out pp_stmt stmt col_reset;
            let flow = man.exec stmt flow |> post_to_flow man in
            pf "%s@[<v 4>%a@]%s@." col_out (Flow.print man.lattice.print) flow col_reset;
            ctx, flow

         | VarDecl ->
            let ctx, stmts, vars = parse_vardec ctx str in
            let flow =
              List.fold_left (fun flow stmt -> man.exec stmt flow |> post_to_flow man) flow stmts
            in
            List.iter (
                fun v ->
                pf "variable %a : %a declared@." pp_var v pp_typ (vtyp v)
              ) vars;
            pf "%s@[<v 4>X♯ ≜%s@." col_out col_reset;
            pf "%s%a%s@." col_out (Flow.print man.lattice.print) flow col_reset;
            ctx, flow

         | FunDecl ->
            let ctx, fdec = parse_fundec ctx str in
            pf "function %s declared@." fdec.fun_orig_name;
            ctx, flow

    with
    | Panic (msg,txt) ->
       pf "%sPanic: %s: %s%s@." col_error msg txt col_reset;
       ctx, flow
    | PanicAtLocation (r,msg,txt) ->
       pf "%sPanic: %s at %a: %s%s@." col_error msg (pp_range) r txt col_reset;
       print_highlight str r;
       ctx, flow
    | PanicAtFrame (r,cs,msg,txt) ->
       pf "%sPanic: %s at %a: %s%s@." col_error msg (pp_range) r txt col_reset;
       print_highlight str r;
       ctx, flow
    | SyntaxError (r,txt) ->
       pf "%sSyntax error at %a: %s%s@." col_error (pp_range) r txt col_reset;
       print_highlight str r;
       ctx, flow
    | e ->
       pf "%sexception %s%s@." col_error (Printexc.to_string e) col_reset;
       ctx, flow
  in
  if !quit then flow
  else repl_loop ctx man flow


(** Main loop. *)
let enter_repl man flow =
  repl_loop (init_ctx ()) man flow



(** {2 Interactive "program"} *)
(*  ************************* *)

type prog_kind +=
   | P_REPL (* no contents *)

let () =
  register_program {
      compare = (fun next -> next);
      print = (fun default fmt prog ->
        match prog.prog_kind with
        | P_REPL ->
           Format.fprintf fmt "read-eval-print loop"
        | _ -> default fmt prog
      );
    }

(** Ignore files and return the constant P_REPL program. *)
let parse_program files : program =
  if files <> [] then Exceptions.warn "File arguments in REPL are ignored";
  { prog_kind = P_REPL;
    prog_range = range_of_string "";
  }


module Domain = struct

  include GenStatelessDomainId(struct
      let name = "universal.repl"
    end)


  let dependencies = []

  let checks = []

  let init prog man flow = flow

  let exec stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = P_REPL }, _) ->
       Some (Post.return (enter_repl man flow))

    | _ -> None

  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)



(* Front-end registration *)
let () =
  register_frontend {
    lang = "repl";
    parse = parse_program;
  }
