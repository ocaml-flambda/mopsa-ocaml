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

(** Preprocessor for expanding macros *)

open Parser
open Lexing
open Mopsa_c_parser.C_AST
open Mopsa_c_parser.Clang_AST
open Mopsa_utils


let debug fmt = Debug.debug ~channel:"c_stubs_parser.passes.preprocessor" fmt

let pp_token fmt token =
  Format.pp_print_string fmt (Lexer.token_to_string token)

let pp_token_list fmt tokens =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
    pp_token
    fmt tokens

(* Stack containing called macros with their tokenized content *)
let stack : (macro * token list) Stack.t = Stack.create ()

let pp_stack fmt stack =
  let elements = Stack.fold (fun acc e -> acc@[e]) [] stack in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt (macro,tokens) -> Format.fprintf fmt "%s: %a" macro.macro_name pp_token_list tokens)
    ) elements

(* Check if we are already expanding a macro *)
let inside_macro macro stack =
  let exception Found in
  try
    Stack.iter
      (fun (m,_) -> if m.macro_name = macro.macro_name then raise Found) stack;
    false
  with Found -> true

(* Get the next token *)
let rec next_token ?(ret2caller=true) lexer lexbuf =
  if Stack.is_empty stack then
    lexer lexbuf
  else
    let macro,tokens = Stack.pop stack in
    match tokens with
    | []        ->
      assert ret2caller;
      next_token lexer lexbuf
    | [token]   ->
      if not ret2caller then Stack.push (macro,[]) stack;
      token
    | token::tl ->
      Stack.push (macro,tl) stack;
      token

(* Parse a string into a token using the lexer *)
let tokenize_string lexer (s:string) : token list =
  let rec iter lexbuf =
    match lexer lexbuf with
    | EOF -> []
    | tk  -> tk :: iter lexbuf
  in
  iter (Lexing.from_string s)

(* Parse the arguments of a macro into a map of tokens *)
let tokenize_arguments macro lexer lexbuf : token list StringMap.t =
  match macro.macro_params with
  | [] -> StringMap.empty
  | hd::tl  ->
    (* Read '(' *)
    if next_token ~ret2caller:false lexer lexbuf <> LPAR then
      raise (Lexer.SyntaxError (Format.asprintf "macro %s is missing '('" macro.macro_name));
    (* Read arguments separated by ',' until reaching ')' *)
    let rec iter param params openpar past_tokens token =
      match token with
      | EOF -> raise (Lexer.SyntaxError (Format.asprintf "macro %s is missing ')'" macro.macro_name));
      | LPAR ->
        iter param params (openpar + 1) (token::past_tokens) (next_token ~ret2caller:false lexer lexbuf)
      | RPAR ->
        if openpar = 0 then
          begin
            if params <> [] then
              raise (Lexer.SyntaxError (
                  Format.asprintf "macro %s is missing %d more argument%a"
                    macro.macro_name
                    (List.length params) (Debug.plurial_int) (List.length params)));
            StringMap.singleton param (List.rev past_tokens)
          end
        else
          iter param params (openpar - 1) (token::past_tokens) (next_token ~ret2caller:false lexer lexbuf)
      | COMMA when openpar = 0 ->
        begin match params with
          | [] ->
            raise (Lexer.SyntaxError "macro %s is given too many arguments");
          | hd::tl ->
            iter hd tl 0 [] (next_token ~ret2caller:false lexer lexbuf) |>
            StringMap.add param (List.rev past_tokens)
        end
      | _ ->
        iter param params openpar (token::past_tokens) (next_token ~ret2caller:false lexer lexbuf)
    in
    iter hd tl 0 [] (next_token ~ret2caller:false lexer lexbuf)

(* Add parenthesis around a list of tokens *)
let add_parenthesis tokens =
  match tokens with
  | [] | [_] -> tokens
  | _        -> LPAR :: tokens @ [RPAR]

(* Parse the content of a macro as a list of tokens and replace parameters with the corresponding arguments *)
let tokeninze_macro macro args lexer : token list =
  let content =
    List.fold_left
      (fun acc s -> acc @ tokenize_string lexer s)
      [] macro.macro_contents
  in
  (* Replace parameters with the corresponding arguments *)
  let rec iter = function
    | [] -> []
    | (IDENT id as hd) :: tl ->
      begin match StringMap.find_opt id args with
        | None        -> hd :: iter tl
        | Some tokens -> add_parenthesis tokens @ iter tl
      end
    | hd::tl -> hd :: iter tl
  in
  add_parenthesis (iter content)

exception AliasFound of string

(* Parse a preprocessor directive *)
let parse_directive lexer lexbuf =
  match next_token lexer lexbuf with
  | ALIAS ->
    begin match next_token lexer lexbuf with
      | IDENT alias -> raise (AliasFound alias)
      | token ->
        raise (Lexer.SyntaxError
          (Format.asprintf "unexpected alias argument %s" (Lexer.token_to_string token)))
    end
  | token ->
    raise (Lexer.SyntaxError
      (Format.asprintf "unknown preprocessor directive %s"
         (Lexer.token_to_string token)))

(* Type of a predicate *)
type predicate = {
  pred_name : string;
  pred_params: string list;
  pred_body : token list;
}

(* Parse a predicate *)
let parse_predicate lexer lexbuf =
  let get_predicate_name () =
    match lexer lexbuf with
    | END_DELIM -> None
    | PREDICATE ->
      begin match lexer lexbuf with
        | IDENT name -> Some name
        | token ->
          raise (Lexer.SyntaxError ("incorrect predicate name " ^ (Lexer.token_to_string token)))
      end
    | token ->
      raise (Lexer.SyntaxError ("incorrect predicate declaration " ^ (Lexer.token_to_string token)))
  in
  let get_predicate_params () =
    match lexer lexbuf with
    | COLON -> []
    | LPAR ->
      let rec iter () =
        match lexer lexbuf with
        | RPAR ->
          begin match lexer lexbuf with
            | COLON -> []
            | token ->
              raise (Lexer.SyntaxError ("missing : after predicate header"))
          end
        | IDENT param ->
          begin match lexer lexbuf with
            | COMMA -> param :: iter ()
            | RPAR ->
              begin match lexer lexbuf with
                | COLON -> [param]
                | token ->
                  raise (Lexer.SyntaxError ("missing : after predicate header"))
              end
            | token ->
              raise (Lexer.SyntaxError ("incorrect predicate parameter separator "^ (Lexer.token_to_string token)))
          end
        | token ->
          raise (Lexer.SyntaxError ("incorrect predicate parameter "^ (Lexer.token_to_string token)))
      in
      iter ()
    | token ->
      raise (Lexer.SyntaxError ("incorrect predicate declaration "^ (Lexer.token_to_string token)))
  in
  let rec get_predicate_body () =
    match lexer lexbuf with
    | SEMICOL -> []
    | EOF | END_DELIM ->
      raise (Lexer.SyntaxError "missing ; at the end of the predicate")
    | token -> token :: get_predicate_body ()
  in
  match get_predicate_name () with
  | None -> None
  | Some name ->
    let params = get_predicate_params () in
    let body = get_predicate_body () in
    Some { pred_name = name;
           pred_params = params;
           pred_body = body }

(* Parse a sequence of predicates *)
let parse_predicates lexer lexbuf =
  match lexer lexbuf with
  | BEGIN_DELIM ->
    let rec iter () =
      match parse_predicate lexer lexbuf with
      | None   -> []
      | Some p -> p :: iter ()
    in
    iter ()

  | token -> raise (Lexer.SyntaxError ("incorrect predicate comment symbol " ^ (Lexer.token_to_string token)))

(* Turn a predicate into a macro *)
let predicate_to_macro pred =
  { macro_name = pred.pred_name;
    macro_params = pred.pred_params;
    macro_contents = List.map Lexer.token_to_string pred.pred_body;
    macro_loc = {
      loc_line = -1;
      loc_column = -1;
      loc_file = "";
    } }

(* Entry point of the preprocessor *)
let rec read predicates macros enums lexer lexbuf =
  let stack0 = Stack.copy stack in
  let token = next_token lexer lexbuf in
  (* Identifiers *may be* enums or macros, so check that *)
  match token with
  | IDENT id ->
    begin
      match StringMap.find_opt id enums with
      (* Enum case *)
      | Some n -> INT_CONST (n, NO_SUFFIX)

      | None   ->
        match StringMap.find_opt id predicates with
        (* Predicate case *)
        | Some pred ->
          (* Save the lexer location *)
          let predicate_start_pos = lexbuf.lex_start_p in
          (* Since predicates are similar to macros, we use the same processing *)
          let macro = predicate_to_macro pred in
          (* Parse the arguments *)
          let args = tokenize_arguments macro lexer lexbuf in
          (* Parse the body of the predicate *)
          let tokens = tokeninze_macro macro args lexer in
          (* Update the tokens stack and repeat the same process *)
          Stack.push (macro,tokens) stack;
          (* Restore the start position of the macro *)
          lexbuf.lex_start_p <- predicate_start_pos;
          read predicates macros enums lexer lexbuf

        | None ->
          match StringMap.find_opt id macros with
          | None -> token

          (* Macro, but inside itself, so treat it as an identifier *)
          | Some macro when inside_macro macro stack0 -> token

          (* Macro expansion case *)
          | Some macro ->
            (* Save the lexer location *)
            let macro_start_pos = lexbuf.lex_start_p in
            (* Parse the arguments *)
            let args = tokenize_arguments macro lexer lexbuf in
            (* Parse the body of the macro *)
            let tokens = tokeninze_macro macro args lexer in
            (* Update the tokens stack and repeat the same process *)
            Stack.push (macro,tokens) stack;
            (* Restore the start position of the macro *)
            lexbuf.lex_start_p <- macro_start_pos;
            read predicates macros enums lexer lexbuf
    end
  | SHARP -> parse_directive lexer lexbuf
  | _ -> token
