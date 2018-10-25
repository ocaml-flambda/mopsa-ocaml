
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TRUE
    | STRING of (
# 4 "src/c_annot/parser.mly"
       (string)
# 12 "_build/c_annot/parser.ml"
  )
    | RIGHT_BRACK
    | RIGHT_BRACE
    | NULL
    | LEFT_BRACK
    | LEFT_BRACE
    | INT of (
# 1 "src/c_annot/parser.mly"
       (int)
# 22 "_build/c_annot/parser.ml"
  )
    | ID of (
# 3 "src/c_annot/parser.mly"
       (string)
# 27 "_build/c_annot/parser.ml"
  )
    | FLOAT of (
# 2 "src/c_annot/parser.mly"
       (float)
# 32 "_build/c_annot/parser.ml"
  )
    | FALSE
    | EOF
    | COMMA
    | COLON
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state

let rec prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 58 "_build/c_annot/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env =
      let (_menhir_env : _menhir_env) = _menhir_env in
      ((let lexer = _menhir_env._menhir_lexer in
      let lexbuf = _menhir_env._menhir_lexbuf in
      let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 97 "_build/c_annot/parser.ml"
        ) = 
# 22 "src/c_annot/parser.mly"
          ( 1 )
# 101 "_build/c_annot/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = _menhir_stack in
        let (_v : (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 108 "_build/c_annot/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_v : (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 115 "_build/c_annot/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let ((_1 : (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 122 "_build/c_annot/parser.ml"
        )) : (
# 18 "src/c_annot/parser.mly"
      (Ast.annot)
# 126 "_build/c_annot/parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv2)) : 'freshtv4)) : 'freshtv6)) : 'freshtv8)) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv12)) : 'freshtv14))

# 233 "/home/ouadjaout/.opam/4.06.0/lib/menhir/standard.mly"
  

# 139 "_build/c_annot/parser.ml"
