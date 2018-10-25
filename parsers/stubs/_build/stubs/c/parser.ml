
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TRUE
    | SUB
    | STRING of (
# 14 "src/stubs/c/parser.mly"
       (string)
# 13 "_build/stubs/c/parser.ml"
  )
    | SEMICOL
    | RSHIFT
    | RPAR
    | RETURN
    | REQUIRES
    | RBRACK
    | RBRACE
    | OR
    | NEW
    | NEQ
    | MUL
    | MOD
    | LT
    | LSHIFT
    | LPAR
    | LOGOR
    | LOGAND
    | LOCAL
    | LE
    | LBRACK
    | LBRACE
    | INT of (
# 12 "src/stubs/c/parser.mly"
       (Z.t)
# 39 "_build/stubs/c/parser.ml"
  )
    | IN
    | IMPLIES
    | IDENT of (
# 17 "src/stubs/c/parser.mly"
       (string)
# 46 "_build/stubs/c/parser.ml"
  )
    | GT
    | GE
    | FREE
    | FORALL
    | FLOAT of (
# 13 "src/stubs/c/parser.mly"
       (float)
# 55 "_build/stubs/c/parser.ml"
  )
    | FALSE
    | EXISTS
    | EQUAL
    | EQ
    | EOF
    | ENSURES
    | DOT
    | DIV
    | COMMA
    | COLON
    | CASE
    | BITXOR
    | BITOR
    | BITNOT
    | BITAND
    | ASSUMES
    | ASSIGNS
    | AND
    | ADD
  
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

and _menhir_state = 
  | MenhirState70
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState48
  | MenhirState46
  | MenhirState41
  | MenhirState38
  | MenhirState35
  | MenhirState32
  | MenhirState29
  | MenhirState27
  | MenhirState21
  | MenhirState18
  | MenhirState13
  | MenhirState10
  | MenhirState8
  | MenhirState2
  | MenhirState0

# 1 "src/stubs/c/parser.mly"
  
    open Ast

    let int_type = assert false

    let pos_to_loc pos = assert false

    let with_range (e: 'a) pos1 pos2 : 'a with_range = (e, (pos_to_loc pos1, pos_to_loc pos2))

# 128 "_build/stubs/c/parser.ml"

let rec _menhir_goto_assigns_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_assigns_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv283 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CASE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ENSURES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState38 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_requires_list), _startpos__1_), _, (_2 : 'tv_local_list)), _, (_3 : 'tv_assigns_list)) = _menhir_stack in
            let _4 = () in
            let _v : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 158 "_build/stubs/c/parser.ml"
            ) = let _endpos = _endpos__4_ in
            let _startpos = _startpos__1_ in
            
# 46 "src/stubs/c/parser.mly"
    (
      with_range {
          stub_requires = _1;
          stub_local    = _2;
          stub_assigns  = _3;
          stub_case     = [];
          stub_ensures  = [];
        }
      _startpos _endpos
    )
# 173 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv284)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv285 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 185 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ENSURES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv286)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state * 'tv_assigns) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv287 * _menhir_state * 'tv_assigns) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_assigns)), _, (_2 : 'tv_assigns_list)) = _menhir_stack in
        let _v : 'tv_assigns_list = 
# 105 "src/stubs/c/parser.mly"
                         ( _1 :: _2 )
# 205 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_assigns_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)) : 'freshtv290)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assigns : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_assigns -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_assigns) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGNS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | CASE | ENSURES | EOF ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv278)

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _endpos__2_), _, (_3 : 'tv_args)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_args = 
# 154 "src/stubs/c/parser.mly"
                    ( _1 :: _3 )
# 242 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)) : 'freshtv268)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_var)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_var)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_var)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_var)), _, (_3 : 'tv_args)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_local_value = 
# 101 "src/stubs/c/parser.mly"
                       ( LV_call (_1, _3) )
# 263 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_local_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_var)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | _ ->
        _menhir_fail ()

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_assigns_list = 
# 104 "src/stubs/c/parser.mly"
    ( [] )
# 281 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_assigns_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv262)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 153 "src/stubs/c/parser.mly"
    ( [] )
# 318 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "src/stubs/c/parser.mly"
       (Z.t)
# 325 "_build/stubs/c/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv259) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 12 "src/stubs/c/parser.mly"
       (Z.t)
# 335 "_build/stubs/c/parser.ml"
    )) : (
# 12 "src/stubs/c/parser.mly"
       (Z.t)
# 339 "_build/stubs/c/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 150 "src/stubs/c/parser.mly"
        ( E_int _1 )
# 344 "_build/stubs/c/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv222)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_args = 
# 155 "src/stubs/c/parser.mly"
         ( [ _1 ] )
# 381 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv230)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s), _endpos__2_), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_assigns = 
# 109 "src/stubs/c/parser.mly"
    ( {assign_target = _3; assign_range = None;} )
# 424 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv247 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv243 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv239 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv240)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv241 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv245 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv255 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv251 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv249 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _endpos__2_), _, (_3 : 'tv_expr)), _, (_5 : 'tv_expr)), _, (_8 : 'tv_expr)) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_assigns = 
# 112 "src/stubs/c/parser.mly"
    ( {assign_target = _3; assign_range = Some (_5, _8);} )
# 494 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv253 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()) : 'freshtv258)) : 'freshtv260)

and _menhir_goto_local_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_local_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv219 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv215 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv213 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        ((let ((((_menhir_stack, _menhir_s), _endpos__2_), _, (_3 : 'tv_var)), _, (_5 : 'tv_local_value)) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_local = 
# 92 "src/stubs/c/parser.mly"
    (
      {
        local_var = _3;
        local_value = _5;
      }
    )
# 536 "_build/stubs/c/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_local) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_local) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | ASSIGNS | CASE | ENSURES | EOF ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv210)) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv217 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)

and _menhir_goto_assumes_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_assumes_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv203 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 573 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | REQUIRES ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSIGNS | ENSURES | LOCAL ->
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv204)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * Lexing.position * _menhir_state * 'tv_assumes) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * Lexing.position * _menhir_state * 'tv_assumes) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_assumes)), _endpos__2_, _, (_2 : 'tv_assumes_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_assumes_list = 
# 133 "src/stubs/c/parser.mly"
                         ( _1 :: _2 )
# 596 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_assumes_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv206)) : 'freshtv208)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 605 "_build/stubs/c/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 614 "_build/stubs/c/parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 622 "_build/stubs/c/parser.ml"
    )) : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 626 "_build/stubs/c/parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv200)) : 'freshtv202)

and _menhir_goto_case_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_case_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv193 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv189 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv187 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, (_1 : 'tv_requires_list), _startpos__1_), _, (_2 : 'tv_local_list)), _, (_3 : 'tv_assigns_list)), _, (_4 : 'tv_case_list)) = _menhir_stack in
            let _5 = () in
            let _v : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 652 "_build/stubs/c/parser.ml"
            ) = 
# 58 "src/stubs/c/parser.mly"
    (
      {
        stub_requires = _1;
        stub_local    = _2;
        stub_assigns  = _3;
        stub_case     = _4;
        stub_ensures  = [];
      }
    )
# 664 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv191 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state * 'tv_case) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv195 * _menhir_state * 'tv_case) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_case)), _, (_2 : 'tv_case_list)) = _menhir_stack in
        let _v : 'tv_case_list = 
# 116 "src/stubs/c/parser.mly"
                   ( _1 :: _2 )
# 683 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_case_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)) : 'freshtv198)
    | _ ->
        _menhir_fail ()

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRING _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 14 "src/stubs/c/parser.mly"
       (string)
# 701 "_build/stubs/c/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv179 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 712 "_build/stubs/c/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSUMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | ASSIGNS | ENSURES | LOCAL | REQUIRES ->
                _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv181 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 734 "_build/stubs/c/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)

and _menhir_goto_local_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_local_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGNS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | CASE | ENSURES | EOF ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv172)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv173 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 769 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGNS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ENSURES ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv174)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state * 'tv_local) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_local) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_local)), _, (_2 : 'tv_local_list)) = _menhir_stack in
        let _v : 'tv_local_list = 
# 88 "src/stubs/c/parser.mly"
                     ( _1 :: _2 )
# 791 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_local_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "src/stubs/c/parser.mly"
       (string)
# 800 "_build/stubs/c/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 17 "src/stubs/c/parser.mly"
       (string)
# 810 "_build/stubs/c/parser.ml"
    )) : (
# 17 "src/stubs/c/parser.mly"
       (string)
# 814 "_build/stubs/c/parser.ml"
    )) = _v in
    ((let _v : 'tv_var = 
# 158 "src/stubs/c/parser.mly"
          ( _1 )
# 819 "_build/stubs/c/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_var) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv155 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
            | NEW ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState13 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
                    let (_v : (
# 17 "src/stubs/c/parser.mly"
       (string)
# 855 "_build/stubs/c/parser.ml"
                    )) = _v in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
                    let ((_1 : (
# 17 "src/stubs/c/parser.mly"
       (string)
# 863 "_build/stubs/c/parser.ml"
                    )) : (
# 17 "src/stubs/c/parser.mly"
       (string)
# 867 "_build/stubs/c/parser.ml"
                    )) = _v in
                    ((let _v : 'tv_resource = 
# 161 "src/stubs/c/parser.mly"
          ( _1 )
# 872 "_build/stubs/c/parser.ml"
                     in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv145) = _menhir_stack in
                    let (_v : 'tv_resource) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
                    let (_v : 'tv_resource) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv141 * _menhir_state) = Obj.magic _menhir_stack in
                    let ((_2 : 'tv_resource) : 'tv_resource) = _v in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    let _1 = () in
                    let _v : 'tv_local_value = 
# 100 "src/stubs/c/parser.mly"
                 ( LV_new _2 )
# 888 "_build/stubs/c/parser.ml"
                     in
                    _menhir_goto_local_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv156)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv162)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | _ ->
        _menhir_fail ()) : 'freshtv168)) : 'freshtv170)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_assumes_list = 
# 132 "src/stubs/c/parser.mly"
    ( [] )
# 945 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_assumes_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv138)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)

and _menhir_goto_ensures_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ensures_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv123 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 988 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv121 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 994 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let ((((((((_menhir_stack, _menhir_s), (_2 : (
# 14 "src/stubs/c/parser.mly"
       (string)
# 999 "_build/stubs/c/parser.ml"
        ))), _endpos__3_), _endpos__4_, _, (_4 : 'tv_assumes_list)), _, (_5 : 'tv_requires_list), _startpos__5_), _, (_6 : 'tv_local_list)), _, (_7 : 'tv_assigns_list)), _, (_8 : 'tv_ensures_list)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_case = 
# 120 "src/stubs/c/parser.mly"
    (
      {
        case_label    = _2;
        case_assumes  = _4;
        case_requires = _5;
        case_local    = _6;
        case_assigns  = _7;
        case_ensures  = _8;
      }
    )
# 1015 "_build/stubs/c/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CASE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv115 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_case)) = _menhir_stack in
            let _v : 'tv_case_list = 
# 115 "src/stubs/c/parser.mly"
         ( [ _1 ] )
# 1036 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_case_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)) : 'freshtv124)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * 'tv_ensures) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state * 'tv_ensures) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ensures)), _, (_2 : 'tv_ensures_list)) = _menhir_stack in
        let _v : 'tv_ensures_list = 
# 140 "src/stubs/c/parser.mly"
                         ( _1 :: _2 )
# 1052 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_ensures_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)) : 'freshtv128)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv135 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv131 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv129 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, (_1 : 'tv_requires_list), _startpos__1_), _, (_2 : 'tv_local_list)), _, (_3 : 'tv_assigns_list)), _, (_4 : 'tv_ensures_list)) = _menhir_stack in
            let _5 = () in
            let _v : (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 1073 "_build/stubs/c/parser.ml"
            ) = 
# 69 "src/stubs/c/parser.mly"
    (
      {
        stub_requires = _1;
        stub_local    = _2;
        stub_assigns  = _3;
        stub_case     = [];
        stub_ensures  = _4;
      }
    )
# 1085 "_build/stubs/c/parser.ml"
             in
            _menhir_goto_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv130)) : 'freshtv132)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv133 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv112)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_local_list = 
# 87 "src/stubs/c/parser.mly"
    ( [] )
# 1138 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_local_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)

and _menhir_goto_formula : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_formula -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv75 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv73 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _, (_3 : 'tv_formula)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_requires = 
# 84 "src/stubs/c/parser.mly"
                                   ( _3 )
# 1197 "_build/stubs/c/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_requires) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * Lexing.position * _menhir_state * 'tv_requires * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | REQUIRES ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSIGNS | CASE | ENSURES | EOF | LOCAL ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv70)) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv77 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv89 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv87 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s), _endpos__2_), _, (_3 : 'tv_formula)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_ensures = 
# 143 "src/stubs/c/parser.mly"
                                  ( _3 )
# 1247 "_build/stubs/c/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv85) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_ensures) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv83 * _menhir_state * 'tv_ensures) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ENSURES ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | CASE | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_ensures) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ensures)) = _menhir_stack in
                let _v : 'tv_ensures_list = 
# 139 "src/stubs/c/parser.mly"
            ( [ _1 ] )
# 1268 "_build/stubs/c/parser.ml"
                 in
                _menhir_goto_ensures_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv84)) : 'freshtv86)) : 'freshtv88)) : 'freshtv90)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv91 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv101 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv99 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s), _endpos__2_), _, (_3 : 'tv_formula)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__4_ in
            let _v : 'tv_assumes = 
# 136 "src/stubs/c/parser.mly"
                                  ( _3 )
# 1304 "_build/stubs/c/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv97) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_assumes) = _v in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv95 * Lexing.position * _menhir_state * 'tv_assumes) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSUMES ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | ASSIGNS | ENSURES | LOCAL | REQUIRES ->
                _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv96)) : 'freshtv98)) : 'freshtv100)) : 'freshtv102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv103 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_formula) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
    | _ ->
        _menhir_fail ()

and _menhir_goto_requires_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_requires_list -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | ASSIGNS | CASE | ENSURES | EOF ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv62)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv63 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1358 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ASSIGNS | ENSURES ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv64)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * Lexing.position * _menhir_state * 'tv_requires * Lexing.position) * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * Lexing.position * _menhir_state * 'tv_requires * Lexing.position) * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_requires), _startpos__1_), _, (_2 : 'tv_requires_list), _startpos__2_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : 'tv_requires_list = 
# 81 "src/stubs/c/parser.mly"
                           ( _1 :: _2 )
# 1381 "_build/stubs/c/parser.ml"
         in
        _menhir_goto_requires_list _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv66)) : 'freshtv68)
    | _ ->
        _menhir_fail ()

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_formula = 
# 146 "src/stubs/c/parser.mly"
          ( F_bool true )
# 1397 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _menhir_s _v) : 'freshtv60)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_formula = 
# 147 "src/stubs/c/parser.mly"
          ( F_bool false )
# 1411 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _menhir_s _v) : 'freshtv58)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * Lexing.position * _menhir_state * 'tv_assumes) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * Lexing.position * _menhir_state * 'tv_requires * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * 'tv_local) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * 'tv_assigns) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * 'tv_ensures) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv19 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1453 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv21 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1462 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv23 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1471 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv25 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1480 "_build/stubs/c/parser.ml"
        )) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv29 * _menhir_state) * (
# 14 "src/stubs/c/parser.mly"
       (string)
# 1494 "_build/stubs/c/parser.ml"
        )) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv35 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv37 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_requires_list * Lexing.position) * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_expr) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_var)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_var)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_requires_list * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv56)

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_requires_list = 
# 80 "src/stubs/c/parser.mly"
    ( [] )
# 1569 "_build/stubs/c/parser.ml"
     in
    _menhir_goto_requires_list _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and stub : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 40 "src/stubs/c/parser.mly"
      (Ast.stub Ast.with_range)
# 1618 "_build/stubs/c/parser.ml"
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
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REQUIRES ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGNS | CASE | ENSURES | EOF | LOCAL ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/ouadjaout/.opam/4.06.0/lib/menhir/standard.mly"
  

# 1649 "_build/stubs/c/parser.ml"
