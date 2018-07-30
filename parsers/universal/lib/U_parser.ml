
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TOK_string of (
# 63 "src/U_parser.mly"
       (string)
# 11 "_build/U_parser.ml"
  )
    | TOK_real of (
# 62 "src/U_parser.mly"
       (string)
# 16 "_build/U_parser.ml"
  )
    | TOK_int of (
# 61 "src/U_parser.mly"
       (string)
# 21 "_build/U_parser.ml"
  )
    | TOK_id of (
# 60 "src/U_parser.mly"
       (string)
# 26 "_build/U_parser.ml"
  )
    | TOK_char of (
# 64 "src/U_parser.mly"
       (char)
# 31 "_build/U_parser.ml"
  )
    | TOK_WHILE
    | TOK_TRUE
    | TOK_STRING
    | TOK_STAR
    | TOK_SEMICOLON
    | TOK_RPAREN
    | TOK_RETURN
    | TOK_REAL
    | TOK_RCURLY
    | TOK_RBRACKET
    | TOK_RAND
    | TOK_PRINT_ALL
    | TOK_PRINT
    | TOK_PLUS
    | TOK_NOT_EQUAL
    | TOK_MINUS
    | TOK_LPAREN
    | TOK_LESS_EQUAL
    | TOK_LESS
    | TOK_LCURLY
    | TOK_LBRACKET
    | TOK_INT
    | TOK_IF
    | TOK_GREATER_EQUAL
    | TOK_GREATER
    | TOK_FOR
    | TOK_FALSE
    | TOK_EXCLAIM
    | TOK_EQUAL_EQUAL
    | TOK_EQUAL
    | TOK_EOF
    | TOK_ELSE
    | TOK_DIVIDE
    | TOK_CONCAT
    | TOK_COMMA
    | TOK_CIRC
    | TOK_CHAR
    | TOK_BAR_BAR
    | TOK_BAR
    | TOK_ASSERT
    | TOK_ARRAY
    | TOK_AND_AND
  
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
  | MenhirState168
  | MenhirState167
  | MenhirState165
  | MenhirState164
  | MenhirState161
  | MenhirState159
  | MenhirState156
  | MenhirState154
  | MenhirState153
  | MenhirState150
  | MenhirState145
  | MenhirState142
  | MenhirState141
  | MenhirState140
  | MenhirState137
  | MenhirState132
  | MenhirState128
  | MenhirState123
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState111
  | MenhirState108
  | MenhirState107
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState20
  | MenhirState17
  | MenhirState10
  | MenhirState7
  | MenhirState0

# 5 "src/U_parser.mly"
  
    open U_ast
    let add_declaration decl prog =
      {prog with gvars = decl :: prog.gvars}
    let add_fundec fundec prog =
      {prog with funs = fundec :: prog.funs}

# 194 "_build/U_parser.ml"

let rec _menhir_goto_list_ext_declaration__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_ext_declaration__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv651 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ASSERT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_IF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LCURLY ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv652)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv653 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_declaration), _startpos_x0_), _, (xs : 'tv_list_ext_declaration__)) = _menhir_stack in
        let _v : 'tv_list_ext_declaration__ = let x =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 265 "_build/U_parser.ml"
          
        in
        
# 187 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 271 "_build/U_parser.ml"
         in
        _menhir_goto_list_ext_declaration__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)) : 'freshtv656)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_TOK_SEMICOLON_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_option_TOK_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv637 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv635 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__9_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_9 : 'tv_option_TOK_SEMICOLON_) : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_var), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_), _), _endpos_x2_, _, (x2 : 'tv_expr), _startpos_x2_), _endpos_x3_, _, (x3 : 'tv_stat), _startpos_x3_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__9_ in
        let _v : 'tv_stat = let st =
          let _endpos_x_ = _endpos_x3_ in
          let _startpos_x_ = _startpos_x3_ in
          let x = x3 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 307 "_build/U_parser.ml"
          
        in
        let e2 =
          let _endpos_x_ = _endpos_x2_ in
          let _startpos_x_ = _startpos_x2_ in
          let x = x2 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 319 "_build/U_parser.ml"
          
        in
        let e1 =
          let _endpos_x_ = _endpos_x1_ in
          let _startpos_x_ = _startpos_x1_ in
          let x = x1 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 331 "_build/U_parser.ml"
          
        in
        let v =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 343 "_build/U_parser.ml"
          
        in
        
# 238 "src/U_parser.mly"
   ( AST_for(v, e1, e2, st) )
# 349 "_build/U_parser.ml"
         in
        _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv636)) : 'freshtv638)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv641 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv639 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__8_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_8 : 'tv_option_TOK_SEMICOLON_) : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos__4_, _), _endpos_x1_, _, (x1 : 'tv_stat), _startpos_x1_), _), _endpos_x2_, _, (x2 : 'tv_stat), _startpos_x2_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__8_ in
        let _v : 'tv_stat = let t =
          let _endpos_x_ = _endpos_x2_ in
          let _startpos_x_ = _startpos_x2_ in
          let x = x2 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 379 "_build/U_parser.ml"
          
        in
        let s =
          let _endpos_x_ = _endpos_x1_ in
          let _startpos_x_ = _startpos_x1_ in
          let x = x1 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 391 "_build/U_parser.ml"
          
        in
        let e =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 403 "_build/U_parser.ml"
          
        in
        
# 226 "src/U_parser.mly"
  ( AST_if (e, s, Some t) )
# 409 "_build/U_parser.ml"
         in
        _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv640)) : 'freshtv642)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv645 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv643 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_option_TOK_SEMICOLON_) : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos__4_, _), _endpos_x1_, _, (x1 : 'tv_stat), _startpos_x1_) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_stat = let s =
          let _endpos_x_ = _endpos_x1_ in
          let _startpos_x_ = _startpos_x1_ in
          let x = x1 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 438 "_build/U_parser.ml"
          
        in
        let e =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 450 "_build/U_parser.ml"
          
        in
        
# 223 "src/U_parser.mly"
  ( AST_if (e, s, None) )
# 456 "_build/U_parser.ml"
         in
        _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv644)) : 'freshtv646)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv649 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv647 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_option_TOK_SEMICOLON_) : 'tv_option_TOK_SEMICOLON_) = _v in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos__4_, _), _endpos_x1_, _, (x1 : 'tv_stat), _startpos_x1_) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_stat = let s =
          let _endpos_x_ = _endpos_x1_ in
          let _startpos_x_ = _startpos_x1_ in
          let x = x1 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 485 "_build/U_parser.ml"
          
        in
        let e =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 497 "_build/U_parser.ml"
          
        in
        
# 229 "src/U_parser.mly"
  ( AST_while (e, s) )
# 503 "_build/U_parser.ml"
         in
        _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv648)) : 'freshtv650)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_ext_declaration__ = 
# 185 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 514 "_build/U_parser.ml"
     in
    _menhir_goto_list_ext_declaration__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_option_TOK_SEMICOLON_ = 
# 100 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( None )
# 524 "_build/U_parser.ml"
     in
    _menhir_goto_option_TOK_SEMICOLON_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run112 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv633) = Obj.magic _menhir_stack in
    let (_endpos_x_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _endpos = _endpos_x_ in
    let _v : 'tv_option_TOK_SEMICOLON_ = 
# 102 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( Some x )
# 540 "_build/U_parser.ml"
     in
    _menhir_goto_option_TOK_SEMICOLON_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv634)

and _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_tvar__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv625 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) = _v in
        ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_tvar), _startpos_x0_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__ = let x =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 567 "_build/U_parser.ml"
          
        in
        
# 217 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 573 "_build/U_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_tvar__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv626)) : 'freshtv628)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv629) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ = 
# 130 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x )
# 588 "_build/U_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv630)) : 'freshtv632)
    | _ ->
        _menhir_fail ()

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_prog -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv607 * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv603 * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv601 * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (t : 'tv_prog)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 614 "_build/U_parser.ml"
            ) = 
# 89 "src/U_parser.mly"
                     ( t )
# 618 "_build/U_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv599) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 626 "_build/U_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv597) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 634 "_build/U_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv595) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 642 "_build/U_parser.ml"
            )) : (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 646 "_build/U_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv596)) : 'freshtv598)) : 'freshtv600)) : 'freshtv602)) : 'freshtv604)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv605 * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv606)) : 'freshtv608)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv609 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_fundec), _startpos_x0_), _endpos__2_, _), _, (prg : 'tv_prog)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_prog = let d =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 672 "_build/U_parser.ml"
          
        in
        
# 255 "src/U_parser.mly"
                                       (add_fundec d prg)
# 678 "_build/U_parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)) : 'freshtv612)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv615 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * Lexing.position * _menhir_state) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_declaration), _startpos_x0_), _endpos__2_, _), _, (prg : 'tv_prog)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_prog = let d =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 697 "_build/U_parser.ml"
          
        in
        
# 253 "src/U_parser.mly"
                                            (add_declaration d prg)
# 703 "_build/U_parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_declaration), _startpos_x0_), _, (prg : 'tv_prog)) = _menhir_stack in
        let _v : 'tv_prog = let d =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 721 "_build/U_parser.ml"
          
        in
        
# 252 "src/U_parser.mly"
                              (add_declaration d prg)
# 727 "_build/U_parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv623 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv621 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) * _menhir_state * 'tv_prog) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_fundec), _startpos_x0_), _, (prg : 'tv_prog)) = _menhir_stack in
        let _v : 'tv_prog = let d =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 745 "_build/U_parser.ml"
          
        in
        
# 254 "src/U_parser.mly"
                         (add_fundec d prg)
# 751 "_build/U_parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)) : 'freshtv624)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv593 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv589 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_LCURLY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv585 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_CHAR ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_REAL ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_STRING ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_ASSERT | TOK_BAR | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv587 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv591 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv592)) : 'freshtv594)

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_CHAR ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_REAL ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_STRING ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv583 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv579 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv577 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___) = Obj.magic _menhir_stack in
        let (_endpos__4_ : Lexing.position) = _endpos in
        ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_var), _startpos_x0_), _startpos__2_), _, (xs0 : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _startpos = _startpos_x0_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_expr = let args =
          let xs = xs0 in
          
# 206 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( xs )
# 860 "_build/U_parser.ml"
          
        in
        let va =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 872 "_build/U_parser.ml"
          
        in
        
# 178 "src/U_parser.mly"
   ( AST_fun_call(va, args))
# 878 "_build/U_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv578)) : 'freshtv580)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv581 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)) : 'freshtv584)

and _menhir_goto_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState159 | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CHAR ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_INT ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_REAL ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STRING ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_ASSERT | TOK_BAR | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv572)
    | MenhirState0 | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ASSERT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_CHAR ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_IF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_INT ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LCURLY ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_REAL ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv573 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState167 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_ASSERT ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_CHAR ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_REAL ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_STRING ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EOF ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv574)
        | TOK_STRING ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState167 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EOF ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167) : 'freshtv576)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stat : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_stat -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv543 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_SEMICOLON ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111
        | TOK_ASSERT | TOK_BAR | TOK_ELSE | TOK_EOF | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LBRACKET | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RCURLY | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv544)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv547 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv545 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState118 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_ASSERT ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv546)
        | TOK_SEMICOLON ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118
        | TOK_ASSERT | TOK_BAR | TOK_EOF | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LBRACKET | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RCURLY | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv548)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv549 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_SEMICOLON ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120
        | TOK_ASSERT | TOK_BAR | TOK_ELSE | TOK_EOF | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LBRACKET | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RCURLY | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv550)
    | MenhirState161 | MenhirState123 | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv551 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ASSERT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_IF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LCURLY ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EOF | TOK_RCURLY ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv552)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv553 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_SEMICOLON ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128
        | TOK_ASSERT | TOK_BAR | TOK_ELSE | TOK_EOF | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LBRACKET | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RCURLY | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv554)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv567 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_RCURLY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv563 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv561 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__9_ : Lexing.position) = _endpos in
            ((let ((((((((_menhir_stack, _menhir_s, (t : 'tv_typ), _startpos_t_), _endpos_f_, _, (f : 'tv_var), _startpos_f_), _startpos__3_), _, (xs0 : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___)), _endpos__5_), _endpos__6_, _startpos__6_), _, (ldec : 'tv_list_ext_declaration__)), _endpos_x0_, _, (x0 : 'tv_stat), _startpos_x0_) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__9_ in
            let _v : 'tv_fundec = let st =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1235 "_build/U_parser.ml"
              
            in
            let args =
              let xs = xs0 in
              
# 206 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( xs )
# 1243 "_build/U_parser.ml"
              
            in
            
# 245 "src/U_parser.mly"
({funname = f; parameters = args; body=st ; locvars = ldec; return_type = t})
# 1249 "_build/U_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv559) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_fundec) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv557 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_ASSERT ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_CHAR ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_REAL ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv555 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_menhir_s : _menhir_state) = MenhirState164 in
                ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | TOK_ASSERT ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_BAR ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CHAR ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_EXCLAIM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RAND ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_REAL ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_STRING ->
                    _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_char _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_int _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_real _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_string _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_EOF ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv556)
            | TOK_STRING ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EOF ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164) : 'freshtv558)) : 'freshtv560)) : 'freshtv562)) : 'freshtv564)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv565 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_ASSERT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_IF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LCURLY ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EOF ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv570)
    | _ ->
        _menhir_fail ()

and _menhir_reduce16 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let e =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1457 "_build/U_parser.ml"
      
    in
    let o =
      let _1 = _10 in
      
# 98 "src/U_parser.mly"
                     ( AST_UNARY_PLUS )
# 1465 "_build/U_parser.ml"
      
    in
    
# 163 "src/U_parser.mly"
    ( AST_unary (o,e) )
# 1471 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce17 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let e =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1490 "_build/U_parser.ml"
      
    in
    let o =
      let _1 = _10 in
      
# 99 "src/U_parser.mly"
                     ( AST_UNARY_MINUS )
# 1498 "_build/U_parser.ml"
      
    in
    
# 163 "src/U_parser.mly"
    ( AST_unary (o,e) )
# 1504 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState132 | MenhirState104 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv537) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ = 
# 130 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x )
# 1523 "_build/U_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)) : 'freshtv538)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv541 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv539 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__) = _v in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos__2_, _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ = let x =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1546 "_build/U_parser.ml"
          
        in
        
# 217 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1552 "_build/U_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)
    | _ ->
        _menhir_fail ()

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run50 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv533 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos__4_ : Lexing.position) = _endpos in
    let (_ : _menhir_state) = _menhir_s in
    ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _, _startpos__2_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos__4_ in
    let _v : 'tv_expr = let e2 =
      let _endpos_x_ = _endpos_x1_ in
      let _startpos_x_ = _startpos_x1_ in
      let x = x1 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1618 "_build/U_parser.ml"
      
    in
    let e1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1630 "_build/U_parser.ml"
      
    in
    
# 175 "src/U_parser.mly"
   ( AST_array_access(e1, e2) )
# 1636 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv534)

and _menhir_reduce22 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _, _startpos__10_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
    let _10 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x1_ in
    let _v : 'tv_expr = let e2 =
      let _endpos_x_ = _endpos_x1_ in
      let _startpos_x_ = _startpos_x1_ in
      let x = x1 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1655 "_build/U_parser.ml"
      
    in
    let o =
      let _1 = _10 in
      
# 108 "src/U_parser.mly"
                     ( AST_MINUS )
# 1663 "_build/U_parser.ml"
      
    in
    let e1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1675 "_build/U_parser.ml"
      
    in
    
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 1681 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce21 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _, _startpos__10_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
    let _10 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x1_ in
    let _v : 'tv_expr = let e2 =
      let _endpos_x_ = _endpos_x1_ in
      let _startpos_x_ = _startpos_x1_ in
      let x = x1 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1700 "_build/U_parser.ml"
      
    in
    let o =
      let _1 = _10 in
      
# 107 "src/U_parser.mly"
                     ( AST_PLUS )
# 1708 "_build/U_parser.ml"
      
    in
    let e1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1720 "_build/U_parser.ml"
      
    in
    
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 1726 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv527) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ = 
# 130 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x )
# 1745 "_build/U_parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv529 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__) = _v in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__ = let x =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 1768 "_build/U_parser.ml"
          
        in
        
# 217 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 1774 "_build/U_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run40 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run38 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run44 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState44 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run42 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run46 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run51 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run63 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_sign_int_literal : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_sign_int_literal -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv515 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_MINUS ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv513 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv523 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position)) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv519 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position)) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv517 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position)) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__6_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_sign_int_literal), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_sign_int_literal), _startpos_x1_) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__6_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2389 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2401 "_build/U_parser.ml"
              
            in
            
# 169 "src/U_parser.mly"
    ( AST_rand (e1, e2) )
# 2407 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv518)) : 'freshtv520)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv521 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position)) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv509 * _menhir_state * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (xs0 : 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_array_constant = let l =
          let xs = xs0 in
          
# 206 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( xs )
# 2446 "_build/U_parser.ml"
          
        in
        
# 137 "src/U_parser.mly"
    (Array.of_list l)
# 2452 "_build/U_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv501) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_array_constant) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv499) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_array_constant) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497) = Obj.magic _menhir_stack in
        let (_endpos_x0_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x0 : 'tv_array_constant) : 'tv_array_constant) = _v in
        let (_startpos_x0_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_x0_ in
        let _endpos = _endpos_x0_ in
        let _v : 'tv_expr = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2483 "_build/U_parser.ml"
          
        in
        
# 155 "src/U_parser.mly"
    ( AST_array_const _1)
# 2489 "_build/U_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv498)) : 'freshtv500)) : 'freshtv502)) : 'freshtv504)) : 'freshtv506)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv507 * _menhir_state * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce76 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * (
# 60 "src/U_parser.mly"
       (string)
# 2508 "_build/U_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t : 'tv_typ), _startpos_t_), _endpos_v_, _, (v : (
# 60 "src/U_parser.mly"
       (string)
# 2514 "_build/U_parser.ml"
    )), _startpos_v_) = _menhir_stack in
    let _startpos = _startpos_t_ in
    let _endpos = _endpos_v_ in
    let _v : 'tv_tvar = 
# 192 "src/U_parser.mly"
                 ( t, v )
# 2521 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv495) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tvar) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState145 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_CHAR ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_REAL ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_STRING ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv478)
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv479 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_tvar), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_TOK_COMMA_ext_tvar__ = let x =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2570 "_build/U_parser.ml"
              
            in
            
# 215 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [ x ] )
# 2576 "_build/U_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_tvar__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv481 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState0 | MenhirState159 | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv485 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv486)
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv489 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv487 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_tvar), _startpos_x0_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declaration = let tv =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2651 "_build/U_parser.ml"
              
            in
            
# 196 "src/U_parser.mly"
    ((tv, None))
# 2657 "_build/U_parser.ml"
             in
            _menhir_goto_declaration _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv488)) : 'freshtv490)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv491 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv492)) : 'freshtv494)
    | _ ->
        _menhir_fail ()) : 'freshtv496)

and _menhir_goto_list_ext_stat__ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_ext_stat__ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState161 | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv457 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_ext_stat__) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_ext_stat__) : 'tv_list_ext_stat__) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_stat), _startpos_x0_) = _menhir_stack in
        let _startpos = _startpos_x0_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_ext_stat__ = let x =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2698 "_build/U_parser.ml"
          
        in
        
# 187 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( x :: xs )
# 2704 "_build/U_parser.ml"
         in
        _menhir_goto_list_ext_stat__ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv456)) : 'freshtv458)
    | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState0 | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_ext_stat__) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473) = Obj.magic _menhir_stack in
        let (_endpos_l_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((l : 'tv_list_ext_stat__) : 'tv_list_ext_stat__) = _v in
        let (_startpos_l_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_l_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_block_no_curly = 
# 211 "src/U_parser.mly"
      (AST_block l)
# 2725 "_build/U_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_block_no_curly) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState88 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_RCURLY ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv461 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv459 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_l_, _, (l : 'tv_block_no_curly), _startpos_l_) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_stat = 
# 217 "src/U_parser.mly"
  ( l )
# 2757 "_build/U_parser.ml"
                 in
                _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv460)) : 'freshtv462)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv463 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
        | MenhirState0 | MenhirState164 | MenhirState165 | MenhirState167 | MenhirState168 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv469 * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv467 * Lexing.position * _menhir_state * 'tv_block_no_curly * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_block_no_curly), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_prog = let st =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2782 "_build/U_parser.ml"
              
            in
            
# 251 "src/U_parser.mly"
       ({gvars = []; funs = [] ; main = st})
# 2788 "_build/U_parser.ml"
             in
            _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)) : 'freshtv470)
        | _ ->
            _menhir_fail ()) : 'freshtv472)) : 'freshtv474)) : 'freshtv476)
    | _ ->
        _menhir_fail ()

and _menhir_reduce82 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 60 "src/U_parser.mly"
       (string)
# 2799 "_build/U_parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_v_, _menhir_s, (v : (
# 60 "src/U_parser.mly"
       (string)
# 2805 "_build/U_parser.ml"
    )), _startpos_v_) = _menhir_stack in
    let _startpos = _startpos_v_ in
    let _endpos = _endpos_v_ in
    let _v : 'tv_var = 
# 141 "src/U_parser.mly"
    (v)
# 2812 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv453) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_var) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState161 | MenhirState156 | MenhirState153 | MenhirState0 | MenhirState132 | MenhirState84 | MenhirState123 | MenhirState88 | MenhirState119 | MenhirState92 | MenhirState115 | MenhirState99 | MenhirState107 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState90 | MenhirState85 | MenhirState7 | MenhirState20 | MenhirState21 | MenhirState22 | MenhirState77 | MenhirState23 | MenhirState25 | MenhirState68 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState51 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState28 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv431 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv429) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState28 in
                ((let _v : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ = 
# 128 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 2871 "_build/U_parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_expr___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv432)
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_CONCAT | TOK_DIVIDE | TOK_EQUAL | TOK_EQUAL_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_GREATER | TOK_GREATER_EQUAL | TOK_IF | TOK_LBRACKET | TOK_LCURLY | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_NOT_EQUAL | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_STAR | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv433 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_var), _startpos_x0_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 2893 "_build/U_parser.ml"
              
            in
            
# 160 "src/U_parser.mly"
    ( AST_identifier _1)
# 2899 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv435 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv439 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv440)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)) : 'freshtv444)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv447 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_CHAR ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_REAL ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_STRING ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState140 in
                ((let _v : 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ = 
# 128 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 2991 "_build/U_parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv449 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)
    | _ ->
        _menhir_fail ()) : 'freshtv454)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expr -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState68 | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv291 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState35 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv292)
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv293 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_TOK_COMMA_ext_expr__ = let x =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3099 "_build/U_parser.ml"
              
            in
            
# 215 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [ x ] )
# 3105 "_build/U_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_TOK_COMMA_ext_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv296)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_DIVIDE | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_STAR | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv297 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3150 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 105 "src/U_parser.mly"
                     ( AST_MULTIPLY )
# 3158 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3170 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3176 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv300)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3233 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 114 "src/U_parser.mly"
                     ( AST_NOT_EQUAL )
# 3241 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3253 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3259 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv304)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv305 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv306)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv309 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3348 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 111 "src/U_parser.mly"
                     ( AST_LESS_EQUAL )
# 3356 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3368 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3374 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv308)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv310)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv312)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3463 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 109 "src/U_parser.mly"
                     ( AST_LESS )
# 3471 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3483 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3489 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv316)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RBRACKET ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv318)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3586 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 112 "src/U_parser.mly"
                     ( AST_GREATER_EQUAL )
# 3594 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3606 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3612 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv320)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv322)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3669 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 110 "src/U_parser.mly"
                     ( AST_GREATER )
# 3677 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3689 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3695 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv326)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv329 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3752 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 113 "src/U_parser.mly"
                     ( AST_EQUAL )
# 3760 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3772 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3778 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv330)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_DIVIDE | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_STAR | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3823 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 106 "src/U_parser.mly"
                     ( AST_DIVIDE )
# 3831 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3843 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3849 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv332)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv334)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TOK_ASSERT | TOK_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3906 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 115 "src/U_parser.mly"
                     ( AST_CONCAT )
# 3914 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3926 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 3932 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv336)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv338)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 3987 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 117 "src/U_parser.mly"
                     ( AST_OR )
# 3995 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4007 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 4013 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv342)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv345 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv343 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let e2 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4066 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 116 "src/U_parser.mly"
                     ( AST_AND )
# 4074 "_build/U_parser.ml"
              
            in
            let e1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4086 "_build/U_parser.ml"
              
            in
            
# 166 "src/U_parser.mly"
    ( AST_binary (o,e1,e2) )
# 4092 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv346)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState70 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv347 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            let (_startpos__3_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_expr = let e =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4133 "_build/U_parser.ml"
              
            in
            
# 172 "src/U_parser.mly"
    ( AST_len (e))
# 4139 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv348)) : 'freshtv350)
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv352)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv353 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let e =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4218 "_build/U_parser.ml"
              
            in
            let o =
              let _1 = _10 in
              
# 100 "src/U_parser.mly"
                     ( AST_NOT )
# 4226 "_build/U_parser.ml"
              
            in
            
# 163 "src/U_parser.mly"
    ( AST_unary (o,e) )
# 4232 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv354)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv356)
    | MenhirState132 | MenhirState77 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TOK_RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv357 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ = let x =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4288 "_build/U_parser.ml"
              
            in
            
# 215 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [ x ] )
# 4294 "_build/U_parser.ml"
             in
            _menhir_goto_separated_nonempty_list_TOK_SEMICOLON_ext_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv360)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState79 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expr), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_expr = 
# 145 "src/U_parser.mly"
    ( e )
# 4351 "_build/U_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv362)) : 'freshtv364)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv366)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv368)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TOK_AND_AND | TOK_ASSERT | TOK_BAR | TOK_BAR_BAR | TOK_COMMA | TOK_EQUAL | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_PLUS | TOK_RAND | TOK_RBRACKET | TOK_RETURN | TOK_RPAREN | TOK_SEMICOLON | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv370)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState83 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_ASSERT ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv372)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv374)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv379 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv377 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState86 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv375 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_stat = let e =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 4571 "_build/U_parser.ml"
              
            in
            
# 235 "src/U_parser.mly"
   ( AST_return e )
# 4577 "_build/U_parser.ml"
             in
            _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv376)) : 'freshtv378)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv380)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState91 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_ASSERT ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv382)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv384)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv387 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv385 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState97 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv386)
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv388)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv395 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_ASSERT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_IF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv389 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState99 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RBRACKET ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv390)
        | TOK_LCURLY ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv391 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState99 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv392)
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv393 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState99 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv394)
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv396)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TOK_ASSERT | TOK_BAR | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv398)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TOK_ASSERT | TOK_BAR | TOK_EXCLAIM | TOK_FALSE | TOK_FOR | TOK_IF | TOK_LCURLY | TOK_LPAREN | TOK_RAND | TOK_RETURN | TOK_TRUE | TOK_WHILE | TOK_char _ | TOK_id _ | TOK_int _ | TOK_real _ | TOK_string _ ->
            _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
        | TOK_EQUAL ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv400)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RBRACKET ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105
        | TOK_SEMICOLON ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv402)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState108 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv405 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv403 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos__4_, _) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__5_ in
                let _v : 'tv_stat = let e =
                  let _endpos_x_ = _endpos_x0_ in
                  let _startpos_x_ = _startpos_x0_ in
                  let x = x0 in
                  let _endpos = _endpos_x_ in
                  let _startpos = _startpos_x_ in
                  
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5137 "_build/U_parser.ml"
                  
                in
                
# 232 "src/U_parser.mly"
  ( AST_assert e )
# 5143 "_build/U_parser.ml"
                 in
                _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv404)) : 'freshtv406)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv407 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv412)
    | MenhirState0 | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState161 | MenhirState156 | MenhirState84 | MenhirState88 | MenhirState123 | MenhirState92 | MenhirState119 | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv413 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState114 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_BAR ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_EXCLAIM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RAND ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_char _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_int _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_real _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_string _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv414)
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv416)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState116 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_stat = let f =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5293 "_build/U_parser.ml"
              
            in
            let e =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5305 "_build/U_parser.ml"
              
            in
            
# 220 "src/U_parser.mly"
  ( AST_assign (e, f) )
# 5311 "_build/U_parser.ml"
             in
            _menhir_goto_stat _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv418)) : 'freshtv420)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv422)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_AND_AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_BAR_BAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_CONCAT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_DIVIDE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_EQUAL_EQUAL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_GREATER ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_GREATER_EQUAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_LBRACKET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_LESS_EQUAL ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT_EQUAL ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | TOK_PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState154 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_tvar), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_declaration = let e =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5376 "_build/U_parser.ml"
              
            in
            let tv =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5388 "_build/U_parser.ml"
              
            in
            
# 198 "src/U_parser.mly"
    ((tv, Some e))
# 5394 "_build/U_parser.ml"
             in
            _menhir_goto_declaration _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv424)) : 'freshtv426)
        | TOK_STAR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv428)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 61 "src/U_parser.mly"
       (string)
# 5409 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 61 "src/U_parser.mly"
       (string)
# 5420 "_build/U_parser.ml"
    )) : (
# 61 "src/U_parser.mly"
       (string)
# 5424 "_build/U_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_sign_int_literal = 
# 181 "src/U_parser.mly"
                       ( i )
# 5432 "_build/U_parser.ml"
     in
    _menhir_goto_sign_int_literal _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv290)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_int _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 61 "src/U_parser.mly"
       (string)
# 5449 "_build/U_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_i_ : Lexing.position) = _endpos in
        let ((i : (
# 61 "src/U_parser.mly"
       (string)
# 5459 "_build/U_parser.ml"
        )) : (
# 61 "src/U_parser.mly"
       (string)
# 5463 "_build/U_parser.ml"
        )) = _v in
        let (_startpos_i_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_sign_int_literal = 
# 182 "src/U_parser.mly"
                       ( i )
# 5473 "_build/U_parser.ml"
         in
        _menhir_goto_sign_int_literal _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv284)) : 'freshtv286)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_int _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 61 "src/U_parser.mly"
       (string)
# 5497 "_build/U_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_i_ : Lexing.position) = _endpos in
        let ((i : (
# 61 "src/U_parser.mly"
       (string)
# 5507 "_build/U_parser.ml"
        )) : (
# 61 "src/U_parser.mly"
       (string)
# 5511 "_build/U_parser.ml"
        )) = _v in
        let (_startpos_i_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_sign_int_literal = 
# 183 "src/U_parser.mly"
                       ( "-"^i )
# 5521 "_build/U_parser.ml"
         in
        _menhir_goto_sign_int_literal _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv278)) : 'freshtv280)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ = 
# 128 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 5537 "_build/U_parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_TOK_SEMICOLON_ext_expr___ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_boolean_constant : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_boolean_constant -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_boolean_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv273) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x0 : 'tv_boolean_constant) : 'tv_boolean_constant) = _v in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 5566 "_build/U_parser.ml"
      
    in
    
# 158 "src/U_parser.mly"
     (AST_bool_const _1)
# 5572 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState141 | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (t : 'tv_typ), _startpos_t_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : 'tv_typ = 
# 203 "src/U_parser.mly"
                                  ( AST_ARRAY t )
# 5601 "_build/U_parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv256)) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState164 | MenhirState167 | MenhirState168 | MenhirState165 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_id _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState137 in
            let (_v : (
# 60 "src/U_parser.mly"
       (string)
# 5625 "_build/U_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TOK_LPAREN ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)
            | TOK_EQUAL | TOK_SEMICOLON ->
                _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * (
# 60 "src/U_parser.mly"
       (string)
# 5643 "_build/U_parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv268)
    | MenhirState159 | MenhirState150 | MenhirState145 | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_id _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState142 in
            let (_v : (
# 60 "src/U_parser.mly"
       (string)
# 5665 "_build/U_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv272)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RBRACKET ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * Lexing.position * _menhir_state * 'tv_fundec * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * Lexing.position * _menhir_state * 'tv_declaration * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv79 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) * _menhir_state * 'tv_list_ext_declaration__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv81 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv85 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_TOK_COMMA_ext_tvar___) * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * Lexing.position * _menhir_state * 'tv_tvar * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state * 'tv_typ * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_typ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv99 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv103 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv105 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv107 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv115 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state * 'tv_stat * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv129 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv133 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv135 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv137 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv139 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv141 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv143 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv155 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv175 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv179 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv247 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_sign_int_literal * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv249 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv254)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_ext_stat__ = 
# 185 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
    ( [] )
# 6199 "_build/U_parser.ml"
     in
    _menhir_goto_list_ext_stat__ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 63 "src/U_parser.mly"
       (string)
# 6206 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv65) = Obj.magic _menhir_stack in
    let (_endpos_v_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 63 "src/U_parser.mly"
       (string)
# 6217 "_build/U_parser.ml"
    )) : (
# 63 "src/U_parser.mly"
       (string)
# 6221 "_build/U_parser.ml"
    )) = _v in
    let (_startpos_v_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_v_ in
    let _endpos = _endpos_v_ in
    let _v : 'tv_string_constant = 
# 130 "src/U_parser.mly"
               (v)
# 6229 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv63) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_string_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_string_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x0 : 'tv_string_constant) : 'tv_string_constant) = _v in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 6260 "_build/U_parser.ml"
      
    in
    
# 151 "src/U_parser.mly"
    ( AST_string_const _1)
# 6266 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv60)) : 'freshtv62)) : 'freshtv64)) : 'freshtv66)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 62 "src/U_parser.mly"
       (string)
# 6273 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
    let (_endpos_v_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 62 "src/U_parser.mly"
       (string)
# 6284 "_build/U_parser.ml"
    )) : (
# 62 "src/U_parser.mly"
       (string)
# 6288 "_build/U_parser.ml"
    )) = _v in
    let (_startpos_v_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_v_ in
    let _endpos = _endpos_v_ in
    let _v : 'tv_real_constant = 
# 127 "src/U_parser.mly"
             (v)
# 6296 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_real_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_real_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x0 : 'tv_real_constant) : 'tv_real_constant) = _v in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 6327 "_build/U_parser.ml"
      
    in
    
# 149 "src/U_parser.mly"
    ( AST_real_const _1)
# 6333 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv52)) : 'freshtv54)) : 'freshtv56)) : 'freshtv58)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 61 "src/U_parser.mly"
       (string)
# 6340 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_endpos_v_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((v : (
# 61 "src/U_parser.mly"
       (string)
# 6351 "_build/U_parser.ml"
    )) : (
# 61 "src/U_parser.mly"
       (string)
# 6355 "_build/U_parser.ml"
    )) = _v in
    let (_startpos_v_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_v_ in
    let _endpos = _endpos_v_ in
    let _v : 'tv_integer_constant = 
# 124 "src/U_parser.mly"
            (v)
# 6363 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_integer_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_integer_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x0 : 'tv_integer_constant) : 'tv_integer_constant) = _v in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 6394 "_build/U_parser.ml"
      
    in
    
# 147 "src/U_parser.mly"
    ( AST_int_const _1)
# 6400 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)) : 'freshtv50)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 60 "src/U_parser.mly"
       (string)
# 6407 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 64 "src/U_parser.mly"
       (char)
# 6417 "_build/U_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 64 "src/U_parser.mly"
       (char)
# 6428 "_build/U_parser.ml"
    )) : (
# 64 "src/U_parser.mly"
       (char)
# 6432 "_build/U_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_char_constant = 
# 133 "src/U_parser.mly"
             (c)
# 6440 "_build/U_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv39) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_char_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_char_constant) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x0 : 'tv_char_constant) : 'tv_char_constant) = _v in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 262 "src/U_parser.mly"
      ( x, (_startpos, _endpos) )
# 6471 "_build/U_parser.ml"
      
    in
    
# 153 "src/U_parser.mly"
    ( AST_char_const _1)
# 6477 "_build/U_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv36)) : 'freshtv38)) : 'freshtv40)) : 'freshtv42)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_boolean_constant = 
# 120 "src/U_parser.mly"
           (true)
# 6549 "_build/U_parser.ml"
     in
    _menhir_goto_boolean_constant _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv30)

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : 'tv_typ = 
# 204 "src/U_parser.mly"
             ( AST_STRING )
# 6565 "_build/U_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv28)

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : 'tv_typ = 
# 202 "src/U_parser.mly"
           ( AST_REAL )
# 6620 "_build/U_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv26)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_MINUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run88 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ASSERT ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FOR ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_IF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LCURLY ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RETURN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RCURLY ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_CHAR ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_REAL ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_STRING ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RBRACKET ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : 'tv_typ = 
# 201 "src/U_parser.mly"
          ( AST_INT )
# 6887 "_build/U_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv20)

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_boolean_constant = 
# 121 "src/U_parser.mly"
            (false)
# 6987 "_build/U_parser.ml"
     in
    _menhir_goto_boolean_constant _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : 'tv_typ = 
# 205 "src/U_parser.mly"
           ( AST_CHAR )
# 7042 "_build/U_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv8)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TOK_BAR ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_EXCLAIM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RAND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_char _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_int _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_real _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_string _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv4)
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

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 80 "src/U_parser.mly"
      (U_ast.prog)
# 7154 "_build/U_parser.ml"
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
    | TOK_ASSERT ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_BAR ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_CHAR ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EXCLAIM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FOR ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_IF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LCURLY ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RAND ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_REAL ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RETURN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_STRING ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_WHILE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_char _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_int _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_real _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_string _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 265 "src/U_parser.mly"
  

# 7231 "_build/U_parser.ml"

# 219 "/home/ouadjaout/.opam/4.05.0/lib/menhir/standard.mly"
  


# 7237 "_build/U_parser.ml"
