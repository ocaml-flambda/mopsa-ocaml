
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNSIGNED
    | UNION
    | TRUE
    | TLONG
    | TINT
    | TFLOAT
    | TDOUBLE
    | TCHAR
    | STRUCT
    | STRING of (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 20 "_build/c_stubs_parser/parser.ml"
  )
    | STAR
    | SIZE
    | SIGNED
    | SEMICOL
    | RSHIFT
    | RPAR
    | RETURN
    | REQUIRES
    | RBRACK
    | PREDICATE
    | PLUS
    | OR
    | OLD
    | OFFSET
    | NOT
    | NEW
    | NEQ
    | MOD
    | MINUS
    | LT
    | LSHIFT
    | LPAR
    | LOR
    | LOCAL
    | LNOT
    | LE
    | LBRACK
    | LAND
    | INT of (
# 23 "src/c_stubs_parser/parser.mly"
       (Z.t)
# 53 "_build/c_stubs_parser/parser.ml"
  )
    | IN
    | IMPLIES
    | IDENT of (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 60 "_build/c_stubs_parser/parser.ml"
  )
    | GT
    | GE
    | FREE
    | FORALL
    | FLOAT of (
# 24 "src/c_stubs_parser/parser.mly"
       (float)
# 69 "_build/c_stubs_parser/parser.ml"
  )
    | FALSE
    | EXISTS
    | EQ
    | EOF
    | ENSURES
    | DOT
    | DIV
    | CONST
    | COLON
    | CHAR of (
# 25 "src/c_stubs_parser/parser.mly"
       (char)
# 83 "_build/c_stubs_parser/parser.ml"
  )
    | CASE
    | BXOR
    | BOR
    | BNOT
    | BASE
    | BAND
    | ASSUMES
    | ASSIGNS
    | ASSIGN
    | ARROW
    | AND
    | ADDROF
  
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
  | MenhirState215
  | MenhirState212
  | MenhirState211
  | MenhirState209
  | MenhirState205
  | MenhirState203
  | MenhirState201
  | MenhirState199
  | MenhirState197
  | MenhirState193
  | MenhirState191
  | MenhirState188
  | MenhirState185
  | MenhirState182
  | MenhirState180
  | MenhirState174
  | MenhirState172
  | MenhirState169
  | MenhirState168
  | MenhirState166
  | MenhirState165
  | MenhirState163
  | MenhirState162
  | MenhirState159
  | MenhirState157
  | MenhirState154
  | MenhirState151
  | MenhirState149
  | MenhirState144
  | MenhirState142
  | MenhirState140
  | MenhirState131
  | MenhirState129
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState122
  | MenhirState117
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState93
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState42
  | MenhirState41
  | MenhirState39
  | MenhirState35
  | MenhirState29
  | MenhirState28
  | MenhirState26
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState7
  | MenhirState4
  | MenhirState1
  | MenhirState0

# 9 "src/c_stubs_parser/parser.mly"
  
    open Ast

    let pos_to_loc pos =
      let open Lexing in
      {
	file = pos.pos_fname;
	line = pos.pos_lnum;
	col = pos.pos_cnum - pos.pos_bol + 1;
      }


# 211 "_build/c_stubs_parser/parser.ml"

let rec _menhir_goto_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 216 "_build/c_stubs_parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv807) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 225 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv805) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 233 "_build/c_stubs_parser/parser.ml"
    )) : (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 237 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv806)) : 'freshtv808)

and _menhir_goto_case_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_case_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv799 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv795 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv793 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_predicate_list)), _endpos__2_, _, (_2 : 'tv_requires_list)), _, (_3 : 'tv_case_list)) = _menhir_stack in
            let _4 = () in
            let _v : (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 261 "_build/c_stubs_parser/parser.ml"
            ) = 
# 109 "src/c_stubs_parser/parser.mly"
    (
      S_multi {
        multi_predicates = _1;
        multi_requires = _2;
        multi_cases     = _3;
      }
    )
# 271 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv794)) : 'freshtv796)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv797 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv798)) : 'freshtv800)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv803 * Lexing.position * _menhir_state * 'tv_case * Lexing.position) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv801 * Lexing.position * _menhir_state * 'tv_case * Lexing.position) * _menhir_state * 'tv_case_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_case), _startpos_x0_), _, (_2 : 'tv_case_list)) = _menhir_stack in
        let _v : 'tv_case_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 296 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 177 "src/c_stubs_parser/parser.mly"
                               ( _1 :: _2 )
# 302 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_case_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv802)) : 'freshtv804)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assigns_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_assigns_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv785 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 317 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ENSURES ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CASE | EOF ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv786)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv789 * Lexing.position * _menhir_state * 'tv_assigns * Lexing.position) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv787 * Lexing.position * _menhir_state * 'tv_assigns * Lexing.position) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_assigns), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_assigns_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_assigns_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 346 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 156 "src/c_stubs_parser/parser.mly"
                                     ( _1 :: _2 )
# 352 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_assigns_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv788)) : 'freshtv790)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv791 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ENSURES | EOF ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211) : 'freshtv792)
    | _ ->
        _menhir_fail ()

and _menhir_goto_local_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_local_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv777 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 381 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGNS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CASE | ENSURES | EOF ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180) : 'freshtv778)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv781 * Lexing.position * _menhir_state * 'tv_local * Lexing.position) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * Lexing.position * _menhir_state * 'tv_local * Lexing.position) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_local), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_local_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_local_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 410 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 126 "src/c_stubs_parser/parser.mly"
                                 ( _1 :: _2 )
# 416 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_local_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv780)) : 'freshtv782)
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv783 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ENSURES ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212) : 'freshtv784)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ensures_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_ensures_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv763 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 445 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv761 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 451 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let ((((((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, (_2 : (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 456 "_build/c_stubs_parser/parser.ml"
        )), _startpos__2_), _endpos__3_), _endpos__4_, _, (_4 : 'tv_assumes_list)), _endpos__5_, _, (_5 : 'tv_requires_list)), _endpos__6_, _, (_6 : 'tv_local_list)), _endpos__7_, _, (_7 : 'tv_assigns_list)), _endpos__8_, _, (_8 : 'tv_ensures_list)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__8_ in
        let _v : 'tv_case = 
# 181 "src/c_stubs_parser/parser.mly"
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
# 474 "_build/c_stubs_parser/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv759) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_case) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv757 * Lexing.position * _menhir_state * 'tv_case * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CASE ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv755 * Lexing.position * _menhir_state * 'tv_case * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_case), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_case_list = let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 503 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 176 "src/c_stubs_parser/parser.mly"
                     ( [ _1 ] )
# 509 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_case_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv756)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209) : 'freshtv758)) : 'freshtv760)) : 'freshtv762)) : 'freshtv764)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv767 * Lexing.position * _menhir_state * 'tv_ensures * Lexing.position) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv765 * Lexing.position * _menhir_state * 'tv_ensures * Lexing.position) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_ensures), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_ensures_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_ensures_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 532 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 201 "src/c_stubs_parser/parser.mly"
                                     ( _1 :: _2 )
# 538 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_ensures_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv766)) : 'freshtv768)
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv775 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv771 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv769 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_predicate_list)), _endpos__2_, _, (_2 : 'tv_requires_list)), _endpos__3_, _, (_3 : 'tv_assigns_list)), _endpos__4_, _, (_4 : 'tv_local_list)), _endpos__5_, _, (_5 : 'tv_ensures_list)) = _menhir_stack in
            let _6 = () in
            let _v : (
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 557 "_build/c_stubs_parser/parser.ml"
            ) = 
# 98 "src/c_stubs_parser/parser.mly"
    (
      S_simple {
        simple_predicates = _1;
        simple_requires = _2;
        simple_assigns  = _3;
        simple_local    = _4;
        simple_ensures  = _5;
      }
    )
# 569 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv770)) : 'freshtv772)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv773 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_ensures_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv774)) : 'freshtv776)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assumes_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_assumes_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv749 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 591 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | REQUIRES ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSIGNS | CASE | ENSURES | EOF | LOCAL ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv750)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * Lexing.position * _menhir_state * 'tv_assumes * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv751 * Lexing.position * _menhir_state * 'tv_assumes * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_assumes), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_assumes_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_assumes_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 620 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 194 "src/c_stubs_parser/parser.mly"
                                     ( _1 :: _2 )
# 626 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_assumes_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv752)) : 'freshtv754)
    | _ ->
        _menhir_fail ()

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_assigns_list = 
# 155 "src/c_stubs_parser/parser.mly"
    ( [] )
# 638 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_assigns_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRING _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv745 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 655 "_build/c_stubs_parser/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv741 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 667 "_build/c_stubs_parser/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSUMES ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSIGNS | CASE | ENSURES | EOF | LOCAL | REQUIRES ->
                _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157) : 'freshtv742)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv743 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 689 "_build/c_stubs_parser/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv744)) : 'freshtv746)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv747 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv748)

and _menhir_run181 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv737 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv738)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv739 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv740)

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_local_list = 
# 125 "src/c_stubs_parser/parser.mly"
    ( [] )
# 767 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_local_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv733 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SIGNED ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | STRUCT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TCHAR ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TDOUBLE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TFLOAT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TINT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TLONG ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | UNION ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | UNSIGNED ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv734)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv735 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv736)

and _menhir_reduce15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_ensures_list = 
# 200 "src/c_stubs_parser/parser.mly"
    ( [] )
# 823 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_ensures_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run192 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv729 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXISTS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FORALL ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FREE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193) : 'freshtv730)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv731 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv732)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_assumes_list = 
# 193 "src/c_stubs_parser/parser.mly"
    ( [] )
# 907 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_assumes_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv725 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXISTS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FORALL ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FREE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv726)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv727 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv728)

and _menhir_run140 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_formula * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXISTS ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FORALL ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FREE ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run144 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_formula * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXISTS ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FORALL ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FREE ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_run142 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_formula * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXISTS ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FORALL ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FREE ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_goto_assigns : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_assigns -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv723 * Lexing.position * _menhir_state * 'tv_assigns * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGNS ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CASE | ENSURES | EOF | LOCAL ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv724)

and _menhir_run46 : _menhir_env -> ('ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) -> Lexing.position -> (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 1179 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv721 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
    let (_endpos__3_ : Lexing.position) = _endpos in
    let ((_3 : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 1189 "_build/c_stubs_parser/parser.ml"
    )) : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 1193 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__3_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos__3_ in
    let _v : 'tv_expr = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 1209 "_build/c_stubs_parser/parser.ml"
      
    in
    
# 232 "src/c_stubs_parser/parser.mly"
                                        ( E_member (_1, _3) )
# 1215 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv722)

and _menhir_run64 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv719 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos__4_ : Lexing.position) = _endpos in
    ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos__4_ in
    let _v : 'tv_expr = let _3 =
      let _endpos_x_ = _endpos_x1_ in
      let _startpos_x_ = _startpos_x1_ in
      let x = x1 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 1239 "_build/c_stubs_parser/parser.ml"
      
    in
    let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 1251 "_build/c_stubs_parser/parser.ml"
      
    in
    
# 231 "src/c_stubs_parser/parser.mly"
                                                    ( E_subscript (_1, _3) )
# 1257 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv720)

and _menhir_run50 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run54 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run65 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run58 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run62 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run67 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run71 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run52 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run73 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run60 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run45 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv717 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)

and _menhir_run47 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv713 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 2127 "_build/c_stubs_parser/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv711 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        let ((_3 : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 2137 "_build/c_stubs_parser/parser.ml"
        )) : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 2141 "_build/c_stubs_parser/parser.ml"
        )) = _v in
        let (_startpos__3_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_x0_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_expr = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2157 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 233 "src/c_stubs_parser/parser.mly"
                                        ( E_arrow (_1, _3) )
# 2163 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv712)) : 'freshtv714)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv715 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)

and _menhir_goto_requires_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_requires_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv703 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGNS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CASE ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ENSURES | EOF | LOCAL ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv704)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv705 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 2199 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSIGNS | CASE | ENSURES | EOF ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163) : 'freshtv706)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv709 * Lexing.position * _menhir_state * (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 2217 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv707 * Lexing.position * _menhir_state * (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 2223 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 2228 "_build/c_stubs_parser/parser.ml"
        )), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_requires_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_requires_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2240 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 119 "src/c_stubs_parser/parser.mly"
                                       ( _1 :: _2 )
# 2246 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_requires_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv708)) : 'freshtv710)
    | _ ->
        _menhir_fail ()

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv693 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv691 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos__2_), _, (_3 : 'tv_args)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_args = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2272 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 295 "src/c_stubs_parser/parser.mly"
                                ( _1 :: _3 )
# 2278 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)) : 'freshtv694)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv701 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv697 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv695 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_var), _startpos__1_), _startpos__2_), _, (_3 : 'tv_args)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_local_value = 
# 139 "src/c_stubs_parser/parser.mly"
                       ( Local_function_call (_1, _3) )
# 2303 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_local_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv696)) : 'freshtv698)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv699 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv700)) : 'freshtv702)
    | _ ->
        _menhir_fail ()

and _menhir_goto_local_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_local_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv689 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv685 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv683 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        let (_endpos__7_ : Lexing.position) = _endpos in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _, (_3 : 'tv_typ)), _endpos__4_, _, (_4 : 'tv_var), _startpos__4_), _, (_6 : 'tv_local_value)) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : 'tv_local = 
# 130 "src/c_stubs_parser/parser.mly"
    (
      {
        local_var = { _4 with var_typ = _3 };
        local_value = _6;
      }
    )
# 2347 "_build/c_stubs_parser/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv681) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_local) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv679 * Lexing.position * _menhir_state * 'tv_local * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LOCAL ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSIGNS | CASE | ENSURES | EOF ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201) : 'freshtv680)) : 'freshtv682)) : 'freshtv684)) : 'freshtv686)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv687 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * _menhir_state * 'tv_local_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv688)) : 'freshtv690)

and _menhir_goto_set : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_set -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv667 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv663 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXISTS ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FORALL ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FREE ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv665 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv673 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv669 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXISTS ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FORALL ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FREE ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv670)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv671 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv672)) : 'freshtv674)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv677 * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv675 * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_var), _startpos__1_), _endpos__3_, _, (_3 : 'tv_set)) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_formula = 
# 215 "src/c_stubs_parser/parser.mly"
                              ( F_in (_1, _3) )
# 2539 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv676)) : 'freshtv678)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv639 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_typ)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ = 
# 240 "src/c_stubs_parser/parser.mly"
              ( T_const _2 )
# 2559 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)) : 'freshtv642)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv645 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_typ)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ = 
# 238 "src/c_stubs_parser/parser.mly"
               ( T_signed _2 )
# 2572 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)) : 'freshtv646)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv649 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv647 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_typ)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ = 
# 239 "src/c_stubs_parser/parser.mly"
                 ( T_unsigned _2 )
# 2585 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)) : 'freshtv650)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv651 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv653 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv657 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv658)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv659 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState125 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv660)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv661 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState166 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv662)
    | _ ->
        _menhir_fail ()

and _menhir_goto_formula : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_formula -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv557 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv555 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_typ)), _endpos__3_, _, (_3 : 'tv_var), _startpos__3_), _endpos__5_, _, (_5 : 'tv_set)), _endpos__6_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x0_ in
        let _v : 'tv_formula = let _7 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2711 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 214 "src/c_stubs_parser/parser.mly"
                                                    ( F_exists ({ _3 with var_typ = _2 }, _5, _7) )
# 2717 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv556)) : 'freshtv558)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv561 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv559 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_typ)), _endpos__3_, _, (_3 : 'tv_var), _startpos__3_), _endpos__5_, _, (_5 : 'tv_set)), _endpos__6_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x0_ in
        let _v : 'tv_formula = let _7 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2740 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 213 "src/c_stubs_parser/parser.mly"
                                                    ( F_forall ({ _3 with var_typ = _2 }, _5, _7) )
# 2746 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv560)) : 'freshtv562)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv565 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv563 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x0_ in
        let _v : 'tv_formula = let _2 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2767 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 212 "src/c_stubs_parser/parser.mly"
                                          ( F_not _2 )
# 2773 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv564)) : 'freshtv566)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv573 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv569 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv567 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_startpos__3_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_formula), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_formula = 
# 207 "src/c_stubs_parser/parser.mly"
                              ( _2 )
# 2806 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv568)) : 'freshtv570)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv571 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv579 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES | OR | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_formula), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_formula), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_formula = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2840 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 284 "src/c_stubs_parser/parser.mly"
       ( OR )
# 2848 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2860 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 211 "src/c_stubs_parser/parser.mly"
                                                      ( F_binop (_2, _1, _3) )
# 2866 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv577 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv583 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv581 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_formula), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_formula), _startpos_x1_) = _menhir_stack in
        let _10 = () in
        let _startpos = _startpos_x0_ in
        let _endpos = _endpos_x1_ in
        let _v : 'tv_formula = let _3 =
          let _endpos_x_ = _endpos_x1_ in
          let _startpos_x_ = _startpos_x1_ in
          let x = x1 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2894 "_build/c_stubs_parser/parser.ml"
          
        in
        let _2 =
          let _1 = _10 in
          
# 283 "src/c_stubs_parser/parser.mly"
        ( AND )
# 2902 "_build/c_stubs_parser/parser.ml"
          
        in
        let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2914 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 211 "src/c_stubs_parser/parser.mly"
                                                      ( F_binop (_2, _1, _3) )
# 2920 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv582)) : 'freshtv584)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv589 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv585 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_formula), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_formula), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_formula = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2949 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 285 "src/c_stubs_parser/parser.mly"
            ( IMPLIES )
# 2957 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 2969 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 211 "src/c_stubs_parser/parser.mly"
                                                      ( F_binop (_2, _1, _3) )
# 2975 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv601 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv597 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv595 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_var), _startpos__2_), _endpos__3_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_predicate = let _4 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3020 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 147 "src/c_stubs_parser/parser.mly"
    (
      {
        predicate_var = { _2 with var_typ = T_predicate };
        predicate_body = _4;
      }
    )
# 3031 "_build/c_stubs_parser/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv593) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_predicate) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv591 * Lexing.position * _menhir_state * 'tv_predicate * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | PREDICATE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSIGNS | CASE | ENSURES | EOF | LOCAL | REQUIRES ->
                _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState215
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215) : 'freshtv592)) : 'freshtv594)) : 'freshtv596)) : 'freshtv598)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv599 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv600)) : 'freshtv602)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv609 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 3089 "_build/c_stubs_parser/parser.ml"
            ) = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3099 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 122 "src/c_stubs_parser/parser.mly"
                                               ( _3 )
# 3105 "_build/c_stubs_parser/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv605) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 3114 "_build/c_stubs_parser/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv603 * Lexing.position * _menhir_state * (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 3122 "_build/c_stubs_parser/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | REQUIRES ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSIGNS | CASE | ENSURES | EOF | LOCAL ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState203
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv604)) : 'freshtv606)) : 'freshtv608)) : 'freshtv610)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv611 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv612)) : 'freshtv614)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv625 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv621 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv619 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_assumes = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3177 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 197 "src/c_stubs_parser/parser.mly"
                                              ( _3 )
# 3183 "_build/c_stubs_parser/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv617) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_assumes) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv615 * Lexing.position * _menhir_state * 'tv_assumes * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSUMES ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSIGNS | CASE | ENSURES | EOF | LOCAL | REQUIRES ->
                _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState205
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv623 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv637 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack)
        | IMPLIES ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv633 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv631 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos_x0_, _, (x0 : 'tv_formula), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_ensures = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3247 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 204 "src/c_stubs_parser/parser.mly"
                                              ( _3 )
# 3253 "_build/c_stubs_parser/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_ensures) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627 * Lexing.position * _menhir_state * 'tv_ensures * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ENSURES ->
                _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CASE | EOF ->
                _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197) : 'freshtv628)) : 'freshtv630)) : 'freshtv632)) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv635 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_formula * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv636)) : 'freshtv638)
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv553) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ_spec = 
# 248 "src/c_stubs_parser/parser.mly"
          ( T_long )
# 3339 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv554)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv551) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ_spec = 
# 245 "src/c_stubs_parser/parser.mly"
         ( T_int )
# 3353 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv549) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ_spec = 
# 246 "src/c_stubs_parser/parser.mly"
           ( T_float )
# 3367 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv550)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ_spec = 
# 247 "src/c_stubs_parser/parser.mly"
            ( T_double )
# 3381 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ_spec = 
# 244 "src/c_stubs_parser/parser.mly"
          ( T_char )
# 3395 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv546)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expr -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3504 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 229 "src/c_stubs_parser/parser.mly"
                                        ( E_addr_of _2 )
# 3510 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv297 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv291 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__10_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3589 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 290 "src/c_stubs_parser/parser.mly"
         ( BASE )
# 3597 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 235 "src/c_stubs_parser/parser.mly"
                                        ( E_builtin_call (_1, _3) )
# 3603 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv292)) : 'freshtv294)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv295 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv299 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _startpos__10_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3645 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 256 "src/c_stubs_parser/parser.mly"
         ( MUL )
# 3653 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3665 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 3671 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv307 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACK ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv305 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv313 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | NEQ | OR | RBRACK | RPAR | RSHIFT | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3776 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 270 "src/c_stubs_parser/parser.mly"
           ( RSHIFT )
# 3784 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3796 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 3802 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _startpos__10_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3846 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 254 "src/c_stubs_parser/parser.mly"
         ( ADD )
# 3854 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3866 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 3872 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv316)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv317 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv321 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3910 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 258 "src/c_stubs_parser/parser.mly"
        ( MOD )
# 3918 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3930 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 3936 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3974 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 257 "src/c_stubs_parser/parser.mly"
        ( DIV )
# 3982 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 3994 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4000 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv333 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _startpos__10_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4044 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 255 "src/c_stubs_parser/parser.mly"
          ( SUB )
# 4052 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4064 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4070 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv343 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | IMPLIES | LAND | LOR | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4130 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 265 "src/c_stubs_parser/parser.mly"
        ( NEQ )
# 4138 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4150 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4156 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv341 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv349 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LT | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv345 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4208 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 266 "src/c_stubs_parser/parser.mly"
       ( LT )
# 4216 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4228 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4234 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv346)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv347 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv355 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | NEQ | OR | RBRACK | RPAR | RSHIFT | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv351 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4282 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 271 "src/c_stubs_parser/parser.mly"
           ( LSHIFT )
# 4290 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4302 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4308 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv352)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv353 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv361 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LT | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4360 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 268 "src/c_stubs_parser/parser.mly"
       ( LE )
# 4368 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4380 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4386 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv367 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LT | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4438 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 267 "src/c_stubs_parser/parser.mly"
       ( GT )
# 4446 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4458 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4464 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LT | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4516 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 269 "src/c_stubs_parser/parser.mly"
       ( GE )
# 4524 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4536 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4542 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | COLON | IMPLIES | LOR | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4614 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 259 "src/c_stubs_parser/parser.mly"
        ( LOR )
# 4622 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4634 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4640 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv376)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | COLON | IMPLIES | LAND | LOR | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4710 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 260 "src/c_stubs_parser/parser.mly"
         ( LAND )
# 4718 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4730 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4736 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | IMPLIES | LAND | LOR | NEQ | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4796 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 264 "src/c_stubs_parser/parser.mly"
       ( EQ )
# 4804 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4816 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4822 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BOR | BXOR | COLON | IMPLIES | LAND | LOR | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4888 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 262 "src/c_stubs_parser/parser.mly"
         ( BXOR )
# 4896 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4908 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 4914 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | IMPLIES | LAND | LOR | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4978 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 263 "src/c_stubs_parser/parser.mly"
         ( BAND )
# 4986 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 4998 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 5004 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv400)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BOR | COLON | IMPLIES | LAND | LOR | OR | RBRACK | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5072 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _1 = _10 in
              
# 261 "src/c_stubs_parser/parser.mly"
        ( BOR )
# 5080 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5092 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 228 "src/c_stubs_parser/parser.mly"
                                                    ( E_binop (_2, _1, _3) )
# 5098 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv415 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv411 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5134 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 276 "src/c_stubs_parser/parser.mly"
         ( BNOT )
# 5142 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 227 "src/c_stubs_parser/parser.mly"
                                        ( E_unop (_1, _2) )
# 5148 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv412)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv413 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv417 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5184 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 275 "src/c_stubs_parser/parser.mly"
         ( LNOT )
# 5192 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 227 "src/c_stubs_parser/parser.mly"
                                        ( E_unop (_1, _2) )
# 5198 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv419 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv427 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv423 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_typ)), _endpos__3_, _startpos__3_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _4 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5237 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 219 "src/c_stubs_parser/parser.mly"
                                   ( E_cast (_2, _4) )
# 5243 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv424)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv425 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv431 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv429 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_startpos__3_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_expr), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_expr = 
# 220 "src/c_stubs_parser/parser.mly"
                   ( _2 )
# 5315 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv430)) : 'freshtv432)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv433 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv437 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5355 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 274 "src/c_stubs_parser/parser.mly"
          ( MINUS )
# 5363 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 227 "src/c_stubs_parser/parser.mly"
                                        ( E_unop (_1, _2) )
# 5369 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv439 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)) : 'freshtv442)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv449 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__10_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5448 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 289 "src/c_stubs_parser/parser.mly"
           ( OFFSET )
# 5456 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 235 "src/c_stubs_parser/parser.mly"
                                        ( E_builtin_call (_1, _3) )
# 5462 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv444)) : 'freshtv446)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv447 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv448)) : 'freshtv450)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv457 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv453 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__10_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5545 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 291 "src/c_stubs_parser/parser.mly"
        ( OLD )
# 5553 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 235 "src/c_stubs_parser/parser.mly"
                                        ( E_builtin_call (_1, _3) )
# 5559 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv452)) : 'freshtv454)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)) : 'freshtv458)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | BAND | BOR | BXOR | COLON | EQ | GE | GT | IMPLIES | LAND | LE | LOR | LSHIFT | LT | MINUS | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_expr), _startpos__2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_expr = 
# 221 "src/c_stubs_parser/parser.mly"
              ( _2 )
# 5601 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv460)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv461 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)) : 'freshtv464)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv471 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv465 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__10_), _startpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expr = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5680 "_build/c_stubs_parser/parser.ml"
              
            in
            let _1 =
              let _1 = _10 in
              
# 288 "src/c_stubs_parser/parser.mly"
         ( SIZE )
# 5688 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 235 "src/c_stubs_parser/parser.mly"
                                        ( E_builtin_call (_1, _3) )
# 5694 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv466)) : 'freshtv468)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND | BAND | BOR | BXOR | COLON | DIV | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RBRACK | RPAR | RSHIFT | SEMICOL | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv473 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_expr = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5734 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 230 "src/c_stubs_parser/parser.mly"
                                        ( E_deref _2 )
# 5740 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv474)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)) : 'freshtv478)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | IMPLIES | OR | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv479 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_formula = let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 5814 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 216 "src/c_stubs_parser/parser.mly"
                                          ( F_free _2 )
# 5820 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv481 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv493 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv489 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv485 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDROF ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BASE ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BNOT ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CHAR _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LNOT ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAR ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OFFSET ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OLD ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PLUS ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | SIZE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv486)
            | IDENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv487 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv491 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv492)) : 'freshtv494)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv501 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv497 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv495 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__6_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos__6_ in
            let _v : 'tv_set = let _5 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6009 "_build/c_stubs_parser/parser.ml"
              
            in
            let _2 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6021 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 279 "src/c_stubs_parser/parser.mly"
                                                            ( S_interval (_2, _5) )
# 6027 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_set _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv496)) : 'freshtv498)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv499 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | MenhirState193 | MenhirState159 | MenhirState151 | MenhirState4 | MenhirState106 | MenhirState144 | MenhirState140 | MenhirState142 | MenhirState107 | MenhirState122 | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AND | IMPLIES | OR | RPAR | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv503 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x0_ in
            let _v : 'tv_formula = let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6104 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 210 "src/c_stubs_parser/parser.mly"
                                          ( F_expr _1 )
# 6110 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv504)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv505 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)) : 'freshtv508)
    | MenhirState174 | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv515 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv509 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174) : 'freshtv510)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv511 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _v : 'tv_args = let _1 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6229 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 296 "src/c_stubs_parser/parser.mly"
                     ( [ _1 ] )
# 6235 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv513 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv518)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv521 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_assigns = let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6356 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 160 "src/c_stubs_parser/parser.mly"
    (
      {
	assigns_target = _3;
	assigns_range = None;
      }
    )
# 6367 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv520)) : 'freshtv522)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv523 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv535 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv531 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv527 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDROF ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BASE ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BNOT ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CHAR _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LNOT ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAR ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OFFSET ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OLD ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | PLUS ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RETURN ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | SIZE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv528)
            | IDENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv529 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACK ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv533 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv543 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LSHIFT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv539 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv537 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__9_ : Lexing.position) = _endpos in
            ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos_x0_, _, (x0 : 'tv_expr), _startpos_x0_), _endpos_x1_, _, (x1 : 'tv_expr), _startpos_x1_), _endpos_x2_, _, (x2 : 'tv_expr), _startpos_x2_) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__9_ in
            let _v : 'tv_assigns = let _8 =
              let _endpos_x_ = _endpos_x2_ in
              let _startpos_x_ = _startpos_x2_ in
              let x = x2 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6563 "_build/c_stubs_parser/parser.ml"
              
            in
            let _5 =
              let _endpos_x_ = _endpos_x1_ in
              let _startpos_x_ = _startpos_x1_ in
              let x = x1 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6575 "_build/c_stubs_parser/parser.ml"
              
            in
            let _3 =
              let _endpos_x_ = _endpos_x0_ in
              let _startpos_x_ = _startpos_x0_ in
              let x = x0 in
              let _endpos = _endpos_x_ in
              let _startpos = _startpos_x_ in
              
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6587 "_build/c_stubs_parser/parser.ml"
              
            in
            
# 168 "src/c_stubs_parser/parser.mly"
    (
      {
	assigns_target = _3;
	assigns_range = Some (_5, _8);
      }
    )
# 6598 "_build/c_stubs_parser/parser.ml"
             in
            _menhir_goto_assigns _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv538)) : 'freshtv540)
        | RSHIFT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv541 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv542)) : 'freshtv544)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_requires_list = 
# 118 "src/c_stubs_parser/parser.mly"
    ( [] )
# 6621 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_requires_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EXISTS ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FORALL ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FREE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAR ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv282)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 294 "src/c_stubs_parser/parser.mly"
    ( [] )
# 6709 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run120 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 6761 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv279) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 6772 "_build/c_stubs_parser/parser.ml"
    )) : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 6776 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_resource = 
# 299 "src/c_stubs_parser/parser.mly"
          ( _1 )
# 6783 "_build/c_stubs_parser/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv277) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_resource) = _v in
    ((match _menhir_s with
    | MenhirState113 | MenhirState127 | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_resource) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv269) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_resource) : 'tv_resource) = _v in
        ((let _endpos = _endpos__1_ in
        let _v : 'tv_set = 
# 280 "src/c_stubs_parser/parser.mly"
             ( S_resource _1 )
# 6806 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_set _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv270)) : 'freshtv272)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_resource) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_resource) : 'tv_resource) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_local_value = 
# 138 "src/c_stubs_parser/parser.mly"
                 ( Local_new _2 )
# 6825 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_local_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
    | _ ->
        _menhir_fail ()) : 'freshtv278)) : 'freshtv280)

and _menhir_reduce24 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_var * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_var), _startpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 226 "src/c_stubs_parser/parser.mly"
                                      ( E_var _1 )
# 6839 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_typ_spec : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ_spec -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_typ_spec) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_typ_spec) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_typ_spec) = Obj.magic _menhir_stack in
        let (_startpos__2_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_typ_spec)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_typ_spec = 
# 251 "src/c_stubs_parser/parser.mly"
                  ( T_pointer _1 )
# 6864 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
    | IDENT _ | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_typ_spec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_typ_spec)) = _menhir_stack in
        let _v : 'tv_typ = 
# 241 "src/c_stubs_parser/parser.mly"
             ( _1 )
# 6874 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_typ_spec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let x0 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_formula = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 6905 "_build/c_stubs_parser/parser.ml"
      
    in
    
# 208 "src/c_stubs_parser/parser.mly"
                                          ( F_bool true )
# 6911 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv258)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 6918 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 6929 "_build/c_stubs_parser/parser.ml"
    )) : (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 6933 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 223 "src/c_stubs_parser/parser.mly"
                            ( E_string _1)
# 6941 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv256)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv252)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)

and _menhir_run106 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXISTS ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FORALL ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FREE ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 234 "src/c_stubs_parser/parser.mly"
                            ( E_return )
# 7123 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv250)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv246)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv242)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EXISTS ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FORALL ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FREE ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run36 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 23 "src/c_stubs_parser/parser.mly"
       (Z.t)
# 7509 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv239) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 23 "src/c_stubs_parser/parser.mly"
       (Z.t)
# 7520 "_build/c_stubs_parser/parser.ml"
    )) : (
# 23 "src/c_stubs_parser/parser.mly"
       (Z.t)
# 7524 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 222 "src/c_stubs_parser/parser.mly"
                            ( E_int _1 )
# 7532 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv240)

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run37 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 24 "src/c_stubs_parser/parser.mly"
       (float)
# 7615 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 24 "src/c_stubs_parser/parser.mly"
       (float)
# 7626 "_build/c_stubs_parser/parser.ml"
    )) : (
# 24 "src/c_stubs_parser/parser.mly"
       (float)
# 7630 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 224 "src/c_stubs_parser/parser.mly"
                            ( E_float _1 )
# 7638 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv238)

and _menhir_run123 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
    let (_endpos_x0_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x0_ : Lexing.position) = _startpos in
    ((let x0 = () in
    let _startpos = _startpos_x0_ in
    let _endpos = _endpos_x0_ in
    let _v : 'tv_formula = let _1 =
      let _endpos_x_ = _endpos_x0_ in
      let _startpos_x_ = _startpos_x0_ in
      let x = x0 in
      let _endpos = _endpos_x_ in
      let _startpos = _startpos_x_ in
      
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 7662 "_build/c_stubs_parser/parser.ml"
      
    in
    
# 209 "src/c_stubs_parser/parser.mly"
                                          ( F_bool false )
# 7668 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv236)

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | SIGNED ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | STRUCT ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TCHAR ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TDOUBLE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TFLOAT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TINT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TLONG ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | UNION ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | UNSIGNED ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 25 "src/c_stubs_parser/parser.mly"
       (char)
# 7706 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 25 "src/c_stubs_parser/parser.mly"
       (char)
# 7717 "_build/c_stubs_parser/parser.ml"
    )) : (
# 25 "src/c_stubs_parser/parser.mly"
       (char)
# 7721 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_expr = 
# 225 "src/c_stubs_parser/parser.mly"
                            ( E_char _1 )
# 7729 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv234)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDROF ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BASE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BNOT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CHAR _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LNOT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OFFSET ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OLD ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SIZE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv230)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDROF ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BASE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BNOT ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CHAR _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LNOT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OFFSET ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OLD ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | PLUS ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SIZE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STAR ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState42 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_goto_predicate_list : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_predicate_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * Lexing.position * _menhir_state * 'tv_predicate_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | REQUIRES ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSIGNS | CASE | ENSURES | EOF | LOCAL ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv224)
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * Lexing.position * _menhir_state * 'tv_predicate * Lexing.position) * Lexing.position * _menhir_state * 'tv_predicate_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * Lexing.position * _menhir_state * 'tv_predicate * Lexing.position) * Lexing.position * _menhir_state * 'tv_predicate_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x0_, _menhir_s, (x0 : 'tv_predicate), _startpos_x0_), _endpos__2_, _, (_2 : 'tv_predicate_list)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_predicate_list = let _1 =
          let _endpos_x_ = _endpos_x0_ in
          let _startpos_x_ = _startpos_x0_ in
          let x = x0 in
          let _endpos = _endpos_x_ in
          let _startpos = _startpos_x_ in
          
# 306 "src/c_stubs_parser/parser.mly"
        ( x, (pos_to_loc _startpos, pos_to_loc _endpos) )
# 7917 "_build/c_stubs_parser/parser.ml"
          
        in
        
# 143 "src/c_stubs_parser/parser.mly"
                                         ( _1 :: _2 )
# 7923 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_predicate_list _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv226)) : 'freshtv228)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 7932 "_build/c_stubs_parser/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv221) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 7943 "_build/c_stubs_parser/parser.ml"
    )) : (
# 29 "src/c_stubs_parser/parser.mly"
       (string)
# 7947 "_build/c_stubs_parser/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_var = 
# 302 "src/c_stubs_parser/parser.mly"
          ( { var_name = _1; var_uid = 0; var_typ = T_unknown; } )
# 7955 "_build/c_stubs_parser/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv219) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_var) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv171 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EXISTS ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FORALL ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FREE ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv173 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_var), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ_spec = 
# 250 "src/c_stubs_parser/parser.mly"
              ( T_union _2 )
# 8048 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)) : 'freshtv180)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_var), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ_spec = 
# 249 "src/c_stubs_parser/parser.mly"
               ( T_struct _2 )
# 8061 "_build/c_stubs_parser/parser.ml"
         in
        _menhir_goto_typ_spec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv182)) : 'freshtv184)
    | MenhirState188 | MenhirState185 | MenhirState182 | MenhirState174 | MenhirState172 | MenhirState117 | MenhirState114 | MenhirState108 | MenhirState7 | MenhirState9 | MenhirState11 | MenhirState13 | MenhirState15 | MenhirState16 | MenhirState93 | MenhirState17 | MenhirState35 | MenhirState39 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState41 | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        (_menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) : 'freshtv186)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACK ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv193 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACK ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv194)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
    | MenhirState193 | MenhirState159 | MenhirState151 | MenhirState4 | MenhirState144 | MenhirState142 | MenhirState140 | MenhirState106 | MenhirState107 | MenhirState122 | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv199 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACK ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv200)
        | AND | ARROW | BAND | BOR | BXOR | DIV | DOT | EQ | GE | GT | IMPLIES | LAND | LBRACK | LE | LOR | LSHIFT | LT | MINUS | MOD | NEQ | OR | PLUS | RPAR | RSHIFT | SEMICOL | STAR ->
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv211 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv207 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState168 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv206)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv208)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv209 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDROF ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BASE ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BNOT ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CHAR _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LNOT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OFFSET ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OLD ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | PLUS ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RETURN ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SIZE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STAR ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | _ ->
        _menhir_fail ()) : 'freshtv220)) : 'freshtv222)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState215 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * Lexing.position * _menhir_state * 'tv_predicate * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState212 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv5 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv7 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * Lexing.position * _menhir_state * 'tv_case * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * Lexing.position * _menhir_state * 'tv_assumes * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * Lexing.position * _menhir_state * (
# 92 "src/c_stubs_parser/parser.mly"
      (Ast.requires)
# 8287 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * Lexing.position * _menhir_state * 'tv_local * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * Lexing.position * _menhir_state * 'tv_assigns * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * 'tv_ensures * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv23 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 8316 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) * Lexing.position * _menhir_state * 'tv_assigns_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv25 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv27 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv31 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 8340 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) * Lexing.position * _menhir_state * 'tv_local_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv39 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv45 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 8379 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 8388 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_assumes_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * Lexing.position) * Lexing.position * (
# 26 "src/c_stubs_parser/parser.mly"
       (string)
# 8402 "_build/c_stubs_parser/parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * Lexing.position * _menhir_state * 'tv_predicate_list) * Lexing.position * _menhir_state * 'tv_requires_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * Lexing.position * _menhir_state * 'tv_predicate_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * Lexing.position * _menhir_state * 'tv_formula * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv67 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv69 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv75 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) * Lexing.position * _menhir_state * 'tv_set) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv77 * _menhir_state) * Lexing.position * _menhir_state * 'tv_expr * Lexing.position))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv81 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * _menhir_state * 'tv_var * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv93 * _menhir_state * Lexing.position) * _menhir_state * 'tv_typ) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * Lexing.position * _menhir_state * 'tv_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_var * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv170)

and _menhir_reduce74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_predicate_list = 
# 142 "src/c_stubs_parser/parser.mly"
    ( [] )
# 8707 "_build/c_stubs_parser/parser.ml"
     in
    _menhir_goto_predicate_list _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

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
# 90 "src/c_stubs_parser/parser.mly"
      (Ast.stub)
# 8739 "_build/c_stubs_parser/parser.ml"
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
    | PREDICATE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGNS | CASE | ENSURES | EOF | LOCAL | REQUIRES ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 233 "/home/ouadjaout/.opam/4.06.0/lib/menhir/standard.mly"
  

# 8770 "_build/c_stubs_parser/parser.ml"
