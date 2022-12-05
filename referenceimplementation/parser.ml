type token =
  | AMPERAMPER
  | BANGEQUAL
  | BARBAR
  | COLON
  | COMMA
  | DISPOSE
  | DLSEG
  | ELSE
  | EMPTY
  | EOF
  | EQUAL
  | EQUALEQUAL
  | FF
  | IDENT of (string)
  | QIDENT of (string)
  | IF
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | LBRACE
  | LBRACKET
  | LIST
  | LISTSEG
  | LOCAL
  | LPAREN
  | MINUSGREATER
  | NAT of (int)
  | NEW
  | POINTSTO
  | RBRACE
  | RBRACKET
  | RESOURCE
  | RPAREN
  | SEMI
  | STAR
  | THEN
  | TREE
  | TT
  | WHEN
  | WHILE
  | WITH
  | XLSEG
  | XOR

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 (* h *)

open Misc
open Parsetree

let mkexp d = { pexp_desc = d; pexp_loc = Location.symbol_loc() }
let mkexp_ghost d = { pexp_desc = d; pexp_loc = Location.none }
let mkstm d = { pstm_desc = d; pstm_loc = Location.symbol_loc() }
let mkstm_ghost d = { pstm_desc = d; pstm_loc = Location.none }

let check_distinct il s loc =
  let is = ref StringSet.empty in
  let f i = if StringSet.mem i !is
  then raise(Error.Not_distinct(s,loc))
  else is := StringSet.add i !is
  in List.iter f il

let mk_formal_params (rl,vl) loc =
  check_distinct (rl@vl) "Formal parameters" loc;
  (rl,vl)

let mk_ref_params cel loc =
  let check_par = function
    | {pexp_desc = Pexp_ident i} -> i
    | _ -> raise(Error.Parameters_not_variables(loc)) in
  let il = List.map check_par cel
  in check_distinct il "Reference parameters" loc;
     il

(* i c w n g r a *)
let parse_error _ =
  raise(
    Error.Parse_error(
      match !Location.lexbuf with
        | None -> Location.symbol_loc()
        | Some lexbuf ->
            (* t P l o u s w s
             * r a g r, s h w a t l f t c
             * p d *)
            Location.mkloc (Parsing.symbol_start_pos()) lexbuf.Lexing.lex_curr_p))

# 91 "parser.ml"
let yytransl_const = [|
  257 (* AMPERAMPER *);
  258 (* BANGEQUAL *);
  259 (* BARBAR *);
  260 (* COLON *);
  261 (* COMMA *);
  262 (* DISPOSE *);
  263 (* DLSEG *);
  264 (* ELSE *);
  265 (* EMPTY *);
    0 (* EOF *);
  266 (* EQUAL *);
  267 (* EQUALEQUAL *);
  268 (* FF *);
  271 (* IF *);
  275 (* LBRACE *);
  276 (* LBRACKET *);
  277 (* LIST *);
  278 (* LISTSEG *);
  279 (* LOCAL *);
  280 (* LPAREN *);
  281 (* MINUSGREATER *);
  283 (* NEW *);
  284 (* POINTSTO *);
  285 (* RBRACE *);
  286 (* RBRACKET *);
  287 (* RESOURCE *);
  288 (* RPAREN *);
  289 (* SEMI *);
  290 (* STAR *);
  291 (* THEN *);
  292 (* TREE *);
  293 (* TT *);
  294 (* WHEN *);
  295 (* WHILE *);
  296 (* WITH *);
  297 (* XLSEG *);
  298 (* XOR *);
    0|]

let yytransl_block = [|
  269 (* IDENT *);
  270 (* QIDENT *);
  272 (* INFIXOP1 *);
  273 (* INFIXOP2 *);
  274 (* INFIXOP3 *);
  282 (* NAT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\005\000\005\000\006\000\007\000\
\004\000\012\000\012\000\013\000\013\000\010\000\010\000\011\000\
\011\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\009\000\009\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\017\000\017\000\018\000\
\018\000\008\000\008\000\016\000\016\000\019\000\019\000\020\000\
\020\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\021\000\021\000\
\021\000\021\000\021\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\000\000\002\000\001\000\001\000\010\000\008\000\
\002\000\000\000\001\000\001\000\003\000\000\000\004\000\000\000\
\002\000\004\000\006\000\006\000\006\000\003\000\003\000\005\000\
\007\000\006\000\007\000\005\000\010\000\000\000\003\000\001\000\
\001\000\001\000\001\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\001\000\001\000\
\003\000\001\000\003\000\001\000\003\000\000\000\001\000\003\000\
\005\000\006\000\004\000\008\000\006\000\014\000\010\000\014\000\
\010\000\008\000\004\000\001\000\003\000\003\000\005\000\003\000\
\003\000\003\000\001\000\003\000\006\000\001\000\003\000\003\000\
\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\084\000\001\000\000\000\
\000\000\005\000\006\000\000\000\011\000\000\000\068\000\075\000\
\081\000\082\000\000\000\000\000\000\000\000\000\083\000\000\000\
\000\000\000\000\000\000\078\000\000\000\000\000\000\000\000\000\
\002\000\004\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\072\000\079\000\000\000\
\000\000\000\000\000\000\076\000\000\000\000\000\000\000\069\000\
\055\000\000\000\080\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\000\000\000\000\000\000\000\067\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\000\000\000\061\000\000\000\000\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\000\000\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\033\000\034\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\060\000\066\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\000\022\000\000\000\000\000\
\000\000\000\000\000\000\047\000\000\000\023\000\036\000\000\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\045\000\
\000\000\000\000\040\000\000\000\063\000\000\000\065\000\000\000\
\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\000\000\028\000\
\053\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\019\000\000\000\000\000\026\000\000\000\020\000\000\000\000\000\
\000\000\025\000\027\000\062\000\064\000\000\000\000\000\029\000"

let yydgoto = "\003\000\
\006\000\026\000\007\000\008\000\009\000\010\000\011\000\051\000\
\094\000\123\000\145\000\012\000\013\000\146\000\147\000\186\000\
\187\000\188\000\072\000\073\000\027\000\028\000"

let yysindex = "\232\000\
\010\255\233\255\000\000\003\255\075\255\000\000\000\000\107\255\
\107\255\000\000\000\000\236\254\000\000\091\255\000\000\000\000\
\000\000\000\000\233\255\109\255\125\255\233\255\000\000\141\255\
\175\255\057\255\084\255\000\000\105\255\105\255\177\255\205\255\
\000\000\000\000\000\000\180\255\051\000\076\000\094\000\082\255\
\069\255\224\000\250\000\233\255\008\001\008\001\011\001\008\001\
\149\255\000\000\165\255\198\255\105\255\217\255\008\001\001\255\
\233\255\225\255\146\255\234\255\025\255\000\000\000\000\252\255\
\150\255\012\000\043\255\000\000\237\255\237\255\047\000\000\000\
\000\000\064\255\000\000\048\000\105\255\025\000\052\000\168\255\
\008\001\251\254\008\001\000\000\008\001\008\001\057\000\000\000\
\097\000\008\001\008\001\008\001\233\255\040\000\000\000\064\000\
\054\000\067\255\233\255\184\255\068\255\190\255\084\000\088\000\
\079\255\085\255\237\255\241\254\109\000\233\255\008\001\008\001\
\057\255\000\000\008\001\000\000\008\001\008\001\008\001\121\000\
\000\000\105\255\156\000\250\254\087\255\097\255\224\255\238\255\
\102\255\103\255\047\000\000\000\108\000\251\255\000\000\124\255\
\122\000\251\255\156\000\251\255\000\000\000\000\143\000\138\000\
\141\000\156\000\139\255\000\000\008\001\008\001\000\000\000\000\
\008\001\008\001\109\000\000\000\169\255\245\000\251\255\251\255\
\000\000\160\000\173\255\251\255\153\000\048\000\000\000\251\255\
\251\255\251\255\251\255\251\255\251\255\181\000\251\255\251\255\
\130\255\240\255\137\255\244\255\000\000\000\000\175\000\135\255\
\020\255\168\000\169\000\000\000\207\255\000\000\000\000\219\255\
\180\000\000\000\000\255\196\255\196\255\210\255\045\255\000\000\
\199\000\000\255\000\000\008\001\000\000\008\001\000\000\186\000\
\206\000\000\000\251\255\110\255\251\255\156\000\048\000\251\255\
\251\255\153\255\154\255\191\000\194\000\000\000\218\000\000\000\
\000\000\225\000\156\000\077\000\081\000\008\001\008\001\000\000\
\000\000\216\000\156\000\000\000\156\000\000\000\059\000\074\000\
\251\255\000\000\000\000\000\000\000\000\212\000\214\000\000\000"

let yyrindex = "\000\000\
\005\000\000\000\000\000\219\000\000\000\000\000\000\000\249\001\
\249\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\251\001\000\000\000\000\000\000\093\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\107\000\000\000\000\000\228\000\229\000\156\255\000\000\000\000\
\000\000\106\000\000\000\156\255\000\000\000\000\000\000\106\000\
\000\000\156\255\000\000\000\000\014\000\020\000\001\000\000\000\
\000\000\026\000\000\000\237\000\229\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\039\000\000\000\118\000\000\000\000\000\000\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\236\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\111\000\
\000\000\000\000\236\000\000\000\000\000\000\000\000\000\000\000\
\000\000\236\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\118\000\000\000\000\000\000\000\127\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\132\000\000\000\238\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\071\000\221\000\234\000\209\000\196\000\000\000\
\000\000\183\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\241\000\000\000\166\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\137\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\127\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\246\255\178\001\000\000\000\000\000\000\000\000\000\000\
\094\255\120\001\037\000\236\255\227\255\046\000\162\255\038\001\
\070\001\073\001\000\000\165\001\013\000\000\000"

let yytablesize = 549
let yytable = "\050\000\
\081\000\169\000\099\000\194\000\003\000\081\000\054\000\029\000\
\037\000\052\000\170\000\040\000\035\000\074\000\121\000\171\000\
\172\000\173\000\044\000\073\000\168\000\169\000\004\000\148\000\
\211\000\070\000\030\000\044\000\044\000\086\000\170\000\056\000\
\078\000\068\000\041\000\171\000\172\000\173\000\071\000\157\000\
\005\000\176\000\048\000\161\000\077\000\163\000\082\000\090\000\
\056\000\030\000\059\000\061\000\227\000\175\000\065\000\067\000\
\095\000\069\000\070\000\074\000\075\000\176\000\173\000\184\000\
\185\000\189\000\048\000\080\000\092\000\192\000\045\000\112\000\
\115\000\195\000\196\000\197\000\198\000\199\000\200\000\046\000\
\202\000\203\000\108\000\119\000\048\000\045\000\176\000\031\000\
\113\000\120\000\044\000\149\000\133\000\098\000\046\000\100\000\
\047\000\101\000\102\000\124\000\063\000\150\000\105\000\106\000\
\107\000\048\000\153\000\154\000\048\000\048\000\048\000\047\000\
\223\000\062\000\036\000\044\000\185\000\049\000\185\000\032\000\
\048\000\228\000\229\000\125\000\126\000\048\000\048\000\127\000\
\048\000\128\000\129\000\130\000\038\000\158\000\204\000\168\000\
\169\000\005\000\048\000\168\000\169\000\206\000\224\000\048\000\
\048\000\170\000\185\000\159\000\039\000\170\000\171\000\172\000\
\173\000\029\000\171\000\172\000\173\000\230\000\231\000\209\000\
\081\000\177\000\178\000\174\000\042\000\179\000\180\000\210\000\
\175\000\168\000\169\000\048\000\175\000\168\000\169\000\162\000\
\176\000\084\000\048\000\170\000\176\000\088\000\167\000\170\000\
\171\000\172\000\173\000\048\000\171\000\172\000\173\000\048\000\
\054\000\018\000\048\000\048\000\076\000\081\000\043\000\063\000\
\053\000\182\000\175\000\055\000\191\000\023\000\175\000\168\000\
\169\000\048\000\176\000\171\000\172\000\173\000\176\000\114\000\
\218\000\170\000\219\000\168\000\169\000\116\000\171\000\172\000\
\173\000\048\000\172\000\173\000\030\000\170\000\077\000\048\000\
\001\000\002\000\171\000\172\000\173\000\176\000\214\000\014\000\
\175\000\015\000\239\000\240\000\016\000\017\000\018\000\019\000\
\176\000\079\000\215\000\176\000\175\000\020\000\021\000\151\000\
\022\000\083\000\023\000\226\000\176\000\081\000\135\000\156\000\
\081\000\048\000\085\000\138\000\024\000\152\000\054\000\205\000\
\236\000\025\000\140\000\207\000\141\000\074\000\048\000\048\000\
\242\000\048\000\243\000\073\000\087\000\048\000\081\000\142\000\
\081\000\070\000\081\000\081\000\054\000\010\000\054\000\056\000\
\054\000\054\000\081\000\074\000\089\000\074\000\071\000\074\000\
\074\000\073\000\091\000\073\000\077\000\073\000\073\000\070\000\
\096\000\070\000\109\000\070\000\070\000\056\000\030\000\056\000\
\097\000\056\000\056\000\093\000\071\000\103\000\071\000\038\000\
\071\000\071\000\077\000\038\000\077\000\168\000\169\000\077\000\
\030\000\168\000\169\000\110\000\044\000\057\000\111\000\170\000\
\058\000\018\000\244\000\170\000\171\000\172\000\173\000\038\000\
\171\000\172\000\173\000\055\000\048\000\023\000\038\000\038\000\
\038\000\245\000\060\000\018\000\237\000\104\000\175\000\032\000\
\032\000\238\000\175\000\048\000\117\000\055\000\176\000\023\000\
\118\000\032\000\176\000\014\000\010\000\010\000\032\000\032\000\
\032\000\014\000\014\000\122\000\014\000\131\000\014\000\032\000\
\014\000\081\000\012\000\012\000\155\000\014\000\024\000\014\000\
\032\000\160\000\014\000\081\000\024\000\024\000\165\000\024\000\
\032\000\024\000\014\000\024\000\014\000\014\000\046\000\046\000\
\024\000\134\000\024\000\048\000\048\000\024\000\164\000\135\000\
\136\000\166\000\137\000\030\000\138\000\024\000\139\000\024\000\
\024\000\030\000\030\000\140\000\030\000\141\000\030\000\039\000\
\030\000\033\000\034\000\039\000\190\000\030\000\193\000\030\000\
\142\000\201\000\143\000\144\000\044\000\044\000\208\000\212\000\
\044\000\213\000\030\000\216\000\030\000\030\000\044\000\039\000\
\217\000\043\000\043\000\044\000\044\000\043\000\039\000\039\000\
\039\000\220\000\221\000\043\000\044\000\042\000\042\000\232\000\
\043\000\042\000\233\000\044\000\044\000\044\000\234\000\042\000\
\235\000\043\000\041\000\041\000\064\000\018\000\041\000\241\000\
\043\000\043\000\043\000\247\000\041\000\042\000\248\000\055\000\
\003\000\023\000\085\000\012\000\042\000\042\000\042\000\030\000\
\135\000\156\000\041\000\050\000\010\000\138\000\066\000\018\000\
\016\000\041\000\041\000\041\000\140\000\052\000\141\000\183\000\
\046\000\055\000\181\000\023\000\017\000\018\000\246\000\071\000\
\018\000\142\000\225\000\222\000\132\000\000\000\000\000\055\000\
\000\000\023\000\055\000\000\000\023\000"

let yycheck = "\029\000\
\000\000\002\001\008\001\166\000\000\000\005\001\000\000\005\001\
\019\000\030\000\011\001\022\000\033\001\000\000\030\001\016\001\
\017\001\018\001\034\001\000\000\001\001\002\001\013\001\030\001\
\005\001\000\000\024\001\034\001\034\001\005\001\011\001\000\000\
\053\000\044\000\022\000\016\001\017\001\018\001\000\000\134\000\
\031\001\042\001\042\001\138\000\000\000\140\000\057\000\005\001\
\036\000\000\000\038\000\039\000\215\000\034\001\042\000\043\000\
\077\000\045\000\046\000\047\000\048\000\042\001\018\001\158\000\
\159\000\160\000\042\001\055\000\005\001\164\000\002\001\005\001\
\005\001\168\000\169\000\170\000\171\000\172\000\173\000\011\001\
\175\000\176\000\093\000\005\001\042\001\002\001\042\001\013\001\
\099\000\005\001\034\001\005\001\122\000\081\000\011\001\083\000\
\028\001\085\000\086\000\110\000\032\001\005\001\090\000\091\000\
\092\000\042\001\005\001\005\001\042\001\042\001\042\001\028\001\
\003\001\032\001\024\001\034\001\211\000\013\001\213\000\013\001\
\042\001\216\000\217\000\111\000\112\000\042\001\042\001\115\000\
\042\001\117\000\118\000\119\000\024\001\010\001\005\001\001\001\
\002\001\031\001\042\001\001\001\002\001\005\001\033\001\042\001\
\042\001\011\001\241\000\024\001\024\001\011\001\016\001\017\001\
\018\001\005\001\016\001\017\001\018\001\005\001\005\001\025\001\
\005\001\149\000\150\000\025\001\024\001\153\000\154\000\033\001\
\034\001\001\001\002\001\042\001\034\001\001\001\002\001\139\000\
\042\001\032\001\042\001\011\001\042\001\032\001\146\000\011\001\
\016\001\017\001\018\001\042\001\016\001\017\001\018\001\042\001\
\013\001\014\001\042\001\042\001\032\001\042\001\024\001\032\001\
\024\001\033\001\034\001\024\001\032\001\026\001\034\001\001\001\
\002\001\042\001\042\001\016\001\017\001\018\001\042\001\032\001\
\204\000\011\001\206\000\001\001\002\001\032\001\016\001\017\001\
\018\001\042\001\017\001\018\001\024\001\011\001\033\001\042\001\
\001\000\002\000\016\001\017\001\018\001\042\001\032\001\007\001\
\034\001\009\001\230\000\231\000\012\001\013\001\014\001\015\001\
\042\001\033\001\032\001\042\001\034\001\021\001\022\001\032\001\
\024\001\033\001\026\001\214\000\042\001\005\001\012\001\013\001\
\008\001\042\001\033\001\017\001\036\001\032\001\008\001\032\001\
\227\000\041\001\024\001\032\001\026\001\008\001\042\001\042\001\
\235\000\042\001\237\000\008\001\033\001\042\001\030\001\037\001\
\032\001\008\001\034\001\035\001\030\001\033\001\032\001\008\001\
\034\001\035\001\042\001\030\001\033\001\032\001\008\001\034\001\
\035\001\030\001\004\001\032\001\008\001\034\001\035\001\030\001\
\032\001\032\001\019\001\034\001\035\001\030\001\013\001\032\001\
\013\001\034\001\035\001\020\001\030\001\013\001\032\001\001\001\
\034\001\035\001\030\001\005\001\032\001\001\001\002\001\035\001\
\031\001\001\001\002\001\020\001\034\001\035\001\033\001\011\001\
\013\001\014\001\032\001\011\001\016\001\017\001\018\001\025\001\
\016\001\017\001\018\001\024\001\042\001\026\001\032\001\033\001\
\034\001\032\001\013\001\014\001\032\001\013\001\034\001\001\001\
\002\001\033\001\034\001\042\001\033\001\024\001\042\001\026\001\
\033\001\011\001\042\001\006\001\032\001\033\001\016\001\017\001\
\018\001\012\001\013\001\023\001\015\001\013\001\017\001\025\001\
\019\001\032\001\032\001\033\001\033\001\024\001\006\001\026\001\
\034\001\024\001\029\001\042\001\012\001\013\001\013\001\015\001\
\042\001\017\001\037\001\019\001\039\001\040\001\032\001\033\001\
\024\001\006\001\026\001\032\001\033\001\029\001\024\001\012\001\
\013\001\029\001\015\001\006\001\017\001\037\001\019\001\039\001\
\040\001\012\001\013\001\024\001\015\001\026\001\017\001\001\001\
\019\001\008\000\009\000\005\001\029\001\024\001\038\001\026\001\
\037\001\013\001\039\001\040\001\001\001\002\001\024\001\032\001\
\005\001\033\001\037\001\024\001\039\001\040\001\011\001\025\001\
\010\001\001\001\002\001\016\001\017\001\005\001\032\001\033\001\
\034\001\032\001\013\001\011\001\025\001\001\001\002\001\033\001\
\016\001\005\001\033\001\032\001\033\001\034\001\013\001\011\001\
\008\001\025\001\001\001\002\001\013\001\014\001\005\001\024\001\
\032\001\033\001\034\001\032\001\011\001\025\001\033\001\024\001\
\000\000\026\001\000\000\033\001\032\001\033\001\034\001\019\001\
\012\001\013\001\025\001\032\001\032\001\017\001\013\001\014\001\
\029\001\032\001\033\001\034\001\024\001\032\001\026\001\027\001\
\032\001\024\001\155\000\026\001\013\001\014\001\241\000\013\001\
\014\001\037\001\213\000\211\000\120\000\255\255\255\255\024\001\
\255\255\026\001\024\001\255\255\026\001"

let yynames_const = "\
  AMPERAMPER\000\
  BANGEQUAL\000\
  BARBAR\000\
  COLON\000\
  COMMA\000\
  DISPOSE\000\
  DLSEG\000\
  ELSE\000\
  EMPTY\000\
  EOF\000\
  EQUAL\000\
  EQUALEQUAL\000\
  FF\000\
  IF\000\
  LBRACE\000\
  LBRACKET\000\
  LIST\000\
  LISTSEG\000\
  LOCAL\000\
  LPAREN\000\
  MINUSGREATER\000\
  NEW\000\
  POINTSTO\000\
  RBRACE\000\
  RBRACKET\000\
  RESOURCE\000\
  RPAREN\000\
  SEMI\000\
  STAR\000\
  THEN\000\
  TREE\000\
  TT\000\
  WHEN\000\
  WHILE\000\
  WITH\000\
  XLSEG\000\
  XOR\000\
  "

let yynames_block = "\
  IDENT\000\
  QIDENT\000\
  INFIXOP1\000\
  INFIXOP2\000\
  INFIXOP3\000\
  NAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program_item_star) in
    Obj.repr(
# 115 "parser.mly"
      ( Pprogram ([Config.list_data_tag; Config.list_link_tag;
                   Config.tree_data_tag;
                   fst Config.tree_link_tags; snd Config.tree_link_tags],
                  _1) )
# 478 "parser.ml"
               : Parsetree.p_program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program_item_star) in
    Obj.repr(
# 119 "parser.mly"
                                 ( Pprogram (_1,_2) )
# 486 "parser.ml"
               : Parsetree.p_program))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                           ( [] )
# 492 "parser.ml"
               : 'program_item_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program_item) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program_item_star) in
    Obj.repr(
# 123 "parser.mly"
                                   ( _1::_2 )
# 500 "parser.ml"
               : 'program_item_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_decl) in
    Obj.repr(
# 126 "parser.mly"
             ( _1 )
# 507 "parser.ml"
               : 'program_item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'resource_decl) in
    Obj.repr(
# 127 "parser.mly"
                  (_1 )
# 514 "parser.ml"
               : 'program_item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'formal_params) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'invariant) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'local_decls) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'statement_star) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'invariant) in
    Obj.repr(
# 131 "parser.mly"
      ( Pfundecl(_1,_3,_5,_7,_8,_10,Location.rhs_loc 10,Location.symbol_loc()) )
# 526 "parser.ml"
               : 'fun_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ident_seq) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Parsetree.a_proposition) in
    Obj.repr(
# 135 "parser.mly"
      ( Presource(_2,_4,_7,Location.symbol_loc()) )
# 535 "parser.ml"
               : 'resource_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident_seq) in
    Obj.repr(
# 138 "parser.mly"
                   ( _1 )
# 542 "parser.ml"
               : 'ident_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                     ( [] )
# 548 "parser.ml"
               : 'ident_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident_notempty_seq) in
    Obj.repr(
# 142 "parser.mly"
                       ( _1 )
# 555 "parser.ml"
               : 'ident_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                     ( [_1] )
# 562 "parser.ml"
               : 'ident_notempty_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_notempty_seq) in
    Obj.repr(
# 146 "parser.mly"
                                   ( _1::_3 )
# 570 "parser.ml"
               : 'ident_notempty_seq))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                             ( [] )
# 576 "parser.ml"
               : 'local_decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_notempty_seq) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'local_decls) in
    Obj.repr(
# 150 "parser.mly"
                                              ( _2 @ _4 )
# 584 "parser.ml"
               : 'local_decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
                             ( [] )
# 590 "parser.ml"
               : 'statement_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement_star) in
    Obj.repr(
# 154 "parser.mly"
                             ( _1::_2 )
# 598 "parser.ml"
               : 'statement_star))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 158 "parser.mly"
   ( mkstm(Pstm_assign(_1, _3)) )
# 606 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 160 "parser.mly"
   ( mkstm(Pstm_fldlookup(_1, _3, _5)) )
# 615 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 162 "parser.mly"
   ( mkstm(Pstm_fldassign(_1, _3, _5)) )
# 624 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    Obj.repr(
# 164 "parser.mly"
   ( mkstm(Pstm_new(_1)) )
# 631 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 166 "parser.mly"
          ( mkstm(Pstm_dispose(_2)) )
# 638 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statement_star) in
    Obj.repr(
# 168 "parser.mly"
   ( mkstm(Pstm_block(_2)) )
# 645 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 170 "parser.mly"
   ( mkstm(Pstm_if(_3, _5, mkstm_ghost(Pstm_block []))) )
# 653 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 172 "parser.mly"
   ( mkstm(Pstm_if(_3, _5, _7)) )
# 662 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'invariant) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 174 "parser.mly"
   ( mkstm(Pstm_while(_5, _3, _6)) )
# 671 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 176 "parser.mly"
          ( mkstm(Pstm_withres(_2,_5,_7)) )
# 680 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'actual_params) in
    Obj.repr(
# 178 "parser.mly"
          ( mkstm(Pstm_fcall(_1,_3)) )
# 688 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'actual_params) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'actual_params) in
    Obj.repr(
# 182 "parser.mly"
          ( mkstm(Pstm_parallel_fcall(_1,_3,_6,_8)) )
# 698 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "parser.mly"
          ( None )
# 704 "parser.ml"
               : 'invariant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Parsetree.a_proposition) in
    Obj.repr(
# 189 "parser.mly"
          ( Some _2 )
# 711 "parser.ml"
               : 'invariant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "parser.mly"
   ( mkexp(Pexp_ident(_1)) )
# 718 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 195 "parser.mly"
   ( mkexp(Pexp_num(_1)) )
# 725 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "parser.mly"
          ( mkexp(Pexp_infix("==", mkexp_ghost(Pexp_num(0)), mkexp_ghost(Pexp_num(0)))) )
# 731 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 199 "parser.mly"
          ( mkexp(Pexp_infix("!=", mkexp_ghost(Pexp_num(0)), mkexp_ghost(Pexp_num(0)))) )
# 737 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 201 "parser.mly"
   ( _2 )
# 744 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 203 "parser.mly"
   ( mkexp(Pexp_prefix(_1, _2)) )
# 752 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 205 "parser.mly"
   ( mkexp(Pexp_infix("&&", _1, _3)) )
# 760 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 207 "parser.mly"
   ( mkexp(Pexp_infix("*", _1, _3)) )
# 768 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 209 "parser.mly"
   ( mkexp(Pexp_infix("^", _1, _3)) )
# 776 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 211 "parser.mly"
   ( mkexp(Pexp_infix("==", _1, _3)) )
# 784 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 213 "parser.mly"
   ( mkexp(Pexp_infix("!=", _1, _3)) )
# 792 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 215 "parser.mly"
   ( mkexp(Pexp_infix(_2, _1, _3)) )
# 801 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 217 "parser.mly"
   ( mkexp(Pexp_infix(_2, _1, _3)) )
# 810 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 219 "parser.mly"
   ( mkexp(Pexp_infix(_2, _1, _3)) )
# 819 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 222 "parser.mly"
                     ( [] )
# 825 "parser.ml"
               : 'expression_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_notempty_seq) in
    Obj.repr(
# 223 "parser.mly"
                            ( _1 )
# 832 "parser.ml"
               : 'expression_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 226 "parser.mly"
                          ( [_1] )
# 839 "parser.ml"
               : 'expression_notempty_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_notempty_seq) in
    Obj.repr(
# 227 "parser.mly"
                                             ( _1::_3 )
# 847 "parser.ml"
               : 'expression_notempty_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident_seq) in
    Obj.repr(
# 230 "parser.mly"
              ( mk_formal_params ([],_1) (Location.symbol_loc()) )
# 854 "parser.ml"
               : 'formal_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_seq) in
    Obj.repr(
# 231 "parser.mly"
                             ( mk_formal_params (_1,_3) (Location.symbol_loc()) )
# 862 "parser.ml"
               : 'formal_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_seq) in
    Obj.repr(
# 234 "parser.mly"
                   ( ([],_1) )
# 869 "parser.ml"
               : 'actual_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression_seq) in
    Obj.repr(
# 235 "parser.mly"
                                       ( (mk_ref_params _1 (Location.rhs_loc 1), _3) )
# 877 "parser.ml"
               : 'actual_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 238 "parser.mly"
                     ( [] )
# 883 "parser.ml"
               : 'a_component_expression_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'a_component_expression_notempty_seq) in
    Obj.repr(
# 239 "parser.mly"
                                        ( _1 )
# 890 "parser.ml"
               : 'a_component_expression_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 242 "parser.mly"
                                        ( [(_1,_3)] )
# 898 "parser.ml"
               : 'a_component_expression_notempty_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'a_component_expression_notempty_seq) in
    Obj.repr(
# 243 "parser.mly"
                                                                       ( (_1,_3)::_5 )
# 907 "parser.ml"
               : 'a_component_expression_notempty_seq))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 247 "parser.mly"
   ( Aspred_list(_3,_5) )
# 915 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 249 "parser.mly"
   ( Aspred_list(Config.list_link_tag, _3) )
# 922 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 251 "parser.mly"
   ( Aspred_listseg(_3,_5,_7) )
# 931 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 253 "parser.mly"
   ( Aspred_listseg(Config.list_link_tag, _3, _5) )
# 939 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 7 : 'a_expression) in
    let _9 = (Parsing.peek_val __caml_parser_env 5 : 'a_expression) in
    let _11 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _13 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 255 "parser.mly"
   ( Aspred_dlseg(DL,_3,_7,_9,_5,_11,_13) )
# 951 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'a_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'a_expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 257 "parser.mly"
   ( Aspred_dlseg(DL, Config.dl_Rlink_tag, _3, _5, Config.dl_Llink_tag,_7, _9) )
# 961 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 7 : 'a_expression) in
    let _9 = (Parsing.peek_val __caml_parser_env 5 : 'a_expression) in
    let _11 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _13 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 259 "parser.mly"
   ( Aspred_dlseg(XL,_3,_7,_9,_5,_11,_13) )
# 973 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'a_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'a_expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'a_expression) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 261 "parser.mly"
   ( Aspred_dlseg(XL, Config.dl_Llink_tag, _3, _5, Config.dl_Llink_tag,_7, _9) )
# 983 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 263 "parser.mly"
          ( Aspred_tree(_3,_5,_7) )
# 992 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 265 "parser.mly"
          ( Aspred_tree(fst Config.tree_link_tags, snd Config.tree_link_tags,
                        _3) )
# 1000 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    Obj.repr(
# 268 "parser.mly"
          ( Aspred_empty )
# 1006 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_component_expression_seq) in
    Obj.repr(
# 270 "parser.mly"
          ( Aspred_pointsto(_1,_3) )
# 1014 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 272 "parser.mly"
          ( Aspred_pointsto(_1,[(Config.list_link_tag, _3)]) )
# 1022 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 274 "parser.mly"
          ( Aspred_pointsto(_1,[(fst Config.tree_link_tags, _3);
                                (snd Config.tree_link_tags, _5)]) )
# 1032 "parser.ml"
               : 'a_space_pred))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Parsetree.a_proposition) in
    Obj.repr(
# 280 "parser.mly"
   ( _2 )
# 1039 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 282 "parser.mly"
   ( Aprop_equal(_1,_3) )
# 1047 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 284 "parser.mly"
   ( Aprop_not_equal(_1,_3) )
# 1055 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    Obj.repr(
# 286 "parser.mly"
   ( Aprop_false )
# 1061 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Parsetree.a_proposition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Parsetree.a_proposition) in
    Obj.repr(
# 288 "parser.mly"
          ( Aprop_star(_1,_3) )
# 1069 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Parsetree.a_proposition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Parsetree.a_proposition) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Parsetree.a_proposition) in
    Obj.repr(
# 290 "parser.mly"
      ( Aprop_ifthenelse(_2,_4,_6) )
# 1078 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'a_space_pred) in
    Obj.repr(
# 292 "parser.mly"
          ( Aprop_spred _1 )
# 1085 "parser.ml"
               : Parsetree.a_proposition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'a_expression) in
    Obj.repr(
# 295 "parser.mly"
                                          ( _2 )
# 1092 "parser.ml"
               : 'a_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'a_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_expression) in
    Obj.repr(
# 296 "parser.mly"
                                          ( Aexp_infix("^",_1,_3) )
# 1100 "parser.ml"
               : 'a_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 297 "parser.mly"
                                          ( Aexp_ident(_1) )
# 1107 "parser.ml"
               : 'a_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 298 "parser.mly"
                                          ( Aexp_ident(_1) )
# 1114 "parser.ml"
               : 'a_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 299 "parser.mly"
                                          ( Aexp_num(_1) )
# 1121 "parser.ml"
               : 'a_expression))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry a_proposition *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Parsetree.p_program)
let a_proposition (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Parsetree.a_proposition)
;;
# 302 "parser.mly"
 (* trailer *)
# 1152 "parser.ml"
