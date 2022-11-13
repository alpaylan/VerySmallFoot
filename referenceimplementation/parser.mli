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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.p_program
val a_proposition :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parsetree.a_proposition
