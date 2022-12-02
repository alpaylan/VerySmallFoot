{-# LANGUAGE RankNTypes #-}
module Parser (parseFunction, parseProgram, parseResource, parseFunctionFile, parseProgramFile, parseResourceFile) where 
import Text.Parsec (getInput, many1, alphaNum, eof, option, (<|>), try, string, char, sepBy, lookAhead, sepBy1, many, chainl1, Parsec)
import qualified Text.Parsec as P
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import Data.Functor
import Control.Monad (join)
import Debug.Trace (trace)
import Program


-- -- Interface
-- -- =========

parseFromFile :: Parser a -> FilePath -> IO (Either P.ParseError a)
parseFromFile p fname = do
    input <- readFile fname
    return (P.parse p fname input)

parseFunction :: String -> Either P.ParseError Function
parseFunction = P.parse funDecl ""

parseProgram :: String -> Either P.ParseError Program
parseProgram = P.parse program ""

parseResource :: String -> Either P.ParseError Resource
parseResource = P.parse resourceDecl ""

parseFunctionFile :: FilePath -> IO (Either P.ParseError Function)
parseFunctionFile = parseFromFile funDecl

parseProgramFile :: FilePath -> IO (Either P.ParseError Program)
parseProgramFile = parseFromFile program

parseResourceFile :: FilePath -> IO (Either P.ParseError Resource)
parseResourceFile = parseFromFile resourceDecl

-- -- Input Language
-- -- ==============
type Parser = Parsec String ()

smallfootStyle :: LanguageDef st
smallfootStyle = 
    javaStyle 
    { Token.commentLine = ""
    , Token.identLetter = alphaNum <|> char '_'
    , Token.reservedNames = ["NULL", "nil", "dispose", "dlseg", "else", "emp", "false",
                        "if", "list", "lseg", "local", "new", "resource",
                        "then", "tree", "true", "when", "while", "with",
                        "xlseg"]
    , Token.reservedOpNames = ["==", "!=", "^",
                      "&&", "*", "/", "%", "+", "-", "<", "<=", ">", ">=",
                      "+","-", "!", "|->"]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser smallfootStyle

-- -- Lexical structure
-- -----------------
-- whitespace ::= blank | comment
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer
--whitespace = (space $> ()) <|> Token.comment
-- blank      ::= horizontal tab | linefeed | vertical tab | formfeed
--                 | carriage return | space
-- comment    ::= "/*" anything "*/"

-- -- Whitespace delimits tokens but is otherwise ignored.  Comments nest.

-- keywords: NULL dispose dlseg else emp false if list lseg local new
--             resource then tree true when while with xlseg

-- -- NULL is a synonym for 0

-- ident    ::= letter alphanum*
ident :: Parser String
ident = whitespace *> Token.identifier lexer <* whitespace
-- field    ::= ident
field :: Parser String
field = ident
-- number   ::= digit+
number :: Parser Integer
number = Token.integer lexer
-- letter   ::= "A"--"Z" | "_" | "a"--"z"
-- alphanum ::= digit | letter
-- digit    ::= "0"--"9"

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

choice = P.choice . fmap P.try 

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

-- Grammar
-- -------

-- program ::= (field_seq ";")? (resource_decl | fun_decl)*
program :: Parser Program
program = do
    whitespace
    fields <- option [] (fieldSeq <* char ';')
    whitespace
    (funcs, reses) <- eitherToLists <$> many ((Right <$> lexeme resourceDecl) <|> (Left <$> lexeme funDecl))
    eof
    return $ Program fields reses funcs

eitherToLists :: [Either Function Resource] -> ([Function],[Resource])
eitherToLists = foldr (\x (fs,rs) -> case x of
    Left f -> (f:fs,rs)
    Right r -> (fs,r:rs)) ([],[])


fieldSeq :: Parser [FieldName]
fieldSeq = identSeq
--     The field_seq declares the valid field names, which by default
--     includes the default fields of all the predicates, see below.

-- resource_decl ::= "resource" ident "(" ident_seq ")" "[" formula "]"
resourceDecl :: Parser Resource
resourceDecl = do
    whitespace
    reserved "resource"
    whitespace
    name <- ident
    whitespace
    args <- parens identSeq
    whitespace
    prop <- brackets prop
    return $ Resource name args prop

identSeq :: Parser [String]
identSeq = sepBy ident (char ',' <* whitespace)
-- fun_decl      ::= ident "(" formals ")" ("[" formula "]")?
--                     "{" local_decl* statement* "}" ("[" formula "]")?
funDecl :: Parser Function
funDecl = do
    whitespace
    funcName <- ident
    whitespace
    (refArgs, valArgs) <- parens formals
    whitespace
    pre <- option (PropConj PropTrue HeapEmp) (brackets prop)
    whitespace
    lookAhead (char '{')
    (localVars, body) <- braces $ do
        whitespace
        localVars <- join <$> many (lexeme localDecl)
        whitespace
        statements <- many (lexeme statement)
        return (localVars, statements)
    whitespace
    post <- option (PropConj PropTrue HeapEmp) (brackets prop)
    return $ Function funcName refArgs valArgs localVars (pre, Block body, post)

blah = 
    "cas(status,location;original,old,nw) [location==original] { \n\
    \    local x, y, z; \n\
    \    if (location == old) { \n\
	\        location = nw; \n\
	\        status = 1; \n\
    \    } else {\n\
	\        status = 0;\n\
    \    }\n\
    \  } [if original==old then location == nw && status==1 else status==0 && location == original ]"

blah' = "local x, y, z; \n\
    \    if (location == old) { \n\
	\        location = nw; \n\
	\        status = 1; \n\
    \    } else {\n\
	\        status = 0;\n\
    \    }\n"

mallocs = "malloc1(i;) \n\
\ [emp] \n\
\{ \n\
\  local n,status,top,next; \n\
\  status=0; \n\
\  while(status == 0) [(if status == 0 then emp else i |->)] { \n\
\    with freelist1 when (true) { \n \
\ i = TOP; \n\
 \   } \n\
 \ \n\
 \   if(i!=nil) { \n\
  \    with freelist1 when (true) { \n\
\ if(TOP == i) { \n\
\ n = i->tl; \n\
\} else { \n\
 \            /*  n = i->tl;   Can't read as don't have permission need emp read rule */ \n\
\} \n\
 \     } \n\
\ \n\
 \     with freelist1 when (true) { \n\
\/* Couldn't be bothered to write a DCAS instruction, so hacked a CAS one. */ \n\
\top = TOP; \n\
\cas(status,top;top,i,n); \n\
\if(status==1) { \n\
 \ next = i->tl; \n\
\  if(next == n) \n\
\    TOP = top; \n\
\  else  \n\
\    status = 0; \n\
\} \n\
 \     } \n\
  \  } \n\
 \ } \n\
\} \n\
\[i |->]"

-- formals       ::= (ident_seq ";")? ident_seq
formals :: Parser ([String], [String])
formals = do
    whitespace
    refArgs <- option [] (try (identSeq <* char ';'))
    whitespace
    valArgs <- identSeq
    return (refArgs, valArgs)
-- ident_seq     ::= /* empty */ | ident ("," ident)*
-- local_decl    ::= "local" ident ("," ident)* ";"
localDecl :: Parser [String]
localDecl = do
    whitespace
    reserved "local"
    whitespace
    vars <- identSeq
    whitespace
    char ';'
    return vars
--     The formals consist of the reference parameters (which are
--     optional) followed by the value parameters.  If a function pre- or
--     post-condition is omitted, it defaults to "emp".

-- statement    ::= ident "=" stmt_exp ";"
--             | ident "=" stmt_exp "->" field ";"
--             | stmt_exp "->" field "=" stmt_exp ";"
--             | ident "=" "new" "(" ")" ";"
--             | "dispose" stmt_exp ";"
--             | "{" statement* "}"
--             | "if" "(" stmt_exp ")" statement ("else" statement)?
--             | "while" "(" stmt_exp ")" ("[" formula "]")? statement
--             | "with" ident "when" "(" stmt_exp ")" statement
--             | ident "(" actuals ")" ";"
--             | ident "(" actuals ")" "||" ident "(" actuals ")" ";"
statement :: Parser Command
statement = choice $ try <$> [assign, assignField, assignFieldExp, allocVar, dispose, statementBlock, ifThenElseStmt, whileStmt, withStmt, callStmt, concCallStmt]

assign :: Parser Command
assign = do
    whitespace
    var <- ident
    whitespace
    char '='
    whitespace
    exp <- stmtExp
    whitespace
    char ';'
    return $ Assign var exp

assignField :: Parser Command
assignField = do
    whitespace
    var <- ident
    whitespace
    char '='
    whitespace
    exp <- stmtExp
    whitespace
    string "->"
    whitespace
    field <- field
    whitespace
    char ';'
    return $ HeapLookup var exp field

assignFieldExp :: Parser Command
assignFieldExp = do
    whitespace
    exp1 <- stmtExp
    whitespace
    string "->"
    whitespace
    field <- field
    whitespace
    char '='
    whitespace
    exp2 <- stmtExp
    whitespace
    char ';'
    return $ HeapAssign exp1 field exp2

allocVar :: Parser Command
allocVar = do
    whitespace
    var <- ident
    whitespace
    char '='
    whitespace
    reserved "new"
    whitespace
    parens (return ())
    whitespace
    char ';'
    return $ New var

dispose :: Parser Command
dispose = do
    whitespace
    reserved "dispose"
    whitespace
    exp <- stmtExp
    whitespace  
    char ';'
    return $ Dispose exp

statementBlock :: Parser Command
statementBlock = do
    whitespace
    statements <- braces (many (lexeme statement))
    return . Block $ statements

ifThenElseStmt :: Parser Command
ifThenElseStmt = do
    whitespace
    reserved "if"
    whitespace
    exp <- parens boolExp
    whitespace
    thenStmt <- statement
    whitespace
    elseStmt <- option (Block []) (lexeme (reserved "else") >> statement)
    whitespace
    return $ IfThenElse exp thenStmt elseStmt

whileStmt :: Parser Command
whileStmt = do
    whitespace
    reserved "while"
    whitespace
    exp <- parens boolExp
    whitespace
    inv <- option (PropConj PropTrue HeapEmp) (brackets prop)
    whitespace
    While exp inv <$> statement

withStmt :: Parser Command
withStmt = do
    whitespace
    reserved "with"
    whitespace
    var <- ident
    whitespace
    reserved "when"
    whitespace
    exp <- parens boolExp
    whitespace
    WithRes var exp <$> statement

callStmt :: Parser Command
callStmt = do
    whitespace
    funcName <- ident
    whitespace
    (refArgs, valArgs) <- parens actuals
    whitespace
    char ';'
    return $ Call (funcName, refArgs, valArgs)

concCallStmt :: Parser Command
concCallStmt = do
    whitespace
    funcName1 <- ident
    whitespace
    (refArgs1, valArgs1) <- parens actuals
    whitespace
    string "||"
    whitespace
    funcName2 <- ident
    whitespace
    (refArgs2, valArgs2) <- parens actuals
    whitespace
    char ';'
    return $ ConcurrentCall (funcName1, refArgs1, valArgs1) (funcName2, refArgs2, valArgs2)



-- actuals      ::= stmt_exp_seq (";" stmt_exp_seq)?
actuals :: Parser ([VarName], [Expression])
actuals = do
    whitespace
    refArgs <- sepBy ident (whitespace >> string "," >> whitespace)
    whitespace
    valArgs <- option [] (char ';' >> stmtExpSeq)
    return (refArgs, valArgs)
-- stmt_exp     ::= "(" stmt_exp ")"
--             | ident | number | "true" | "false"
--             | prefix_op stmt_exp | stmt_exp infix_op stmt_exp
stmtExp :: Parser Expression
stmtExp = chainl1 stmtTerm (lexeme (char '^') $> Xor)

stmtTerm :: Parser Expression
stmtTerm = whitespace >> choice [parens stmtExp, 
            Var <$> ident, 
            Nil <$ reserved "NULL",
            Const . fromIntegral <$> number]

-- stmt_exp_seq ::= /* empty */ | stmt_exp ("," stmt_exp)*
stmtExpSeq :: Parser [Expression]
stmtExpSeq = sepBy stmtExp (whitespace >> char ',' >> whitespace)
-- infix_op     ::= "==" | "!=" | "^" | "&&" | "*" | "/" | "%" | "+" | "-" | "<" | "<=" | ">" | ">="

boolExp :: Parser BoolExpression
boolExp = choice [parens boolExp, 
            boolEq,
            boolNeq,
            boolNot,
            BoolTrue <$ reserved "true",
            BoolNot BoolTrue <$ reserved "false"
            ]
  where
    boolEq = do
        whitespace
        exp1 <- stmtExp
        whitespace
        reservedOp "=="
        whitespace
        BoolEq exp1 <$> stmtExp
    boolNeq = do
        whitespace
        exp1 <- stmtExp
        whitespace
        reservedOp "!="
        whitespace
        BoolNEq exp1 <$> stmtExp
    boolNot = do
        whitespace
        reservedOp "!"
        whitespace
        BoolNot <$> boolExp

-- prefix_op    ::= "+" | "-"

--     If a loop invariant is omitted, it defaults to "emp".  All of the
--     infix_op's except equality "==", non-equality "!=", and
--     exclusive-or "^" are essentially unimplemented.

-- formula  ::= "(" formula ")"
--             | "false"
--             | form_exp "==" form_exp
--             | form_exp "!=" form_exp
--             | "emp"
--             | formula "*" formula
--             | form_exp "|->" ((field ":" form_exp)*
--                             | form_exp | form_exp "," form_exp)
--             | "list" "(" (field ";")? form_exp ")"
--             | "lseg" "(" (field ";")? form_exp "," form_exp ")"
--             | ("dlseg" | "xlseg") "(" (field ";" field ";")?
--                 form_exp "," form_exp "," form_exp "," form_exp ")"
--             | "tree" "(" (field ";" field ";")? form_exp ")"
--             | "if" form_exp ("==" | "!=") form_exp "then" formula "else" formula

prop :: Parser Prop
prop = choice [parens prop, ifThenElseProp, conjProp, pureProp', heapProp']
  where
    ifThenElseProp = do
        whitespace
        reserved "if"
        whitespace
        pProp <- pureProp 
        whitespace
        reserved "then"
        whitespace
        propT <- prop
        whitespace
        reserved "else"
        whitespace
        propF <- prop
        whitespace
        return $ PropIfThenElse pProp propT propF
    conjProp = do
        whitespace
        pProp <- option PropTrue pureProp
        whitespace
        string ";"
        whitespace
        PropConj pProp <$> option HeapEmp heapProp
    pureProp' = do
        whitespace
        PropConj <$> pureProp <*> pure HeapEmp
    heapProp' = do
        whitespace
        PropConj PropTrue <$> heapProp

pureProp :: Parser PureProp
pureProp = chainl1 (choice [assert, true]) and
  where
    and = lexeme (reservedOp "&&") $> PropAnd
    assert = PropAssert <$> boolExp
    true = PropTrue <$ (whitespace >> string "true" >> whitespace)

heapProp :: Parser HeapProp
heapProp = chainl1 (choice [parens heapProp, pointsTo, heapTree, heapLS, heapLst, heapXORL, heapEmp]) heapSep
  where
    pointsTo = do
      whitespace
      recd <- stmtExp
      whitespace
      string "|->"
      whitespace
      PointsTo recd <$> sepBy fieldExp (char ',')
    fieldExp = do
      whitespace
      field <- ident
      whitespace
      string ":"
      whitespace
      exp <- stmtExp
      return (field, exp)
    heapTree = do
      whitespace
      reserved "tree"
      whitespace
      HeapTree <$> stmtExp
    heapLS = do
      whitespace
      reserved "lseg"
      lexeme (char '(') $> ()
      start <- stmtExp
      whitespace
      lexeme (string ",") $> ()
      HeapListSegment start <$> (stmtExp <* (whitespace >> lexeme (char ')')))
    heapXORL = do
      whitespace
      reserved "xlseg"
      string "("
      whitespace
      e1 <- stmtExp <* (whitespace >> lexeme (string ","))
      whitespace
      e2 <- stmtExp <* (whitespace >> lexeme (string ","))
      whitespace
      e3 <- stmtExp
      string ")"
      whitespace
      HeapXORList e1 e2 e3 <$> stmtExp
    heapSep = lexeme (reservedOp "*") $> HeapSep
    heapLst = do
      whitespace
      reserved "list"
      HeapListSegment <$> parens (whitespace *> stmtExp <* whitespace) <*> pure Nil
    heapEmp = HeapEmp <$ (whitespace >> reserved "emp" >> whitespace)

       
-- form_exp ::= "(" form_exp ")" | ident | number | form_exp "^" form_exp

--     Here, unlike in the related papers, formulae are not composed of
--     distinct boolean (pure) and heap (spatial) parts.  Instead "E==F"
--     and "E!=F" are only satisfied by the empty heap, like "emp", and
--     "*" is the only conjunction.

--     For the "|->" predicate, in the second alternative the "tl" field
--     is assumed, and in the third alternative "l" and "r" fields are
--     assumed.  For the inductive predicates, omitted fields default to:
--     - "hd" and "tl" for lists;
--     - "d", "l", and "r" for doubly-linked lists;
--     - "d" and "l" for xor-linked lists; and
--     - "d", "l", and "r" for trees.


-- -- precedences (increasing) and associativities:
-- -- 1  * &&         L
-- -- 2  == !=        L
-- -- 3  = < <= > >=  L
-- -- 4  + -          L
-- -- 5  ^ |-> / %    L
