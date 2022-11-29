{-# LANGUAGE RankNTypes #-}
module Parser () where 

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import Data.Functor
import Program

-- -- Input Language
-- -- ==============
type Parser = Parsec String ()

smallfootStyle :: LanguageDef st
smallfootStyle = 
    javaStyle 
    { Token.commentLine = ""
    , Token.identLetter = alphaNum <|> char '_'
    , Token.reservedNames = ["NULL", "dispose", "dlseg", "else", "emp", "false",
                        "if", "list", "lseg", "local", "new", "resource",
                        "then", "tree", "true", "when", "while", "with",
                        "xlseg"]
    , Token.reservedOpNames = ["==", "!=", "^",
                      "&&", "*", "/", "%", "+", "-", "<", "<=", ">", ">=",
                      "+","-"]
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
ident = Token.identifier lexer
-- field    ::= ident
field :: Parser String
field = ident
-- number   ::= digit+
number :: Parser Int
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

-- Grammar
-- -------

-- program ::= (field_seq ";")? (resource_decl | fun_decl)*
program :: Parser Program
program = do
    whitespace
    fields <- option [] (fieldSeq <* char ';')
    whitespace
    decls <- many (resourceDecl <|> funDecl)
    eof
    return $ Program fields decls

fieldSeq :: Parser [FieldName]
fieldSeq = identSeq
--     The field_seq declares the valid field names, which by default
--     includes the default fields of all the predicates, see below.

-- resource_decl ::= "resource" ident "(" ident_seq ")" "[" formula "]"
resourceDecl :: Parser ProgramDeclaration
resourceDecl = do
    whitespace
    reserved "resource"
    whitespace
    name <- Id <$> ident
    whitespace
    args <- parens identSeq
    whitespace
    formula <- brackets formula
    return . ResourceDeclaration $ Resource name args formula

identSeq :: Parser [Identifier]
identSeq = sepBy (Id <$> ident) (char ',' <* whitespace)
-- fun_decl      ::= ident "(" formals ")" ("[" formula "]")?
--                     "{" local_decl* statement* "}" ("[" formula "]")?
funDecl :: Parser ProgramDeclaration
funDecl = do
    whitespace
    funcName <- Id <$> ident
    whitespace
    (refArgs, valArgs) <- parens formals
    whitespace
    pre <- option (SingleBooleanPredicate BooleanTrue) (brackets formula)
    whitespace
    body <- braces $ do
        whitespace
        localDecls <- many localDecl
        whitespace
        statements <- many statement
        return (localDecls, statements)
    whitespace
    post <- option (SingleBooleanPredicate BooleanTrue) (brackets formula)
    return . FunctionDeclaration $ Function (FunctionHeader funcName refArgs valArgs) (HoareTriple pre body post)



-- formals       ::= (ident_seq ";")? ident_seq
formals :: Parser ([PassByReferenceArgument], [PassByValueArgument])
formals = do
    whitespace
    refArgs <- option [] (identSeq <* char ';')
    whitespace
    valArgs <- identSeq
    return (refArgs, valArgs)
-- ident_seq     ::= /* empty */ | ident ("," ident)*
-- local_decl    ::= "local" ident ("," ident)* ";"
localDecl :: Parser Command
localDecl = do
    whitespace
    reserved "local"
    whitespace
    vars <- identSeq
    whitespace
    char ';'
    return $ LocalVariableDeclaration vars
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
statement = choice [assign, assignField, assignFieldExp, allocVar, dispose, statementBlock, ifThenElseStmt, whileStmt, withStmt, callStmt, callOrStmt]

assign :: Parser Command
assign = do
    whitespace
    var <- Id <$> ident
    whitespace
    char '='
    whitespace
    exp <- stmtExp
    whitespace
    char ';'
    return . Assignment $ VariableAssignment var exp

assignField :: Parser Command
assignField = do
    whitespace
    var <- Id <$> ident
    whitespace
    char '='
    whitespace
    exp <- stmtExp
    whitespace
    string "->"
    whitespace
    field <- Id <$> field
    whitespace
    char ';'
    return . Assignment $ HeapLookup var exp field

assignFieldExp :: Parser Command
assignFieldExp = do
    whitespace
    exp1 <- stmtExp
    whitespace
    string "->"
    whitespace
    field <- Id <$> field
    whitespace
    char '='
    whitespace
    exp2 <- stmtExp
    whitespace
    char ';'
    return . Assignment $ HeapMutation exp1 field exp2

allocVar :: Parser Command
allocVar = do
    whitespace
    var <- Id <$> ident
    whitespace
    char '='
    whitespace
    reserved "new"
    whitespace
    parens (return ())
    whitespace
    char ';'
    return . Assignment $ Allocation var

dispose :: Parser Command
dispose = do
    whitespace
    reserved "dispose"
    whitespace
    exp <- stmtExp
    whitespace
    char ';'
    return . Assignment $ Deallocation exp

statementBlock :: Parser Command
statementBlock = do
    whitespace
    statements <- braces (many statement)
    return . foldr Sequence Skip $ statements

ifThenElseStmt :: Parser Command
ifThenElseStmt = do
    whitespace
    reserved "if"
    whitespace
    exp <- parens stmtExp
    whitespace
    thenStmt <- statement
    whitespace
    elseStmt <- option Skip (reserved "else" >> statement)
    return $ IfThenElse exp thenStmt elseStmt

whileStmt :: Parser Command
whileStmt = do
    whitespace
    reserved "while"
    whitespace
    exp <- parens stmtExp
    whitespace
    inv <- option (SingleBooleanPredicate BooleanTrue) (brackets formula)
    whitespace
    WhileDo exp inv <$> statement

withStmt :: Parser Command
withStmt = do
    whitespace
    reserved "with"
    whitespace
    var <- Id <$> ident
    whitespace
    reserved "when"
    whitespace
    exp <- parens stmtExp
    whitespace
    WithResourceWhen var exp <$> statement

callStmt :: Parser Command
callStmt = do
    whitespace
    funcName <- Id <$> ident
    whitespace
    (refArgs, valArgs) <- parens actuals
    whitespace
    char ';'
    return $ FunctionCall (FunctionHeader funcName refArgs valArgs)


-- actuals      ::= stmt_exp_seq (";" stmt_exp_seq)?
actuals :: Parser ([Expression], [Expression])
actuals = do
    whitespace
    refArgs <- stmtExpSeq
    whitespace
    valArgs <- option [] (char ';' >> stmtExpSeq)
    return (refArgs, valArgs)
-- stmt_exp     ::= "(" stmt_exp ")"
--             | ident | number | "true" | "false"
--             | prefix_op stmt_exp | stmt_exp infix_op stmt_exp
stmtExp :: Parser Expression
stmtExp = choice [parens stmtExp, Variable . Id <$> ident, numberStmtExp, booleanStmtExp, prefixOpStmtExp, infixOpStmtExp]
  where
    numberStmtExp = NumConst <$> number
    booleanStmtExp = choice [reserved "true" >> return (BoolConst True), reserved "false" >> return (BoolConst BooleanFalse)]
    prefixOpStmtExp = do
        whitespace
        op <- prefixOp
        whitespace
        PrefixOp op <$> stmtExp
    infixOpStmtExp = do
        whitespace
        exp1 <- stmtExp
        whitespace
        op <- infixOp
        whitespace
        InfixOp op exp1 <$> stmtExp

-- stmt_exp_seq ::= /* empty */ | stmt_exp ("," stmt_exp)*
stmtExpSeq :: Parser [Expression]
stmtExpSeq = sepBy stmtExp (char ',')
-- infix_op     ::= "==" | "!=" | "^" | "&&" | "*" | "/" | "%" | "+" | "-" | "<" | "<=" | ">" | ">="
infixOp :: Parser InfixOp
infixOp = choice [
    op "==" Equals,
    op "!=" NotEquals,
    op "^" Xor,
    op "&&" And,
    op "*" Multiply,
    op "/" Divide,
    op "%" Modulo,
    op "+" Plus,
    op "-" Minus,
    op "<" LessThan,
    op "<=" LessThanEquals,
    op ">" GreaterThan,
    op ">=" GreaterThanEquals
  ]
  where
    op opStr opVal = reservedOp opStr $> opVal


-- prefix_op    ::= "+" | "-"
prefixOp :: Parser PrefixOp
prefixOp = choice [
    op "+" Pos,
    op "-" Negate
  ]
  where
    op opStr opVal = reservedOp opStr $> opVal

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
formula :: Parser Assertion
formula = choice [parens formula, reserved "false" $> SingleBooleanPredicate BooleanFalse, equalityFormula, inequalityFormula, empFormula, sepConjunctionFormula, heapFormula, listFormula, lsegFormula, dlsegFormula, treeFormula, ifThenElseFormula]
  where
    equalityFormula = do
        whitespace
        exp1 <- formExp
        whitespace
        reservedOp "=="
        whitespace
        exp2 <- formExp
        return . SingleBooleanPredicate . Conjunction $ [BoolEquals exp1 exp2]
    inequalityFormula = do
        whitespace
        exp1 <- formExp
        whitespace
        reservedOp "!="
        whitespace
        exp2 <- formExp
        return . SingleBooleanPredicate . Conjunction $ [BoolNotEquals exp1 exp2]
    empFormula = reserved "emp" $> SingleHeapPredicate Emp
    sepConjunctionFormula = do
        whitespace
        exp1 <- formula
        whitespace
        reservedOp "*"
        whitespace
        exp2 <- formula
        return . SingleHeapPredicate . SeparatingConjunction $ [SingleHeapPredicate exp1, exp2] 
    heapFormula = do
        whitespace
        exp <- formExp
        whitespace
        string "|->" 
        whitespace
        heap <- many fieldColonExp
        return . SingleHeapPredicate . SinglePredicate $ PointsToHeap exp (Heap heap)
    fieldColonExp = do
        whitespace
        field <- Id <$> ident
        whitespace
        char ':'
        whitespace
        HeapRecord field <$> formExp
    listFormula = do
        whitespace
        reserved "list"
        whitespace
        (field, exp) <- parens (option (Id "next", Null) (formExp `sepBy` char ';'))
        return $ ListPredicate field exp
    lsegFormula = do
        whitespace
        reserved "lseg"
        whitespace
        (field, exp1, exp2) <- parens (option (Id "next", Null, Null) (formExp `sepBy` char ';'))
        return $ LsegPredicate field exp1 exp2
    dlsegFormula = do
        whitespace
        reserved "dlseg"
        whitespace
        (field1, field2, exp1, exp2, exp3) <- parens (option (Id "next", Id "prev", Null, Null, Null) (formExp `sepBy` char ';'))
        return $ DlsegPredicate field1 field2 exp1 exp2 exp3
    treeFormula = do
        whitespace
        reserved "tree"
        whitespace
        (field1, field2, exp) <- parens (option (Id "left", Id "right", Null) (formExp `sepBy` char ';'))
        return $ TreePredicate field1 field2 exp
    ifThenElseFormula = do
        whitespace
        reserved "if"
-- form_exp ::= "(" form_exp ")" | ident | number | form_exp "^" form_exp
formExp :: Parser Expression
formExp = choice [parens formExp, Variable . Id <$> ident, numberFormExp, prefixOpFormExp, infixOpFormExp]
  where
    numberFormExp = NumConst <$> number
    prefixOpFormExp = do
        whitespace
        op <- prefixOp
        whitespace
        PrefixOp op <$> formExp
    infixOpFormExp = do
        whitespace
        exp1 <- formExp
        whitespace
        op <- infixOp
        whitespace
        InfixOp op exp1 <$> formExp
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
