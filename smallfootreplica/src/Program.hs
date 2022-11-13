module Program
    (
    exampleProgramBody,
    exampleProgram
    ) where

data Ident = Id String
instance Show Ident where
    show (Id ident) = id ident

    
showListCommaSeparated :: (Show a) => [a] -> String
showListCommaSeparated = showListDelimSeparated ", "

showListNewlineSeparated :: (Show a) => [a] -> String
showListNewlineSeparated = showListDelimSeparated "\n\n\n"

showListDelimSeparated :: (Show a) => String -> [a] -> String
showListDelimSeparated _ [] = ""
showListDelimSeparated _ [x] = show x
showListDelimSeparated delim (x:x':xs) = show x ++ delim ++ showListDelimSeparated delim (x':xs)

data Command = 
    Assignment Assignment
    | Sequence Command Command
    | IfThenElse BoolExpression Command Command
    | WhileDo BoolExpression Invariant Command
    | FunctionCall Ident [PassByReferenceArgument] [PassByValueArgument]
    -- | ConcurrentFunctionCall FunctionCall FunctionCall
    | WithResourceWhen Resource BoolExpression Command
    | LocalVariableDeclaration [Variable]
instance Show Command where
    show (Assignment s) = show s
    show (Sequence c1 c2) = show c1 ++ ";\n" ++ show c2
    show (IfThenElse b c1 c2) = "if (" ++ show b ++ ") { \n" ++ show c1 ++ "\n" ++ "} else {\n" ++ show c2 ++ "\n}\n"
    show (WhileDo b i c) = "while (" ++ show b ++ ") {\n" ++ show i ++ "\n" ++ show c ++ "\n}"
    show (FunctionCall ident passByReferenceArguments passByValueArguments) = show ident ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ "; " ++ showListCommaSeparated passByValueArguments ++ ")"
    -- show (ConcurrentFunctionCall f1 f2) = show f1 ++ "\n||\n" ++ show f2
    show (WithResourceWhen r b c) = "with " ++ show r ++ " when " ++ show b ++ " {\n" ++ show c ++ "\n}"
    show (LocalVariableDeclaration v) = "local " ++ showListCommaSeparated v



data Assignment =
    AssignExprToX Variable Expression
    | AssignExprFieldToX Variable Expression Field
    | AssignExpressionToExprField Expression Field Expression
    | Allocate Variable
    | Dispose Expression
instance Show Assignment where
    show (AssignExprToX var expr) = show var ++ " = " ++ show expr
    show (AssignExprFieldToX var expr field) = show var ++ " = " ++ show expr ++ " -> " ++ show field
    show (AssignExpressionToExprField expr field expr') = show expr ++ " = " ++ show expr' ++ " -> " ++ show field
    show (Allocate var) = show var ++ " = new()"
    show (Dispose expr) = "dispose(" ++ show expr ++ ")"

data BoolExpression =
    BoolEquals Expression Expression
    | BoolNotEquals Expression Expression
instance Show BoolExpression where
    show (BoolEquals expr expr') = show expr ++ " == " ++ show expr'
    show (BoolNotEquals expr expr') = show expr ++ " != " ++ show expr'

data Expression =
    Variable Variable
    | Nil
    | Constant Constant
    | Xor Expression Expression
instance Show Expression where
    show (Variable var) = show var
    show Nil = "NULL"
    show (Constant c) = show c
    show (Xor expr expr') = show expr ++ " xor " ++ show expr'

type Variable = Ident
type Constant = Ident

-- Assertions 

data BooleanPredicate = 
    BooleanTrue
    | BooleanFalse
    | Conjunction [BoolExpression]
instance Show BooleanPredicate where
    show BooleanTrue = "true"
    show BooleanFalse = "false"
    show (Conjunction exprs) = case exprs of
        [] -> ""
        [expr] -> show expr
        expr:expr':exprs' -> show expr ++ " /\\ " ++ show (Conjunction (expr':exprs'))

data HeapletPredicate =
    PointsToHeap Expression Heap
    | HeapTree Expression
    | HeapLinkedList Expression Expression
    | HeapXORList Expression Expression Expression Expression
instance Show HeapletPredicate where
    show (PointsToHeap expr heap) = show expr ++ " -> " ++ show heap
    show (HeapTree expr) = "tree(" ++ show expr ++ ")"
    show (HeapLinkedList expr expr') = "ls(" ++ show expr ++ ", " ++ show expr' ++ ")"
    show (HeapXORList expr expr' expr'' expr''') = "xlseg(" ++ show expr ++ ", " ++ show expr' ++ ", " ++ show expr'' ++ ", " ++ show expr''' ++ ")"

data Heap = Heap [(Field, Expression)]
instance Show Heap where
    show (Heap fields) = 
        "(" ++
            (case fields of
                [] -> ""
                [(field, expr)] -> show field ++ ": " ++ show expr
                (field, expr):(field', expr'):fields' -> show field ++ ": " ++ show expr ++ ", " ++ show (Heap ((field', expr'):fields')))
        ++ ")"

type Field = Ident

data HeapPredicate =
    Emp
    | SinglePredicate HeapletPredicate
    | SeparatingConjunction [HeapletPredicate]
instance Show HeapPredicate where
    show Emp = "emp"
    show (SinglePredicate hp) = show hp
    show (SeparatingConjunction heaplets) = case heaplets of
        [] -> ""
        [heaplet] -> show heaplet
        heaplet:heaplet':heaplets' -> show heaplet ++ " * " ++ show (SeparatingConjunction (heaplet':heaplets'))


data Assertion =
    SingleBooleanPredicate BooleanPredicate
    | SingleHeapPredicate HeapPredicate
    | AssertionConjunction BooleanPredicate HeapPredicate
    | AssertionIfThenElse BoolExpression Assertion Assertion
instance Show Assertion where
    show (SingleBooleanPredicate bp) = show bp
    show (SingleHeapPredicate hp) = show hp
    show (AssertionConjunction b h) = show b ++ " /\\ " ++ show h
    show (AssertionIfThenElse b a a') = "if (" ++ show b ++ ") {\n" ++ show a ++ "\n} else {\n" ++ show a' ++ "\n}"

type Invariant = Assertion

type PassByReferenceArgument = Variable
type PassByValueArgument = Variable
type Resource = Variable

data Program = Program [Field] [FunctionDefinition]
instance Show Program where
    show (Program fields functions) = showListCommaSeparated fields ++ ";\n\n\n" ++ showListNewlineSeparated functions

data FunctionDefinition = FunctionDefinition Ident [PassByReferenceArgument] [PassByValueArgument] Precondition Command Postcondition
instance Show FunctionDefinition where
    show (FunctionDefinition ident passByReferenceArguments passByValueArguments precondition command postcondition) = 
        show ident ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ "; " ++ showListCommaSeparated passByValueArguments ++ ") [" ++ show precondition ++ "] {\n" ++ show command ++ "}[" ++ show postcondition ++ "]"

type Precondition = Assertion
type Postcondition = Assertion

exampleProgramBody :: Command
--  tree_copy(s;t) [tree(t)] {
--   local i, j, ii, jj;
--   if(t == NULL) s = t;
--   else { 
--     i = t->l;
--     j = t->r;
--     tree_copy(ii;i);
--     tree_copy(jj;j);
--     s = new();
--     s->l = ii;
--     s->r = jj;
--   }
-- } [tree(s) * tree(t)]
exampleProgramBody = 
    LocalVariableDeclaration [Id "i", Id "j", Id "ii", Id "jj"] `Sequence`
    IfThenElse (BoolEquals (Variable (Id "t")) Nil)
        (Assignment (AssignExprToX (Id "s") (Variable (Id "t"))))
        (Assignment (AssignExprFieldToX (Id "i") (Variable (Id "t")) (Id "l")) `Sequence`
        Assignment (AssignExprFieldToX (Id "j") (Variable (Id "t")) (Id "r")) `Sequence`
        FunctionCall (Id "tree_copy") [Id "ii"] [Id "i"] `Sequence`
        FunctionCall (Id "tree_copy") [Id "jj"] [Id "j"] `Sequence`
        Assignment (Allocate (Id "s")) `Sequence`
        Assignment (AssignExpressionToExprField (Variable (Id "s")) (Id "l") (Variable (Id "ii"))) `Sequence`
        Assignment (AssignExpressionToExprField (Variable (Id "s")) (Id "r") (Variable (Id "jj"))))


exampleProgram :: Program
exampleProgram = Program 
        [Id "l", Id "r"] 
        [FunctionDefinition (Id "tree_copy") [Id "s"] [Id "t"] 
        (SingleHeapPredicate (SinglePredicate (HeapTree (Variable (Id "t")))))
        exampleProgramBody 
        (SingleHeapPredicate (SeparatingConjunction [HeapTree (Variable (Id "s")), HeapTree (Variable (Id "t"))]))]


