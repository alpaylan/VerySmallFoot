module Program where


import Data.Set (Set)
import qualified Data.Set as Set

    
-- Wrapper Types
newtype Identifier = Id String deriving (Eq, Ord)
instance Show Identifier where
    show (Id ident) = ident
    
    
data HeapRecord = HeapRecord FieldName Expression deriving (Eq, Ord)
instance Show HeapRecord where
    show (HeapRecord f e) = show f ++ ": " ++ show e

newtype Heap = Heap [HeapRecord] deriving (Eq, Ord)
instance Show Heap where
    show (Heap fields) = "(" ++ showListCommaSeparated fields ++ ")"



-- Type Aliases
type VariableName = Identifier
type ConstantName = Identifier
type FunctionName = Identifier
type FieldName = Identifier
type PassByReferenceArgument = Identifier
type PassByValueArgument = Identifier
type ResourceName = Identifier

type Precondition = Assertion
type Postcondition = Assertion
type Invariant = Assertion


-- Custom Print Functions
showListCommaSeparated :: (Show a) => [a] -> String
showListCommaSeparated = showListDelimSeparated ", "

showListNewlineSeparated :: (Show a) => [a] -> String
showListNewlineSeparated = showListDelimSeparated "\n\n\n"

showListDelimSeparated :: (Show a) => String -> [a] -> String
showListDelimSeparated _ [] = ""
showListDelimSeparated _ [x] = show x
showListDelimSeparated delim (x:x':xs) = show x ++ delim ++ showListDelimSeparated delim (x':xs)


-- Data Types
data FunctionHeader = FunctionHeader FunctionName [PassByReferenceArgument] [PassByValueArgument]
    deriving (Eq, Ord)
instance Show FunctionHeader where
    show (FunctionHeader name [] []) =
        show name ++ "()"
    -- show (FunctionHeader name passByReferenceArguments []) = 
    --     show name ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ ")"
    -- show (FunctionHeader name [] passByValueArguments) =
    --     show name ++ "(" ++ showListCommaSeparated passByValueArguments ++ ")"
    show (FunctionHeader name passByReferenceArguments passByValueArguments) = 
        show name ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ "; " ++ showListCommaSeparated passByValueArguments ++ ")"

data Command = 
    Assignment Assignment
    | Sequence Command Command
    | IfThenElse BoolExpression Command Command
    | WhileDo BoolExpression Invariant Command
    | FunctionCall FunctionHeader
    | ConcurrentFunctionCall FunctionHeader FunctionHeader
    | WithResourceWhen ResourceName BoolExpression Command
    | LocalVariableDeclaration [VariableName]
    deriving (Eq, Ord)
instance Show Command where
    show (Assignment s) = show s
    show (Sequence c1 c2) = show c1 ++ ";\n" ++ show c2
    show (IfThenElse b c1 c2) = "if (" ++ show b ++ ") { \n" ++ show c1 ++ "\n" ++ "} else {\n" ++ show c2 ++ "\n}\n"
    show (WhileDo b i c) = "while (" ++ show b ++ ") {\n" ++ show i ++ "\n" ++ show c ++ "\n}"
    show (FunctionCall f) = show f
    show (ConcurrentFunctionCall f1 f2) = show f1 ++ "\n||\n" ++ show f2
    show (WithResourceWhen r b c) = "with " ++ show r ++ " when " ++ show b ++ " {\n" ++ show c ++ "\n}"
    show (LocalVariableDeclaration v) = "local " ++ showListCommaSeparated v



data Assignment =
    VariableAssignment VariableName Expression
    | HeapLookup VariableName Expression FieldName
    | HeapMutation Expression FieldName Expression
    | Allocation VariableName
    | Deallocation Expression
    deriving (Eq, Ord)
instance Show Assignment where
    show (VariableAssignment var expr) = show var ++ " = " ++ show expr
    show (HeapLookup var expr field) = show var ++ " = " ++ show expr ++ " -> " ++ show field
    show (HeapMutation expr field expr') = show expr ++ " = " ++ show expr' ++ " -> " ++ show field
    show (Allocation var) = show var ++ " = new()"
    show (Deallocation expr) = "dispose(" ++ show expr ++ ")"

data BoolExpression =
    BoolEquals Expression Expression
    | BoolNotEquals Expression Expression
    deriving (Eq, Ord)
instance Show BoolExpression where
    show (BoolEquals expr expr') = show expr ++ " == " ++ show expr'
    show (BoolNotEquals expr expr') = show expr ++ " != " ++ show expr'

bNot :: BoolExpression -> BoolExpression
bNot (BoolEquals expr expr') = BoolNotEquals expr expr'
bNot (BoolNotEquals expr expr') = BoolEquals expr expr'

data Expression =
    Variable VariableName
    | Nil
    | Constant ConstantName
    | Xor Expression Expression
    deriving (Eq, Ord)
instance Show Expression where
    show (Variable var) = show var
    show Nil = "NULL"
    show (Constant c) = show c
    show (Xor expr expr') = show expr ++ " xor " ++ show expr'


type ResourceInvariant = Command
data Resource = Resource    { 
                            resourceName :: ResourceName 
                            , resources  ::[VariableName] 
                            , resourceInvariant :: ResourceInvariant
                            }
    deriving (Eq, Ord)
instance Show Resource where
    show (Resource name resources body) = "resource " ++ show name ++ " (" ++ showListCommaSeparated resources ++ ") [\n" ++ show body ++ "\n]"



-- Assertions 

data BooleanPredicate = 
    BooleanTrue
    | BooleanFalse
    | Conjunction [BoolExpression]
    deriving (Eq, Ord)
instance Show BooleanPredicate where
    show BooleanTrue = "true"
    show BooleanFalse = "false"
    show (Conjunction exprs) = showListDelimSeparated " /\\ " exprs

extendBooleanPredicate :: BooleanPredicate -> BoolExpression -> BooleanPredicate
extendBooleanPredicate BooleanTrue expr = Conjunction [expr]
extendBooleanPredicate BooleanFalse expr = BooleanFalse
extendBooleanPredicate (Conjunction exprs) expr = Conjunction (expr:exprs)

data HeapletPredicate =
    PointsToHeap Expression Heap
    | HeapTree Expression
    | HeapLinkedList Expression Expression
    | HeapXORList Expression Expression Expression Expression
    deriving (Eq, Ord)
instance Show HeapletPredicate where
    show (PointsToHeap expr heap) = show expr ++ " -> " ++ show heap
    show (HeapTree expr) = "tree(" ++ show expr ++ ")"
    show (HeapLinkedList expr expr') = "ls(" ++ show expr ++ ", " ++ show expr' ++ ")"
    show (HeapXORList expr expr' expr'' expr''') = "xlseg(" ++ show expr ++ ", " ++ show expr' ++ ", " ++ show expr'' ++ ", " ++ show expr''' ++ ")"

data HeapPredicate =
    Emp
    | SinglePredicate HeapletPredicate
    | SeparatingConjunction [HeapletPredicate]
    deriving (Eq, Ord)
instance Show HeapPredicate where
    show Emp = "emp"
    show (SinglePredicate hp) = show hp
    show (SeparatingConjunction heaplets) = showListDelimSeparated " * " heaplets



data Assertion =
    SingleBooleanPredicate BooleanPredicate
    | SingleHeapPredicate HeapPredicate
    | AssertionConjunction BooleanPredicate HeapPredicate
    | AssertionIfThenElse BoolExpression Assertion Assertion
    deriving (Eq, Ord)


extendAssertionBool :: Assertion -> BoolExpression -> Assertion
extendAssertionBool (SingleBooleanPredicate b) b' = SingleBooleanPredicate (extendBooleanPredicate b b')
extendAssertionBool (SingleHeapPredicate h) b' = AssertionConjunction (Conjunction [b']) h
extendAssertionBool (AssertionConjunction b h) b' = AssertionConjunction (extendBooleanPredicate b b') h
extendAssertionBool (AssertionIfThenElse b a a') b' = AssertionIfThenElse b (extendAssertionBool a b') (extendAssertionBool a' b')



instance Show Assertion where
    show (SingleBooleanPredicate bp) = show bp
    show (SingleHeapPredicate hp) = show hp
    show (AssertionConjunction b h) = show b ++ " /\\ " ++ show h
    show (AssertionIfThenElse b a a') = "if (" ++ show b ++ ") {\n" ++ show a ++ "\n} else {\n" ++ show a' ++ "\n}"


data ProgramDeclaration = 
    FunctionDeclaration Function 
    | ResourceDeclaration Resource
    deriving (Eq, Ord)
instance Show ProgramDeclaration where
    show (FunctionDeclaration f) = show f
    show (ResourceDeclaration r) = show r

data Program = Program [FieldName] [ProgramDeclaration]
instance Show Program where
    show (Program fields declarations) = showListCommaSeparated fields ++ ";\n\n\n" ++ showListNewlineSeparated declarations


data HoareTriple = HoareTriple  {
                                precondition :: Precondition
                                , command :: Command
                                , postcondition :: Postcondition
                                } deriving (Eq, Ord)

data Function = Function FunctionHeader HoareTriple
    deriving (Eq, Ord)
instance Show Function where
    show (Function functionHeader procedureBody) = 
        show functionHeader ++ "[" ++ show (precondition procedureBody) ++ "] {\n" ++ show (command procedureBody) ++ "}[" ++ show (postcondition procedureBody) ++ "]"



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
        (Assignment (VariableAssignment (Id "s") (Variable (Id "t"))))
        (Assignment (HeapLookup (Id "i") (Variable (Id "t")) (Id "l")) `Sequence`
        Assignment (HeapLookup (Id "j") (Variable (Id "t")) (Id "r")) `Sequence`
        FunctionCall (FunctionHeader (Id "tree_copy") [Id "ii"] [Id "i"]) `Sequence`
        FunctionCall (FunctionHeader (Id "tree_copy") [Id "jj"] [Id "j"]) `Sequence`
        Assignment (Allocation (Id "s")) `Sequence`
        Assignment (HeapMutation (Variable (Id "s")) (Id "l") (Variable (Id "ii"))) `Sequence`
        Assignment (HeapMutation (Variable (Id "s")) (Id "r") (Variable (Id "jj"))))


exampleProgram :: Program
exampleProgram = Program 
        [Id "l", Id "r"] 
        [FunctionDeclaration (Function (FunctionHeader (Id "tree_copy") [Id "s"] [Id "t"])
        HoareTriple{
        precondition = (SingleHeapPredicate (SinglePredicate (HeapTree (Variable (Id "t")))))
        , command = exampleProgramBody 
        , postcondition = (SingleHeapPredicate (SeparatingConjunction [HeapTree (Variable (Id "s")), HeapTree (Variable (Id "t"))]))
        })]

