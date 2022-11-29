module Program where


import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- Wrapper Types
-- newtype String = Id String deriving (Eq, Ord)
-- instance Show String where
--     show (Id ident) = ident

-- instance Subst String where
--     subst m i = fromMaybe i $ lookup i m


data HeapRecord = HeapRecord String Expression deriving (Eq, Ord)
instance Show HeapRecord where
    show (HeapRecord f e) = show f ++ ": " ++ show e

instance Subst HeapRecord where
    subst m (HeapRecord f e) = HeapRecord f (subst m e)

newtype Heap = Heap [HeapRecord] deriving (Eq, Ord)
instance Show Heap where
    show (Heap fields) = "(" ++ showListCommaSeparated fields ++ ")"

instance Subst Heap where
    subst m (Heap fields) = Heap (map (subst m) fields)



-- Type Aliases
type VariableName = String
type ConstantName = String
type FunctionName = String
type FieldName = String
type ResourceName = String

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


-- Substitution
class Subst a where
  subst :: [(String, String)] -> a -> a


-- Data Types
-- data FunctionHeader = FunctionHeader FunctionName [VariableName] [VariableName]
--     deriving (Eq, Ord)
-- instance Show FunctionHeader where
--     show (FunctionHeader name [] [])
--         show name ++ "()"
--     -- show (FunctionHeader name passByReferenceArguments []) =
--     --     show name ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ ")"
--     -- show (FunctionHeader name [] passByValueArguments) =
--     --     show name ++ "(" ++ showListCommaSeparated passByValueArguments ++ ")"
--     show (FunctionHeader name passByReferenceArguments passByValueArguments) =
--         show name ++ "(" ++ showListCommaSeparated passByReferenceArguments ++ "; " ++ showListCommaSeparated passByValueArguments ++ ")"

-- data FunctionCall = FunctionCall FunctionName [VariableName] [Expression]
--     deriving (Eq, Ord)
-- instance Show FunctionCall where
--     show (FunctionCall name [] []) =
--       show name ++ "()"
--     show (FunctionCall name refArgs valExps) =
--       show name ++ "(" ++ showListCommaSeparated refArgs ++ "; " ++ showListCommaSeparated valExps ++ ")"

data Command =
    Assignment Assignment
    | Sequence Command Command
    | IfThenElse BoolExpression Command Command
    | WhileDo BoolExpression Invariant Command
    | Call FunctionName [VariableName] [Expression]
    | ConcurrentCall FunctionName [VariableName] [Expression] FunctionName [VariableName] [Expression]
    | WithResourceWhen ResourceName BoolExpression Command
    deriving (Eq, Ord)
instance Show Command where
    show (Assignment s) = show s
    show (Sequence c1 c2) = show c1 ++ ";\n" ++ show c2
    show (IfThenElse b c1 c2) = "if (" ++ show b ++ ") { \n" ++ show c1 ++ "\n" ++ "} else {\n" ++ show c2 ++ "\n}\n"
    show (WhileDo b i c) = "while (" ++ show b ++ ") {\n" ++ show i ++ "\n" ++ show c ++ "\n}"
    show (Call f ref val) = show f
    show (ConcurrentCall f1 ref1 val1 f2 ref2 val2) = show f1 ++ "\n||\n" ++ show f2
    show (WithResourceWhen r b c) = "with " ++ show r ++ " when " ++ show b ++ " {\n" ++ show c ++ "\n}"



data Assignment =
    VariableAssignment String Expression
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

instance Subst BoolExpression where
    subst m (BoolEquals e1 e2) = BoolEquals (subst m e1) (subst m e2)
    subst m (BoolNotEquals e1 e2) = BoolNotEquals (subst m e1) (subst m e2)

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

instance Subst Expression where
    subst m (Variable var) = Variable $ fromMaybe var $ lookup var m
    subst _ Nil = Nil
    subst _ (Constant c) = Constant c
    subst m (Xor e1 e2) = Xor (subst m e1) (subst m e2)


type ResourceInvariant = HeapletPredicate
data Resource = Resource ResourceName [VariableName] ResourceInvariant
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

instance Subst BooleanPredicate where
    subst _ BooleanTrue = BooleanTrue
    subst _ BooleanFalse = BooleanFalse
    subst m (Conjunction bes) = Conjunction (map (subst m) bes)

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

instance Subst HeapletPredicate where
    subst m (PointsToHeap e heap) = PointsToHeap (subst m e) (subst m heap)
    subst m (HeapTree e) = HeapTree (subst m e)
    subst m (HeapLinkedList e e') = HeapLinkedList (subst m e) (subst m e')
    subst m (HeapXORList e1 e2 e3 e4) = HeapXORList (subst m e1) (subst m e2) (subst m e3) (subst m e4)


data HeapPredicate =
    Emp
    | SinglePredicate HeapletPredicate
    | SeparatingConjunction [HeapletPredicate]
    deriving (Eq, Ord)
instance Show HeapPredicate where
    show Emp = "emp"
    show (SinglePredicate hp) = show hp
    show (SeparatingConjunction heaplets) = showListDelimSeparated " * " heaplets

instance Subst HeapPredicate where
    subst _ Emp = Emp
    subst m (SinglePredicate hp) = SinglePredicate (subst m hp)
    subst m (SeparatingConjunction heaplets) = SeparatingConjunction (map (subst m) heaplets)



data Assertion =
    SingleBooleanPredicate BooleanPredicate
    | SingleHeapPredicate HeapPredicate
    | AssertionConjunction BooleanPredicate HeapPredicate
    | AssertionIfThenElse BoolExpression Assertion Assertion
    deriving (Eq, Ord)

instance Subst Assertion where
    subst m (SingleBooleanPredicate bp) = SingleBooleanPredicate (subst m bp)
    subst m (SingleHeapPredicate hp) = SingleHeapPredicate (subst m hp)
    subst m (AssertionConjunction bp hp) = AssertionConjunction (subst m bp) (subst m hp)
    subst m (AssertionIfThenElse be at af) = AssertionIfThenElse (subst m be) (subst m at) (subst m af)


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


-- name, pass-by-ref args, pass-by-value args, local vars, body
data Function = Function FunctionName [VariableName] [VariableName] [VariableName] HoareTriple
    deriving (Eq, Ord)
instance Show Function where
    show (Function f ref val local (p, c, q)) =
      f ++ showFuncArgs ref val ++ " [" ++ show p ++ "] {\n" ++
      "  local " ++ showListCommaSeparated local ++ "\n" ++
      show c ++ "} [" ++ show q ++ "]"

showFuncArgs :: [String] -> [String] -> String
showFuncArgs [] [] = "()"
showFuncArgs ref [] = "(" ++ showListCommaSeparated ref ++ ")"
showFuncArgs [] val = "(" ++ showListCommaSeparated val ++ ")"
showFuncArgs ref val = "(" ++ showListCommaSeparated ref ++ "; " ++ showListCommaSeparated val ++ ")"

data Program = Program [FieldName] [Resource] [Function]
instance Show Program where
    show (Program fields ress funcs) =
      showListCommaSeparated fields ++ ";\n\n\n" ++
      showListNewlineSeparated ress ++
      showListNewlineSeparated funcs


type HoareTriple = (Precondition, Command, Postcondition)



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
  IfThenElse (BoolEquals (Variable "t") Nil)
    (Assignment (VariableAssignment "s" (Variable "t")))
    (Assignment (HeapLookup "i" (Variable "t") "l") `Sequence`
     Assignment (HeapLookup "j" (Variable "t") "r") `Sequence`
     Call "tree_copy" ["ii"] [Variable "i"] `Sequence`
     Call "tree_copy" ["jj"] [Variable "j"] `Sequence`
     Assignment (Allocation "s") `Sequence`
     Assignment (HeapMutation (Variable "s") "l" (Variable "ii")) `Sequence`
     Assignment (HeapMutation (Variable "s") "r" (Variable "jj")))


-- name, pass-by-ref args, pass-by-value args, local vars, body
exampleProgram :: Program
exampleProgram =
  Program
    ["l", "r"]
    []
    [Function
       "tree_copy" ["s"] ["t"]
       ["i", "j", "ii", "jj"]
       (SingleHeapPredicate (SinglePredicate (HeapTree (Variable "t"))),
        exampleProgramBody,
        SingleHeapPredicate (SeparatingConjunction [HeapTree (Variable "s"), HeapTree (Variable "t")]))]

