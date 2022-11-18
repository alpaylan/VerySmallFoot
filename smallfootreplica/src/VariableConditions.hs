
module VariableConditions
    (
        -- var,
        mod,
        -- req
    )
    where
        
import Program

import Data.Set (Set)
import qualified Data.Set as Set

(\/) :: Set VariableName -> Set VariableName -> Set VariableName
(\/) = Set.union


fve :: Expression -> Set VariableName
fve (Variable v) = Set.singleton v
fve (Constant _) = Set.empty
fve (Xor e1 e2) = fv e1 \/ fv e2
fve Nil = Set.empty

fvb :: BoolExpression -> Set VariableName
fvb (BoolEquals e1 e2) = fv e1 \/ fv e2
fvb (BoolNotEquals e1 e2) = fv e1 \/ fv e2

fvbp :: BooleanPredicate -> Set VariableName
fvbp (Conjunction exprs) = Set.unions $ map fv exprs
fvbp _ = Set.empty

fva :: Assertion -> Set VariableName
fva (SingleBooleanPredicate b) = fv b
fva (SingleHeapPredicate h) = fv h
fva (AssertionConjunction booleanPredicate heapPredicate) = 
    fv booleanPredicate \/ fv heapPredicate
fva (AssertionIfThenElse b a1 a2) = fv b \/ fv a1 \/ fv a2

fvhp :: HeapPredicate -> Set VariableName
fvhp Emp = Set.empty
fvhp (SinglePredicate hp) = fv hp
fvhp (SeparatingConjunction heaplets) = Set.unions $ map fv heaplets

fvhlp :: HeapletPredicate -> Set VariableName
fvhlp (PointsToHeap e h) = fv e \/ fv h
fvhlp (HeapTree e) = fv e
fvhlp (HeapLinkedList e1 e2) = fv e1 \/ fv e2
fvhlp (HeapXORList e1 e2 e3 e4) = fv e1 \/ fv e2 \/ fv e3 \/ fv e4

fvp :: Heap -> Set VariableName
fvp (Heap h) = Set.unions $ map fv h

fvhr :: HeapRecord -> Set VariableName
fvhr (HeapRecord f e) = fv e

class FV a where
    fv :: a -> Set VariableName
instance FV Expression where
    fv = fve
instance FV BoolExpression where
    fv = fvb
instance FV BooleanPredicate where
    fv = fvbp
instance FV Assertion where
    fv = fva
instance FV HeapPredicate where
    fv = fvhp
instance FV HeapletPredicate where
    fv = fvhlp
instance FV Heap where
    fv = fvp
instance FV HeapRecord where
    fv = fvhr
instance FV a => FV [a] where
    fv = Set.unions . map fv
instance FV Identifier where
    fv = Set.singleton
newtype Context = Context Program


getFunction :: [ProgramDeclaration] -> FunctionName -> Function
getFunction [] fname = error ("Function " ++ show fname ++ " not found")
getFunction ((FunctionDeclaration(Function (FunctionHeader f p v) body)):fs) fname =
    if f == fname then Function (FunctionHeader f p v) body
    else getFunction fs fname
getFunction (_:fs) fname = getFunction fs fname




varC :: Context -> Command -> Set VariableName
varC _ (Assignment (VariableAssignment var expression)) = 
    Set.singleton var \/ fv expression
varC _ (Assignment (HeapLookup var expression _)) =
    Set.singleton var \/ fv expression
varC _ (Assignment (HeapMutation expression _ expression')) =
    fv expression \/ fv expression'
varC _ (Assignment (Allocation var)) = Set.singleton var
varC _ (Assignment (Deallocation expression)) = fv expression
varC ctx (Sequence c1 c2) = varC ctx c1 \/ varC ctx c2
varC ctx (IfThenElse b c1 c2) = fv b \/ varC ctx c1 \/ varC ctx c2
varC ctx (WhileDo b i c) = (fv b `Set.intersection` fv i) \/ varC ctx c
varC (Context (Program fields functions)) (FunctionCall (FunctionHeader id p v)) = 
    let f = getFunction functions id in
    varF (Context (Program fields functions)) f  \/ Set.fromList p \/ fv v
varC ctx(ConcurrentFunctionCall function1 function2) = error "todo"
varC ctx (WithResourceWhen resource b c) = error "todo"

varF :: Context -> Function -> Set VariableName
varF ctx (Function (FunctionHeader _ pointers values) (HoareTriple {precondition = p, command = c, postcondition = q})) = 
    (varC ctx c \/ fv p \/ fv q) `Set.difference` (Set.fromList pointers \/ Set.fromList values)


    
    

modC :: Context -> Command -> Set VariableName
modC _ (Assignment (VariableAssignment var _)) = Set.singleton var
modC _ (Assignment (HeapLookup var _ _)) = Set.singleton var
modC _ (Assignment (HeapMutation expression _ var)) = fv var \/ fv expression
modC _ (Assignment (Allocation var)) = Set.singleton var
modC _ (Assignment (Deallocation expression)) = fv expression
modC ctx (Sequence c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (IfThenElse b c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (WhileDo b i c) = modC ctx c
modC (Context (Program fields functions)) (FunctionCall (FunctionHeader id p _)) =
    let f = getFunction functions id in
    modF (Context (Program fields functions)) f \/ Set.fromList p

    

modF :: Context -> Function -> Set VariableName
modF ctx (Function (FunctionHeader _ pointers values) (HoareTriple {command = c})) = 
    modC ctx c `Set.difference` (Set.fromList pointers \/ Set.fromList values)


reqC :: Context -> Command -> Set VariableName
reqC _ (Assignment (VariableAssignment _ _)) = Set.empty


type ModifiedVariables = Set VariableName
type AccessedVariables = Set VariableName
type Resources = Set VariableName
er :: Context -> ModifiedVariables -> AccessedVariables -> Resources
er ctx m a = error "todo"