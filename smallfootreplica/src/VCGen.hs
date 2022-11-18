module VCGen where

import Data.Set (Set)
import qualified Data.Set as Set

import Program
import VariableConditions


newtype JsrInstruction = JsrInstruction (Set VariableName) deriving (Eq, Ord)

data SymbolicInstruction = 
    Eps
    | SymbolicAssignment Assignment
    | SymbolicJump Precondition JsrInstruction Postcondition
    | SymbolicIfThenElse BoolExpression SymbolicInstruction SymbolicInstruction
    | SymbolicSequence SymbolicInstruction SymbolicInstruction
    deriving (Eq, Ord)
data SymbolicHoareTriple = SymbolicHoareTriple  {
                                                    si_precondition :: Precondition
                                                    , si :: SymbolicInstruction
                                                    , si_postcondition :: Postcondition
                                                    } deriving (Eq, Ord)



vcg :: Context -> FunctionName -> HoareTriple -> Set SymbolicHoareTriple

vcg ctx g pcq =
    let HoareTriple p c q  =  pcq in
    let (si, l) = chop ctx g c in
        SymbolicHoareTriple p si q `Set.insert` l 


chop :: Context -> FunctionName -> Command -> (SymbolicInstruction, Set SymbolicHoareTriple)

chop ctx g (Assignment s) = (SymbolicAssignment s, Set.empty)
chop ctx g (Sequence c1 c2) = 
    let (si1, l1) = chop ctx g c1 in
    let (si2, l2) = chop ctx g c2 in
        (SymbolicSequence si1 si2, l1 `Set.union` l2)
chop ctx g (IfThenElse b c1 c2) =
    let (si1, l1) = chop ctx g c1 in
    let (si2, l2) = chop ctx g c2 in
        (SymbolicIfThenElse b si1 si2, l1 `Set.union` l2)
chop ctx g (WhileDo b i c) =
    (SymbolicJump i (JsrInstruction (modC ctx c)) (extendAssertionBool i (bNot b))
    , vcg ctx g (HoareTriple (extendAssertionBool i b) c i))
chop ctx g (FunctionCall (FunctionHeader f p v)) =
    (Eps, Set.empty)

    -- let function = getFunction ctx f in

    --     (SymbolicSequence
    --     (SymbolicJump (SingleHeapPredicate Emp) (JsrInstruction Set.empty) (AssertionConjunction (SingleBooleanPredicate (Conjunction [])) (SingleHeapPredicate Emp))
    --     ()
    --     , Set.empty)


