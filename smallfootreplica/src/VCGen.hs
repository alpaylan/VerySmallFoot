module VCGen (chop) where

import Control.Monad.State
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Program
import VariableConditions


-- newtype JsrInstruction = JsrInstruction (Set VariableName) deriving (Eq, Ord)

data SymbolicInstruction
    = Eps
    | SymbolicAssignment Assignment
    | SymbolicJump Precondition (Set VariableName) Postcondition
    | SymbolicIfThenElse BoolExpression SymbolicInstruction SymbolicInstruction
    | SymbolicSequence SymbolicInstruction SymbolicInstruction
    deriving (Eq, Ord)

data SymbolicHoareTriple = SymbolicHoareTriple
    { sht_pre  :: Precondition
    , sht_si   :: SymbolicInstruction
    , sht_post :: Postcondition
    } deriving (Eq, Ord)

type FreshMonad = State [Identifier]

fvs :: String -> [Identifier]
fvs pre =
  fvs' 0
  where
    fvs' :: Integer -> [Identifier]
    fvs' n = Id (pre ++ show n) : fvs' (n + 1)

fresh :: FreshMonad Identifier
fresh = do
  xs <- get
  put $ tail xs
  return $ head xs

vcg :: Context -> FunctionName -> HoareTriple -> FreshMonad (Set SymbolicHoareTriple)
vcg ctx g (HoareTriple p c q) = do
  (si, l) <- chop' ctx g c
  return $ SymbolicHoareTriple p si q `Set.insert` l


chop' :: Context -> FunctionName -> Command -> FreshMonad (SymbolicInstruction, Set SymbolicHoareTriple)
chop' _ _ (Assignment s) = return (SymbolicAssignment s, Set.empty)
chop' ctx g (Sequence c1 c2) = do
  (si1, l1) <- chop' ctx g c1
  (si2, l2) <- chop' ctx g c2
  return (SymbolicSequence si1 si2, l1 `Set.union` l2)
chop' ctx g (IfThenElse b c1 c2) = do
  (si1, l1) <- chop' ctx g c1
  (si2, l2) <- chop' ctx g c2
  return (SymbolicIfThenElse b si1 si2, l1 `Set.union` l2)
chop' ctx g (WhileDo b i c) = do
  l <- vcg ctx g (HoareTriple (extendAssertionBool i b) c i)
  return (SymbolicJump
          i
          (modC ctx c)
          (extendAssertionBool i (bNot b)),
          l)
chop' ctx _ (Call (FunctionCall f xs es)) = do
  v' <- mapM (const fresh) es
  let m = zip p xs ++ zip v v'
  return
    (SymbolicSequence
     (SymbolicJump (SingleHeapPredicate Emp)
                   Set.empty
                   (AssertionConjunction (Conjunction (zipWith BoolEquals (map Variable v') es)) Emp))
     (SymbolicJump (subst m fp)
                   (Set.map (subst m) (modC ctx fc))
                   (subst m fq)),
     Set.empty)
  where
    Function (FunctionHeader _ p v) (HoareTriple {precondition = fp, command = fc, postcondition = fq}) = getFunction ctx f
chop' ctx g (ConcurrentCall (FunctionCall f1 p1 v1) (FunctionCall f2 p2 v2)) =
  return (Eps, Set.empty)
chop' ctx g (WithResourceWhen res b c) = do
  (si, l) <- chop' ctx g c
  return (_, l)
chop' ctx g (LocalVariableDeclaration vars) =
  return (Eps, Set.empty)

chop :: Context -> FunctionName -> Command -> (SymbolicInstruction, Set SymbolicHoareTriple)
chop ctx g c = evalState (chop' ctx g c) (fvs "x")


    -- let function = getFunction ctx f in

    --     (SymbolicSequence
    --     (SymbolicJump (SingleHeapPredicate Emp) (JsrInstruction Set.empty) (AssertionConjunction (SingleBooleanPredicate (Conjunction [])) (SingleHeapPredicate Emp))
    --     ()
    --     , Set.empty)


