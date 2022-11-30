module VCGen (chop) where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Program
import VariableConditions

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union

(/\) :: (Ord a) => Set a -> Set a -> Set a
(/\) = Set.intersection

data SCommand
    = SAssign VarName Expression
    | SHeapLookup VarName Expression FieldName
    | SHeapAssign Expression FieldName Expression
    | SNew VarName
    | SDispose Expression
    | SBlock [SCommand]
    | SJump Precondition (Set VarName) Postcondition
    | SIfThenElse BoolExpression SCommand SCommand

type SymbolicHoareTriple = (Precondition, SCommand, Postcondition)

type FreshVars = State [VarName]


fvs :: String -> [VarName]
fvs pre =
  fvs' 0
  where
    fvs' :: Integer -> [VarName]
    fvs' n = (pre ++ show n) : fvs' (n + 1)


fresh :: FreshVars VarName
fresh = do
  xs <- get
  put $ tail xs
  return $ head xs

vcg :: Context -> FunName -> HoareTriple -> FreshVars [SymbolicHoareTriple]
vcg ctx g (p, c, q) = do
  (si, l) <- chop' ctx g c
  return $ (p, si, q) : l


varAsserts :: [VarName] -> [Expression] -> PureProp
varAsserts vs es =
  foldr (\(v, e) acc -> PropAnd (PropAssert (BoolEq (Var v) e)) acc)
        PropTrue
        (zip vs es)


chop' :: Context -> FunName -> Command -> FreshVars (SCommand, [SymbolicHoareTriple])
-- chop' _ _ (Assignment s) = return (SymbolicAssignment s, [])
chop' _ _ (Assign x e) = return (SAssign x e, [])
chop' _ _ (HeapLookup x e f) = return (SHeapLookup x e f, [])
chop' _ _ (HeapAssign e1 f e2) = return (SHeapAssign e1 f e2, [])
chop' _ _ (New x) = return (SNew x, [])
chop' _ _ (Dispose e) = return (SDispose e, [])
chop' ctx g (Block cs) = do
  res <- mapM (chop' ctx g) cs
  return (SBlock $ map fst res, concatMap snd res)
chop' ctx g (IfThenElse b c1 c2) = do
  (si1, l1) <- chop' ctx g c1
  (si2, l2) <- chop' ctx g c2
  return (SIfThenElse b si1 si2, l1 ++ l2)
chop' ctx g (While b i c) = do
  l <- vcg ctx g (extendPropAnd i b, c, i)
  return (SJump
          i
          (modC ctx c)
          (extendPropAnd i (BoolNot b)),
          l)
chop' ctx _ (Call (f, xs, es)) = do
  v' <- mapM (const fresh) es
  let m = zip p xs ++ zip v v'
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (PropConj (varAsserts v' es) HeapEmp)
       , SJump (subst m fp)
               (Set.map (substVar m) (modC ctx fc))
               (subst m fq)
       ],
     [])
  where
    Function _ p v _ (fp, fc, fq) = getFunction ctx f
chop' ctx _ (ConcurrentCall (f1, xs1, es1) (f2, xs2, es2)) = do
  v1' <- mapM (const fresh) es1
  v2' <- mapM (const fresh) es2
  let m1 = zip p1 xs1 ++ zip v1 v1'
      m2 = zip p2 xs2 ++ zip v2 v2'
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (PropConj (PropAnd (varAsserts v1' es1) (varAsserts v2' es2))
                         HeapEmp)
       , SJump (propSepConj (subst m1 fp1) (subst m2 fp2))
               (Set.map (substVar m1) (modC ctx fc1) \/ Set.map (substVar m2) (modC ctx fc2))
               (propSepConj (subst m1 fq1) (subst m2 fq2))
       ],
     [])
  where
    Function _ p1 v1 _ (fp1, fc1, fq1) = getFunction ctx f1
    Function _ p2 v2 _ (fp2, fc2, fq2) = getFunction ctx f2
chop' ctx g (WithRes res b c) = do
  (si, l) <- chop' ctx g c
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (extendPropAnd r b)
       , si
       , SJump r
               (Set.fromList xs \/ u)
               (PropConj PropTrue HeapEmp)
       ],
     l)
  where
    Resource _ xs r = getResource ctx res
    u = fv r /\ Set.unions (Set.map (modF ctx . getFunction ctx) (par ctx g))
chop' _ _ _ = undefined

-- par :: Context -> FunName -> Set FunName
-- par = undefined

chop :: Context -> FunName -> Command -> (SCommand, [SymbolicHoareTriple])
chop ctx g c = evalState (chop' ctx g c) (fvs "_fv")
