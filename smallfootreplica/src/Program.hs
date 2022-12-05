module Program
  ( VarName
  , FunName
  , FieldName
  , ResName
  , Precondition
  , Postcondition
  , Invariant
  , HoareTriple
  , Command(..)
  , BoolExpression(..)
  , Expression(..)
  , Resource(..)
  , Function(..)
  , HeapProp(..)
  , PureProp(..)
  , Prop(..)
  , Program(..)
  , extendPropAnd
  , extendPropSep
  , propSepConj
  , Subst
  , subst
  , substVar
  , FV
  , fv
  ) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union


-- Type Aliases
type VarName = String
type FunName = String
type FieldName = String
type ResName = String

type Precondition = Prop
type Postcondition = Prop
type Invariant = Prop


-- Data Types
data Command
  = Assign VarName Expression
  | HeapLookup VarName Expression FieldName
  | HeapAssign Expression FieldName Expression
  | New VarName
  | Dispose Expression
  | Block [Command]
  | IfThenElse BoolExpression Command Command
  | While BoolExpression Invariant Command
  | Call ActualArgs
  | ConcurrentCall ActualArgs ActualArgs
  | WithRes ResName BoolExpression Command
  deriving Show

type ActualArgs = (FunName, [VarName], [Expression])

data BoolExpression
  = BoolEq Expression Expression
  | BoolNEq Expression Expression
  | BoolNot BoolExpression
  | BoolTrue
  deriving Show

data Expression
  = Var VarName
  | Nil
  | Const Integer
  | Xor Expression Expression
  deriving (Show, Eq)

data Resource = Resource ResName [VarName] Invariant
  deriving Show

data Function = Function FunName [VarName] [VarName] [VarName] HoareTriple
  deriving Show
-- name, pass-by-ref args, pass-by-value args, local vars, body


-- Props
data HeapProp
  = PointsTo Expression [(FieldName, Expression)]
  | HeapTree Expression
  | HeapListSegment Expression Expression
  | HeapXORList Expression Expression Expression Expression
  | HeapSep HeapProp HeapProp
  | HeapEmp
  deriving Show
data PureProp
  = PropAssert BoolExpression
  | PropAnd PureProp PureProp
  | PropTrue
  deriving Show

data Prop
  = PropIfThenElse PureProp Prop Prop
  | PropConj PureProp HeapProp
  deriving Show

extendPropAnd :: Prop -> BoolExpression -> Prop
extendPropAnd (PropIfThenElse pp pt pf) be = PropIfThenElse pp (extendPropAnd pt be) (extendPropAnd pf be)
extendPropAnd (PropConj p h) be = PropConj (PropAnd p (PropAssert be)) h

extendPropSep :: Prop -> HeapProp -> Prop
extendPropSep (PropIfThenElse pp pt pf) hp = PropIfThenElse pp (extendPropSep pt hp) (extendPropSep pf hp)
extendPropSep (PropConj p h) hp = PropConj p (HeapSep h hp)

propSepConj :: Prop -> Prop -> Prop
propSepConj (PropConj p1 h1) (PropConj p2 h2) = PropConj (PropAnd p1 p2) (HeapSep h1 h2)
propSepConj (PropIfThenElse pp pt pf) p = PropIfThenElse pp (propSepConj pt p) (propSepConj pf p)
propSepConj p (PropIfThenElse pp pt pf) = PropIfThenElse pp (propSepConj pt p) (propSepConj pf p)

data Program = Program [FieldName] [Resource] [Function]
    deriving Show

type HoareTriple = (Precondition, Command, Postcondition)


-- Substitution
class Subst a where
  subst :: [(VarName, VarName)] -> a -> a

substVar :: [(VarName, VarName)] -> VarName -> VarName
substVar m x = fromMaybe x $ lookup x m


instance Subst Expression where
  subst m (Var var) = Var $ substVar m var
  subst _ Nil = Nil
  subst _ (Const c) = Const c
  subst m (Xor e1 e2) = Xor (subst m e1) (subst m e2)

instance Subst BoolExpression where
  subst m (BoolEq e1 e2) = BoolEq (subst m e1) (subst m e2)
  subst m (BoolNEq e1 e2) = BoolNEq (subst m e1) (subst m e2)
  subst m (BoolNot be) = BoolNot (subst m be)
  subst _ BoolTrue = BoolTrue

instance Subst HeapProp where
  subst m (PointsTo e heap) = PointsTo (subst m e) (fmap (fmap (subst m)) heap)
  subst m (HeapTree e) = HeapTree (subst m e)
  subst m (HeapListSegment e e') = HeapListSegment (subst m e) (subst m e')
  subst m (HeapXORList e1 e2 e3 e4) = HeapXORList (subst m e1) (subst m e2) (subst m e3) (subst m e4)
  subst m (HeapSep hp1 hp2) = HeapSep (subst m hp1) (subst m hp2)
  subst _ HeapEmp = HeapEmp

instance Subst PureProp where
  subst m (PropAssert be) = PropAssert (subst m be)
  subst m (PropAnd p1 p2) = PropAnd (subst m p1) (subst m p2)
  subst _ PropTrue = PropTrue

instance Subst Prop where
  subst m (PropIfThenElse pp p1 p2) = PropIfThenElse (subst m pp) (subst m p1) (subst m p2)
  subst m (PropConj p h) = PropConj (subst m p) (subst m h)


-- Free Variables
class FV a where
  fv :: a -> Set VarName


instance FV Expression where
  fv (Var v) = Set.singleton v
  fv (Const _) = Set.empty
  fv (Xor e1 e2) = fv e1 \/ fv e2
  fv Nil = Set.empty

instance FV BoolExpression where
  fv (BoolEq e1 e2) = fv e1 \/ fv e2
  fv (BoolNEq e1 e2) = fv e1 \/ fv e2
  fv (BoolNot be) = fv be
  fv BoolTrue = Set.empty

instance FV Prop where
  fv (PropIfThenElse pp p1 p2) = fv pp \/ fv p1 \/ fv p2
  fv (PropConj p h) = fv p \/ fv h

instance FV PureProp where
  fv (PropAssert be) = fv be
  fv (PropAnd p1 p2) = fv p1 \/ fv p2
  fv PropTrue = Set.empty

instance FV HeapProp where
  fv (PointsTo e h) = fv e \/ Set.unions (map (fv . snd) h)
  fv (HeapTree e) = fv e
  fv (HeapListSegment e1 e2) = fv e1 \/ fv e2
  fv (HeapXORList e1 e2 e3 e4) = fv e1 \/ fv e2 \/ fv e3 \/ fv e4
  fv (HeapSep h1 h2) = fv h1 \/ fv h2
  fv HeapEmp = Set.empty

instance FV a => FV [a] where
    fv = Set.unions . map fv

instance FV Command where
  fv (Assign x e) = Set.singleton x \/ fv e
  fv (HeapLookup x e _) = Set.singleton x \/ fv e
  fv (HeapAssign e1 _ e2) = fv e1 \/ fv e2
  fv (New x) = Set.singleton x
  fv (Dispose e) = fv e
  fv (IfThenElse b c1 c2) = fv b \/ fv c1 \/ fv c2
  fv (While b i c) = fv b \/ fv i \/ fv c
  fv (Block cs) = Set.unions $ map fv cs
  fv (Call (_, xs, es)) = Set.fromList xs \/ fv es
  fv (ConcurrentCall call1 call2) =
    fv (Call call1) \/ fv (Call call2)
  fv (WithRes _ b c) =
    fv b \/ fv c
