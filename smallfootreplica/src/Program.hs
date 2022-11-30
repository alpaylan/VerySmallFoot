module Program where

import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as Set


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

type ActualArgs = (FunName, [VarName], [Expression])

data BoolExpression
  = BoolEq Expression Expression
  | BoolNEq Expression Expression
  | BoolNot BoolExpression
  | BoolTrue

-- negateBE :: BoolExpression -> BoolExpression
-- negateBE (BoolEq e1 e2) = BoolNEq e1 e2
-- negateBE (BoolNEq e1 e2) = BoolEq e1 e2

data Expression
  = Var VarName
  | Nil
  | Const Integer
  | Xor Expression Expression

data Resource = Resource ResName [VarName] Invariant
data Function = Function FunName [VarName] [VarName] [VarName] HoareTriple
-- name, pass-by-ref args, pass-by-value args, local vars, body


-- Props
data HeapProp
  = PointsTo Expression [(FieldName, Expression)]
  | HeapTree Expression
  | HeapListSegment Expression Expression
  | HeapXORList Expression Expression Expression Expression
  | HeapSep HeapProp HeapProp
  | HeapEmp

data PureProp
  = PropAssert BoolExpression
  | PropAnd PureProp PureProp
  | PropTrue

data Prop
  = PropIfThenElse PureProp Prop Prop
  | PropConj PureProp HeapProp

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
