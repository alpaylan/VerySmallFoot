module Oracle (oracle, Entailment(..)) where

import Program


data Entailment = Entailment Prop Prop

oracle :: Entailment -> Bool
oracle _ = False
