module Oracle (oracle, Entailment(..)) where

import Program


data Entailment = Entailment Prop Prop

oracle :: Entailment -> Bool
oracle _ = False

-- p |= q <=> \forall s, h. s,h |= p -> s,h |= q
-- s, h |= E1 = E2          <=>     s, h |= [|E1|]s = [|E2|]s
-- s, h |= ¬P               <=>     s, h /= P
-- s, h |= True             <=>     True\
-- s, h |= \pi1 /\ \pi2     <=>     s, h |= \pi1 /\ s, h |= \pi2
-- s, h |= emp              <=>     h = \emptyset
-- s, h |= \sig1 * \sig2    <=>     \exists h1, h2. h = h1 * h2 /\ s, h1 |= \sig1 /\ s, h2 |= \sig2
-- s, h |= \pi | \sig       <=>     s, h |= \pi /\ s, h |= \sig
semanticModel :: Entailment -> [Prop]
semanticModel (Entailment p q) = undefined --[modelBool p, modelHeap q]

--modelBool ()

