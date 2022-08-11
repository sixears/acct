module Acct.Mapish
  ( Mapish(..) )
where

--------------------------------------------------------------------------------

class Mapish π where
  type Key π
  type Value π
  adjust ∷ ((Value π) → (Value π)) → (Key π) → π → π
  insert ∷ (Key π) → (Value π) → π → π
  empty  ∷ π

-- that's all, folks! ----------------------------------------------------------
