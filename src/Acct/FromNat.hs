module Acct.FromNat
  ( FromNat( fromI, fromNat ) )
where

import Base1T
import Prelude  ( Integral )

--------------------------------------------------------------------------------

class FromNat α where
  fromNat ∷ ℕ → α
  fromI ∷ Integral ν ⇒ ν → α
  fromI = fromNat ∘ fromIntegral

instance FromNat ℕ where
  fromNat = id

instance FromNat ℤ where
  fromNat = fromIntegral

-- that's all, folks! ----------------------------------------------------------
