module Acct.Util
  ( mcons )
where

import Base1T

--------------------------------------------------------------------------------

{-| Cons onto a list, creating it if necessary. -}
mcons ∷ α → 𝕄 [α] → [α]
mcons x = maybe [x] (x:)

-- that's all, folks! ----------------------------------------------------------
