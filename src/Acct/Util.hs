module Acct.Util
  ( Pretty( pretty ), (⋮), (⋰), mcons )
where

import Base1T  hiding  (fromList, toList)

-- base --------------------------------

import GHC.Exts  ( IsList( fromList, toList ) )

--------------------------------------------------------------------------------

class Pretty α where
  pretty ∷ α → 𝕋

------------------------------------------------------------

{-| Cons onto a list, creating it if necessary. -}
mcons ∷ α → 𝕄 [α] → [α]
mcons x = maybe [x] (x:)

(⋮) ∷ IsList ψ ⇒ Item ψ → ψ → ψ
x ⋮ xs = fromList (x:toList xs)

(⋰) ∷ IsList ψ ⇒ Item ψ → 𝕄 ψ → ψ
x ⋰ xs = maybe (fromList [x]) (x⋮) xs

-- that's all, folks! ----------------------------------------------------------
