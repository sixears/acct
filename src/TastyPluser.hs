module TastyPluser
  ( lits, litt, shrinkString, shrinkText )
where

import Base1T

-- base --------------------------------

import qualified  Data.List  as  List

import Data.Char   ( showLitChar )
import Data.List   ( zip )
import Data.Tuple  ( uncurry )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( pack, unpack )

--------------------------------------------------------------------------------

{-| Shrink a string @s@ by generating all the substrings that are @s@ with one
    character removed -}
shrinkString ∷ 𝕊 → [𝕊]
shrinkString s =
    ((uncurry (⊕)) ⊳ zip (List.init $ List.inits s) (List.tail $ List.tails s))

{-| Shrink a string @s@ by generating all the substrings that are @s@ with one
    character removed -}
shrinkText ∷ 𝕋 → [𝕋]
shrinkText s =
    ((uncurry (⊕)) ⊳ zip (List.init $ Text.inits s) (List.tail $ Text.tails s))

----------------------------------------

{-| Convert a string to its representation as printable chars, i.e., converting
    e.g., newline to "\\n", etc. -}
lits ∷ 𝕊 → 𝕊
-- use foldr' to be strict, avoiding a space leak
lits s = foldr' ($) "" (showLitChar ⊳ s)

litt ∷ 𝕋 → 𝕋
-- use foldr' to be strict, avoiding a space leak
litt = pack ∘ lits ∘ unpack



-- that's all, folks! ----------------------------------------------------------