{-# LANGUAGE UnicodeSyntax #-}

module Acct.Parser
  ( newline, noNLCR, printableAscii, wspaces, wspaces' )
where

import Base1T

-- base --------------------------------

import Data.Char  ( isSpace )

-- parsers -----------------------------

import Text.Parser.Char  ( CharParsing, char, noneOf, oneOf, satisfy )

--------------------------------------------------------------------------------

nl ∷ ℂ
nl = '\n'

cr ∷ ℂ
cr = '\r'

nlcr ∷ [ℂ]
nlcr = [ nl, cr ]

notNLCR ∷ (Monad η, CharParsing η) ⇒ η ℂ
notNLCR = noneOf nlcr

{- | A printable ASCII char, i.e., not DEL, or newline, etc. -}
printableAscii ∷ (Monad η, CharParsing η) ⇒ η ℂ
printableAscii = oneOf [ ' ' .. '~' ]

noNLCR ∷ (Monad η, CharParsing η) ⇒ η 𝕊
noNLCR = many notNLCR

newline ∷ (Monad η, CharParsing η) ⇒ η 𝕊
newline = (⊕) ⊳ many (char '\r') ⊵ (pure ⊳ char '\n')

{- | Like `isSpace`, but excludes nl/cr chars. -}
isWSpace ∷ ℂ → 𝔹
isWSpace c = isSpace c ∧ c ∉ nlcr

wspace ∷ (Monad η, CharParsing η) ⇒ η ℂ
wspace = satisfy isWSpace

wspaces ∷ (Monad η, CharParsing η) ⇒ η 𝕊
wspaces = many wspace

wspaces' ∷ (Monad η, CharParsing η) ⇒ η 𝕊
wspaces' = some wspace

-- that's all, folks! ----------------------------------------------------------
