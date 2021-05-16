{-# LANGUAGE UnicodeSyntax #-}

module Acct.Parser
  ( noNLCR, wspaces )
where

-- base --------------------------------

import Control.Applicative  ( many )
import Data.Char            ( isSpace )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode  ( (∧) )
import Data.List.Unicode  ( (∉) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool    ( 𝔹 )
import Data.MoreUnicode.Char    ( ℂ )
import Data.MoreUnicode.String  ( 𝕊 )

-- parsec ------------------------------

import Text.Parsec.Char  ( noneOf, satisfy )
import Text.Parsec.Prim  ( ParsecT, Stream, skipMany )

--------------------------------------------------------------------------------

nl ∷ ℂ
nl = '\n'

cr ∷ ℂ
cr = '\r'

nlcr ∷ [ℂ]
nlcr = [ nl, cr ]

notNLCR ∷ Stream s m ℂ ⇒ ParsecT s u m ℂ
notNLCR = noneOf nlcr

noNLCR ∷ Stream s m ℂ ⇒ ParsecT s u m 𝕊
noNLCR = many notNLCR

{- | Like `isSpace`, but excludes nl/cr chars. -}
isWSpace ∷ ℂ → 𝔹
isWSpace c = isSpace c ∧ c ∉ nlcr

wspace ∷ Stream s m ℂ ⇒ ParsecT s u m ℂ
wspace = satisfy isWSpace

wspaces ∷ Stream s m ℂ ⇒ ParsecT s u m ()
wspaces = skipMany wspace

-- that's all, folks! ----------------------------------------------------------
