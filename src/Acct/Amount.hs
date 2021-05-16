{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Acct.Amount
  ( Amount( Amount ), tests )
where

import Prelude  ( (*), abs, quot, rem )

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Either          ( Either( Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Text.Read            ( read )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )
import Prelude.Unicode      ( ℤ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit )
import Text.Parsec.Combinator  ( many1, option )
import Text.Parsec.Prim        ( try )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data Sign = SIGN_PLUS | SIGN_MINUS

instance Parsecable Sign where
  parser = try (pure SIGN_MINUS ⋪ char '-') ∤ pure SIGN_PLUS ⋪ char '+'

signmult ∷ Sign → ℤ
signmult SIGN_PLUS  = 1
signmult SIGN_MINUS = -1

------------------------------------------------------------

newtype Amount  = Amount ℤ  deriving (Eq,Show)

instance Printable Amount where
  print (Amount p) = P.text $ [fmt|£%d.%02d|] (p `quot` 100) (abs p `rem` 100)

instance Parsecable Amount where
  parser = let penceP      = (\ x y → [x,y]) ⊳ (char '.' ⋫ digit) ⊵ digit
               pence       = option "00" penceP
               mkval pnds pnc sgn = signmult sgn * read (pnds ⊕ pnc)
            in Amount ⊳ (mkval ⊳ many1 digit ⊵ pence ⊵ parser)

parseAmountTests ∷ TestTree
parseAmountTests =
  let
    parse' ∷ 𝕋 → 𝕋 → Either ParseError Amount
    parse' = parsec
    a1           = Amount 100
    a11_01       = Amount (-1101)
    one          = "1+"
    eleventy_one = "11.01-"
   in
    testGroup
      "Amount.parse"
      [ testCase "1" $ Right a1 @=? parse' "1" one
      , testCase "-11.01" $ Right a11_01 @=? parse' "-11.01" eleventy_one
      ]

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Amount" [ parseAmountTests ]

-- that's all, folks! ----------------------------------------------------------
