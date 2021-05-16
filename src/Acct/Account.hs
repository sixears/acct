{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Acct.Account
  ( Account, tests )
where

-- base --------------------------------

import Data.Bool      ( not )
import Data.Char      ( isAlphaNum, isUpper )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.List      ( filter )
import Data.Maybe     ( Maybe( Nothing ) )
import GHC.Exts       ( IsString )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode        ( (∧) )
import Data.Eq.Unicode          ( (≡), (≢) )
import Data.Function.Unicode    ( (∘) )
import Data.Ord.Unicode         ( (≤) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable, Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ), isValid )

-- more-unicode ------------------------

import Data.MoreUnicode              ( ℂ, 𝕊, 𝕋 )
import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- parsers -----------------------------

import Text.Parser.Char         ( alphaNum, upper )
import Text.Parser.Combinators  ( some )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, choose, listOf1, oneof )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( propInvertibleText, runTestsP, runTestsReplay, runTestTree )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import qualified  Data.Text
import Data.Text  ( find, index, length, pack )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

--------------------------------------------------------------------------------

newtype Account = Account 𝕋 deriving (Eq,IsString,Printable,Show)

instance Validity Account where
  validate (Account a) = let allAlphaNum = declare "all alphanumeric" $
                               Nothing ≡ find (not ∘ isAlphaNum) a
                             nonEmpty = declare "length ≥ 2" $ 2 ≤ length a
                             startsWithCap = declare "starts with a capital" $
                               isUpper (index a 0)
                          in ю [ nonEmpty, startsWithCap, allAlphaNum ]

instance Parsecable Account where
  parser = Account ∘ pack ⊳ ((:) ⊳ upper ⊵ some alphaNum)

instance Textual Account where
  textual = Account ∘ pack ⊳ ((:) ⊳ upper ⊵ some alphaNum)

instance GenValid Account where
  genValid = let anum ∷ Gen ℂ
                 anum = oneof [choose('A','Z'),choose('a','z'),choose('0','9')]
             in Account ∘ pack ⊳ ((:) ⊳ choose ('A','Z') ⊵ listOf1 anum)
  shrinkValid a@(Account t) = filter (\ x → x ≢ a ∧ isValid x) $
                                     Account ⊳ Data.Text.inits t

instance Arbitrary Account where
  arbitrary = genValid
  shrink {- (Account a) -} = -- every valid (non-empty) prefix
                       {-
                       Account ⊳ tail (Data.Text.inits a) -- tail is safe on
                                                          -- inits, as
                                                          -- inits "" ≡ [""]
                       -} shrinkValid

parseAccountTests ∷ TestTree
parseAccountTests =
  let parse' ∷ 𝕋 → 𝕋 → Either ParseError Account
      parse' = parsec
   in testGroup "Account.parse"
                [ testCase "Cars" $
                    Right (Account "Cars") @=? parse' "Cars" "Cars"
                , testProperty "invertibleText" (propInvertibleText @Account)
                ]

----------------------------------------


tests ∷ TestTree
tests = testGroup "Acct.Transaction" [ parseAccountTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
