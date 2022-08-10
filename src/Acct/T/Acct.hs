{-# LANGUAGE UnicodeSyntax #-}

module Acct.T.Acct
  ( tests )
where

-- base --------------------------------

import System.Exit  ( ExitCode )
import System.IO    ( IO )

-- base-unicode-symbols ----------------

import Numeric.Natural.Unicode  ( ℕ )

-- more-unicode ------------------------

import Data.MoreUnicode.String  ( 𝕊 )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Acct.Amount
import qualified  Acct.Comment
import qualified  Acct.Date
import qualified  Acct.Expression
import qualified  Acct.Month
import qualified  Acct.Stmt
import qualified  Acct.TrxBrk
import qualified  Acct.TrxBrkHead
import qualified  Acct.TrxSimp
import qualified  Acct.Year

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct" [ Acct.Amount.tests
                         , Acct.Comment.tests
                         , Acct.Month.tests
                         , Acct.Stmt.tests
                         , Acct.Year.tests
                         , Acct.Date.tests
                         , Acct.TrxSimp.tests
                         , Acct.TrxBrk.tests
                         , Acct.TrxBrkHead.tests
                         , Acct.Expression.tests
                         ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------


