{-# LANGUAGE UnicodeSyntax #-}

module Acct.T.Acct
  ( tests )
where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Acct.Amount
import qualified  Acct.Account
import qualified  Acct.Comment
import qualified  Acct.Date
import qualified  Acct.Entries
import qualified  Acct.Entry
import qualified  Acct.Month
import qualified  Acct.OStmt
import qualified  Acct.Stmt
import qualified  Acct.TrxBrk
import qualified  Acct.TrxBrkHead
import qualified  Acct.TrxSimp
import qualified  Acct.Year

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct" [ Acct.Amount.tests
                         , Acct.Comment.tests
                         , Acct.Stmt.tests
                         , Acct.OStmt.tests
                         , Acct.Account.tests
                         , Acct.Month.tests
                         , Acct.Year.tests
                         , Acct.Date.tests
                         , Acct.TrxSimp.tests
                         , Acct.TrxBrk.tests
                         , Acct.TrxBrkHead.tests
                         , Acct.Entry.tests
                         , Acct.Entries.tests
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


