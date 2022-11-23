{-# LANGUAGE UnicodeSyntax #-}

module Acct.T.Acct
  ( tests )
where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Acct.Account
import qualified  Acct.Amount
import qualified  Acct.Annotation
import qualified  Acct.Date
import qualified  Acct.Entries
import qualified  Acct.EntryItem
import qualified  Acct.EntryItemRaw
import qualified  Acct.EntrySource
import qualified  Acct.Month
import qualified  Acct.OStmt
import qualified  Acct.OStmtIndex
import qualified  Acct.ParseEntry
import qualified  Acct.SComment
import qualified  Acct.Stmt
import qualified  Acct.StmtIndex
import qualified  Acct.TComment
import qualified  Acct.TEntry
import qualified  Acct.TrxSimp
import qualified  Acct.TrxBrkHead
import qualified  Acct.TrxBrk
import qualified  Acct.Year

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct" [ Acct.Amount.tests
                         , Acct.TComment.tests
                         , Acct.SComment.tests
                         , Acct.Stmt.tests
                         , Acct.OStmt.tests
                         , Acct.Account.tests
                         , Acct.Month.tests
                         , Acct.Year.tests
                         , Acct.Date.tests
                         , Acct.TrxSimp.tests
                         , Acct.TrxBrkHead.tests
                         , Acct.TrxBrk.tests
                         , Acct.TEntry.tests
                         , Acct.EntrySource.tests
                         , Acct.EntryItem.tests
                         , Acct.EntryItemRaw.tests
                         , Acct.Annotation.tests
                         , Acct.StmtIndex.tests
                         , Acct.OStmtIndex.tests
                         , Acct.Entries.tests
                         , Acct.ParseEntry.tests
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


