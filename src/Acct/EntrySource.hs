{-| Where an entry was found, i.e., within a string or a named file. -}
module Acct.EntrySource
  ( EntrySource( SourceFile, SourceString ), tests )
where

import Base1T

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( oneof )
import Test.QuickCheck.Modifiers  ( getPrintableString )

-- tasty-plus --------------------------

import TastyPlus    ( shrinkList )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data EntrySource = SourceString 𝕊 | SourceFile AbsFile  deriving (Eq,Show)

--------------------

instance Validity EntrySource where
  validate (SourceString s) = trivialValidation s
  validate (SourceFile f)   = trivialValidation f

--------------------

instance GenValid EntrySource where
  genValid    = oneof [ SourceFile ⊳ arbitrary
                      , SourceString ∘ getPrintableString ⊳ arbitrary ]
  shrinkValid (SourceString s) = SourceString ⊳ shrinkList s
  shrinkValid (SourceFile f) = SourceFile ⊳ shrink f

----------------------------------------

instance Printable EntrySource where
  print (SourceString s) = print s
  print (SourceFile   f) = print f

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.EntrySource" [ ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
