{-| An `Annotation` is any thing that is paired with an annotation.
    The main point of interest is that for, e.g., `Eq` and similar classes;
    the annotation is ignored.
-}
module Acct.Annotation
  ( Annotation(..), tests )
where

import Base1T

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( suchThat )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), isValid )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.StmtIndex  ( NotStmtIndex( notStmtIndex ) )

--------------------------------------------------------------------------------

data Annotation ι ρ = Annotation ι ρ
  deriving Show

instance Eq ι ⇒ Eq (Annotation ι ρ) where
  Annotation i _ == Annotation i' _ = i == i'

--------------------

instance Validity ι ⇒ Validity (Annotation ι ρ) where
  validate (Annotation i _)   = validate i

--------------------

instance (GenValid ι, GenValid ρ) ⇒ GenValid (Annotation ι ρ) where
  genValid = (Annotation ⊳ genValid ⊵ genValid) `suchThat` isValid

  shrinkValid (Annotation i s) =
    ю [ Annotation i ⊳ shrinkValid s, (\ i' → Annotation i' s) ⊳ shrinkValid i ]

--------------------

instance (GenValid ι, GenValid ρ) ⇒ Arbitrary (Annotation ι ρ) where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable ι ⇒ Printable (Annotation ι ρ) where
  print (Annotation t _) = print t

----------------------------------------

instance NotStmtIndex α ⇒ NotStmtIndex (Annotation α ρ) where
  notStmtIndex x (Annotation a _) = notStmtIndex x a

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Annotation" [ ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
