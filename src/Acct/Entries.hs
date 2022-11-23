{-# LANGUAGE OverloadedLists #-}

{-| An `Entries` is a list of `Entry`s. -}
module Acct.Entries
  ( Entries( Entries ), emptyEntries, tests )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List    ( inits, tails, zip )
import Data.Monoid  ( Monoid( mempty ) )
import Data.Tuple   ( uncurry )
import GHC.Exts     ( IsList( toList ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( listOf )

-- safe --------------------------------

import Safe  ( initSafe, tailSafe )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- text --------------------------------

import Data.Text  ( intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account      ( acct )
import Acct.Annotation   ( Annotation( Annotation ) )
import Acct.Date         ( dte )
import Acct.EntryItem    ( EntryItem ( TAcctStart, TOStmtStart, TTEntry ) )
import Acct.EntrySource  ( EntrySource( SourceString ) )
import Acct.OStmtName    ( ostmtname )
import Acct.SComment     ( scmt )
import Acct.Stmt         ( stmt )
import Acct.TComment     ( tcmt )
import Acct.TEntry       ( TEntry ( TBrk, SheetComment, TSimpleTrx ) )
import Acct.TrxBrkHead   ( tbh_ )
import Acct.TrxSimp      ( tsimp_ )
import Acct.TrxBrk       ( trxBrk )

--------------------------------------------------------------------------------

newtype Entries ι κ = Entries [Annotation ι κ]
  deriving (Eq,Show)

instance Semigroup (Entries ι κ) where
  (Entries es) <> (Entries es') = Entries (es ◇ es')

instance Monoid (Entries ι κ) where
  mempty = Entries []

instance IsList (Entries ι κ) where
  type Item (Entries ι κ) = Annotation ι κ
  toList (Entries es) = es
  fromList = Entries

emptyEntries ∷ ∀ ι κ . Entries ι κ
emptyEntries = Entries []

{- | A comment, account start, other account start, simple or broken-down
     transaction; so, roughly, a "line" of an input file (albeit several lines
     for a broken-down transaction.
-}
instance Validity (Entries ι κ) where
  validate = trivialValidation

--------------------

instance (GenValid ι, GenValid κ) ⇒ GenValid (Entries ι κ) where
  genValid    = Entries ⊳ listOf arbitrary
  shrinkValid (Entries es) =
    Entries ∘ uncurry (⊕) ⊳ zip (initSafe $ inits es) (tailSafe $ tails es)

--------------------

instance (GenValid ι, GenValid κ) ⇒ Arbitrary (Entries ι κ) where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance (Printable ι, Printable κ) ⇒ Printable (Entries ι κ) where
  print (Entries es) = P.text $ intercalate "\n" (toText ⊳ es)

----------

printTests ∷ TestTree
printTests =
  let
    unline  = intercalate "\n"
    tcomm   = TTEntry $ SheetComment [scmt|% a comment|]
    tacct   = TAcctStart [acct|Acct|]
    tost    = TOStmtStart [ostmtname|Y|]
    tsimp1  = TTEntry $ TSimpleTrx (tsimp_ (-1234) [dte|1973-01-01|] [acct|Act|]
                                           (𝕵 [stmt|77|]) 𝕹
                                           (𝕵 [tcmt|my comment|]))
    tsimp2  = TTEntry $ TSimpleTrx (tsimp_ 4321 [dte|1976-04-01|] [acct|Act|] 𝕹
                                           𝕹 𝕹)
    thead   = tbh_ (-1234) [dte|1973-01-01|] (𝕵 [stmt|77|]) 𝕹 𝕹
    t0      = tsimp_ (-1300) [dte|1969-02-02|] [acct|Act|] 𝕹 𝕹 𝕹
    t1      = tsimp_ 66 [dte|1966-05-26|] [acct|Bct|] 𝕹 𝕹 𝕹
    tb      = TTEntry ∘ TBrk $ trxBrk thead (t0 :| [t1])
    mkEntry ∷ ι → (Annotation ι EntrySource)
    mkEntry t = Annotation t (SourceString "")
  in
    testGroup "print"
      [ testCase "print1c" $ "% a comment" ≟ toText (Entries [ mkEntry tcomm ])
      , testCase "print1a" $ "Start: Acct" ≟ toText (Entries [ mkEntry tacct ])
      , testCase "print1o" $ "oStart: Y" ≟ toText (Entries [ mkEntry tost ])
      , testCase "tsimp1" $
            "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
          ≟ toText (Entries [ mkEntry tsimp1 ])
      , testCase "tsimp2" $
            "43.21+\t#D<1.iv.76>A<Act>"
          ≟ toText (Entries [ mkEntry tsimp2 ])
      , testCase "print1b" $
            unline [ "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"]
          ≟ toText (Entries [ mkEntry tb ])
      , testCase "print2" $
            unline [ "% a comment"
                   , "Start: Acct"
                   , "oStart: Y"
                   , "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
                   , "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"
                   ]
          ≟ toText (Entries $ mkEntry ⊳ [ tcomm, tacct, tost, tsimp1, tb ])
      , testCase "print3" $
            unline [ "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"
                   , "Start: Acct"
                   , "% a comment"
                   , "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
                   , "oStart: Y"
                   ]
          ≟ toText (Entries $ mkEntry ⊳ [ tb, tacct, tcomm, tsimp1, tost ])
    ]

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Entries" [ printTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
