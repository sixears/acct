{- | Broken-down transaction, e.g.,
            , testParse (intercalate "\n" [

@
        10.13+ #D<6.viii.96>B<>X<5>
        #10+   #D<4.vi.96>X<5>A<Foo>
        #0.13+ #D<5.i.96>X<5>A<Bar>
        ##
@

-}
module Acct.TrxBrk
  ( TrxBrk( TrxBrk ), trx, tests )
where

import Base1T

-- base --------------------------------

import Data.Foldable  ( sum )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( endByNonEmpty, unexpected )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, listOf1 )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck      ( testProperty )

-- text --------------------------------

import Data.Text  ( intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus    ( testParse, testParseE, tParse )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( acct )
import Acct.Amount      ( amount )
import Acct.Date        ( date )
import Acct.OStmt       ( ostmt )
import Acct.Parser      ( newline, wspaces )
import Acct.Stmt        ( stmt )
import Acct.TrxBrkHead  ( TrxBrkHead, tbh_ )
import Acct.TrxSimp     ( TrxSimp, tsimp_ )

--------------------------------------------------------------------------------

data TrxBrk = TrxBrk TrxBrkHead (NonEmpty TrxSimp)
  deriving (Eq,Show)

--------------------

instance Validity TrxBrk where
  validate (TrxBrk h ts) =
    let totalsMatch = declare "amount totals sum" $
                        h ⊣ amount ≡ sum (view amount ⊳ ts)
    in  totalsMatch

--------------------

instance GenValid TrxBrk where
  genValid    = do
    let listOf1' ∷ Gen α → Gen (NonEmpty α)
        listOf1' g = fromList ⊳ listOf1 g
    ts ← listOf1' arbitrary
    let total = sum (view amount ⊳ ts)
    TrxBrk ⊳ (tbh_ total ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary) ⊵ pure ts
  shrinkValid = pure

--------------------

instance Arbitrary TrxBrk where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable TrxBrk where
  print (TrxBrk h tx) =
    P.text $ [fmt|%T\n%t\n##|] h
                               (intercalate "\n" ∘ toList $ ("#"⊕) ∘toText ⊳ tx)

----------

printTests ∷ TestTree
printTests =
  testGroup "print"
    [ testCase "10.13+" $
        let
          h  = TrxBrk (tbh_ 1013 [date|1996-8-6|] (𝕵 [stmt|5|]) 𝕹)
          t1 = tsimp_ 1000 [date|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|])
                           (𝕵 [ostmt|X:6|]) 𝕹
          t2 = tsimp_ 13 [date|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|]) 𝕹 𝕹
        in
          intercalate "\n" [ "10.13+\t#D<6.iix.96>B<>X<5>"
                           , "#10.00+\t#D<4.vi.96>A<Foo>X<5>O<X:6>"
                           , "#0.13+\t#D<5.i.96>A<Bar>X<5>"
                           , "##"
                           ] ≟ toText (h (t1 :| [t2]))
    ]

--------------------

instance Textual TrxBrk where
  textual =
    let
      nlhash = newline ⋪ char '#'
      check_breakdown (h,ts) = do
        let h_am  = h ⊣ amount
            ts_am = sum (view amount ⊳ ts)

        if h_am ≡ ts_am
        then return (TrxBrk h ts)
        else unexpected $ [fmt|breakdown total was %T, expected %T|] ts_am h_am

    in
      ((,) ⊳ (textual ⋪ nlhash)
           ⊵ ((wspaces ⋫ textual) `endByNonEmpty` nlhash)
           ⋪ char '#' ⋪ wspaces
      ) ≫ check_breakdown

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ let
                h  = tbh_ 1013 [date|1996-8-6|] (𝕵 [stmt|5|]) 𝕹
                t1 = tsimp_ 1000 [date|1996-6-4|] [acct|Foo|] (𝕵 [stmt|6|])
                                 𝕹 𝕹
                t2 = tsimp_ 13 [date|1996-1-5|] [acct|Bar|] (𝕵 [stmt|7|])
                               (𝕵 [ostmt|P|]) 𝕹
              in
                testParse (intercalate "\n" [ "10.13+ #D<6.viii.96>B<>X<5>"
                                            , "#10+   #D<4.vi.96>X<6>A<Foo>"
                                            , "#0.13+ #D<5.i.96>X<7>A<Bar>O<P>"
                                            , "##"
                                            ]) $ TrxBrk h (t1 :| [t2])
            , testParseE (intercalate "\n" [ "10.13+ #D<6.viii.96>B<>X<5>"
                                           , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                           , "#1.30+ #D<5.i.96>X<5>A<Bar>"
                                           , "##"
                                           ])
                         (tParse @TrxBrk)
                         (ю [ "unexpected breakdown total "
                            , "was 11.30+, expected 10.13+" ])
            , testProperty "invertibleString" (propInvertibleString @TrxBrk)
            , testProperty "invertibleText" (propInvertibleText @TrxBrk)
            ]

----------------------------------------

trx ∷ TrxBrk → NonEmpty TrxSimp
trx (TrxBrk _ ts) = ts

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TrxBrk" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
