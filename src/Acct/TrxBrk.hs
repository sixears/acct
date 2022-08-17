{-# LANGUAGE DeriveAnyClass #-}

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
  ( TrxBrk, trx, trxBrk, tests )
where

import Base1T

-- base --------------------------------

import GHC.Generics   ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

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

import Acct.Account      ( acct )
import Acct.Amount       ( amount, aTotal )
import Acct.Date         ( date )
import Acct.OStmt        ( ostmt )
import Acct.Parser       ( newline, wspaces )
import Acct.Stmt         ( stmt )
import Acct.TrxBrkHead   ( TrxBrkHead, tbh_ )
import Acct.TrxSimp      ( TrxSimp, parent, tsimp_ )

--------------------------------------------------------------------------------

data TrxBrk = TrxBrk TrxBrkHead (NonEmpty TrxSimp)
  deriving (Eq,Generic,NFData,Show)

trxBrk ∷ TrxBrkHead → NonEmpty TrxSimp → TrxBrk
trxBrk h ts = TrxBrk h $ (& parent ⊩ h) ⊳ ts

--------------------

instance Validity TrxBrk where
  validate (TrxBrk h ts) =
    let totalsMatch = declare "amount totals sum" $
                        h ⊣ amount ≡ aTotal ts
    in  totalsMatch

--------------------

instance GenValid TrxBrk where
  genValid    = do
    let listOf1' ∷ Gen α → Gen (NonEmpty α)
        listOf1' g = fromList ⊳ listOf1 g
    ts ← listOf1' arbitrary
    h ← tbh_ ⊳ pure (aTotal ts) ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary
    let ts' = (& parent ⊩ h) ⊳ ts
    return $ TrxBrk h ts'

  shrinkValid _ = []

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
          h  = tbh_ 1013 [date|1996-8-6|] (𝕵 [stmt|5|]) 𝕹 𝕹
          t1 = tsimp_ 1000 [date|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|])
                           (𝕵 [ostmt|X:6|]) 𝕹
                      & parent ⊩ h
          t2 = tsimp_ 13 [date|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|]) 𝕹 𝕹
                      & parent ⊩ h
          b  = TrxBrk h (t1 :| [t2])
        in
          intercalate "\n" [ "10.13+\t#D<6.iix.96>B<>X<5>"
                           , "#10.00+\t#D<4.vi.96>A<Foo>X<5>O<X:6>"
                           , "#0.13+\t#D<5.i.96>A<Bar>X<5>"
                           , "##"
                           ] ≟ toText b
    ]

--------------------

instance Textual TrxBrk where
  textual =
    let
      nlhash = newline ⋪ char '#'

      check_breakdown b@(TrxBrk h ts) = do
        let h_am  = h ⊣ amount
            ts_am = aTotal ts
        if h_am ≡ ts_am
        then return b
        else unexpected $ [fmt|breakdown total was %T, expected %T|] ts_am h_am

      set_parents (h,ts) = TrxBrk h $ (& parent ⊩ h) ⊳ ts

    in
      ((,) ⊳ (textual ⋪ nlhash)
           ⊵ ((wspaces ⋫ textual) `endByNonEmpty` nlhash)
           ⋪ char '#' ⋪ wspaces
      ) ≫ return ∘ set_parents ≫ check_breakdown

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ let
                h  = tbh_ 1013 [date|1996-8-6|] (𝕵 [stmt|5|]) (𝕵 [ostmt|X|]) 𝕹
                t1 = tsimp_ 1000 [date|1996-6-4|] [acct|Foo|] (𝕵 [stmt|6|])
                                 𝕹 𝕹
                            & parent ⊩ h
                t2 = tsimp_ 13 [date|1996-1-5|] [acct|Bar|] (𝕵 [stmt|7|])
                               (𝕵 [ostmt|P|]) 𝕹
                            & parent ⊩ h
              in
                testParse (intercalate "\n" [ "10.13+ #D<6.viii.96>B<>X<5>O<X>"
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
