module Acct.Year
  ( Year, tests )
where

import Base1T
import Prelude  ( Bounded( minBound, maxBound ), Enum, Integral, Num, Real
                , mod )

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( count, try )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseBoundedIntegral )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( testParse, testParseE, tParse )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.FromNat  ( FromNat( fromNat ) )

------------------------------------------------------------

newtype Year = Year { unYear ∷ ℕ }
  deriving (Enum,Eq,Integral,Num,Ord,Real,Show)

--------------------

instance Bounded Year where
  minBound = Year 1991
  maxBound = Year 2090

--------------------

instance Validity Year where
  validate (Year y) = ю [ declare "year > 1990" $ y > 1990
                        , declare "year ≤ 2090" $ y ≤ 2090 ]

--------------------

instance GenValid Year where
  genValid    = chooseBoundedIntegral (minBound,maxBound)
  shrinkValid = pure

--------------------

instance Arbitrary Year where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Year where
  print (Year y) = P.string $ [fmt|%02d|] (y `mod` 100)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "96" (Year 1996), test "22" (Year 2022) ]


--------------------

instance TextualPlus Year where
  textual' = textual

instance Textual Year where
  textual = let y2 (read → y') = Year $ y' + if y' ≤ 90 then 2000 else 1900
            in  try ((Year ∘ read) ⊳ count 4 digit) ∤ y2 ⊳ count 2 digit

----------

parseTests ∷ TestTree
parseTests =
  testGroup "tParse"
            [ testParse "96"   (Year 1996)
            , testParse "1996" (Year 1996)
            , testParse "2006" (Year 2006)
            , testParse "06"   (Year 2006)
            , testParseE "0"   (tParse @Year) "expected: digit"
            , testParseE "x"   (tParse @Year) "expected: digit"
            , testProperty "invertibleString" (propInvertibleString @Year)
            , testProperty "invertibleText" (propInvertibleText @Year)
            ]

--------------------

instance FromNat Year where
  fromNat = Year

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Year" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
