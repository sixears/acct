module Acct.Month
  ( Month, m, month, mth, tests )
where

import Base1T
import Prelude   ( Bounded( minBound, maxBound ), Enum( fromEnum, toEnum )
                 , error )

-- base --------------------------------

import Data.Char   ( toLower )
import Data.List   ( lookup, zip )
import Data.Maybe  ( fromJust )
import Data.Tuple  ( swap )
import Text.Read   ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( digit, oneOf )
import Text.Parser.Combinators  ( (<?>), try, unexpected )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseEnum )

-- safe --------------------------------

import Safe  ( toEnumMay, toEnumSafe )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( cons, pack, singleton, take, toTitle, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.FromNat  ( FromNat( fromNat ) )

--------------------------------------------------------------------------------

data Month = January | February | March
           | April   | May      | June
           | July    | August   | September
           | October | November | December
  deriving (Eq,Lift,Show)

----------------------------------------

romans ∷ [(𝕊,Month)]
romans = zip [ "i", "ii", "iii", "iv", "v", "vi"
             , "vii", "iix", "ix", "x", "xi", "xii"]
             [January .. December]
       ⊕ [("viii",August)]

----------------------------------------

names ∷ [(𝕋,Month)]
names = [ (pack $ show o, o) | o ← [January .. December]]
      ⊕ [ (take 3 ∘ pack $ show o, o) | o ← [January .. December]]

----------------------------------------

instance Bounded Month where
  minBound = January
  maxBound = December

--------------------

instance Enum Month where
  toEnum  1 = January
  toEnum  2 = February
  toEnum  3 = March
  toEnum  4 = April
  toEnum  5 = May
  toEnum  6 = June
  toEnum  7 = July
  toEnum  8 = August
  toEnum  9 = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December
  toEnum  i = error $ [fmt|Invalid month: %d|] i

  fromEnum January   =  1
  fromEnum February  =  2
  fromEnum March     =  3
  fromEnum April     =  4
  fromEnum May       =  5
  fromEnum June      =  6
  fromEnum July      =  7
  fromEnum August    =  8
  fromEnum September =  9
  fromEnum October   = 10
  fromEnum November  = 11
  fromEnum December  = 12

--------------------

instance Validity Month where
  validate = trivialValidation

--------------------

instance GenValid Month where
  genValid    = chooseEnum (January,December)
  shrinkValid = pure

--------------------

instance Arbitrary Month where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Month where
  print o = -- that `fromJust` looks dicey, but it's safe because romans has an
            -- entry for each Month
            P.text $
              [fmt|%s|] (fromJust $ o `lookup` (swap ⊳ romans))

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "vi" June, test "iix" August ]

--------------------

instance Textual Month where
  textual = do t ← some (oneOf "iIvVxX")
               case (toLower ⊳ t) `lookup` romans of
                 𝕵 o → return o
                 𝕹   → unexpected $ [fmt|roman month %s|] t
          ∤ do t ←   try ((\ a b → a `cons` singleton b) ⊳ oneOf "01" ⊵ digit)
                   ∤ singleton ⊳ digit
               case toEnumMay (read $ unpack t) of
                 𝕵 o → return o
                 𝕹   → unexpected $ [fmt|numeric month %t|] t
          ∤ do let f a b c d = a `cons` b `cons` c `cons` d
               o ← f ⊳ oneOf "ADFJMNOSadfjmnos"
                     ⊵ (oneOf "ACEOPUaceopu") ⊵ (oneOf "BCGLNPRTVYbcglnprtvy")
                     ⊵ (pack ⊳ many (oneOf "ABCEHILMNORSTUYabcehilmnorstuy"))
               case (toTitle o) `lookup` names of
                 𝕵 n  → return n
                 𝕹    → unexpected $ [fmt|unrecognized month '%t'|] o
          <?> "month"

instance TextualPlus Month where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse "i"    January
            , testParse "VI"   June
            , testParse "IIX"  August
            , testParse "vIii" August
            , testParse "Xi"   November
            , testParse "2"  February
            , testParse "07" July
            , testParse "january"   January
            , testParse "February"  February
            , testParse "march"     March
            , testParse "April"     April
            , testParse "may"       May
            , testParse "June"      June
            , testParse "july"      July
            , testParse "August"    August
            , testParse "sepTEMBER" September
            , testParse "October"   October
            , testParse "november"  November
            , testParse "December"  December
            , testParse "Jan" January
            , testParse "feb" February
            , testParse "Mar" March
            , testParse "apr" April
            , testParse "May" May
            , testParse "jun" June
            , testParse "Jul" July
            , testParse "aug" August
            , testParse "Sep" September
            , testParse "oct" October
            , testParse "Nov" November
            , testParse "dec" December
            , testParseE "Xx" (tParse @Month) "unexpected roman month Xx"
            , testParseE "januray"(tParse @Month)"unexpected unrecognized month"
            , testParseE "13" (tParse @Month) "unexpected numeric month 13"
            , testProperty "invertibleString" (propInvertibleString @Month)
            , testProperty "invertibleText" (propInvertibleText @Month)
            ]

----------------------------------------

instance FromNat Month where
  {-| Convert number to month.  Any number out-of-range gives a valid month
      (but which one is undefined). -}
  fromNat ∷ ℕ → Month
  fromNat = toEnumSafe @Month ∘ fromIntegral

----------------------------------------

{-| QuasiQuoter for `Month` -}
month ∷ QuasiQuoter
month = mkQQExp "Month" (liftTParse' @Month tParse')

{-| Brief alias for `month` -}
mth ∷ QuasiQuoter
mth = month

{-| Very brief alias for `month` -}
m ∷ QuasiQuoter
m = month

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Month" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
