module Acct.Date
  ( Date, HasDate( date ), dte, tests )
where

import Base1T

import Prelude  ( Enum( fromEnum ), Int )

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char, digit, oneOf )
import Text.Parser.Combinators  ( (<?>), try, unexpected )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseInt, suchThatMap )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE, LitE ), Lit( IntegerL )
                                   , Lift( liftTyped ), TExp( TExp ) )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- time --------------------------------

import Data.Time.Calendar  ( Day( ModifiedJulianDay )
                           , fromGregorian, fromGregorianValid, toGregorian )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.FromNat  ( fromI )
import Acct.Month    ( Month )
import Acct.Year     ( Year )
import Acct.Util     ( Pretty( pretty ) )


--------------------------------------------------------------------------------

newtype Date  = Date Day deriving (Eq,NFData,Ord,Show)

fromYMD ∷ ℤ → Int → Int → Date
fromYMD y m a = Date $ fromGregorian y m a

--------------------

instance Lift Date where
  liftTyped (Date (ModifiedJulianDay i)) =
    return ∘ TExp $
      AppE (ConE 'Date) (AppE (ConE 'ModifiedJulianDay) (LitE $ IntegerL i))

--------------------

instance Validity Date where
  validate = trivialValidation

--------------------

instance GenValid Date where
  genValid    =
    let genYMD = (,,) ⊳ arbitrary @Year ⊵ arbitrary ⊵ chooseInt (1,31)
        validDate (y,m,a) = Date ⊳ fromGregorianValid (fromIntegral y) m a
     in genYMD `suchThatMap` validDate
  shrinkValid = pure

--------------------

instance Arbitrary Date where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Date where
  print (Date t) =
    let (y,m,a) = toGregorian t
    in  P.text $ [fmt|%d.%T.%T|] a (fromI @Month m) (fromI @Year y)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "4.vi.96" (fromYMD 1996 6 4)
                      , test "22.vii.22" (fromYMD 2022 7 22)
                      , test "22.iix.22" (fromYMD 2022 8 22)
                      ]

--------------------

instance Pretty Date where
  pretty (Date t) =
    let (y,m,d) = toGregorian t
    in  [fmt|%04d-%02d-%02d|] y m d

--------------------

instance Textual Date where
  textual = let cons2 a b = a : [b]

                mday = read ⊳ (   try (cons2 ⊳ oneOf "012" ⊵ digit)
                                ∤ try (cons2 ⊳ char '3' ⊵ oneOf "01")
                                ∤ (pure ⊳ digit)
                              )
                parseDMY ∷ (Monad η, CharParsing η) ⇒ (ℕ,Month,Year) → η Date
                parseDMY (d,m,y) = do
                  case fromGregorianValid (toInteger y) (fromEnum m) (fromIntegral d) of
                    Just t → return $ Date t
                    Nothing → unexpected $ [fmt|invalid date %02d.%T.%T|] d m y

                parseYMD ∷ (Monad η, CharParsing η) ⇒ (Year,Month,ℕ) → η Date
                parseYMD (y,m,d) = parseDMY (d,m,y)

                -- parser of three things, separated by a common separator.
                sep3 ∷ Applicative ψ ⇒ ψ ω → ψ α → ψ β → ψ γ → ψ (α,β,γ)
                sep3 w a b c = (,,) ⊳ a ⋪ w ⊵ b ⋪ w ⊵ c

             in try (sep3 (char '-') textual textual mday ≫ parseYMD <?> "Date")
                ∤ (sep3 (char '.') mday textual textual ≫ parseDMY) <?> "Date"

instance TextualPlus Date where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  let
    parseD = tParse @Date
   in
    testGroup "parse"
              [ testParseE "30.L.96"   parseD "expected: month"
              , testParseE "40.xii.96" parseD ""
              , testParseE "30.ii.96"  parseD "invalid date 30.ii.96"
              , testParse  "30.i.96"   (fromYMD 1996 1 30)
              , testParse  "30.i.1996" (fromYMD 1996 1 30)
              , testParse  "1996-01-30" (fromYMD 1996 1 30)
              , testParse  "30.1.96"   (fromYMD 1996 1 30)
              , testParseE  "30.001.96" parseD ""
              , testParse  "30.jan.96"   (fromYMD 1996 1 30)
              , testParse  "30.January.96"   (fromYMD 1996 1 30)
              , testParse  "30.january.96"   (fromYMD 1996 1 30)
              , testParse  "30.jAnUaRy.96"   (fromYMD 1996 1 30)
              , testParse  "30.i.2006" (fromYMD 2006 1 30)
              , testParse  "30.i.01"   (fromYMD 2001 1 30)
              , testParse  "30.i.90"   (fromYMD 2090 1 30)
              , testParse  "30.i.1896" (fromYMD 1896 1 30)
              , testParseE "31.iv.96"  parseD "invalid date 31.iv.96"
              , testParse  "1.i.1896"  (fromYMD 1896 1 1)
              , testParse  "31.xii.89" (fromYMD 2089 12 31)
              , testProperty "invertibleString" (propInvertibleString @Date)
              , testProperty "invertibleText" (propInvertibleText @Date)
              ]

----------------------------------------

{-| QuasiQuoter for `Date` -}
dte ∷ QuasiQuoter
dte = mkQQExp "Date" (liftTParse' @Date tParse')

------------------------------------------------------------

class HasDate α where
  date ∷ Lens' α Date

instance HasDate Date where
  date = id

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Date" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
