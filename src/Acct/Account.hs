module Acct.Account
  ( Account, HasAccount( account ), acct, tests )
where

import Base1T  hiding  ( index )

-- base --------------------------------

import Data.Char  ( isAlphaNum, isUpper )
import Data.List  ( filter )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ), isValid )

-- parsers -----------------------------

import Text.Parser.Char         ( alphaNum, upper )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, choose, listOf1, oneof )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import qualified  Data.Text
import Data.Text  ( find, index, length, pack, unpack )

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

--------------------------------------------------------------------------------

newtype Account = Account 𝕋 deriving (Eq,Lift,NFData,Ord,Printable,Show)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "Foo" (Account "Foo") ]

--------------------

instance Validity Account where
  validate (Account t) = let allAlphaNum = declare "all alphanumeric" $
                               Nothing ≡ find (not ∘ isAlphaNum) t
                             nonEmpty = declare "length ≥ 2" $ 2 ≤ length t
                             startsWithCap = declare "starts with a capital" $
                               isUpper (index t 0)
                          in ю [ nonEmpty, startsWithCap, allAlphaNum ]

--------------------

instance GenValid Account where
  genValid = let anum ∷ Gen ℂ
                 anum = oneof [choose('A','Z'),choose('a','z'),choose('0','9')]
             in Account ∘ pack ⊳ ((:) ⊳ choose ('A','Z')
                                  ⊵ listOf1 anum)
  shrinkValid (Account t) = filter (\ x → x ≢ (Account t) ∧ isValid x) $
                                     Account ⊳ Data.Text.inits t

--------------------

instance Arbitrary Account where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Textual Account where
  textual = Account ∘ pack ⊳ ((:) ⊳ upper ⊵ some alphaNum)

instance TextualPlus Account where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "tParse"
            [ testParse "Cars" (Account "Cars")
            , testParseE "cars" (tParse @Account) "expected: uppercase letter"
            , testProperty "invertibleString" (propInvertibleString @Account)
            , testProperty "invertibleText" (propInvertibleText @Account)
            ]

----------------------------------------

{-| QuasiQuoter for `Account` -}
acct ∷ QuasiQuoter
acct = mkQQExp "Account" (liftTParse' @Account tParse')

------------------------------------------------------------

class HasAccount α where
  account ∷ Lens' α Account

instance HasAccount Account where
  account = id

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Account" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
