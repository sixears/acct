{-| A comment attached to a transaction. -}

module Acct.TComment
  ( TComment( TComment ), tcmt, tcomment, tests )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ), isValid )

-- parsers -----------------------------

import Text.Parser.Char  ( noneOf )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseAny, listOf, suchThat )

-- tasty-plus --------------------------

import TastyPlus    ( (≟), propInvertibleString, propInvertibleText,shrinkText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( find, pack, unpack )

-- textual-plus -------------------

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

--------------------------------------------------------------------------------

newtype TComment = TComment 𝕋  deriving  (Eq,Generic,Lift,NFData,Printable,Show)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "tcomment!" (TComment "tcomment!") ]

--------------------

instance Validity TComment where
  validate (TComment t) = let noLT = declare "no '<' characters" $
                                Nothing ≡ find (≡ '<') t
                              noGT = declare "no '>' characters" $
                                Nothing ≡ find (≡ '>') t
                              noNL = declare "no newline characters" $
                                Nothing ≡ find (≡ '\n') t
                              noRN = declare "no return characters" $
                                Nothing ≡ find (≡ '\r') t
                              noTab = declare "no tab characters" $
                                Nothing ≡ find (≡ '\t') t
                              notEmpty = declare "not empty" $ "" ≢ t
                          in  ю [ noLT, noGT, noNL, noRN, noTab, notEmpty ]

--------------------

instance GenValid TComment where
  genValid    = (TComment ∘ pack ⊳ listOf chooseAny) `suchThat` isValid
  shrinkValid = pure

--------------------

instance Arbitrary TComment where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Textual TComment where
  textual = TComment ∘ pack ⊳ some (noneOf "<>\n\r\t")

instance TextualPlus TComment where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "tParse"
            [ testParse "comm" (TComment "comm")
            , testParseE "<com" (tParse @TComment) "error"
            , testProperty "invertibleString" (propInvertibleString @TComment)
            , testProperty "invertibleText" (propInvertibleText @TComment)
            ]

----------------------------------------

{-| QuasiQuoter for `TComment` -}
tcomment ∷ QuasiQuoter
tcomment = mkQQExp "TComment" (liftTParse' @TComment tParse')

{-| Brief alias for `tcomment` -}
tcmt ∷ QuasiQuoter
tcmt = tcomment


--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TComment" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests


-- that's all, folks! ----------------------------------------------------------
