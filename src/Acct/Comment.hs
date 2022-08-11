module Acct.Comment
  ( Comment( Comment ), c, cmt, comment, tests )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenUnchecked( genUnchecked, shrinkUnchecked )
                         , GenValid( genValid, shrinkValid )
                         , genUnchecked, isValid
                         )

-- parsers -----------------------------

import Text.Parser.Char  ( noneOf )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseAny, listOf, suchThat )

-- tasty-plus --------------------------

import TastyPlus    ( (≟), propInvertibleString, propInvertibleText )
import TastyPluser  ( shrinkText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( find, pack, unpack )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

--------------------------------------------------------------------------------

newtype Comment = Comment 𝕋
  deriving (Eq,Generic,Lift,Printable,Show)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "comment!" (Comment "comment!") ]

--------------------

instance Validity Comment where
  validate (Comment t) = let noLT = declare "no '<' characters" $
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
                          in ю [ noLT, noGT, noNL, noRN, noTab, notEmpty ]

--------------------

instance GenUnchecked Comment where
  genUnchecked = Comment ∘ pack ⊳ listOf chooseAny
  shrinkUnchecked (Comment t) = Comment ⊳ shrinkText t

--------------------

instance GenValid Comment where
  genValid    = genUnchecked `suchThat` isValid
  shrinkValid = pure

--------------------

instance Arbitrary Comment where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Textual Comment where
  textual = Comment ∘ pack ⊳ some (noneOf "<>\n\r\t")

----------

parseTests ∷ TestTree
parseTests =
  testGroup "tParse"
            [ testParse "comm" (Comment "comm")
            , testParseE "<com" (tParse @Comment) "error"
            , testProperty "invertibleString" (propInvertibleString @Comment)
            , testProperty "invertibleText" (propInvertibleText @Comment)
            ]

----------------------------------------

{-| QuasiQuoter for `Comment` -}
comment ∷ QuasiQuoter
comment = mkQQExp "Comment" (liftTParse' @Comment tParse')

{-| Brief alias for `comment` -}
cmt ∷ QuasiQuoter
cmt = comment

{-| Very brief alias for `comment` -}
c ∷ QuasiQuoter
c = comment

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Comment" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests


-- that's all, folks! ----------------------------------------------------------
