module Acct.OStmtIndex
  ( OStmtIndex, ostmtindex )
where

import Base1T

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( (<?>), optional )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseInt, elements )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE, LitE, SigE )
                                   , Lit( IntegerL )
                                   , Lift( liftTyped ), TExp( TExp )
                                   , Type( ConT )
                                   )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

--------------------------------------------------------------------------------

newtype OStmtIndex = OStmtIndex (𝕄 ℕ)  deriving  (Eq,Ord,Show)

--------------------

instance Default OStmtIndex where
  def = OStmtIndex 𝕹

--------------------

instance Lift OStmtIndex where
  liftTyped (OStmtIndex (𝕵 i)) = return ∘ TExp $
    AppE (ConE 'OStmtIndex)
         (AppE (ConE '𝕵) (SigE (LitE ∘ IntegerL $ toInteger i) (ConT ''ℕ)))
  liftTyped (OStmtIndex 𝕹) = return ∘ TExp $
    AppE (ConE 'OStmtIndex) (ConE '𝕹)

--------------------

instance Validity OStmtIndex where
  validate = trivialValidation

--------------------

instance GenValid OStmtIndex where
  genValid    = do
    i ← fromIntegral ⊳ chooseInt (0,1_000_000)
    i' ← elements [𝕹, 𝕵 i]
    return $ OStmtIndex i'

  shrinkValid (OStmtIndex 𝕹)                 = []
  shrinkValid (OStmtIndex (𝕵 n)) | n ≡ 0     = [OStmtIndex 𝕹]
  shrinkValid (OStmtIndex (𝕵 n)) | otherwise =
    (OStmtIndex 𝕹) : [OStmtIndex (𝕵 n') | n' ← [0..(n-1)]]


--------------------

instance Arbitrary OStmtIndex where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable OStmtIndex where
  print (OStmtIndex 𝕹)     = P.string ""
  print (OStmtIndex (𝕵 i)) = P.string (show i)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "71" (OStmtIndex (𝕵 71))
                      , test ""   (OStmtIndex 𝕹) ]

--------------------

-- a number i ⇒ (Just i) or nothing ⇒ Nothing
instance Textual OStmtIndex where
  textual =
    OStmtIndex ⊳ optional (read ⊳ some digit) <?> "Other Statement Number"

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse ""     (OStmtIndex 𝕹)
            , testParse "6"  (OStmtIndex $ 𝕵 6)
            , testParse "66" (OStmtIndex $ 𝕵 66)
            , testParseE "p"   (tParse' @OStmtIndex) ""
            , testParseE "p:"  (tParse' @OStmtIndex) ""
            , testParseE "-6"  (tParse' @OStmtIndex) ""
            , testParseE "p:6" (tParse' @OStmtIndex) ""
            , testParseE ":p"  (tParse' @OStmtIndex) ""
            , testParseE ":6"  (tParse' @OStmtIndex) ""
            , testProperty "invertibleString"
                           (propInvertibleString @OStmtIndex)
            , testProperty "invertibleText"
                           (propInvertibleText @OStmtIndex)
            ]

----------------------------------------

{-| QuasiQuoter for `OStmtIndex` -}
ostmtindex ∷ QuasiQuoter
ostmtindex = mkQQExp "OStmtIndex" (liftTParse' @OStmtIndex tParse')

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.OStmt" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
