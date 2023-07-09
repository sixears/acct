module Acct.Stmt
  ( HasStmt( stmtx ), HasStmtY( stmtY ), Stmt( Stmt ), stmt, tests )
where

import Base1T

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( (<?>) )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseInt )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( unpack )

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

--------------------------------------------------------------------------------

newtype Stmt    = Stmt ℕ  deriving (Eq,Lift,NFData,Ord,Show)

--------------------

instance Validity Stmt where
  validate = trivialValidation

--------------------

instance GenValid Stmt where
  genValid      = Stmt ∘ fromIntegral ⊳ chooseInt (0,1_000_000)
  shrinkValid _ = []

--------------------

instance Arbitrary Stmt where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Stmt where
  print (Stmt n) = P.string (show n)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "71" (Stmt 71) ]

--------------------

instance Textual Stmt where
  textual = (Stmt ∘ read ⊳ some digit) <?> "Statement Number"

instance TextualPlus Stmt where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse "6" (Stmt 6)
            , testParse "66" (Stmt 66)
            , testParseE "six" (tParse @Stmt) "expected: Statement Number"
            , testProperty "invertibleString" (propInvertibleString @Stmt)
            , testProperty "invertibleText" (propInvertibleText @Stmt)
            ]

----------------------------------------

{-| QuasiQuoter for `Stmt` -}
stmt ∷ QuasiQuoter
stmt = mkQQExp "Stmt" (liftTParse' @Stmt tParse')

------------------------------------------------------------

class HasStmt α where
  stmtx ∷ Lens' α Stmt

instance HasStmt Stmt where
  stmtx = id

------------------------------------------------------------

class HasStmtY α where
  stmtY ∷ Lens' α (𝕄 Stmt)

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Stmt" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
