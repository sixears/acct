module Acct.StmtIndex
  ( GetStmtIndex( stmtIndexGet ), NotStmtIndex(..), StmtIndex
  , stmtIndex, stmtindex, tests )
where

import Base1T

-- data-textual ------------------------

import Data.Ord      ( Ord( compare ), Ordering( EQ, GT, LT ) )
import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Combinators  ( (<?>), optional )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( elements )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Stmt  ( HasStmtY( stmtY ), Stmt, stmt )

--------------------------------------------------------------------------------

newtype StmtIndex = StmtIndex { unStmtIndex ∷ 𝕄 Stmt }
  deriving (Eq,Lift,NFData,Show)

stmtIndex ∷ 𝕄 Stmt → StmtIndex
stmtIndex = StmtIndex

--------------------

instance Default StmtIndex where
  def = StmtIndex 𝕹

--------------------

instance Ord StmtIndex where
  -- invert sense of 𝕹 to be "greater" than all others, being as'twere, the
  -- next pending statement
  compare (StmtIndex 𝕹)    (StmtIndex 𝕹)       = EQ
  compare (StmtIndex (𝕵 i)) (StmtIndex (𝕵 i')) = i `compare` i'
  compare (StmtIndex (𝕵 _)) (StmtIndex 𝕹)      = LT
  compare (StmtIndex 𝕹)    (StmtIndex (𝕵 _))   = GT
--------------------

instance Validity StmtIndex where
  validate = trivialValidation

--------------------

instance GenValid StmtIndex where
  genValid    = do
    i ← arbitrary
    i' ← elements [𝕹, 𝕵 i]
    return $ StmtIndex i'

  shrinkValid (StmtIndex 𝕹)     = []
  shrinkValid (StmtIndex (𝕵 _)) = [StmtIndex 𝕹]

--------------------

instance Arbitrary StmtIndex where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable StmtIndex where
  print (StmtIndex 𝕹)     = P.text ""
  print (StmtIndex (𝕵 i)) = P.text (toText i)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "71" (StmtIndex (𝕵 [stmt|71|]))
                      , test ""   (StmtIndex 𝕹) ]

--------------------

-- a number i ⇒ (Just i) or nothing ⇒ Nothing
instance Textual StmtIndex where
  textual =
    StmtIndex ⊳ optional textual <?> "Statement Number"

instance TextualPlus StmtIndex where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse ""     (StmtIndex 𝕹)
            , testParse "6"    (StmtIndex $ 𝕵 [stmt|6|])
            , testParse "66"   (StmtIndex $ 𝕵 [stmt|66|])
            , testParseE "p"   (tParse' @StmtIndex) ""
            , testParseE "p:"  (tParse' @StmtIndex) ""
            , testParseE "-6"  (tParse' @StmtIndex) ""
            , testParseE "p:6" (tParse' @StmtIndex) ""
            , testParseE ":p"  (tParse' @StmtIndex) ""
            , testParseE ":6"  (tParse' @StmtIndex) ""
            , testProperty "invertibleString"
                           (propInvertibleString @StmtIndex)
            , testProperty "invertibleText"
                           (propInvertibleText @StmtIndex)
            ]

----------------------------------------

{-| QuasiQuoter for `StmtIndex` -}
stmtindex ∷ QuasiQuoter
stmtindex = mkQQExp "StmtIndex" (liftTParse' @StmtIndex tParse')

------------------------------------------------------------

class GetStmtIndex α where
  stmtIndexGet ∷ α → StmtIndex

instance GetStmtIndex StmtIndex where
  stmtIndexGet = id

------------------------------------------------------------

instance HasStmtY StmtIndex where
  stmtY = lens unStmtIndex (\ _ ms → StmtIndex ms)

------------------------------------------------------------

class NotStmtIndex α where
  {-| Does this thing /not/ correspond to a given stmt index?  Notably, any
      thing that doesn't apply to a stmt index - e.g., a comment - will return
      𝕱. -}
  notStmtIndex ∷ StmtIndex → α → 𝔹

-- tests -----------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.StmtIndex" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
