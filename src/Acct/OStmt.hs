{-# LANGUAGE DeriveAnyClass #-}

module Acct.OStmt
  ( HasOStmtY( oStmtY ), OStmt, oAcct, oIndex, ostmt, tests )
where

import Base1T

-- base --------------------------------

import Data.List     ( filter )
import GHC.Generics  ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( (<?>), option )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )

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

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.OStmtIndex  ( OStmtIndex, ostmtindex )
import Acct.OStmtName   ( OStmtName, ostmtname )

--------------------------------------------------------------------------------

data OStmt = OStmt { _oAcct ∷ OStmtName, _oIndex ∷ OStmtIndex }
  deriving (Eq,Generic,Lift,NFData,Show)

oAcct ∷ Lens' OStmt OStmtName
oAcct = lens _oAcct (\ os a → os { _oAcct = a })

oIndex ∷ Lens' OStmt OStmtIndex
oIndex = lens _oIndex (\ os i → os { _oIndex = i })

--------------------

instance Validity OStmt where
  validate = trivialValidation

--------------------

instance GenValid OStmt where
  genValid = OStmt ⊳ arbitrary ⊵ arbitrary

  shrinkValid (OStmt n c) =
    filter (≢ OStmt n c) $
    [ OStmt n' c' | n' ← n : shrinkValid n, c' ← c : shrinkValid c ]

--------------------

instance Arbitrary OStmt where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable OStmt where
  print (OStmt c n) | n ≡ def   = print c
                    | otherwise = P.text $ [fmt|%T:%T|] c n

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "P:71" (OStmt [ostmtname|P|] [ostmtindex|71|])
                      , test "P" (OStmt [ostmtname|P|] [ostmtindex||]) ]

--------------------

instance Textual OStmt where
  textual = (OStmt ⊳ textual ⊵ option def (char ':' ⋫ textual))
          <?> "Other Statement Number"

instance TextualPlus OStmt where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  let
    eosn = "expected: Other Statement Number"
  in
    testGroup "parse"
              [ testParse "P"    (OStmt [ostmtname|P|] [ostmtindex||])
              , testParse "P:6"  (OStmt [ostmtname|P|] [ostmtindex|6|])
              , testParse "P:66" (OStmt [ostmtname|P|] [ostmtindex|66|])
              , testParseE "p"   (tParse @OStmt) eosn
              , testParseE "p:"  (tParse @OStmt) eosn
              , testParseE "p:6" (tParse @OStmt) eosn
              , testParseE ":p"  (tParse @OStmt) eosn
              , testParseE ":6"  (tParse @OStmt) eosn
              , testProperty "invertibleString" (propInvertibleString @OStmt)
              , testProperty "invertibleText" (propInvertibleText @OStmt)
              ]

----------------------------------------

{-| QuasiQuoter for `OStmt` -}
ostmt ∷ QuasiQuoter
ostmt = mkQQExp "OStmt" (liftTParse' @OStmt tParse')

------------------------------------------------------------

class HasOStmtY α where
  oStmtY ∷ Lens' α (𝕄 OStmt)

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
