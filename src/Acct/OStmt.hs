module Acct.OStmt
  ( HasOStmtY( oStmtY ), OStmt, oAcct, oIndex, ostmt, tests )
where

import Base1T

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, digit )
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
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.OStmtName  ( OStmtName, ostmtname )

--------------------------------------------------------------------------------

data OStmt = OStmt { _oAcct ∷ OStmtName
                   , _oIndex ∷ (𝕄 ℕ) }
  deriving (Eq,Lift,Show)

oAcct ∷ Lens' OStmt OStmtName
oAcct = lens _oAcct (\ os a → os { _oAcct = a })

oIndex ∷ Lens' OStmt (𝕄 ℕ)
oIndex = lens _oIndex (\ os i → os { _oIndex = i })

--------------------

instance Validity OStmt where
  validate = trivialValidation

--------------------

instance GenValid OStmt where
  genValid    = do
    c ← arbitrary
    n' ← fromIntegral ⊳ chooseInt (0,1_000_000)
    n ← elements [𝕹, 𝕵 n']
    return $ OStmt c n

  shrinkValid (OStmt _ 𝕹)                = []
  shrinkValid (OStmt c (𝕵 n)) | n ≡ 0     = [OStmt c 𝕹]
  shrinkValid (OStmt c (𝕵 n)) | otherwise =
    (OStmt c 𝕹) : [OStmt c (𝕵 n') | n' ← [0..(n-1)]]

--------------------

instance Arbitrary OStmt where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable OStmt where
  print (OStmt c 𝕹) = print c
  print (OStmt c (𝕵 n)) = P.text $ [fmt|%T:%d|] c n

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "P:71" (OStmt [ostmtname|P|] (𝕵 71))
                      , test "P" (OStmt [ostmtname|P|] 𝕹) ]

--------------------

instance Textual OStmt where
  textual = (OStmt ⊳ textual ⊵ optional (char ':' ⋫ (read ⊳ some digit))) <?> "Other Statement Number"

----------

parseTests ∷ TestTree
parseTests =
  let
    eosn = "expected: Other Statement Number"
  in
    testGroup "parse"
              [ testParse "P"    (OStmt [ostmtname|P|] 𝕹)
              , testParse "P:6"  (OStmt [ostmtname|P|] (𝕵 6))
              , testParse "P:66" (OStmt [ostmtname|P|] (𝕵 66))
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
