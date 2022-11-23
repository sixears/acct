{-| A comment loose in a trx file. -}

module Acct.SComment
  ( SComment( SComment ), scmt, scomment, tests )
where

import Base1T

-- base --------------------------------

import Data.Char      ( isAscii, isPrint, isSpace )
import Data.Function  ( flip )
import Data.List      ( filter )
import GHC.Enum       ( enumFromTo, toEnum )
import GHC.Generics   ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ), isValid )

-- lens --------------------------------

import Control.Lens.Each  ( each )
import Control.Lens.Fold  ( (^..) )

-- parsers -----------------------------

import Text.Parser.Char         ( satisfy, string )
import Text.Parser.Combinators  ( option )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( elements, listOf,oneof, suchThat )

-- tasty-plus --------------------------

import TastyPlus    ( (≟), propInvertibleString, propInvertibleText,shrinkText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( dropWhile, find, pack, take, unpack )

-- textual-plus -------------------

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Parser  ( wspaces )

--------------------------------------------------------------------------------

newtype SComment = SComment 𝕋  deriving  (Eq,Generic,Lift,NFData,Printable,Show)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print" [ test "tcomment!" (SComment "tcomment!") ]

--------------------

instance Validity SComment where
  validate (SComment t) = let stripL = dropWhile isSpace
                              ws_or_hyphen = declare "whitespace or hyphen" $
                                "" ≡ stripL t ∨ "%" ≡ take 1 t
                              noNL = declare "no newline characters" $
                                Nothing ≡ find (≡ '\n') t
                              noRN = declare "no return characters" $
                                Nothing ≡ find (≡ '\r') t
                          in  ю [ ws_or_hyphen, noNL, noRN ]

--------------------

instance GenValid SComment where
  genValid    = flip suchThat isValid $
                  let
                    ws = listOf (elements " \t\f\v")
                    enumOver from to = enumFromTo (toEnum from) (toEnum to)
                    prints' = listOf ∘ elements $ filter isPrint $ enumOver 0 127
                    cmtpfx a b = "%" ⊕ a ⊕ b
                  in
                    SComment ∘ pack ⊳ ((⊕) ⊳ ws ⊵ oneof [pure "", cmtpfx ⊳ ws ⊵  prints'])
  shrinkValid (SComment t) = filter isValid (SComment ⊳ shrinkText t)

--------------------

instance Arbitrary SComment where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Textual SComment where
  textual =
    let
      printable = satisfy $ \ c → isPrint c ∧ isAscii c
      cmtsfx    = (,,) ⊳ string "%" ⊵ wspaces ⊵ many printable
      tcomment  = SComment ∘ pack
    in
      fmap tcomment $ (⊕) ⊳ wspaces ⊵ option "" (ю ∘ (^.. each) ⊳ cmtsfx)

instance TextualPlus SComment where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  let tParseE s = testParseE s (tParse' @SComment)
  in  testGroup "tParse"
                [ testParse "% comm" (SComment "% comm")
                , testParse "" (SComment "")
                , testParse "  \t" (SComment "  \t")
                , testParse "%" (SComment "%")
                , testParse "\t%" (SComment "\t%")
                , testParse "  %  com " (SComment "  %  com ")
                , testParseE "- com" (tParse' @SComment) "error"
                , tParseE "com %" "error"
                , tParseE "%\n" "error"
                , tParseE "%\r" "error"
                , tParseE "\r" "error"
                , testProperty "invertibleString"
                               (propInvertibleString @SComment)
                , testProperty "invertibleText"
                               (propInvertibleText @SComment)
                ]

----------------------------------------

{-| QuasiQuoter for `SComment` -}
scomment ∷ QuasiQuoter
scomment = mkQQExp "SComment" (liftTParse' @SComment tParse')

{-| Brief alias for `tcomment` -}
scmt ∷ QuasiQuoter
scmt = scomment

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.SComment" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests


-- that's all, folks! ----------------------------------------------------------
