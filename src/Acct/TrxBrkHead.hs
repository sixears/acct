{-# LANGUAGE DeriveAnyClass #-}

module Acct.TrxBrkHead
  ( TrxBrkHead, tbh, tbh_, trxBrkHead, tests )
where

import Base1T

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )
import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char, string )
import Text.Parser.Permutation  ( (<||>), (<|?>), (<$$>), permute )

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

import TextualPlus  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount      ( Amount, HasAmount( amount ) )
import Acct.Date        ( Date, HasDate( date ), dte )
import Acct.Parser      ( wspaces )
import Acct.OStmt       ( HasOStmtY( oStmtY ), OStmt, ostmt )
import Acct.Stmt        ( HasStmtY( stmtY ), Stmt, stmt )
import Acct.StmtIndex   ( GetStmtIndex( stmtIndexGet ), stmtIndex )
import Acct.TComment    ( TComment, tcmt )
import Acct.Util        ( Pretty( pretty ) )

--------------------------------------------------------------------------------

{- | A transaction breakdown header. -}
data TrxBrkHead = TrxBrkHead { _amount  ∷ Amount
                             , _date    ∷ Date
                             , _stmt    ∷ 𝕄 Stmt
                             , _ostmt   ∷ 𝕄 OStmt
                             , _comment ∷ 𝕄 TComment
                             }
  deriving (Eq,Generic,Lift,NFData,Show)

{-| Super-simple c'tor fn, for use in tests only -}
tbh_ ∷ Amount → Date → 𝕄 Stmt → 𝕄 OStmt → 𝕄 TComment → TrxBrkHead
tbh_ am dt st os cm = TrxBrkHead am dt st os cm

--------------------

instance Arbitrary TrxBrkHead where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Validity TrxBrkHead where
  validate = trivialValidation

--------------------

instance GenValid TrxBrkHead where
  genValid    =
    TrxBrkHead ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary
  shrinkValid = pure

--------------------

instance Printable TrxBrkHead where
  print (TrxBrkHead am dt st os cm) =
    let st' = maybe "" [fmt|X<%T>|] st
        cm' = maybe "" [fmt|C<%T>|] cm
        os' = maybe "" [fmt|O<%T>|] os
     in P.text $ [fmt|%T\t#D<%T>B<>%t%t%t|] am dt st' os' cm'

----------

printTests ∷ TestTree
printTests =
  let
    test exp tsp = testCase (unpack exp) $ exp ≟ toText tsp
  in
    testGroup "print"
              [ test "10.00+\t#D<4.vi.96>B<>X<5>"
                     (tbh_ 1000 [dte|1996-6-4|] (𝕵 [stmt|5|]) 𝕹 𝕹)
              , test "0.01-\t#D<12.xii.01>B<>C<comment>"
                     (tbh_ (-1) [dte|2001-12-12|] 𝕹 𝕹 (𝕵 [tcmt|comment|]))
              , test "0.10-\t#D<22.iix.22>B<>X<1>O<P:2>C<comment>"
                     (tbh_ (-10) [dte|2022-8-22|] (𝕵 [stmt|1|])
                                 (𝕵 [ostmt|P:2|]) (𝕵 [tcmt|comment|]))
              ]

--------------------

instance Pretty TrxBrkHead where
  pretty (TrxBrkHead am dt st os cm) =
    let st' = maybe "" [fmt|X<%T>|] st
        os' = maybe "" [fmt|O<%T>|] os
        cm' = maybe "" [fmt|C<%T>|] cm
     in [fmt|%-10t  #D<%t>%t%t%t|] (pretty am) (pretty dt) st' os' cm'

--------------------

instance Textual TrxBrkHead where
  textual =
    let -- a "marked" textual value, i.e., surrounded with c<...>
        mark  c = char c ⋫ char '<' ⋫ textual ⋪ char '>' ⋪ wspaces
        -- a "mark" with no value, i.e., with c<>
        mark' c = char c ⋫ char '<' ⋫ string "" ⋪ char '>' ⋪ wspaces
        optm  c = (𝕹, 𝕵 ⊳ mark c)
        parts ∷ (Monad η, CharParsing η) ⇒
                η (Date, 𝕊, 𝕄 Stmt, 𝕄 OStmt, 𝕄 TComment)
        parts =
          permute $ (,,,,) <$$> mark 'D' <||> mark' 'B'
                           <|?> optm 'X' <|?> optm 'O' <|?> optm 'C'
        construct am (dt,_,st,os,cm) = TrxBrkHead am dt st os cm
    in construct ⊳ (textual ⋪ wspaces ⋪ char '#') ⊵ parts

instance TextualPlus TrxBrkHead where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse "10.13+ #D<6.viii.96>B<>X<5>"
                        (tbh_ 1013 [dte|1996-8-6|] (𝕵 [stmt|5|]) 𝕹 𝕹)
            , testParse "0.28+  #D<8.VIII.96>B<>C<int>X<5>O<P:2>" $
                        tbh_ 28 [dte|1996-8-8|]
                                (𝕵 [stmt|5|]) (𝕵 [ostmt|P:2|]) (𝕵 [tcmt|int|])
            , -- X is not a date
              testParseE "6.28+  #X<8.VIII.96>B<>C<int>X<5>"
                         (tParse' @TrxBrkHead) "error"
            , -- O is invalid
              testParseE "6.28+  #X<8.VIII.96>B<>C<int>O<5>"
                         (tParse' @TrxBrkHead) "error"
            , -- J is invalid
              testParseE "7.28+  #J<77>D<8.VIII.96>B<>C<int>X<5>"
                         (tParse' @TrxBrkHead) "error"

            , -- missing date
              testParseE "8.28+  #B<>C<int to 8 Aug>X<5>"
                         (tParse' @TrxBrkHead)"expected: \"D\""
            , -- repeated date
              testParseE "9.28+  #D<8.VIII.96>B<>C<int>X<5>D<1.i.97>"
                         (tParse' @TrxBrkHead) ""
            , testProperty "invertibleString" (propInvertibleString @TrxBrkHead)
            , testProperty "invertibleText" (propInvertibleText @TrxBrkHead)
            ]

--------------------

instance HasAmount TrxBrkHead where
  amount = lens _amount (\ hd am → hd { _amount = am })

--------------------

instance GetStmtIndex TrxBrkHead where
  stmtIndexGet t = stmtIndex $ t ⊣ stmtY

--------------------

instance HasStmtY TrxBrkHead where
  stmtY = lens _stmt (\ ts st → ts { _stmt = st })

--------------------

instance HasOStmtY TrxBrkHead where
  oStmtY = lens _ostmt (\ th os → th { _ostmt = os })

--------------------

instance HasDate TrxBrkHead where
  date = lens _date (\ th dt → th { _date = dt })

----------------------------------------

{-| QuasiQuoter for `TrxBrkHead` -}
trxBrkHead ∷ QuasiQuoter
trxBrkHead = mkQQExp "TrxBrkHead" (liftTParse' @TrxBrkHead tParse')

{-| Very brief alias for `trxBrkHead` -}
tbh ∷ QuasiQuoter
tbh = trxBrkHead

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TrxBrkHead" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
