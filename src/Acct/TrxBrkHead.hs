module Acct.TrxBrkHead
  ( TrxBrkHead, tbh, tbh_, trxBrkHead, tests )
where

import Base1T

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

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

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount      ( Amount, HasAmount( amount ) )
import Acct.Comment     ( Comment, cmt )
import Acct.Date        ( Date, date )
import Acct.Parser      ( wspaces )
import Acct.Stmt        ( Stmt, stmt )

--------------------------------------------------------------------------------

{- | A transaction breakdown header. -}
data TrxBrkHead = TrxBrkHead { _amount  ∷ Amount
                             , _date    ∷ Date
                             , _stmt    ∷ 𝕄 Stmt
                             , _comment ∷ 𝕄 Comment
                             }
  deriving (Eq,Lift,Show)

{-| Super-simple c'tor fn, for use in tests only -}
tbh_ ∷ Amount → Date → 𝕄 Stmt → 𝕄 Comment → TrxBrkHead
tbh_ am dt st cm = TrxBrkHead am dt st cm

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
    TrxBrkHead ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary
  shrinkValid = pure

--------------------

instance Printable TrxBrkHead where
  print (TrxBrkHead am dt st cm) =
    let st' = maybe "" [fmt|X<%T>|] st
        cm' = maybe "" [fmt|C<%T>|] cm
     in P.text $ [fmt|%T\t#D<%T>B<>%t%t|] am dt st' cm'

----------

printTests ∷ TestTree
printTests =
  let
    test exp tsp = testCase (unpack exp) $ exp ≟ toText tsp
  in
    testGroup "print"
              [ test "10.00+\t#D<4.vi.96>B<>X<5>"
                     (tbh_ 1000 [date|1996-6-4|] (𝕵 [stmt|5|]) 𝕹)
              , test "0.01-\t#D<12.xii.01>B<>C<comment>"
                     (tbh_ (-1) [date|2001-12-12|] 𝕹 (𝕵 [cmt|comment|]))
              , test "0.10-\t#D<22.iix.22>B<>X<1>C<comment>"
                     (tbh_ (-10) [date|2022-8-22|] (𝕵 [stmt|1|])
                                 (𝕵 [cmt|comment|]))
              ]

--------------------

instance Textual TrxBrkHead where
  textual =
    let -- a "marked" textual value, i.e., surrounded with c<...>
        mark  c = char c ⋫ char '<' ⋫ textual ⋪ char '>' ⋪ wspaces
        -- a "mark" with no value, i.e., with c<>
        mark' c = char c ⋫ char '<' ⋫ string "" ⋪ char '>' ⋪ wspaces
        optm  c = (𝕹, 𝕵 ⊳ mark c)
        parts ∷ (Monad η, CharParsing η) ⇒ η (Date, 𝕊, 𝕄 Stmt, 𝕄 Comment)
        parts =
          permute $ (,,,) <$$> mark 'D' <||> mark' 'B' <|?>optm 'X' <|?>optm 'C'
        construct am (dt,_,st,cm) = TrxBrkHead am dt st cm
    in construct ⊳ (textual ⋪ wspaces ⋪ char '#') ⊵ parts

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse "10.13+ #D<6.viii.96>B<>X<5>"
                        (tbh_ 1013 [date|1996-8-6|] (𝕵 [stmt|5|]) 𝕹)
            , testParse "0.28+  #D<8.VIII.96>B<>C<int>X<5>"
                        (tbh_ 28 [date|1996-8-8|] (𝕵 [stmt|5|]) (𝕵 [cmt|int|]))
            , -- X is not a date
              testParseE "6.28+  #X<8.VIII.96>B<>C<int>X<5>"
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