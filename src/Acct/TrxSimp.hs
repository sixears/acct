module Acct.TrxSimp
  ( TrxSimp, tsimp, tsimp_, tests )
where

import Base1T

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char )
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

import Acct.Account  ( Account, HasAccount( account ), acct )
import Acct.Amount   ( Amount, HasAmount( amount ) )
import Acct.Comment  ( Comment, cmt )
import Acct.Date     ( Date, date )
import Acct.Parser   ( wspaces )
import Acct.OStmt    ( HasOStmtY( oStmtY ), OStmt, ostmt )
import Acct.Stmt     ( Stmt, stmt )

--------------------------------------------------------------------------------

{- | A simple standard single-line transaction; or a single entry in a
     broken-down transaction. -}
data TrxSimp = TrxSimp { _amount  ∷ Amount
                       , _date    ∷ Date
                       , _account ∷ Account
                       , _stmt    ∷ 𝕄 Stmt
                       , _ostmt   ∷ 𝕄 OStmt
                       , _comment ∷ 𝕄 Comment
                       }
  deriving (Eq,Lift,Show)

{-| Super-simple c'tor fn, for use in tests only -}
tsimp_ ∷ Amount → Date → Account → 𝕄 Stmt → 𝕄 OStmt → 𝕄 Comment → TrxSimp
tsimp_ am dt ac st os cm = TrxSimp am dt ac st os cm

--------------------

instance Validity TrxSimp where
  validate = trivialValidation

--------------------

instance GenValid TrxSimp where
  genValid    =
    TrxSimp ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary
                        ⊵ arbitrary ⊵ arbitrary
  shrinkValid = pure

--------------------

instance Arbitrary TrxSimp where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable TrxSimp where
  print (TrxSimp am dt ac st os cm) =
    let st' = maybe "" [fmt|X<%T>|] st
        os' = maybe "" [fmt|O<%T>|] os
        cm' = maybe "" [fmt|C<%T>|] cm
     in P.text $ [fmt|%T\t#D<%T>A<%T>%t%t%t|] am dt ac st' os' cm'

----------

printTests ∷ TestTree
printTests =
  let
    test exp tsp = testCase (unpack exp) $ exp ≟ toText tsp
  in
    testGroup "print"
              [ test "10.00+\t#D<4.vi.96>A<Foo>X<5>"
                     (tsimp_ 1000 [date|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|]) 𝕹
                                  𝕹)
              , test "0.01-\t#D<12.xii.01>A<Bar>C<comment>"
                     (tsimp_ (-1) [date|2001-12-12|] [acct|Bar|] 𝕹
                             𝕹 (𝕵 [cmt|comment|]))
              , test "0.10-\t#D<22.iix.22>A<Baz>X<1>C<comment>"
                     (tsimp_ (-10) [date|2022-8-22|] [acct|Baz|] (𝕵 [stmt|1|])
                             𝕹 (𝕵 [cmt|comment|]))
              , test "0.10-\t#D<22.iix.22>A<Baz>O<X>C<comment>"
                     (tsimp_ (-10) [date|2022-8-22|] [acct|Baz|] 𝕹
                             (𝕵 [ostmt|X|]) (𝕵 [cmt|comment|]))
              , test "0.10-\t#D<22.iix.22>A<Baz>X<1>O<X:6>C<comment>"
                     (tsimp_ (-10) [date|2022-8-22|] [acct|Baz|] (𝕵 [stmt|1|])
                             (𝕵 [ostmt|X:6|]) (𝕵 [cmt|comment|]))
              ]

--------------------

instance Textual TrxSimp where
  textual =
    let mark c = char c ⋫ char '<' ⋫ textual ⋪ char '>' ⋪ wspaces
        optm c = (𝕹, 𝕵 ⊳ mark c)
        parts ∷ (Monad η, CharParsing η) ⇒
                η (Date, Account, 𝕄 Stmt, 𝕄 OStmt, 𝕄 Comment)
        parts =
          permute $ (,,,,) <$$> mark 'D' <||> mark 'A'
                          <|?> optm 'X' <|?> optm 'O' <|?> optm 'C'
        construct am (dt,ac,st,os,cm) = TrxSimp am dt ac st os cm
    in construct ⊳ (textual ⋪ wspaces ⋪ char '#') ⊵ parts

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse "10.13+ #D<6.viii.96>A<Bl>X<5>"
                        (tsimp_ 1013 [date|1996-8-6|] [acct|Bl|] (𝕵 [stmt|5|])
                                     𝕹 𝕹)
            , testParse "0.28+  #D<8.VIII.96>A<Car>C<int.>X<5>"
                        (tsimp_ 28 [date|1996-8-8|] [acct|Car|] (𝕵 [stmt|5|])
                                   𝕹 (𝕵 [cmt|int.|]))
            , testParse "15294.97-\t#D<10.xi.40>A<YHyww>X<604244>"
                        (tsimp_ (-1529497) [date|2040-11-10|] [acct|YHyww|]
                                           (𝕵 [stmt|604244|]) 𝕹 𝕹)
            , -- X is not a date
              testParseE "6.28+  #X<8.VIII.96>A<Car>C<int.>X<5>"
                         (tParse' @TrxSimp) "error"
            , -- J is invalid
              testParseE "7.28+  #J<77>D<8.VIII.96>A<CarFund>C<int.>X<5>"
                         (tParse' @TrxSimp) "error"

            , -- O is invalid (lower-case)
              testParseE "7.28+  #D<77>D<8.VIII.96>A<CarFund>O<x>X<5>"
                         (tParse' @TrxSimp) "error"

            , -- O is invalid (missing ':')
              testParseE "7.28+  #D<77>D<8.VIII.96>A<CarFund>O<X6>X<5>"
                         (tParse' @TrxSimp) "error"

            , -- O is invalid (missing stmt number)
              testParseE "7.28+  #D<77>D<8.VIII.96>A<CarFund>O<X:>X<5>"
                         (tParse' @TrxSimp) "error"

            , -- O is invalid (bad stmt number)
              testParseE "7.28+  #D<77>D<8.VIII.96>A<CarFund>O<X:X>X<5>"
                         (tParse' @TrxSimp) "error"

            , -- missing date
              testParseE "8.28+  #A<CarFund>C<int to 8 Aug>X<5>"
                         (tParse' @TrxSimp) "expected: \"D\""

            , -- repeated date
              testParseE "9.28+  #D<8.VIII.96>A<CarFund>C<int.>X<5>D<1.i.97>"
                         (tParse' @TrxSimp) ""
            , testProperty "invertibleString" (propInvertibleString @TrxSimp)
            , testProperty "invertibleText" (propInvertibleText @TrxSimp)
            ]

--------------------

instance HasAmount TrxSimp where
  amount = lens _amount (\ ts am → ts { _amount = am })

--------------------

instance HasAccount TrxSimp where
  account = lens _account (\ ts ac → ts { _account = ac })

--------------------

instance HasOStmtY TrxSimp where
  oStmtY = lens _ostmt (\ ts os → ts { _ostmt = os })

----------------------------------------

{-| QuasiQuoter for `TrxSimp` -}
tsimp ∷ QuasiQuoter
tsimp = mkQQExp "TrxSimp" (liftTParse' @TrxSimp tParse')

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TrxSimp" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
