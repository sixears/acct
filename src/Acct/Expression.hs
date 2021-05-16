{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Acct.Expression
  ( Expression, Transactions( unTransactions ), emptyTrans
  , tests )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative    ( pure )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Data.Functor.Identity  ( Identity )
import Data.List.NonEmpty     ( NonEmpty( (:|) ) )
import Data.Maybe             ( Maybe( Just, Nothing ), catMaybes )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.Show              ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( endOfLine, string, upper )
import Text.Parsec.Combinator  ( eof, sepBy )
import Text.Parsec.Error       ( Message( UnExpect ), newErrorMessage )
import Text.Parsec.Prim        ( Parsec, Stream, (<?>), getPosition, try )
import Text.Parsec.Pos         ( SourcePos, newPos, sourceLine, sourceName )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError( ParseError ) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqR, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( pack, unlines, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account            ( Account )
import Acct.Amount             ( Amount )
import Acct.Comment            ( Comment )
import Acct.Date               ( Date )
import Acct.Parser             ( noNLCR, wspaces )
import Acct.Stmt               ( Stmt )
import Acct.TransactionSimple  ( TransactionSimple, tsimp, tsimp', tsimp_ )

--------------------------------------------------------------------------------

data TransBreakdownHeader = TransBreakdownHeader  { _bdamount  ∷ Amount
                                                  , _bddate    ∷ Date
                                                  , _bdstmt    ∷ 𝕄 Stmt
                                                  , _bdcomment ∷ 𝕄 Comment
                                                  }

  deriving (Eq,Show)

------------------------------------------------------------

{- | The data of an expression (shorn of source pos). -}
data ExpressionData = TrxComment 𝕋
                    | TAcctStart Account
                    | TOStmtStart ℂ
                    -- Simple transactions have an account (A<...>);
                    -- Breakdown transactions have a marker (B<>);
                    -- each trx one xor the other.
                    -- Broken down trx (prefixed with '#') must have an
                    -- account (A<...>).
                    | TSimpleTrx TransactionSimple
                    | TBreakdown TransBreakdownHeader [TransactionSimple]
  deriving (Eq,Show)

--------------------------------------

instance Parsecable ExpressionData where
  parser = go <?> "Expression"
           where
             -- e.g., "-- This is a comment" or ""
             comment    = wspaces ⋫ (string "-- " ⋫ noNLCR)
             tcomment   = TrxComment ∘ pack ⊳ comment
             -- e.g., "Start: CarFund"
             start      = string "Start:" ⋫ wspaces ⋫ parser
             acctStart  = TAcctStart ⊳ start
             -- e.g., "oStart: P"
             ostart     = string "oStart:" ⋫ wspaces ⋫ upper
             oStmtStart = TOStmtStart ⊳ ostart
             -- e.g., "10.13+\t#D<6.viii.96>A<Bills>X<5>"
             simpleTrx ∷ Stream s Identity ℂ ⇒ Parsec s u ExpressionData
             simpleTrx = TSimpleTrx ⊳ parser
{-
             simpleTrx  = let betweens a b = between (string a) (string b)
                              markup c = betweens [c,'<'] ['>']
                           in TSimpleTrx ⊳ (parser ⋪ wspaces ⋪ char '#')
                                         ⊵ markup 'D' parser
                                         ⊵ markup 'A' parser
                                         ⊵ optionMaybe (markup 'X' parser)
-}
             -- Must either have an account, or be a breakdown (either A<...>
             -- or B<>)
--             go         = try acctStart ∤ try oStmtStart ∤ try simpleTrx
                        -- tcomment may validly consume nothing, so that must
                        -- be the final parsing choice.
--                        ∤ tcomment
             go = tries $ acctStart :| [oStmtStart,simpleTrx,tcomment]

instance Printable ExpressionData where
  print (TrxComment    t) = P.text $ [fmt|COMMENT     : '%t'|] t
  print (TAcctStart  t)   = P.text $ [fmt|ACCT_START  : '%T'|] t
  print (TOStmtStart c)   = P.text $ [fmt|O_ACCT_START: '%s'|] [c]
  print (TSimpleTrx t)    = print t
  print (TBreakdown _ _)    = P.text $ "BREAKDOWN"

------------------------------------------------------------

{- | The conjunction of transaction data (`ExpressionData`) with where it
     was defined (`SourcePos`).
     An `Expression` may be a transaction (simple or broken-down), an Account
     start or an OtherAccount start, or a `Comment`.
 -}
data Expression = Expression SourcePos ExpressionData
  deriving (Eq, Show)

instance Printable Expression where
  print (Expression p t) =
    P.text $ [fmt|%s#%d: %T|] (sourceName p) (sourceLine p) t

instance Parsecable Expression where
  parser = Expression ⊳ getPosition ⊵ parser -- (pack ⊳ many (noneOf "\r\n"))

parseTransactionTests ∷ TestTree
parseTransactionTests =
  let
    cbar   = "-- bar"
    cbar2  = " -- bar"
    start  = "Start: Acct"
    ostmt  = "oStart: Y"
    simple1 = "10.13+\t#D<6.viii.96>A<Bills>X<5>"
    simple2 = "6.28+\t#D<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>"


    test ∷ 𝕊 → ExpressionData → 𝕋 → TestTree
    test s exp txt = testCase s $
          Right (Expression (newPos s 1 1) exp)
      @=? parsec @_ @ParseError (pack s) txt
   in
    testGroup "Expression.parse"
              [ test "cbar"   (TrxComment "bar") cbar
              , test "cbar2"  (TrxComment "bar") cbar2
              , test "start"  (TAcctStart "Acct")  start
              , test "ostmt"  (TOStmtStart 'Y')  ostmt
              , test "simple1" (TSimpleTrx $ tsimp 1013 (1996,8,6) "Bills" 5){-(TSimpleTrx (Amount 1013)
                               (Date $ fromGregorian 1996 8 6) (Account "Bills")
                               (Just $ Stmt 5)) -}simple1
              , test "simple2" (TSimpleTrx $ tsimp' 628 (1996,8,8) "CarFund" 5 "int to 8 Aug") {- (Amount 628)
                               (Date $ fromGregorian 1996 8 8) (Account "CarFund")
                               Nothing) -} simple2
              ]
--              [ test "simple1" (tsimp 1013 (1996,8,6) "Bills" 5) simple1
--              , test "simple2" (tsimp' 28 (1996,8,8) "CarFund" 5 "int to 8 Aug") simple2

------------------------------------------------------------

newtype Transactions = Transactions { unTransactions ∷ [Expression] }
  deriving (Eq,Show)

emptyTrans ∷ Transactions
emptyTrans = Transactions []

instance Parsecable Transactions where
  parser = Transactions ⊳ (catMaybes ⊳ ((try (Just ⊳ parser) ∤ (pure Nothing ⋪ wspaces)) `sepBy` (endOfLine <?> "Expression")) ⋪ eof)

parseTransactionsTests ∷ TestTree
parseTransactionsTests =
  let
    tcomm' t n s = Expression (newPos s n 1) (TrxComment t)
    tcomm t      = tcomm' t 1
    tcomm1'      = tcomm' "This is a comment"
    tcomm1       = tcomm1' 1
    tcomm2'      = tcomm' "This is another comment"
    tcomm_'      = tcomm' ""
    test ∷ 𝕋 → [𝕊 → Expression] → 𝕋 → TestTree
    test name exp txt = testGroup (unpack name) $
                          let got   =
                                unTransactions ⊳ parsec @_ @ParseError name txt
                              nameS = unpack name
                           in assertListEqR (unpack name) got (($ nameS) ⊳ exp)
    testE ∷ 𝕊 → ℕ → Message → 𝕋 → TestTree
    testE name n exp txt = testCase name $
                             let
                               pos = newPos name (fromIntegral n) 1
                               e   = ParseError $ newErrorMessage exp pos
                               got ∷ Either ParseError Transactions
                               got = parsec @_ @ParseError (pack name) txt
                              in
                               -- I can't get equality to work on apparently
                               -- equal ParseErrors, but show()ing the strings
                               -- works, and is good enough for here
                               show (Left @_ @Transactions e) @=? show got
    testU name n exp txt = testE name n (UnExpect exp) txt
    trx1 name n = Expression (newPos name n 1)
    tAcctStart  n t name = trx1 name n (TAcctStart t)
    tOStmtStart n t name = trx1 name n (TOStmtStart t)
    tSimple     n am dt ac st cm name =
      trx1 name n (TSimpleTrx $ tsimp_ am dt ac st cm)
   in
    testGroup
      "Transactions.parse"
      [ test "empty" [ ] ""
      , test "foobar" [tcomm "foo", tcomm' "bar " 2] " -- foo\r\n-- bar "
      , testU "sampleE1" 1 "'X'\nexpecting Expression or end of input" sampleE1
      , testU "sampleE2" 2 "'X'\nexpecting Expression or end of input" sampleE2
      , test "sample0" [tcomm1] sample0
      , test "sample1" [tcomm1,tAcctStart 3 "CarFund"] sample1
      , test "sample2" [tcomm1,tcomm2' 2] sample2
      , test "sample3" [tOStmtStart 1 'P'] sample3
      , test "sample4" [tcomm1,tAcctStart 3 "CarFund",tOStmtStart 4 'P'] sample4
      , test "sample5" [tSimple 1 1013 (1996,8,6) "Bills" (Just 5) Nothing]
                       sample5
      , test "sample6" [tSimple 1 (-14789) (1996,8,6) "Save" (Just 5) Nothing]
                       sample6
      , test "sample7" [tSimple 1 (628) (1996,8,8) "CarFund" (Just 5)
                                  (Just "int to 8 Aug")]
                       sample7
      , test "sample8" [tSimple 1 (628) (1996,8,8) "CarFund" (Just 5)
                                  (Just "int to 8 Aug")]
                       sample8
      , test "sampleX" [tcomm1,tcomm_' 2,tAcctStart 3 "CarFund",
                        tOStmtStart 4 'P', tcomm_' 5] sampleX
      ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

sampleE1 ∷ 𝕋
sampleE1 = "X"

sampleE2 ∷ 𝕋
sampleE2 = unlines [ "-- This is a comment"
                   , "X"
                   ]

sampleE3 ∷ 𝕋
sampleE3 = unlines [ "-- This is a comment"
                   , ""
                   , "X"
                   ]

sample0 ∷ 𝕋
sample0 = "-- This is a comment"


sample1 ∷ 𝕋
sample1 = unlines [ "-- This is a comment"
                  , ""
                  , "Start: CarFund"
                  ]

sample2 ∷ 𝕋
sample2 = unlines [ "-- This is a comment"
                  , "-- This is another comment"
                  ]

sample3 ∷ 𝕋
sample3 = "oStart: P"

sample4 ∷ 𝕋
sample4 = unlines [ "-- This is a comment"
                  , ""
                  , "Start: CarFund"
                  , "oStart: P"
                  ]

sample5 ∷ 𝕋
sample5 = "10.13+\t#D<6.viii.96>A<Bills>X<5>"

sample6 ∷ 𝕋
sample6 = "147.89-\t#D<6.VIII.96>A<Save>X<5>"

sample7 ∷ 𝕋
sample7 = "6.28+\t#D<8.viii.96>A<CarFund>C<int to 8 Aug>X<5>"

sample8 ∷ 𝕋
sample8 = unlines [ "337.53+\t#D<1.viii.96>B<>X<5>"
                  , "#107.53+  #D<1.viii.96>A<Save>C<for Hx>"
                  , "#230+#D<1.viii.96>A<Food>"
                  ]

sampleX ∷ 𝕋
sampleX = unlines [ "-- This is a comment"
                  , ""
                  , "Start: CarFund"
                  , "Start: Bills"
                  , "Start: Tithe"
                  -- line 6
                  , "oStart: P"
                  , "oStart: M"
                  , "oStart: A"
                  -- line 9
                  , "10.13+\t#D<6.viii.96>A<Bills>X<5>"
                  , "472.50+  #D<6.viii.96>A<Tithe>X<5>"
                  , "28.07-\t#D<6.viii.96>A<CarFund>X<5>"
                  , "21.79-\t#D<6.viii.96>A<Food>X<5>"
                  , "147.89-\t#D<6.VIII.96>A<Save>X<5>"
                  , "6.28+\t#D<8.viii.96>A<CarFund>C<int to 8 Aug>X<5>"
                  , "2.58+\t#D<8.viii.96>A<Save>C<int to 8 Aug>X<5>"
                  , "1.70+\t#D<15.ix.96>A<CarFund>C<error correction>X<6>"
                  , ""
                  -- line 18
                  , "902.55+\t#D<1.viii.96>B<>X<5>"
                  , "#107.53+  #D<1.viii.96>A<Save>C<for Hx>"
                  , "#230+#D<1.viii.96>A<Food>"
                  , "#100+\t#D<1.vii.96>A<Tithe>"
                  , "#35+\t#D<1.viii.96>A<Bills>"
                  , "#160+\t#D<1.viii.96>A<Petrol>"
                  , "#40+\t#D<1.viii.96>A<CarFund>C<lounge decoration>"
                  , "#230.02+\t#D<1.viii.96>A<Save>"
                  , "##"
                  , "19.99-\t#D<8.ii.97>A<CarFund>C<needle>O<P:1>X<13>"
                  , "6.17-\t#D<8.ii.97>A<CarFund>C<tapes Mx>O<P:1>X<13>"
                  , "5.99-\t#D<24.ii.97>A<CarFund>C<slippers>X<12>"
                  , "0.81+\t#D<12.iii.97>A<CarFund>C<error from12>X<12>"
                  , "5.49-\t#D<9.v.20>A<Entz>C<fair email Abi>O<A>"
                  , "5.49-\t#D<9.v.20>A<Entz>C<fair email X>O<A>"
                  , "9.80-\t#D<1.vii.20>A<LunchM>C<bike coffees>O<A>"
                  , "9.50-\t#D<17.vii.20>A<Entz>C<ice-cream Wrest>O<R>"
                  ]

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Expression" [ parseTransactionTests
                                    , parseTransactionsTests
                                    ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
