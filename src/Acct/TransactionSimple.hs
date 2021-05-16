{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Acct.TransactionSimple
  ( TransactionSimple, tsimp, tsimp', tsimp_, tests )
where

import Prelude  ( Int )

-- base --------------------------------

import Control.Applicative    ( many )
import Control.Monad          ( return )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Data.Functor.Identity  ( Identity )
import Data.List              ( filter )
import Data.Maybe             ( Maybe( Just, Nothing ), maybe )
import GHC.Enum               ( Enum )
import Text.Show              ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (∤) )
import Data.MoreUnicode.Bool         ( 𝔹 )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, string )
import Text.Parsec.Combinator  ( between )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, parserFail )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, assertLeft )

-- text --------------------------------

import Data.Text  ( isInfixOf, isSuffixOf, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Calendar  ( fromGregorian  )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account  ( Account )
import Acct.Amount   ( Amount( Amount ) )
import Acct.Comment  ( Comment( Comment ) )
import Acct.Date     ( Date( Date ) )
import Acct.Parser   ( wspaces )
import Acct.Stmt     ( Stmt( Stmt ) )

--------------------------------------------------------------------------------

data TPart = TDate     Date
           | TAccount  Account
           | TStmt     Stmt
           | TComment  Comment
  deriving (Eq,Show)

data TPartType = TTDate | TTAccount | TTStmt | TTComment
  deriving (Enum,Eq,Show)

instance Printable TPartType where
  print TTDate    = P.text "Date"
  print TTAccount = P.text "Account"
  print TTStmt    = P.text "Stmt"
  print TTComment = P.text "Comment"

tpartType ∷ TPart → TPartType
tpartType (TDate    _) = TTDate
tpartType (TAccount _) = TTAccount
tpartType (TStmt    _) = TTStmt
tpartType (TComment _) = TTComment

unTComment ∷ 𝕄 TPart → 𝕄 Comment
unTComment (Just (TComment t)) = Just t
unTComment _            = Nothing

unTStmt ∷ 𝕄 TPart → 𝕄 Stmt
unTStmt (Just (TStmt t)) = Just t
unTStmt _                = Nothing

instance Printable TPart where
  print t = print (tpartType t)

tpartGetMaybe ∷ TPartType → [TPart] → Either 𝕋 (𝕄 TPart)
tpartGetMaybe t xs = case filter ((t ≡) ∘ tpartType) xs of
                       []  → Right Nothing
                       [x] → Right (Just x)
                       _   → Left $ [fmt|Too many %Ts found (%L)|] t xs

tpartGetOne ∷ TPartType → [TPart] → Either 𝕋 TPart
tpartGetOne t xs = case tpartGetMaybe t xs of
                     Right Nothing  → Left $ [fmt|No %T found|] t
                     Right (Just x) → Right x
                     Left  e        → Left e

tpartGetOneAccount ∷ [TPart] → Either 𝕋 Account
tpartGetOneAccount tps = case tpartGetOne TTAccount tps of
                           Right (TAccount ac) → Right ac
                           Right _             → Left "Internal Error: TAccount"
                           Left e              → Left e

tpartGetOneDate ∷ [TPart] → Either 𝕋 Date
tpartGetOneDate tps = case tpartGetOne TTDate tps of
                           Right (TDate ac) → Right ac
                           Right _             → Left "Internal Error: TDate"
                           Left e              → Left e

instance Parsecable TPart where
  parser = let betweens a b = between (string a) (string b)
               markup c = betweens [c,'<'] ['>']
             in TDate    ⊳ markup 'D' parser
              ∤ TAccount ⊳ markup 'A' parser
              ∤ TStmt    ⊳ markup 'X' parser
              ∤ TComment ⊳ markup 'C' parser

parseTPartTests ∷ TestTree
parseTPartTests =
  let
    parse'  ∷ 𝕋 → 𝕋 → Either ParseError TPart
    parse'  = parsec
    test ∷ 𝕋 → TPart → TestTree
    test txt exp = testCase (unpack txt) $ Right exp @=? parse' txt txt
   in
    testGroup "TPart.parse"
              [ test "D<1.i.96>" (TDate $ Date $ fromGregorian 1996 1 1)
              , test "A<Record>" (TAccount "Record")
              , test "C<int to 8 Aug>" (TComment $ Comment "int to 8 Aug")
              , test "X<5>"      (TStmt $ Stmt 5)
              ]

------------------------------------------------------------

{- | A simple standard single-line transaction; or a single entry in a
     broken-down transaction. -}
data TransactionSimple = TransactionSimple { _amount  ∷ Amount
                                           , _date    ∷ Date
                                           , _account ∷ Account
                                           , _stmt    ∷ 𝕄 Stmt
                                           , _comment ∷ 𝕄 Comment
                                           }
  deriving (Eq,Show)

instance Printable TransactionSimple where
  print (TransactionSimple am dt ac st cm) = P.text $ [fmt|TSIMPLE: %T <%T> [%T] %t # %t|]
                                             am dt ac (maybe "Nothing" toText st) (maybe "" toText cm)

parseEither ∷ Printable ε ⇒ Either ε α → ParsecT s u m α
parseEither (Left e)  = parserFail (toString e)
parseEither (Right a) = return a

simpleParts ∷ Amount → [TPart] → Either 𝕋 TransactionSimple
simpleParts am tps = do
  dt ← tpartGetOneDate tps
  ac ← tpartGetOneAccount tps
  st ← tpartGetMaybe TTStmt    tps
  cm ← tpartGetMaybe TTComment tps
  return $ TransactionSimple am dt ac (unTStmt st) (unTComment cm)

instance Parsecable TransactionSimple where
  parser ∷ Stream s Identity ℂ ⇒ Parsec s u TransactionSimple
  parser  = do (am,tps) ← (,) ⊳ (parser ⋪ wspaces ⋪ char '#') ⊵ many parser
               parseEither (simpleParts am tps)

tsimp_ ∷ ℤ → (ℤ,Int,Int) → Account → 𝕄 ℕ → 𝕄 Comment → TransactionSimple
tsimp_ am (y,m,d) ac st cm = let am' = Amount am
                                 dt = (Date $ fromGregorian y m d)
                              in TransactionSimple am' dt ac (Stmt ⊳ st) cm

tsimp ∷ ℤ → (ℤ,Int,Int) → Account → ℕ → TransactionSimple
tsimp am dt ac st = tsimp_ am dt ac (Just st) Nothing

tsimp' ∷ ℤ → (ℤ,Int,Int) → Account → ℕ → Comment → TransactionSimple
tsimp' am dt ac st cm = tsimp_ am dt ac (Just st) (Just cm)

parseTransactionSimpleTests ∷ TestTree
parseTransactionSimpleTests =
  let
    parse'  ∷ 𝕋 → 𝕋 → Either ParseError TransactionSimple
    parse'  = parsec
    simple1 = "10.13+\t#D<6.viii.96>A<Bills>X<5>"
    simple2 = "0.28+\t#D<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>"
    -- X is not date
    e1      = "6.28+\t#X<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>"
    -- J is invalid
    e2      = "6.28+\t#J<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>"
    -- missing date
    e3      = "6.28+\t#A<CarFund>C<int to 8 Aug>X<5>"
    -- repeated date
    e4      = "6.28+\t#D<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>D<1.i.97>"

    test ∷ 𝕊 → TransactionSimple → 𝕋 → TestTree
    test s exp txt = testCase s $ Right exp @=? parse' (pack s) txt

    testE ∷ 𝕋 → 𝕋 → TestTree
    testE t txt = testCase (unpack t) $ assertIsLeft (parse' t txt)

    testE' ∷ 𝕋 → 𝕋 → (ParseError → 𝔹) → TestTree
    testE' t txt p = testCase (unpack t) $
      assertLeft (\e → assertBool (toString e) $ p e) (parse' t txt)
    testEinfix ∷ 𝕋 → 𝕋 → 𝕋 → TestTree
    testEinfix t txt exp = testE' t txt (\e → exp `isInfixOf` toText e)
    testEsuffix ∷ 𝕋 → 𝕋 → 𝕋 → TestTree
    testEsuffix t txt exp = testE' t txt (\e → exp `isSuffixOf` toText e)

  in
    testGroup "TransactionSimple.parse"
              [ test "simple1" (tsimp 1013 (1996,8,6) "Bills" 5) simple1
              , test "simple2" (tsimp' 28 (1996,8,8) "CarFund" 5 "int to 8 Aug")
                               simple2
              , testE "e1" e1
              , testE "e2" e2
              , testEsuffix "e3" e3 "\nNo Date found"
              , testEinfix "e4" e4 "\nToo many Dates found "
              ]

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TransactionSimple" [ parseTPartTests
                                           , parseTransactionSimpleTests ]

-- that's all, folks! ----------------------------------------------------------
