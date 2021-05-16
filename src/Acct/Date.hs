{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module Acct.Date
  ( Date( Date ), tests )
where

import Prelude  ( (+), error, fromIntegral )

-- base --------------------------------

import Control.Applicative  ( Applicative( pure ) )
import Control.Monad        ( return )
import Data.Char            ( toLower )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.List            ( lookup )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import GHC.Enum             ( Enum( fromEnum, toEnum ) )
import Text.Read            ( read )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Data.Ord.Unicode         ( (≤) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, oneOf )
import Text.Parsec.Combinator  ( count, many1 )
import Text.Parsec.Error       ( Message( Message, UnExpect ), newErrorMessage )
import Text.Parsec.Prim        ( ParsecT, Stream, parserFail, try, unexpected )
import Text.Parsec.Pos         ( newPos )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError( ParseError ) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Calendar  ( Day
                           , fromGregorian, fromGregorianValid, toGregorian  )

--------------------------------------------------------------------------------

{- | Parser of three things, separated by a common separator. -}
sep3 ∷ Applicative ψ ⇒ ψ ω → ψ α → ψ β → ψ γ → ψ (α,β,γ)
sep3 w a b c = (,,) ⊳ a ⋪ w ⊵ b ⋪ w ⊵ c

----------------------------------------

data Month = January | February | March
           | April   | May      | June
           | July    | August   | September
           | October | November | December
  deriving (Eq,Show)

instance Printable Month where
  print m = P.text $ [fmt|%02d|] (fromEnum m)

romans ∷ [(𝕊,ℕ)]
romans = [("i",1),("ii",2),("iii",3),("iv",4),("v",5),("vi",6),("vii",7)
         ,("viii",8),("iix",8),("ix",9),("x",10),("xi",11),("xii",12)]

instance Parsecable Month where
  parser = do m ← many1 (oneOf "iIvVxX")
              case (toLower ⊳ m) `lookup` romans of
                Just i  → return ∘ toEnum $ fromIntegral i
                Nothing → unexpected m

instance Enum Month where
  toEnum  1 = January
  toEnum  2 = February
  toEnum  3 = March
  toEnum  4 = April
  toEnum  5 = May
  toEnum  6 = June
  toEnum  7 = July
  toEnum  8 = August
  toEnum  9 = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December
  toEnum  i = error $ [fmt|Invalid month: %d|] i


  fromEnum January   =  1
  fromEnum February  =  2
  fromEnum March     =  3
  fromEnum April     =  4
  fromEnum May       =  5
  fromEnum June      =  6
  fromEnum July      =  7
  fromEnum August    =  8
  fromEnum September =  9
  fromEnum October   = 10
  fromEnum November  = 11
  fromEnum December  = 12

------------------------------------------------------------

newtype Year = Year { unYear ∷ ℕ } deriving (Eq,Show)

instance Printable Year where
  print (Year y) = P.string $ show y

instance Parsecable Year where
  parser = let y2 (read → y') = Year $ y' + if y' ≤ 90 then 2000 else 1900
            in try ((Year ∘ read) ⊳ count 4 digit)
              ∤ y2 ⊳ count 2 digit

parseYearTests ∷ TestTree
parseYearTests =
  let
    parse' ∷ 𝕋 → 𝕋 → Either ParseError Year
    parse' = parsec
    test t y = testCase (unpack t) $ Right (Year y) @=? parse' t t
   in
    testGroup
      "Year.parse"
      [ test "96" 1996
      , test "1996" 1996
      , test "2006" 2006
      , test "06" 2006
      ]

------------------------------------------------------------

newtype Date  = Date Day  deriving (Eq,Show)

instance Printable Date where
  print (Date t) = let (y,m,d) = toGregorian t
                    in P.text $ [fmt|%04d-%02d-%02d|] y m d

parseDMY ∷ Stream s m t ⇒ (𝕊,Month,Year) → ParsecT s u m Date
parseDMY (d,m,y) = do
  case fromGregorianValid (fromIntegral $ unYear y) (fromEnum m) (read d) of
    Just d' → return $ Date d'
    Nothing → parserFail $ [fmt|invalid date %s.%T.%T|] d m y

instance Parsecable Date where

  parser = let digit2 = count 2 digit
               mday = try digit2 ∤ (pure ⊳ digit)
            in sep3 (char '.') mday parser parser ≫ parseDMY

parseDateTests ∷ TestTree
parseDateTests =
  let
    parse' ∷ 𝕋 → 𝕋 → Either ParseError Date
    parse' = parsec
    test t y m d = testCase (unpack t) $
                     Right (Date $ fromGregorian y m d) @=? parse' t t
    testShow ∷ Show α ⇒ 𝕊 → α → α → TestTree
    testShow name exp got = testCase name $ show exp @=? show got
    testE txt r c err = let pos = newPos (unpack txt) r c
                            exp = Left $ ParseError (newErrorMessage err pos)
                         in testShow (unpack txt) exp (parse' txt txt)
    testU txt r c err = testE txt r c (UnExpect err)
    testM txt r c err = testE txt r c (Message err)
   in
    testGroup
      "Date.parse"
      [ testU "30.L.96"   1  4 "\"L\""
      , testM "40.xii.96" 1 10 "invalid date 40.12.1996"
      , testM "30.ii.96"  1  9 "invalid date 30.02.1996"
      , test "30.i.96"    1996 1 30
      , test "30.i.1996"  1996 1 30
      , test "30.i.2006"  2006 1 30
      , test "30.i.01"    2001 1 30
      , test "30.i.90"    2090 1 30
      , test "30.i.1896"  1896 1 30
      , testM "31.iv.96"  1 9 "invalid date 31.04.1996"
      , test "1.i.1896"  1896 1 1
      , test "31.xii.89"  2089 12 31
      ]

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Date" [ parseDateTests, parseYearTests ]

-- that's all, folks! ----------------------------------------------------------
