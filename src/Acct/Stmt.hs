{-# LANGUAGE UnicodeSyntax #-}

module Acct.Stmt
  ( Stmt( Stmt ), tests )
where

-- base --------------------------------

import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Text.Read      ( read )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( digit )
import Text.Parsec.Combinator  ( many1 )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

newtype Stmt    = Stmt ℕ    deriving (Eq,Show)

instance Printable Stmt where
  print (Stmt n) = P.string (show n)

instance Parsecable Stmt where
  parser = Stmt ∘ read ⊳ many1 digit

parseStmtTests ∷ TestTree
parseStmtTests =
  let parse ∷ 𝕋 → 𝕋 → Either ParseError Stmt
      parse = parsec
   in testGroup "Stmt.parse"
                [ testCase "6"  $ Right (Stmt 6)  @=? parse "6"  "6"
                , testCase "66" $ Right (Stmt 66) @=? parse "66" "66"
                ]

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Stmt" [ parseStmtTests ]

-- that's all, folks! ----------------------------------------------------------
