{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Acct.Comment
  ( Comment( Comment ) )
where

-- base --------------------------------

import Data.Eq    ( Eq )
import GHC.Exts   ( IsString )
import Text.Show  ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- parsec ------------------------------

import Text.Parsec.Char        ( noneOf )
import Text.Parsec.Combinator  ( many1 )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- text --------------------------------

import Data.Text  ( pack )

--------------------------------------------------------------------------------

newtype Comment = Comment 𝕋
  deriving (Eq,IsString,Printable, Show)

--------------------

instance Parsecable Comment where
  parser = Comment ∘ pack ⊳ many1 (noneOf "<>\n\r\t")

-- that's all, folks! ----------------------------------------------------------
