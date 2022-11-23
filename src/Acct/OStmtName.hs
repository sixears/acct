module Acct.OStmtName
  ( OStmtName, ostmtname )
where

import Base1T

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( oneOf )
import Text.Parser.Combinators  ( (<?>) )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( choose )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE, LitE ), Lit( CharL )
                                   , Lift( liftTyped ), TExp( TExp ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

--------------------------------------------------------------------------------

newtype OStmtName = OStmtName ℂ  deriving  (Eq,NFData,Ord,Show)

--------------------

instance Lift OStmtName where
  liftTyped (OStmtName c) = return ∘ TExp $
    AppE (ConE 'OStmtName) (LitE $ CharL c)

--------------------

instance Validity OStmtName where
  validate (OStmtName c) = declare "uppercase" $ c ∈ [ 'A' .. 'Z' ]

--------------------

instance GenValid OStmtName where
  genValid      = OStmtName ⊳ choose ('A','Z')
  shrinkValid _ = []

--------------------

instance Arbitrary OStmtName where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable OStmtName where
  print (OStmtName c) = P.char c

--------------------

instance Textual OStmtName where
  textual = OStmtName ⊳ oneOf [ 'A' .. 'Z' ] <?> "Other Statement Name"


instance TextualPlus OStmtName where
  textual' = textual

----------------------------------------

{-| QuasiQuoter for `OStmtName` -}
ostmtname ∷ QuasiQuoter
ostmtname = mkQQExp "OStmtName" (liftTParse' @OStmtName tParse')

-- that's all, folks! ----------------------------------------------------------
