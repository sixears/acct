{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

{-| a thing that may be added to a `Stmt`; i.e., a trx (simp, or brk) -}
module Acct.StmtEntry
  ( StmtEntry(..), accts, oAccts )
where

import Base1T

-- base --------------------------------

import Data.Maybe  ( catMaybes )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account    ( Account, account )
import Acct.Amount     ( HasAmount( amount ) )
import Acct.Date       ( HasDate( date ) )
import Acct.OStmtName  ( OStmtName )
import Acct.OStmt      ( HasOStmtY( oStmtY ), oAcct )
import Acct.StmtIndex  ( GetStmtIndex( stmtIndexGet ) )
import Acct.TrxBrk     ( TrxBrk, trx )
import Acct.TrxSimp    ( TrxSimp )

--------------------------------------------------------------------------------

data StmtEntry = SE_SIMP TrxSimp | SE_BRK TrxBrk
  deriving (Eq,Show)

--------------------

instance HasAmount StmtEntry where
  amount = lens (\ case
                    (SE_SIMP t) → t ⊣ amount
                    (SE_BRK  t) → t ⊣ amount
                )
                (\ s a → case s of
                           SE_SIMP t → SE_SIMP $ t & amount ⊢ a
                           SE_BRK  t → SE_BRK  $ t & amount ⊢ a
                )

--------------------

instance HasDate StmtEntry where
  date = lens (\ case
                  (SE_SIMP t) → t ⊣ date
                  (SE_BRK  t) → t ⊣ date)
              (\ s a → case s of
                  SE_SIMP t → SE_SIMP $ t & date ⊢ a
                  SE_BRK  t → SE_BRK  $ t & date ⊢ a
              )

--------------------

instance GetStmtIndex StmtEntry where
  stmtIndexGet (SE_SIMP t) = stmtIndexGet t
  stmtIndexGet (SE_BRK  t) = stmtIndexGet t

--------------------

instance Printable StmtEntry where
  print (SE_SIMP t) = print t
  print (SE_BRK  b) = print b

----------------------------------------

accts ∷ StmtEntry → NonEmpty Account
accts (SE_SIMP t) = pure $ t ⊣ account
accts (SE_BRK  b) = view account ⊳ trx b

----------------------------------------

oAccts ∷ StmtEntry → [OStmtName]
oAccts (SE_SIMP t) = catMaybes [view oAcct ⊳ t ⊣ oStmtY]
oAccts (SE_BRK  b) =
  catMaybes ∘ toList $ (fmap (view oAcct) ∘ view oStmtY) ⊳ trx b

-- that's all, folks! ----------------------------------------------------------
