{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

{-| Map from account name to a list of that account's transactions. -}
module Acct.StmtEntry
  ( StmtEntry(..) )
where

import Base1T  hiding  ( toList )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount     ( HasAmount( amount ) )
import Acct.Date       ( HasDate( date ) )
import Acct.StmtIndex  ( GetStmtIndex( stmtIndexGet ) )
import Acct.TrxBrk     ( TrxBrk )
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
  print (SE_SIMP ts) = print ts
  print (SE_BRK  tb) = print tb

-- that's all, folks! ----------------------------------------------------------
