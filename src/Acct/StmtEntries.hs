{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

{-| Map from account name to a list of that account's transactions. -}
module Acct.StmtEntries
  ( StmtEntries )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import GHC.Exts  ( IsList( toList ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.StmtEntry  ( StmtEntry )

--------------------------------------------------------------------------------

newtype StmtEntries = StmtEntries [StmtEntry]  deriving  (Eq,Show)

--------------------

instance IsList StmtEntries where
  type instance Item StmtEntries = StmtEntry
  fromList xs = StmtEntries $ fromList xs
  toList (StmtEntries xs) = toList xs

-- that's all, folks! ----------------------------------------------------------
