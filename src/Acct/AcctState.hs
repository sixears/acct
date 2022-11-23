{-| Accounts, Statements, OtherStatements; each as (ultimately) a collection of
    transactions: gathered together as a record type e.g., to use in MonadState.
-}

module Acct.AcctState
  ( AcctState, accounts, newAcctState, otherAccounts, stmts )
where

import Base1T  hiding  ( (∈), toList )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- tasty-plus --------------------------

import TastyPluser  ( TestCmp'( testCmp' ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.AcctMap   ( AcctMap )
import Acct.OAcctMap  ( OAcctMap )
import Acct.StmtMap   ( StmtMap )

--------------------------------------------------------------------------------

------------------------------------------------------------

data AcctState = AcctState { _accounts      ∷ AcctMap
                           , _otherAccounts ∷ OAcctMap
                           , _stmts         ∷ StmtMap
                           }
  deriving (Eq,Show)

----------------------------------------

instance Printable AcctState where
  print (AcctState acts oacts sts) = P.text $ [fmt|%T\n%T\n%T|] acts oacts sts

instance TestCmp' AcctState where
  testCmp' nm as as' =
    testGroup nm
    [ testCmp' (nm ⊕ ":accounts")      (as ⊣ accounts) (view accounts ⊳ as')
    , testCmp' (nm ⊕ ":otherAccounts") (as ⊣ otherAccounts)
                                       (view otherAccounts ⊳ as')
    , testCmp' (nm ⊕ ":stmts")         (as ⊣ stmts) (view stmts ⊳ as')
    ]

----------------------------------------

accounts ∷ Lens' AcctState AcctMap
accounts = lens _accounts (\ s a → s { _accounts = a })

----------------------------------------

otherAccounts ∷ Lens' AcctState OAcctMap
otherAccounts = lens _otherAccounts (\ s a → s { _otherAccounts = a })

----------------------------------------

stmts ∷ Lens' AcctState StmtMap
stmts = lens _stmts (\ s x → s { _stmts = x })

----------------------------------------

newAcctState ∷ AcctState
newAcctState = AcctState ф ф ф

-- that's all, folks! ----------------------------------------------------------
