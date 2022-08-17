module Acct.AcctState
  ( AcctState, accounts, addToAcct, newAcctState, otherAccounts, parseEntry
  , startAcct, stmts )
where

import Base1T  hiding  ( (∈), toList )

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail, fail )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

-- import Control.DeepSeq  ( rnf )

-- lens --------------------------------

import Control.Lens.At      ( at, ix )
import Control.Lens.Fold    ( has )
import Control.Lens.Getter  ( use )
import Control.Lens.Setter  ( (%=), (.=) )

-- mtl ---------------------------------

import Control.Monad.State  ( MonadState )

-- parsers -----------------------------

import Text.Parser.Char  ( CharParsing )

-- tasty-plus --------------------------

import TastyPluser  ( TestCmp( testCmp ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account, HasAccount( account ) )
import Acct.AcctMap     ( AcctMap )
import Acct.Entry       ( Entry( TAcctStart, TBrk, TrxComment, TOStmtStart
                               , TSimpleTrx ) )
import Acct.OAcctMap    ( OAcctMap, addTrx )
import Acct.OStmt       ( oAcct )
import Acct.OStmtName   ( OStmtName )
import Acct.StmtMap     ( StmtMap )
import Acct.TrxBrk      ( trx )
import Acct.TrxSimp     ( TrxSimp, oStmtGetY, stmtGetY )
import Acct.Util        ( mcons )

--------------------------------------------------------------------------------

------------------------------------------------------------

data AcctState = AcctState { _accounts      ∷ AcctMap
                           , _otherAccounts ∷ OAcctMap
                           , _stmts         ∷ StmtMap
                           }
  deriving (Eq,Show)

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

----------------------------------------

startAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ AcctMap → Account → η ()
startAcct accts a =
--  if a ∈ accts
  if has (ix a) accts
  then fail $ [fmt|Cannot re-start extant account '%T'|] a
--  else accounts %= insert a []
  else accounts ∘ at a .= 𝕵 []

----------------------------------------

startOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ OAcctMap → OStmtName → η ()
startOAcct oaccts c =
--  if c ∈ oaccts
  if has (ix c) oaccts
  then fail $ [fmt|Cannot re-start other account '%T'|] c
--  else otherAccounts %= insert c (fromList [])
--  else otherAccounts ∘ at c %= const (𝕵 ф) -- insert c (fromList [])
  else otherAccounts ∘ at c .= 𝕵 ф -- insert c (fromList [])

----------------------------------------

addToAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ TrxSimp → η ()
addToAcct t = do
  acctmap ← use accounts
  let a = t ⊣ account
  if has (ix a) acctmap
--  if a ∈ acctmap
--  then accounts %= adjust (\ ts → t:ts) a
  then accounts ∘ at a %= 𝕵 ∘ mcons t
  else fail $ [fmt|Not a valid account '%T' (%T)|] a t

----------------------------------------

addToOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ TrxSimp → η ()
addToOAcct t = do
  oaccts ← use otherAccounts
  case oStmtGetY t of
    𝕵 oa →
      let
        toact = oa ⊣ oAcct
      in
        if has (ix toact) oaccts -- toact ∈ oaccts
        then otherAccounts %= addTrx t
        else fail $ [fmt|Not a valid other account '%T' (%T)|] (oa ⊣ oAcct) t
    𝕹    → return ()

----------------------------------------

addToStmt ∷ (MonadFail η, MonadState AcctState η) ⇒ TrxSimp → η ()
addToStmt t = do
  let st = stmtGetY t
  stmts ∘ at st %= 𝕵 ∘ mcons t

----------------------------------------

parseEntry' ∷ (MonadState AcctState η, MonadFail η) ⇒ Entry → η (𝕄 Entry)
parseEntry' (TrxComment _) = return 𝕹
parseEntry' (TAcctStart a) = do
  accts ← use accounts
  startAcct accts a
  return 𝕹
parseEntry' (TOStmtStart c) = do
  oaccts ← use otherAccounts
  startOAcct oaccts c
  return 𝕹
parseEntry' e@(TSimpleTrx t) = do
--  return $ rnf t
  addToAcct t
  addToOAcct t
  addToStmt t
  return (𝕵 e)
parseEntry' e@(TBrk t) = do
--  return $ rnf t
  forM_ (trx t) $ \ t' → do
    addToAcct t'
    addToOAcct t'
    addToStmt t'
  return (𝕵 e)

--------------------

parseEntry ∷ (MonadState AcctState η, MonadFail η, CharParsing η) ⇒ η (𝕄 Entry)
parseEntry = textual ≫ parseEntry'

instance TestCmp AcctState where
  testCmp nm as as' =
    testGroup nm $
    [ testCmp (nm ⊕ ":accounts") (as ⊣ accounts) (as' ⊣ accounts)
    , testCmp (nm ⊕ ":otherAccounts") (as ⊣ otherAccounts) (as' ⊣ otherAccounts)
    , testCmp (nm ⊕ ":stmts") (as ⊣ stmts) (as' ⊣ stmts)
    ]

-- that's all, folks! ----------------------------------------------------------
