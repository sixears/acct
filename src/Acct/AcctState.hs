module Acct.AcctState
  ( AcctState
  , accounts, addToAcct, newAcctState, otherAccounts, parseEntry, startAcct )
where

import Base1T  hiding  ( (∈), toList )

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail, fail )

-- containers-plus ---------------------

import ContainersPlus.Member  ( (∈) )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- lens --------------------------------

import Control.Lens.Getter  ( use )
import Control.Lens.Setter  ( (%=) )

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
import Acct.Mapish      ( Mapish( adjust, empty, insert ) )
import Acct.OAcctMap    ( OAcctMap, addTrx )
import Acct.OStmt       ( HasOStmtY( oStmtY ), oAcct )
import Acct.OStmtName   ( OStmtName )
import Acct.TrxBrk      ( trx )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

------------------------------------------------------------

data AcctState = AcctState { _accounts      ∷ AcctMap
                           , _otherAccounts ∷ OAcctMap }
  deriving (Eq,Show)

----------------------------------------

accounts ∷ Lens' AcctState AcctMap
accounts = lens _accounts (\ s a → s { _accounts = a })

----------------------------------------

otherAccounts ∷ Lens' AcctState OAcctMap
otherAccounts = lens _otherAccounts (\ s a → s { _otherAccounts = a })

----------------------------------------

newAcctState ∷ AcctState
newAcctState = AcctState empty empty

----------------------------------------

startAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ AcctMap → Account → η ()
startAcct accts a =
  if a ∈ accts
  then fail $ [fmt|Cannot re-start extant account '%T'|] a
  else accounts %= insert a []

----------------------------------------

startOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ OAcctMap → OStmtName → η ()
startOAcct oaccts c =
  if c ∈ oaccts
  then fail $ [fmt|Cannot re-start other account '%T'|] c
  else otherAccounts %= insert c (fromList [])

----------------------------------------

addToAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ AcctMap → TrxSimp → η ()
addToAcct acctmap t =
  let a = t ⊣ account
  in  if a ∈ acctmap
      then accounts %= adjust (t:) a
      else fail $ [fmt|Not a valid account '%T' (%T)|] a t

----------------------------------------

addToOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ TrxSimp → η ()
addToOAcct t = do
  oaccts ← use otherAccounts
  case t ⊣ oStmtY of
    𝕵 oa →
      let
        toact = oa ⊣ oAcct
      in
        if toact ∈ oaccts
        then otherAccounts %= addTrx t
        else fail $ [fmt|Not a valid other account '%T' (%T)|] oa t
    𝕹    → return ()

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
  accts ← use accounts
  addToAcct accts t
  addToOAcct t
  return (𝕵 e)
parseEntry' e@(TBrk t) = do
  accts ← use accounts
  forM_ (trx t) $ \ t' → do
    addToAcct accts t'
    addToOAcct t'
  return (𝕵 e)

--------------------

parseEntry ∷ (MonadState AcctState η, MonadFail η, CharParsing η) ⇒ η (𝕄 Entry)
parseEntry = textual ≫ parseEntry'

instance TestCmp AcctState where
  testCmp nm as as' =
    testGroup nm $
    [ testCmp (nm ⊕ ":accounts") (as ⊣ accounts) (as' ⊣ accounts)
    , testCmp (nm ⊕ ":otherAccounts") (as ⊣ otherAccounts) (as' ⊣ otherAccounts)
    ]

-- that's all, folks! ----------------------------------------------------------
