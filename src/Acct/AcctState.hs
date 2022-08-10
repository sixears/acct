module Acct.AcctState
  ( AcctState, TestCmp( testCmp )
  , accounts, addToAcct, newAcctState, otherAccounts, parseEntry, startAcct )
where

import Base1T  hiding  ( (∈), toList )

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail, fail )
import Data.List           ( sort )
import GHC.Exts            ( IsList( fromList, toList ) )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map

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

import TastyPlus  ( assertListEq, assertListEqS )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account, HasAccount( account ) )
import Acct.Entry       ( Entry( TAcctStart, TBrk, TrxComment, TOStmtStart
                               , TSimpleTrx ) )
import Acct.OStmt       ( HasOStmtY( oStmtY ), oAcct, oIndex )
import Acct.TrxBrk      ( trx )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

class TestCmp α where
  testCmp ∷ HasCallStack ⇒ TestName → α → α → TestTree

class Mapish π where
  type Key π
  type Value π
  adjust ∷ ((Value π) → (Value π)) → (Key π) → π → π
  insert ∷ (Key π) → (Value π) → π → π

newtype AcctMap = AcctMap (Map.Map Account [TrxSimp])
  deriving (Eq,Show)

instance IsList AcctMap where
  type instance Item AcctMap = (Account,[TrxSimp])
  fromList xs = AcctMap $ fromList xs
  toList (AcctMap xs) = toList xs

instance Mapish AcctMap where
  type Key AcctMap = Account
  type Value AcctMap = [TrxSimp]
  adjust f k (AcctMap m) = AcctMap (Map.adjust f k m)
  insert k v (AcctMap m) = AcctMap (Map.insert k v m)

instance TestCmp AcctMap where
  testCmp nm (AcctMap am) (AcctMap am') =
    testGroup nm $ do
      let ks  = sort $ Map.keys am
          ks' = sort $ Map.keys am'
          vs = Map.intersectionWith (,) am am'
      ю [ [ assertListEq "account names" ks ks' ]
        , [ assertListEq ("account: " ⊕ toText k) v v'
          | (k,(v,v')) ← Map.toList vs ]
        ]

newtype OAcctMap = OAcctMap (Map.Map ℂ (Map.Map (𝕄 ℕ) [TrxSimp]))
  deriving (Eq,Show)

instance IsList OAcctMap where
  type instance Item OAcctMap = (ℂ,Map.Map (𝕄 ℕ) [TrxSimp])
  fromList xs = OAcctMap $ fromList xs
  toList (OAcctMap xs) = toList xs


instance Mapish OAcctMap where
  type Key OAcctMap = ℂ
  type Value OAcctMap = (Map.Map (𝕄 ℕ) [TrxSimp])
  adjust f k (OAcctMap m) = OAcctMap (Map.adjust f k m)
  insert k v (OAcctMap m) = OAcctMap (Map.insert k v m)


instance TestCmp OAcctMap where
  testCmp nm (OAcctMap oam) (OAcctMap oam') =
    testGroup nm $
      let
        ks  = Map.keys oam
        ks' = Map.keys oam'
        vs  ∷ Map.Map ℂ ((Map.Map (𝕄 ℕ) [TrxSimp]),(Map.Map (𝕄 ℕ) [TrxSimp]))
        vs  = Map.intersectionWith (,) oam oam'
      in
        [ assertListEq "other account names" ks ks' ]
        ⊕
        ю (fmap (\ (oa,(oks,oks')) →
                   assertListEqS ([fmt|other account %T keys|] oa)
                                 (Map.keys oks') (Map.keys oks)
                   ⊕ fmap (\ (i,(ts,ts')) →
                             assertListEq ([fmt|other account %T index %s|]
                                           oa (maybe "Nothing" show i))
                                          ts ts')
                          (Map.toList $ Map.intersectionWith (,) oks oks')
                )
           (Map.toList vs)
          )


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
newAcctState = AcctState (AcctMap ф) (OAcctMap ф)

----------------------------------------

startAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ AcctMap → Account → η ()
startAcct (AcctMap accts) a =
  if a ∈ accts
  then fail $ [fmt|Cannot re-start extant account '%T'|] a
  else accounts %= insert a []

----------------------------------------

startOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ OAcctMap → ℂ → η ()
startOAcct (OAcctMap oaccts) c =
  if c ∈ oaccts
  then fail $ [fmt|Cannot re-start other account '%s'|] [c]
  else otherAccounts %= insert c (fromList [])

----------------------------------------

addToAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ AcctMap → TrxSimp → η ()
addToAcct (AcctMap accts) t =
  let a = t ⊣ account
  in  case a `Map.lookup` accts of
        𝕹   → fail $ [fmt|Not a valid account '%T' (%T)|] a t
        𝕵 _ →  accounts %= adjust (t:) a

----------------------------------------

addToOAcct ∷ (MonadFail η, MonadState AcctState η) ⇒ TrxSimp → η ()
addToOAcct t = do
  OAcctMap oaccts ← use otherAccounts
  case t ⊣ oStmtY of
    𝕵 oa →
      let
        toact = oa ⊣ oAcct
      in
        case toact `Map.lookup` oaccts of
          𝕹      → fail $ [fmt|Not a valid other account '%T' (%T)|] oa t
          𝕵 oact →
            let
              f ∷ 𝕄 [TrxSimp] → 𝕄 [TrxSimp]
              f 𝕹      = 𝕵 [t]
              f (𝕵 ts) = 𝕵 (t:ts)
              i = oa ⊣ oIndex
            in
              otherAccounts %= adjust (\ m → Map.alter f i m) toact
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
