{-| Map from account name to a list of that account's transactions. -}
module Acct.AcctMap
  ( AcctMap )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List    ( sort )
import Data.Maybe   ( fromMaybe )
import Data.Monoid  ( Monoid )
import GHC.Exts     ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- lens --------------------------------

import Control.Lens.At  ( At( at ), Index, Ixed( ix ), IxValue )

-- tasty-plus --------------------------

import TastyPlus    ( assertListEqIO, assertListEqIO' )
import TastyPluser  ( TestCmp'( testCmp' ) )

-- text --------------------------------

import Data.Text  ( pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

newtype AcctMap = AcctMap { unAcctMap ∷ Map.Map Account [TrxSimp] }
  deriving (Eq,Monoid,Semigroup,Show)

type instance Index   AcctMap = Account
type instance IxValue AcctMap = [TrxSimp]

instance Ixed AcctMap where
  ix k f (AcctMap m) =  AcctMap ⊳ ix k f m

instance At AcctMap where
  at k f (AcctMap m) = AcctMap ⊳ Map.alterF f k m

--------------------

instance IsList AcctMap where
  type instance Item AcctMap = (Account,[TrxSimp])
  fromList xs = AcctMap $ fromList xs
  toList (AcctMap xs) = toList xs

--------------------

instance Printable AcctMap where
  print smap = P.text $ [fmt|%w|] (unAcctMap smap)

--------------------

instance TestCmp' AcctMap where
  testCmp' nm (AcctMap am) am'_io {- (AcctMap am') -} =
    testGroup nm $ do
      let ks  = sort $ Map.keys am
      ю [ -- compare the lists of account names
          [ assertListEqIO ("account names"∷𝕋) ks
                           (sort ∘ Map.keys ∘ unAcctMap ⊳ am'_io) ]
        , -- we use assertListEqIO' here to use show to show discrepant TrxSimp;
          -- the printable instance doesn't show all the details (e.g., parents)
          -- for each account, compare the lists of transactions
          [ assertListEqIO' (pack ∘ show) ("account: " ⊕ toText k) v
                            ((fromMaybe [] ∘ (⩼ ix k)) ⊳ am'_io)
          | (k,v) ← Map.toList am ]
        ]

-- that's all, folks! ----------------------------------------------------------
