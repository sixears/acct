{-| Map from account name to a list of that account's transactions. -}
module Acct.AcctMap
  ( AcctMap )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List    ( sort )
import Data.Monoid  ( Monoid )
import GHC.Exts     ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- lens --------------------------------

import Control.Lens.At  ( At( at ), Index, Ixed( ix ), IxValue )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEq )
import TastyPluser  ( TestCmp( testCmp ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

newtype AcctMap = AcctMap (Map.Map Account [TrxSimp])
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

-- that's all, folks! ----------------------------------------------------------
