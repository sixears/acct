{-| Map from account name to a list of that account's transactions. -}
module Acct.AcctMap
  ( AcctMap )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List  ( sort )
import GHC.Exts   ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( MemberType, member ) )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEq )
import TastyPluser  ( TestCmp( testCmp ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account )
import Acct.Mapish      ( Mapish( Key, Value, adjust, empty, insert ) )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

newtype AcctMap = AcctMap (Map.Map Account [TrxSimp])
  deriving (Eq,Show)

--------------------

instance IsList AcctMap where
  type instance Item AcctMap = (Account,[TrxSimp])
  fromList xs = AcctMap $ fromList xs
  toList (AcctMap xs) = toList xs

--------------------

instance Mapish AcctMap where
  type Key AcctMap = Account
  type Value AcctMap = [TrxSimp]
  adjust f k (AcctMap m) = AcctMap (Map.adjust f k m)
  empty                  = AcctMap ф
  insert k v (AcctMap m) = AcctMap (Map.insert k v m)

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

--------------------

instance HasMember AcctMap where
  type MemberType AcctMap = Account
  member k (AcctMap m) = Map.member k m

-- that's all, folks! ----------------------------------------------------------
