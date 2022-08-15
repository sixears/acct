{-| Map from account name to a list of that account's transactions. -}
module Acct.StmtMap
  ( StmtMap )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.List  ( sort )
import GHC.Exts   ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( MemberType, member ) )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEq )
import TastyPluser  ( TestCmp( testCmp ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Mapish     ( Mapish( Key, Value, adjust, empty, insert ) )
import Acct.StmtIndex  ( StmtIndex )
import Acct.TrxSimp    ( TrxSimp )

--------------------------------------------------------------------------------

newtype StmtMap = StmtMap (Map.Map StmtIndex [TrxSimp])
  deriving (Eq,Show)

--------------------

instance IsList StmtMap where
  type instance Item StmtMap = (StmtIndex,[TrxSimp])
  fromList xs = StmtMap $ fromList xs
  toList (StmtMap xs) = toList xs

--------------------

instance Mapish StmtMap where
  type Key StmtMap = StmtIndex
  type Value StmtMap = [TrxSimp]
  adjust f k (StmtMap m) = StmtMap (Map.adjust f k m)
  empty                  = StmtMap ф
  insert k v (StmtMap m) = StmtMap (Map.insert k v m)

--------------------

instance TestCmp StmtMap where
  testCmp nm (StmtMap am) (StmtMap am') =
    testGroup nm $ do
      let ks  = sort $ Map.keys am
          ks' = sort $ Map.keys am'
          vs = Map.intersectionWith (,) am am'
      ю [ [ assertListEq "account names" ks ks' ]
        , [ assertListEq ("account: " ⊕ toText k) v v'
          | (k,(v,v')) ← Map.toList vs ]
        ]

--------------------

instance HasMember StmtMap where
  type MemberType StmtMap = StmtIndex
  member k (StmtMap m) = Map.member k m

-- that's all, folks! ----------------------------------------------------------
