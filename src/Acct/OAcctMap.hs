{-| Map from account name to a list of that account's transactions. -}
module Acct.OAcctMap
  ( OAcctMap, addTrx )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import GHC.Exts  ( IsList( toList ) )

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

import qualified  Acct.OStmtMap  as  OStmtMap

import Acct.Mapish     ( Mapish( Key, Value, adjust, empty, insert ) )
import Acct.OStmt      ( oAcct, oIndex )
import Acct.OStmtMap   ( OStmtMap )
import Acct.OStmtName  ( OStmtName )
import Acct.TrxSimp    ( TrxSimp, oStmtGetY )

--------------------------------------------------------------------------------

newtype OAcctMap = OAcctMap (Map.Map OStmtName OStmtMap)
  deriving (Eq,Show)

--------------------

instance IsList OAcctMap where
  type instance Item OAcctMap = (OStmtName,OStmtMap)
  fromList xs = OAcctMap $ fromList xs
  toList (OAcctMap xs) = toList xs

--------------------

instance HasMember OAcctMap where
  type MemberType OAcctMap = OStmtName
  member k (OAcctMap m) = Map.member k m

--------------------

instance Mapish OAcctMap where
  type Key OAcctMap = OStmtName
  type Value OAcctMap = OStmtMap
  adjust f k (OAcctMap m) = OAcctMap (Map.adjust f k m)
  empty                   = OAcctMap ф
  insert k v (OAcctMap m) = OAcctMap (Map.insert k v m)

--------------------

instance TestCmp OAcctMap where
  testCmp nm (OAcctMap oam) (OAcctMap oam') =
    testGroup nm $
      let
        ks  = Map.keys oam
        ks' = Map.keys oam'
        vs  ∷ Map.Map OStmtName (OStmtMap,OStmtMap)
        vs  = Map.intersectionWith (,) oam oam'
      in
        ю [ [ assertListEq "OAcctMap names" ks ks' ]
          , (fmap(\ (oa,(osm,osm')) → testCmp ([fmt|OAcctMap %T|] oa) osm osm')
                 (Map.toList vs))
          ]

----------------------------------------

addTrx ∷ TrxSimp → OAcctMap → OAcctMap
addTrx t o@(OAcctMap m) =
  case oStmtGetY t of
    𝕹     → o
    𝕵 ost → let c = ost ⊣ oAcct
                i = ost ⊣ oIndex
            in  case c `Map.lookup` m of
                  𝕹     → OAcctMap $ fromList [(c,OStmtMap.addTrx empty t i)]
                  𝕵 osm → insert (ost ⊣ oAcct) (OStmtMap.addTrx osm t i) o

-- that's all, folks! ----------------------------------------------------------
