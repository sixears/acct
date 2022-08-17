{-| Map from account name to a list of that account's transactions. -}
module Acct.OStmtMap
  ( OStmtMap, addTrx )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.Monoid  ( Monoid )
import GHC.Exts     ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- lens --------------------------------

import Control.Lens.At  ( At( at ), Index, Ixed( ix ), IxValue )

-- tasty-plus --------------------------

import TastyPlus  ( assertListEqS )
import TastyPluser  ( TestCmp( testCmp ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.OStmtIndex  ( OStmtIndex )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

newtype OStmtMap = OStmtMap (Map.Map OStmtIndex [TrxSimp])
  deriving (Eq,Monoid,Semigroup,Show)

type instance Index   OStmtMap = OStmtIndex
type instance IxValue OStmtMap = [TrxSimp]

instance Ixed OStmtMap where
  ix k f (OStmtMap m) =  OStmtMap ⊳ ix k f m

instance At OStmtMap where
  at k f (OStmtMap m) = OStmtMap ⊳ Map.alterF f k m

--------------------

instance IsList OStmtMap where
  type instance Item OStmtMap = (OStmtIndex,[TrxSimp])
  fromList xs = OStmtMap $ fromList xs
  toList (OStmtMap xs) = toList xs

--------------------

{-
instance HasMember OStmtMap where
  type MemberType OStmtMap = OStmtIndex
  member k (OStmtMap m) = Map.member k m
-}

--------------------

{-
instance Mapish OStmtMap where
  type Key   OStmtMap = OStmtIndex
  type Value OStmtMap = [TrxSimp]
  adjust f k (OStmtMap m) = OStmtMap (Map.adjust f k m)
  empty                   = OStmtMap ф
  insert k v (OStmtMap m) = OStmtMap (Map.insert k v m)
-}

--------------------

instance TestCmp OStmtMap where
  testCmp nm (OStmtMap osm) (OStmtMap osm') =
    testGroup nm $
      let
        ks  = Map.keys osm
        ks' = Map.keys osm'
        vs  ∷ Map.Map OStmtIndex ([TrxSimp],[TrxSimp])
        vs  = Map.intersectionWith (,) osm osm'
      in
        assertListEqS "OStmtMap keys" ks' ks
        ⊕
        ю (fmap (\ (oa,(oks,oks')) →
                   assertListEqS ([fmt|OStmtMap %w keys|] oa) (oks') (oks))
           (Map.toList vs)
          )

----------------------------------------

{-| Insert a trx into an OStmtMap -}
addTrx ∷ OStmtMap → TrxSimp → OStmtIndex → OStmtMap
addTrx (OStmtMap m) t i =
  let f 𝕹      = 𝕵 [t]
      f (𝕵 ts) = 𝕵 (t:ts)
   in OStmtMap $ Map.alter f i m

-- that's all, folks! ----------------------------------------------------------
