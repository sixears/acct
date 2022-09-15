{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

{-| Map from account name to a list of that account's transactions. -}
module Acct.StmtMap
  ( StmtMap )
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

import Acct.StmtIndex    ( StmtIndex )
import Acct.StmtEntries  ( StmtEntries )

--------------------------------------------------------------------------------

newtype StmtMap = StmtMap (Map.Map StmtIndex StmtEntries)
  deriving         (Eq,Show)
  deriving newtype (Monoid,Semigroup)

type instance Index   StmtMap = StmtIndex
type instance IxValue StmtMap = StmtEntries

instance Ixed StmtMap where
  ix k f (StmtMap m) =  StmtMap ⊳ ix k f m

instance At StmtMap where
  at k f (StmtMap m) = StmtMap ⊳ Map.alterF f k m

--------------------

instance IsList StmtMap where
  type instance Item StmtMap = (StmtIndex,StmtEntries)
  fromList xs = StmtMap $ fromList xs
  toList (StmtMap xs) = toList xs

--------------------

instance TestCmp StmtMap where
  testCmp nm (StmtMap am) (StmtMap am') =
    testGroup nm $ do
      let ks  = sort $ Map.keys am
          ks' = sort $ Map.keys am'
          vs = Map.intersectionWith (,) am am'
      ю [ [ assertListEq "stmt names" ks ks' ]
        , [ assertListEq ("stmt: " ⊕ toText k) (toList v) (toList v')
          | (k,(v,v')) ← Map.toList vs ]
        ]

-- that's all, folks! ----------------------------------------------------------
