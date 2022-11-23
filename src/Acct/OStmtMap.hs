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

import TastyPlus  ( assertListEqIO' )
import TastyPluser  ( TestCmp'( testCmp' ) )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.OStmtIndex  ( OStmtIndex )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

newtype OStmtMap = OStmtMap { unOStmtMap ∷ Map.Map OStmtIndex [TrxSimp] }
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

instance TestCmp' OStmtMap where
  testCmp' nm (OStmtMap osm) osm' =
    testGroup nm $
      assertListEqIO' (pack ∘ show) ("OStmtMap keys" ∷ 𝕋)
                      (Map.keys osm) (Map.keys ∘ unOStmtMap ⊳ osm')
      :
      (fmap (\ oa → assertListEqIO' (pack ∘ show)
                                    ([fmtT|OStmtMap %w keys|] oa)
                                    (osm Map.! oa)
                                    ((Map.! oa) ∘ unOStmtMap ⊳ osm')
            )
            (Map.keys osm)
      )

----------------------------------------

{-| Insert a trx into an OStmtMap -}
addTrx ∷ OStmtMap → TrxSimp → OStmtIndex → OStmtMap
addTrx (OStmtMap m) t i =
  let f 𝕹      = 𝕵 [t]
      f (𝕵 ts) = 𝕵 (t:ts)
   in OStmtMap $ Map.alter f i m

-- that's all, folks! ----------------------------------------------------------
