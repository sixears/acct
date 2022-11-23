{-| Map from account name to a list of that account's transactions. -}
module Acct.OAcctMap
  ( OAcctMap, addTrx )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.Monoid  ( Monoid )
import GHC.Exts     ( IsList( toList ) )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- lens --------------------------------

import Control.Lens.At  ( At( at ), Index, Ixed( ix ), IxValue )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- tasty-plus --------------------------

import TastyPlus    ( assertListEqIO )
import TastyPluser  ( TestCmp'( testCmp' ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Acct.OStmtMap  as  OStmtMap

import Acct.OStmt      ( oAcct, oIndex )
import Acct.OStmtMap   ( OStmtMap )
import Acct.OStmtName  ( OStmtName )
import Acct.TrxSimp    ( TrxSimp, oStmtGetY )

--------------------------------------------------------------------------------

newtype OAcctMap = OAcctMap { unOAcctMap ∷ Map.Map OStmtName OStmtMap }
  deriving (Eq,Monoid,Semigroup,Show)

type instance Index   OAcctMap = OStmtName
type instance IxValue OAcctMap = OStmtMap

instance Ixed OAcctMap where
  ix k f (OAcctMap m) =  OAcctMap ⊳ ix k f m

instance At OAcctMap where
  at k f (OAcctMap m) = OAcctMap ⊳ Map.alterF f k m

--------------------

instance IsList OAcctMap where
  type instance Item OAcctMap = (OStmtName,OStmtMap)
  fromList xs = OAcctMap $ fromList xs
  toList (OAcctMap xs) = toList xs

--------------------

instance Printable OAcctMap where
  print smap = P.text $ [fmt|%w|] (unOAcctMap smap)

--------------------

instance TestCmp' OAcctMap where
  testCmp' nm (OAcctMap oam) oam' =
    testGroup nm $
      let
        ks  = Map.keys oam
        ks' = Map.keys ∘ unOAcctMap ⊳ oam'
        chk (k,v) =
          testCmp' ([fmt|OAcctMap %T|] k) v ((Map.! k) ∘ unOAcctMap <$> oam')
      in
        ю [ [ assertListEqIO "OAcctMap names" ks ks' ]
          , (fmap chk (Map.toList oam))
          ]

----------------------------------------

addTrx ∷ TrxSimp → OAcctMap → OAcctMap
addTrx t o@(OAcctMap m) =
  case oStmtGetY t of
    𝕹     → o
    𝕵 ost → let c = ost ⊣ oAcct
                i = ost ⊣ oIndex
            in  case c `Map.lookup` m of
                  𝕹     → OAcctMap $ fromList [(c,OStmtMap.addTrx ф t i)]
--                  𝕵 osm → insert (ost ⊣ oAcct) (OStmtMap.addTrx osm t i) o
                  𝕵 osm → at (ost ⊣ oAcct) ⊩ (OStmtMap.addTrx osm t i) $ o

-- that's all, folks! ----------------------------------------------------------
