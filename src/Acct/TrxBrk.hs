{-# LANGUAGE DeriveAnyClass #-}

{- | Broken-down transaction, e.g.,
            , testParse (intercalate "\n" [

@
        10.13+ #D<6.viii.96>B<>X<5>
        #10+   #D<4.vi.96>X<5>A<Foo>
        #0.13+ #D<5.i.96>X<5>A<Bar>
        ##
@

-}
module Acct.TrxBrk
  ( TrxBrk, hd, inferredStmt, isConsistent, prettyBrk, trx, trxBrk, tests )
where

import Base1T

-- base --------------------------------

import Data.Foldable  ( all )
import Data.Maybe     ( isJust )
import GHC.Generics   ( Generic )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- parsers -----------------------------

import Text.Parser.Char         ( char )
import Text.Parser.Combinators  ( Parsing, endByNonEmpty, unexpected )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, elements, listOf1 )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck      ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' ) )

-- trifecta-plus -----------------------

import TrifectaPlus    ( testParse, testParseE, tParse )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account      ( acct )
import Acct.Amount       ( HasAmount( amount ), aTotal )
import Acct.Date         ( HasDate( date ), dte )
import Acct.OStmt        ( ostmt )
import Acct.Parser       ( newline, wspaces )
import Acct.Stmt         ( stmt, stmtY )
import Acct.StmtIndex    ( GetStmtIndex( stmtIndexGet ), StmtIndex, stmtindex )
import Acct.TrxBrkHead   ( TrxBrkHead, tbh_ )
import Acct.TrxSimp      ( TrxSimp, parent, tsimp_ )
import Acct.Util         ( Pretty( pretty ) )

--------------------------------------------------------------------------------

data TrxBrk = TrxBrk { _hd  ∷ TrxBrkHead
                     , _trx ∷ (NonEmpty TrxSimp) }
  deriving (Eq,Generic,NFData,Show)

--------------------

instance Validity TrxBrk where
  validate (TrxBrk h ts) =
    let totalsMatch = declare "amount totals sum" $
                        h ⊣ amount ≡ aTotal ts
    in  totalsMatch

--------------------

instance GenValid TrxBrk where
  genValid    = do
    let listOf1' ∷ Gen α → Gen (NonEmpty α)
        listOf1' g = fromList ⊳ listOf1 g
    ts ← listOf1' arbitrary
    h ← tbh_ ⊳ pure (aTotal ts) ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary
    ts' ← forM ts (\ t → do
                      -- set each sub-trx to have parent h, and either the same
                      -- X<stmt> or no X<stmt>
                      x ← elements [𝕹, h ⊣ stmtY]
                      return $ t & parent ⊩ h & stmtY ⊢ x)
    return $ TrxBrk h ts'

  shrinkValid _ = []

--------------------

instance Arbitrary TrxBrk where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable TrxBrk where
  print (TrxBrk h tx) =
    P.text $ [fmt|%T\n%t\n##|] h
                               (intercalate "\n" ∘ toList $ ("#"⊕) ∘toText ⊳ tx)

----------

printTests ∷ TestTree
printTests =
  testGroup "print"
    [ testCase "10.13+" $
        let
          h  = tbh_ 1013 [dte|1996-8-6|] (𝕵 [stmt|5|]) 𝕹 𝕹
          t1 = tsimp_ 1000 [dte|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|])
                           (𝕵 [ostmt|X:6|]) 𝕹
                      & parent ⊩ h
          t2 = tsimp_ 13 [dte|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|]) 𝕹 𝕹
                      & parent ⊩ h
          b  = TrxBrk h (t1 :| [t2])
        in
          intercalate "\n" [ "10.13+\t#D<6.iix.96>B<>X<5>"
                           , "#10.00+\t#D<4.vi.96>A<Foo>X<5>O<X:6>"
                           , "#0.13+\t#D<5.i.96>A<Bar>X<5>"
                           , "##"
                           ] ≟ toText b
    ]

--------------------

instance Pretty TrxBrk where
  pretty (TrxBrk h _) = pretty h

--------------------

instance Textual TrxBrk where
  textual =
    let
      nlhash = newline ⋪ char '#'

      check_breakdown ∷ (Monad η, Parsing η) ⇒ TrxBrk → η TrxBrk
      check_breakdown b@(TrxBrk h ts) = do
        let h_am  = h ⊣ amount
            ts_am = aTotal ts
        if h_am ≡ ts_am
        then return b
        else unexpected $ [fmt|breakdown total was %T, expected %T|] ts_am h_am

      -- If sub-members of a breakdown have an X<…> marker, it must match the
      -- X<…> of the breakdown header.
      -- Else, all sub-members must have the same marker or none

      check_stmt ∷ (Monad η, Parsing η) ⇒ TrxBrk → η TrxBrk
      check_stmt b@(TrxBrk h (t:|ts)) = do
        let h_x ∷ StmtIndex
            h_x = stmtIndexGet h -- header stmt
            e_msg e e' =
              unexpected $ [fmt|inconsistent X<…> markers:\n#%T\n#%T|] e e'
        case h_x ⊣ stmtY of
          𝕹   → do -- all sub-members must have the same X<…> marker as each
                    -- other
                let t_x = stmtIndexGet t
                forM_ ts (\ t' → if t_x ≡ stmtIndexGet t'
                                 then return ()
                                 else e_msg t t'
                         )
                return b
          𝕵 _ → do -- all sub-members must have the same X<…> marker, or none
                forM_ (toList (t:|ts))
                      (\ t' → if stmtIndexGet t' ∈ [h_x, [stmtindex||]]
                              then return ()
                              else e_msg h t'
                      )
                return b

      set_parents (h,ts) = TrxBrk h $ (& parent ⊩ h) ⊳ ts
    in
      ((,) ⊳ (textual ⋪ nlhash)
           ⊵ ((wspaces ⋫ textual) `endByNonEmpty` nlhash)
           ⋪ char '#' ⋪ wspaces
      ) ≫ return ∘ set_parents ≫ check_breakdown ≫ check_stmt

instance TextualPlus TrxBrk where
  textual' = textual

----------

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ let
                h  = tbh_ 1013 [dte|1996-8-6|] (𝕵 [stmt|5|]) (𝕵 [ostmt|X|]) 𝕹
                t1 = tsimp_ 1000 [dte|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|])
                                 𝕹 𝕹
                            & parent ⊩ h
                t2 = tsimp_ 13 [dte|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|])
                               (𝕵 [ostmt|P|]) 𝕹
                            & parent ⊩ h
              in
                testParse (intercalate "\n" [ "10.13+ #D<6.viii.96>B<>X<5>O<X>"
                                            , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                            , "#0.13+ #D<5.i.96>X<5>A<Bar>O<P>"
                                            , "##"
                                            ]) $ TrxBrk h (t1 :| [t2])
            , testParseE (intercalate "\n" [ "10.13+ #D<6.viii.96>B<>X<5>"
                                           , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                           , "#1.30+ #D<5.i.96>X<5>A<Bar>"
                                           , "##"
                                           ])
                         (tParse @TrxBrk)
                         (ю [ "unexpected breakdown total "
                            , "was 11.30+, expected 10.13+" ])
            , testParseE (intercalate "\n" [ "2+ #D<1.i.73>B<>X<5>"
                                           , "#1+   #D<4.vi.96>X<5>A<Foo>"
                                           , "#1+ #D<5.i.96>X<6>A<Bar>"
                                           , "##"
                                           ])
                         (tParse @TrxBrk)
                         (ю [ "unexpected inconsistent X<…> markers: " ])
            , testProperty "invertibleString" (propInvertibleString @TrxBrk)
            , testProperty "invertibleText" (propInvertibleText @TrxBrk)
            ]

--------------------

instance HasAmount TrxBrk where
  amount = hd ∘ amount

--------------------

instance HasDate TrxBrk where
  date = hd ∘ date

--------------------

instance GetStmtIndex TrxBrk where
  stmtIndexGet = stmtIndexGet ∘ view hd

----------------------------------------

hd ∷ Lens' TrxBrk TrxBrkHead
hd = lens _hd (\ t h → t { _hd = h })

----------------------------------------

trxBrk ∷ TrxBrkHead → NonEmpty TrxSimp → TrxBrk
trxBrk h ts = TrxBrk h $ (& parent ⊩ h) ⊳ ts

----------------------------------------

trx ∷ TrxBrk → NonEmpty TrxSimp
trx (TrxBrk _ ts) = ts

----------------------------------------

{-| If the head trx has a stmtindex, then that.
    Else if /every/ sub-trx has the same stmtindex, then that. -}
inferredStmt ∷ TrxBrk → 𝕄 StmtIndex
inferredStmt (TrxBrk h (t:|ts)) =
  let h_i = stmtIndexGet h
  in  case h_i ⊣ stmtY of
        𝕵 _ → 𝕵 h_i
        𝕹   → if all (≡ stmtIndexGet t) (stmtIndexGet ⊳ ts)
              then 𝕵 $ stmtIndexGet t
              else 𝕹

----------------------------------------

{-| A broken-down transaction is "Consistent" if either
    - the head transaction has a stmt marker, or
    - every sub-transaction has the same stmt marker.

    Thus a "consistent" broken-down transaction may be safely output as part
    of a statement output.
 -}
isConsistent ∷ TrxBrk → 𝔹
isConsistent = isJust ∘ inferredStmt

----------------------------------------

{-| Convert a full trx breakdown, with all its sub-trx, to text suitable for
    re-ingestion. -}
prettyBrk ∷ TrxBrk → 𝕋
prettyBrk (TrxBrk h ts) =
  Text.unlines $ (pretty h : ((("#" ⊕) ∘ pretty) ⊳ toList ts)) ⊕ ["##"]

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TrxBrk" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
