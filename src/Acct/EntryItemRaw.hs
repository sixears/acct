{-| An `EntryItemRaw` is an entry item parsed direct from a file, un-processed;
    thus including acct management & imports. -}
module Acct.EntryItemRaw
  ( EntryItemRaw(..), tests )
where

import Base1T

-- base --------------------------------

import Data.Function  ( flip )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- fpath -------------------------------

import FPath.AbsFile  ( absfile )
import FPath.File     ( File( FileA, FileR ) )
import FPath.RelFile  ( relfile )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char  ( text )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( oneof )

-- tasty-plus --------------------------

import TastyPlus    ( (≟) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( intercalate, unwords )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' )
                    , propInvertibleString, propInvertibleText )

-- trifecta-plus -----------------------

import TrifectaPlus  ( testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account      ( acct )
import Acct.Date         ( dte )
import Acct.EntryItem    ( EntryItem( TAcctStart, TTEntry, TOStmtStart ) )
import Acct.OStmt        ( ostmt )
import Acct.OStmtName    ( ostmtname )
import Acct.Parser       ( wspaces )
import Acct.SComment     ( scmt )
import Acct.Stmt         ( stmt )
import Acct.TComment     ( tcmt )
import Acct.TEntry       ( TEntry( SheetComment, TBrk, TSimpleTrx ) )
import Acct.TrxBrkHead   ( tbh_ )
import Acct.TrxSimp      ( tsimp_ )
import Acct.TrxBrk       ( trxBrk )

--------------------------------------------------------------------------------

{- | A comment, account start, other account start, simple or broken-down
     transaction; so, roughly, a "line" of an input file (albeit several lines
     for a broken-down transaction.
-}
data EntryItemRaw  = EIIEI   EntryItem -- EntryItemRaw EntryItem
                 | TImport File
  deriving (Eq,Show)

--------------------

instance Validity EntryItemRaw where
  validate (EIIEI   ei) = validate ei
  validate (TImport f)  = trivialValidation f

--------------------

instance GenValid EntryItemRaw where
  genValid = oneof [ EIIEI   ⊳ arbitrary
                   , TImport ⊳ arbitrary
                   ]

  shrinkValid (EIIEI   ei) = EIIEI ⊳ shrinkValid ei
  shrinkValid (TImport f)  = TImport ⊳ shrink f

--------------------

instance Arbitrary EntryItemRaw where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable EntryItemRaw where
  print (EIIEI   ei) = print ei
  print (TImport f)  = P.text $ [fmt|import: %T|] f

----------

printTests ∷ TestTree
printTests =
  testGroup "print"
    [ testCase "SheetComment" $
        "% a comment" ≟ toText (EIIEI ∘ TTEntry $ SheetComment [scmt|% a comment|])
    , testCase "TAcctStart" $
        "Start: Quux" ≟ toText (EIIEI (TAcctStart [acct|Quux|]))

    , testCase "TOStmtStart" $
        "oStart: J" ≟ toText (EIIEI (TOStmtStart [ostmtname|J|]))
    , testCase "TImport" $
        "import: foo/bar" ≟ toText (TImport $ FileR [relfile|foo/bar|])
    , testCase "TSimpleTrx" $
          "10.13+\t#D<6.iix.96>A<Acme>X<5>"
        ≟ toText (EIIEI ∘ TTEntry $ TSimpleTrx (tsimp_ 1013 [dte|1996-8-6|]
                                                       [acct|Acme|] (𝕵 [stmt|5|])
                                                       𝕹 𝕹
                                               )
                 )
    , testCase "TBrk" $
        let
          h  = trxBrk (tbh_ 1013 [dte|1996-8-6|] (𝕵 [stmt|5|]) 𝕹 𝕹)
          t1 = tsimp_ 1000 [dte|1996-6-4|] [acct|Foo|] (𝕵 [stmt|5|]) 𝕹 𝕹
          t2 = tsimp_ 13 [dte|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|]) 𝕹 𝕹
        in
          intercalate "\n" [ "10.13+\t#D<6.iix.96>B<>X<5>"
                           , "#10.00+\t#D<4.vi.96>A<Foo>X<5>"
                           , "#0.13+\t#D<5.i.96>A<Bar>X<5>"
                           , "##"
                           ] ≟ toText (EIIEI ∘ TTEntry ∘ TBrk $ h (t1 :| [t2]))
    ]

--------------------------------------

instance TextualPlus EntryItemRaw where
  textual' = TImport ⊳ (text "import:" ⋫ wspaces ⋫ textual) ∤ EIIEI ⊳ textual'

------------------------------------------------------------

{- | An `Entry` may be a transaction (simple or broken-down), an Account
     start or an OtherAccount start, or a `TComment`.
 -}
parseTests ∷ TestTree
parseTests =
  let
    testParse'    = flip testParse
    unline        = intercalate "\n"
    testParseE' a = testParseE a (tParse' @EntryItem)
  in
    testGroup "parse"
              [ testParse "%  bar "     $ EIIEI (TTEntry (SheetComment [scmt|%  bar |]))
              , testParse "  % bar"     $ EIIEI (TTEntry (SheetComment [scmt|  % bar|]))
              , testParse ""            $ EIIEI (TTEntry (SheetComment [scmt||]))
              , testParse "  "          $ EIIEI (TTEntry (SheetComment [scmt|  |]))
              , testParse "Start: Acct"      $ EIIEI (TAcctStart [acct|Acct|])
              , testParse "oStart: Y"        $ EIIEI (TOStmtStart [ostmtname|Y|])
              , testParse "import: /tmp/foo" $ TImport (FileA [absfile|/tmp/foo|])

              , testParse "200+ #D<2.xi.11>A<Act>"
                          (EIIEI ∘ TTEntry ∘ TSimpleTrx $ tsimp_ 20000 [dte|2011-11-02|]
                                                                 [acct|Act|] 𝕹 𝕹 𝕹)

              --------------------------

              , let
                  t = tsimp_ 1013 [dte|1996-8-6|] [acct|Bill|] (𝕵 [stmt|5|])
                                  𝕹 𝕹
                in
                  testParse "10.13+ #D<6.viii.96>A<Bill>X<5>" $
                    EIIEI (TTEntry (TSimpleTrx t))

              --------------------------

              , let
                  t' = tsimp_ (-628) [dte|1996-8-8|] [acct|CarFund|]
                              (𝕵 [stmt|5|]) 𝕹 (𝕵 [tcmt|int to 8 Aug|])
                  t = EIIEI ∘ TTEntry $ TSimpleTrx t'
                in
                  testParse "6.28- #D<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>" t

              --------------------------

              , let
                  h = tbh_ 1013 [dte|1996-8-6|] (𝕵 [stmt|5|]) 𝕹 𝕹
                  t1 = tsimp_ 1000 [dte|1996-6-4|] [acct|Foo|] (𝕵[stmt|5|])
                                   𝕹 𝕹
                  t2 = tsimp_ 13 [dte|1996-1-5|] [acct|Bar|] (𝕵 [stmt|5|])
                                 𝕹 𝕹
                  t = TBrk (trxBrk h (t1 :| [t2]))
                in testParse' t (unline [ "10.13+ #D<6.viii.96>B<>X<5>"
                                        , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                        , "#0.13+ #D<5.i.96>X<5>A<Bar>"
                                        , "##"
                                        ])

              --------------------------

              , testParseE' (unline [ "10.13+ #D<6.viii.96>B<>X<5>"
                                    , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                    , "#0.31+ #D<5.i.96>X<5>A<Bar>"
                                    , "##"
                                    ])
                            (unwords ["breakdown total"
                                     ,"was 10.31+, expected 10.13+"
                                     ])

              --------------------------

              , testParseE' "200+ #D<2.xi.11>A<act>"
                            "expected: uppercase letter"

              --------------------------

              , let
                  h = tbh_ (-2373843) [dte|2048-2-28|] (𝕵 [stmt|427510|])
                           (𝕵 [ostmt|X|]) (𝕵 [tcmt|éçñ|])
                  t1 = tsimp_ (-2373843) [dte|2041-3-31|] [acct|GX|]
                              (𝕵 [stmt|427510|]) (𝕵 [ostmt|R:370047|]) 𝕹
                  t = TBrk (trxBrk h (t1 :| []))
                  l1 = "23,738.43-\t#D<28.ii.48>B<>X<427510>O<X>C<éçñ>"
                  l2 = "#23,738.43-\t#D<31.iii.41>A<GX>X<427510>O<R:370047>"
                in testParse' t (unline [ l1, l2, "##" ])

              , testProperty "invertibleString"
                              (propInvertibleString @EntryItemRaw)
              , testProperty "invertibleText" (propInvertibleText @EntryItemRaw)
              ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.EntryItemRaw" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
