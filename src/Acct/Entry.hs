{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

{-| An `Entry` is a Comment, Account Start, Other Statement Start,
    Simple Trx or Broken Down Trx; allied with a source position.
-}
module Acct.Entry
  ( Entry( TrxComment, TAcctStart, TOStmtStart, TSimpleTrx, TBrk ), tests )
where

import Base1T

-- base --------------------------------

import Data.Char      ( isAscii, isPrint, isSpace )
import Data.Function  ( flip )
import Data.List      ( filter )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( satisfy, string )
import Text.Parser.Combinators  ( try )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( elements, listOf, oneof, suchThat )

-- tasty-plus --------------------------

import TastyPlus    ( (≟), propInvertibleString, propInvertibleText )
import TastyPluser  ( shrinkText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck      ( testProperty )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( empty, intercalate, pack, unwords )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus  ( testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), declare, isValid )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account      ( Account, acct )
import Acct.Comment      ( cmt )
import Acct.Date         ( dte )
import Acct.OStmtName    ( OStmtName, ostmtname )
import Acct.Parser       ( wspaces )
import Acct.Stmt         ( stmt )
import Acct.TrxBrkHead   ( tbh_ )
import Acct.TrxSimp      ( TrxSimp, tsimp_ )
import Acct.TrxBrk       ( TrxBrk, trxBrk )

--------------------------------------------------------------------------------

{- | A comment, account start, other account start, simple or broken-down
     transaction; so, roughly, a "line" of an input file (albeit several lines
     for a broken-down transaction.
-}
data Entry = TrxComment 𝕋
           | TAcctStart Account
           | TOStmtStart OStmtName
           | TSimpleTrx TrxSimp
           | TBrk TrxBrk
  deriving (Eq,Show)

--------------------

instance Validity Entry where
  validate (TrxComment t)  = declare "does not start with whitespace" $
                               t ≡ empty ∨ ﬧ (isSpace $ Text.head t)
  validate (TAcctStart a)  = validate a
  validate (TOStmtStart c) = validate c
  validate (TSimpleTrx a)  = validate a
  validate (TBrk a)        = validate a

--------------------

instance GenValid Entry where
  genValid    = let
                  asciiPrint = [ ' ' .. '~' ] -- ascii 32--126
                  genComment = TrxComment ∘ pack ⊳ listOf (elements asciiPrint)
                  genOStmt   = TOStmtStart ⊳ arbitrary
                  genEntry   = oneof [ genComment
                                     , TAcctStart ⊳ arbitrary
                                     , genOStmt
                                     , TSimpleTrx ⊳ arbitrary
                                     , TBrk ⊳ arbitrary
                                     ]
                in
                  genEntry `suchThat` isValid
  shrinkValid (TrxComment t)  = filter isValid $ TrxComment ⊳ shrinkText t
  shrinkValid (TOStmtStart _) = []
  shrinkValid (TAcctStart a)  = TAcctStart ⊳ shrinkValid a
  shrinkValid (TSimpleTrx a)  = TSimpleTrx ⊳ shrinkValid a
  shrinkValid (TBrk a)        = TBrk ⊳ shrinkValid a

--------------------

instance Arbitrary Entry where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Entry where
  print (TrxComment    t) = P.text $ [fmt|-- %t|]      t
  print (TAcctStart  t)   = P.text $ [fmt|Start: %T|]  t
  print (TOStmtStart c)   = P.text $ [fmt|oStart: %T|] c
  print (TSimpleTrx t)    = print t
  print (TBrk t)          = print t

----------

printTests ∷ TestTree
printTests =
  testGroup "print"
    [ testCase "TrxComment" $
      "-- a comment" ≟ toText (TrxComment "a comment")
    , testCase "TAcctStart" $
        "Start: Quux" ≟ toText (TAcctStart [acct|Quux|])
    , testCase "TOStmtStart" $
        "oStart: J" ≟ toText (TOStmtStart [ostmtname|J|])
    , testCase "TSimpleTrx" $
          "10.13+\t#D<6.iix.96>A<Acme>X<5>"
        ≟ toText (TSimpleTrx (tsimp_ 1013 [dte|1996-8-6|] [acct|Acme|]
                                          (𝕵 [stmt|5|]) 𝕹 𝕹))
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
                           ] ≟ toText (TBrk $ h (t1 :| [t2]))
    ]

--------------------------------------

instance Textual Entry where
  textual =
    let
      printable = satisfy (\ c → isPrint c ∧ isAscii c)
    in  -- e.g., "-- This is a comment" or ""
        TrxComment ∘ pack ⊳ (wspaces ⋫ string "-- " ⋫ wspaces ⋫ many printable)
        -- e.g., "Start: CarFund"
      ∤ TAcctStart ⊳ (string "Start:" ⋫ wspaces ⋫ textual)
        -- e.g., "oStart: P"
      ∤ TOStmtStart ⊳ (string "oStart:" ⋫ wspaces ⋫ textual)
        -- e.g., "10.13+\t#D<6.viii.96>A<Bills>X<5>"
        -- we need the `try` to allow back-tracking for the TBrk
      ∤ try (TSimpleTrx ⊳ textual)
        -- e.g.,
        -- >   10.13+\t#D<6.iix.96>B<>X<5>
        -- >   #10.00+\t#D<4.vi.96>A<Foo>X<5>
        -- >   #0.13+\t#D<5.i.96>A<Bar>X<5>
        -- >   ##
      ∤ TBrk ⊳ textual

------------------------------------------------------------

{- | An `Entry` may be a transaction (simple or broken-down), an Account
     start or an OtherAccount start, or a `Comment`.
 -}
parseTests ∷ TestTree
parseTests =
  let
    testParse' = flip testParse
    unline     = intercalate "\n"
  in
    testGroup "parse"
              [ testParse "--  bar "    $ TrxComment "bar "
              , testParse "  -- bar"    $ TrxComment "bar"
              , testParse "Start: Acct" $ TAcctStart [acct|Acct|]
              , testParse "oStart: Y"   $ TOStmtStart [ostmtname|Y|]
              , let
                  t = tsimp_ 1013 [dte|1996-8-6|] [acct|Bill|] (𝕵 [stmt|5|])
                                  𝕹 𝕹
                in
                  testParse "10.13+ #D<6.viii.96>A<Bill>X<5>" $ TSimpleTrx t

              , let
                  t' = tsimp_ (-628) [dte|1996-8-8|] [acct|CarFund|]
                              (𝕵 [stmt|5|]) 𝕹 (𝕵 [cmt|int to 8 Aug|])
                  t = TSimpleTrx t'
                in
                  testParse "6.28- #D<8.VIII.96>A<CarFund>C<int to 8 Aug>X<5>" t

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

              , testParseE (unline [ "10.13+ #D<6.viii.96>B<>X<5>"
                                   , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                   , "#0.31+ #D<5.i.96>X<5>A<Bar>"
                                   , "##"
                                   ])
                           (tParse' @Entry)
                           (unwords ["unexpected breakdown total"
                                    ,"was 10.31+, expected 10.13+"
                                    ])
              , testProperty "invertibleString" (propInvertibleString @Entry)
              , testProperty "invertibleText" (propInvertibleText @Entry)
              ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Entry" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
