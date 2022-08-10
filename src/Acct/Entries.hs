{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

{-| An `Entries` is a list of `Entry`s. -}
module Acct.Entries
  ( Entry, Entries( unEntries ), emptyEntries
  , tests )
where

import Debug.Trace  ( traceShow, trace )
import Base1T  hiding  ( (∈) )
import Prelude  ( fromIntegral )

-- base --------------------------------

import qualified  Data.List  as  List

import Control.Applicative    ( optional, pure )
import Control.Monad.Fail     ( MonadFail, fail )
import Data.Char              ( isAscii, isPrint, isSpace, showLitChar )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq )
import Data.Function          ( ($) )
import Data.Functor.Identity  ( Identity )
import Data.List              ( filter, inits, tails, zip )
import Data.List.NonEmpty     ( NonEmpty( (:|) ) )
import Data.Maybe             ( Maybe( Just, Nothing ), catMaybes )
import Data.Tuple             ( uncurry )
import GHC.Stack              ( callStack )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.Show              ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Numeric.Natural.Unicode  ( ℕ )

-- containers --------------------------

import qualified  Data.Map.Lazy  as  Map
import Data.Set  ( Set, member, insert )

-- containers-plus ---------------------

import ContainersPlus.Member  ( (∈) )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- lens --------------------------------

import Control.Lens.Getter  ( use, uses, view )
import Control.Lens.Setter  ( (%=) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.State  ( MonadState, gets, modify, runStateT )

-- parsec ------------------------------

import Text.Parsec.Error       ( Message( UnExpect ), newErrorMessage )
import Text.Parsec.Prim        ( Parsec, Stream, (<?>), getPosition )
import Text.Parsec.Pos         ( SourcePos, newPos, sourceLine, sourceName )

-- parsec-plus-base --------------------

import Parsec.Error  ( ParseError( ParseError ) )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), parsec )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing
                                , char, oneOf, satisfy, string, upper )
import Text.Parser.Combinators  ( endBy, endByNonEmpty, eof, try, sepBy
                                , sepEndBy, unexpected )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( Gen, elements, listOf, oneof, suchThat )

-- safe --------------------------------

import Safe  ( initSafe, tailSafe )

-- tasty-plus --------------------------

import TastyPlus    ( (≟) , assertListEq, assertListEqS, propInvertibleString
                    , propInvertibleText )
import TastyPluser  ( shrinkText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck      ( testProperty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), assertFailure, testCase )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( empty, intercalate, pack, unlines, unpack, unwords )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- trifecta ----------------------------

import qualified  Text.Trifecta  as  Trifecta
import Text.Trifecta  ( Result( Failure, Success ), _Failure, _Success )

-- trifecta-plus -----------------------

import TrifectaPlus  ( eiText, testParse, testParse', testParseE, tname,tParse )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account, HasAccount( account ), acct )
import Acct.AcctState   ( AcctState, TestCmp( testCmp ), accounts, addToAcct
                        , newAcctState, otherAccounts, parseEntry, startAcct )
import Acct.Amount      ( Amount, amt )
import Acct.Comment     ( Comment, cmt )
import Acct.Date        ( Date, date )
import Acct.Entry       ( Entry( TAcctStart, TBrk, TrxComment, TOStmtStart
                               , TSimpleTrx ) )
import Acct.OStmt       ( OStmt, ostmt )
import Acct.Parser      ( newline, noNLCR, wspaces )
import Acct.Stmt        ( Stmt, stmt )
import Acct.TrxBrkHead  ( TrxBrkHead, tbh_ )
import Acct.TrxSimp     ( TrxSimp, tsimp_ )
import Acct.TrxBrk      ( TrxBrk( TrxBrk ) )

--------------------------------------------------------------------------------

newtype Entries = Entries { unEntries ∷ [Entry] }
  deriving (Eq,Show)

emptyEntries ∷ Entries
emptyEntries = Entries []

{- | A comment, account start, other account start, simple or broken-down
     transaction; so, roughly, a "line" of an input file (albeit several lines
     for a broken-down transaction.
-}
instance Validity Entries where
  validate = trivialValidation

--------------------

instance GenValid Entries where
  genValid    = Entries ⊳ listOf arbitrary
  shrinkValid (Entries es) =
    Entries ∘ uncurry (⊕) ⊳ zip (initSafe $ inits es) (tailSafe $ tails es)

--------------------

instance Arbitrary Entries where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Entries where
  print (Entries es) = P.text $ intercalate "\n" (toText ⊳ es)

----------

printTests ∷ TestTree
printTests =
  let
    unline = intercalate "\n"
    tcomm  = TrxComment "a comment"
    tacct  = TAcctStart [acct|Acct|]
    tost   = TOStmtStart 'Y'
    tsimp  = TSimpleTrx (tsimp_ (-1234) [date|1973-01-01|] [acct|Act|]
                                (𝕵 [stmt|77|]) 𝕹 (𝕵 [cmt|my comment|]))
    thead  = tbh_ (-1234) [date|1973-01-01|] (𝕵 [stmt|77|]) 𝕹
    t0     = tsimp_ (-1300) [date|1969-02-02|] [acct|Act|] 𝕹 𝕹 𝕹
    t1     = tsimp_ 66 [date|1966-05-26|] [acct|Bct|] 𝕹 𝕹 𝕹
    tb     = TBrk $ TrxBrk thead (t0 :| [t1])
  in
    testGroup "print"
      [ testCase "print1c" $ "-- a comment" ≟ toText (Entries [ tcomm ])
      , testCase "print1a" $ "Start: Acct" ≟ toText (Entries [ tacct ])
      , testCase "print1o" $ "oStart: Y" ≟ toText (Entries [ tost ])
      , testCase "print1s" $
            "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
          ≟ toText (Entries [ tsimp ])
      , testCase "print1b" $
            unline [ "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"]
          ≟ toText (Entries [ tb ])
      , testCase "print2" $
            unline [ "-- a comment"
                   , "Start: Acct"
                   , "oStart: Y"
                   , "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
                   , "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"
                   ]
          ≟ toText (Entries [ tcomm, tacct, tost, tsimp, tb ])
      , testCase "print3" $
            unline [ "12.34-\t#D<1.i.73>B<>X<77>"
                   , "#13.00-\t#D<2.ii.69>A<Act>"
                   , "#0.66+\t#D<26.v.66>A<Bct>"
                   , "##"
                   , "Start: Acct"
                   , "-- a comment"
                   , "12.34-\t#D<1.i.73>A<Act>X<77>C<my comment>"
                   , "oStart: Y"
                   ]
          ≟ toText (Entries [ tb, tacct, tcomm, tsimp, tost ])
    ]

--------------------------------------

instance Textual Entries where
  textual =
    let line = wspaces ⋫ many (char '\r') ⋫ char '\n'
     in Entries ⊳ (many line ⋫ textual `sepEndBy` some line)

------------------------------------------------------------

tParseTests ∷ TestTree
tParseTests =
  let
   in
    testGroup
      "tParse"
      [ testParse ""                       (Entries [])
      , testParse "-- comment"             (Entries [TrxComment "comment"])
      , testParse "-- comment\n"           (Entries [TrxComment "comment"])
      , testParse "-- com\n-- ment"        (Entries [ TrxComment "com"
                                                    , TrxComment "ment" ])
      , testParse "-- com\n-- ment\n"      (Entries [ TrxComment "com"
                                                    , TrxComment "ment" ])
      , testParse "-- comment\n\n"         (Entries [TrxComment "comment"])
      , testParse "\n-- comment"           (Entries [TrxComment "comment"])
      , testParse "\n\n-- comment\n\n"     (Entries [TrxComment "comment"])
      , testParse "-- comment\nStart: Foo" (Entries [ TrxComment "comment"
                                                    , TAcctStart [acct|Foo|] ])
      ]

----------------------------------------

-- XXX split up AcctState
-- XXX separate entries for oStmt :n
-- XXX factor out new oaccounts creation
parseString ∷ 𝕊 → Result ([Entry], AcctState)
parseString  =
  let line = wspaces ⋫ many (char '\r') ⋫ char '\n'
      p    = many line ⋫ parseEntry `sepEndBy` some line ⋪ eof
   in first catMaybes ⩺ Trifecta.parseString (runStateT p newAcctState) ф

--------------------

parseTests ∷ TestTree
parseTests = testGroup "parse" $
  let
    parseT ∷ HasCallStack ⇒ 𝕋 → ([Entry], AcctState) → TestTree
    parseT t (e,s) = -- testParse' (parseString ∘ unpack)
      case parseString $ unpack t of
        Success (e',s') →
          testGroup (tname t) $
--            [ testCmp "AcctState" s s' {- testCase "AcctState" $ s @=? s' -} ]
--            ⊕
            [ {- testGroup "AcctState.accounts" $
                assertListEqS "AcctState.accounts" (Map.toList (s ⊣ accounts))
                                                   (Map.toList (s' ⊣ accounts))
            , testGroup "AcctState.oaccounts" $
                assertListEqS "AcctState.oaccounts" (Map.toList (s ⊣ otherAccounts))
                                                   (Map.toList (s' ⊣ otherAccounts)) -}
              testCmp "AcctState" s s'
            ]
            ⊕
            [ assertListEq "[Entry]" e e' ]
        Failure e → testCase (tname t) $ assertFailure (unpack $ eiText e)

    parseE input = testParseE input (parseString ∘ unpack)
  in
    [ parseT "" ([],newAcctState)
    , parseT "-- comment"         ([],newAcctState)
    , parseT "-- com\n-- ment"    ([],newAcctState)
    , parseT " \r\r\n\t\n"        ([],newAcctState)
    , parseT "\n\n-- comment\n"   ([],newAcctState)
    , let
        as = newAcctState & accounts ⊢ fromList [([acct|Quux|],[])]
      in
        parseT "-- comment\n\nStart: Quux" ([],as)
    , let
        as = newAcctState & otherAccounts ⊢ fromList [('P',fromList [])]
      in
        parseT "-- comment\n\noStart: P" ([],as)
    , let
        as = newAcctState & otherAccounts ⊢ fromList [('D',fromList [])]
                          & accounts ⊢ fromList [([acct|Car|],[])]
      in
        parseT "Start: Car\n\noStart: D" ([],as)
    , let
        t  = tsimp_ [amt|10+|] [date|2073-01-01|] [acct|Foo|] 𝕹 𝕹 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Foo|],[t])]
      in
        parseT "Start: Foo\n10+ #D<1.i.73>A<Foo>" ([TSimpleTrx t],as)
    , let
        t  = tsimp_ [amt|5+|] [date|2022-08-10|] [acct|Baz|] (𝕵 [stmt|4|])
                              (𝕵 [ostmt|B|]) 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Baz|],[t])]
                          & otherAccounts ⊢ fromList [('B',fromList [(𝕹,[t])])]
      in
        parseT "Start: Baz\noStart: B\n5+ #D<10.viii.22>O<B>X<4>A<Baz>"
               ([TSimpleTrx t],as)
    , let
        t  = tsimp_ [amt|8-|] [date|2022-07-10|] [acct|Baz|] (𝕵 [stmt|4|])
                              (𝕵 [ostmt|B:6|]) 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Baz|],[t])]
                          & otherAccounts ⊢ fromList [('B',fromList[(𝕵 6,[t])])]
      in
        parseT "Start: Baz\noStart: B\n8- #D<10.vii.22>O<B:6>A<Baz>X<4>"
               ([TSimpleTrx t],as)
    , let
        t  = tsimp_ [amt|20-|] [date|2069-02-02|] [acct|Bar|] 𝕹 𝕹 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Bar|],[t])]
      in
        parseT "\n\r\nStart: Bar\n20- #D<2.2.69>A<Bar>\n\n" ([TSimpleTrx t],as)
    , let
        t1  = tsimp_ [amt|107.53-|] [date|1996-08-01|] [acct|Save|] 𝕹 𝕹
                     (𝕵 [cmt|for Hx|])
        t2  = tsimp_ [amt|230+|] [date|1996-08-01|] [acct|Food|] 𝕹 𝕹 𝕹
        tb = TrxBrk (tbh_ [amt|122.47+|] [date|1996-08-01|] (𝕵 [stmt|5|]) 𝕹)
                    (t1 :| [t2])
        as = newAcctState & accounts ⊢ fromList [ ([acct|Food|],[t2])
                                                , ([acct|Save|],[t1])]
      in
        parseT (unlines [ "Start: Save"
                        , "Start: Food"
                        , "122.47+\t#D<1.viii.96>B<>X<5>"
                        , "#107.53-  #C<for Hx>A<Save>D<1.viii.96>"
                        , "#230+#A<Food>D<1.viii.96>"
                        , "##"
                        ])
               ([TBrk tb],as)
    , parseE "Start: Foo\n10+ #D<1.i.73>A<Food>" "Not a valid account 'Food'"
    , parseE "x"                     "error: expected"
    , parseE "-- comment\nX"         "error: expected"
    , parseE "  \r\n-- comment\n\nX" "error: expected"
    , parseE (unlines [ "Start: Save"
                      , "Start: Food"
                      , "122.74+\t#D<1.viii.96>B<>X<5>"
                      , "#107.53-  #C<for Hx>A<Save>D<1.viii.96>"
                      , "#230+#A<Food>D<1.viii.96>"
                      , "##"
                      ])
             "breakdown total was 122.47+, expected"
    , parseE (unlines [ "Start: Save"
                      , "Start: Food"
                      , "122.47+\t#D<1.viii.96>B<>X<5>"
                      , "#107.53-  #C<for Hx>A<Save>D<1.viii.96>"
                      , "#230+#A<Fool>D<1.viii.96>"
                      , "##"
                      ])
             "Not a valid account 'Fool'"
    , let
        t01 = tsimp_ [amt|10.13+|]  [date|1996-08-06|] [acct|Bills|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t02 = tsimp_ [amt|472.50+|] [date|1996-08-06|]  [acct|Tithe|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t03 = tsimp_ [amt|28.07-|]  [date|1996-08-06|] [acct|CarFund|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t04 = tsimp_ [amt|21.79-|]  [date|1996-08-06|] [acct|Food|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t05 = tsimp_ [amt|147.89-|] [date|1996-08-06|] [acct|Save|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t06 = tsimp_ [amt|6.28+|]   [date|1996-08-08|] [acct|CarFund|]
                                    (𝕵 [stmt|5|]) 𝕹 (𝕵 [cmt|int to 8 Aug|])
        t07 = tsimp_ [amt|2.58+|]   [date|1996-08-08|] [acct|Save|]
                                    (𝕵 [stmt|5|]) 𝕹 (𝕵 [cmt|int to 8 Aug|])
        t08 = tsimp_ [amt|1.70+|]   [date|1996-09-15|] [acct|CarFund|]
                                    (𝕵 [stmt|6|]) 𝕹 (𝕵 [cmt|error correction|])
        b01 = tsimp_ [amt|107.53+|] [date|1.viii.96|] [acct|Save|] 𝕹 𝕹
                                    (𝕵 [cmt|for Hx|])
        b02 = tsimp_ [amt|230+|] [date|1.viii.96|] [acct|Food|] 𝕹 𝕹 𝕹
        b03 = tsimp_ [amt|100+|] [date|1.vii.96|] [acct|Tithe|] 𝕹 𝕹 𝕹
        b04 = tsimp_ [amt|35+|]  [date|1.viii.96|] [acct|Bills|] 𝕹 𝕹 𝕹
        b05 = tsimp_ [amt|160+|] [date|1.viii.96|] [acct|Petrol|] 𝕹 𝕹 𝕹
        b06 = tsimp_ [amt|40+|]  [date|1.viii.96|] [acct|CarFund|] 𝕹 𝕹
                                 (𝕵 [cmt|lounge decoration|])
        b07 = tsimp_ [amt|230.02+|] [date|1.viii.96|] [acct|Save|] 𝕹 𝕹 𝕹
        t09 = let
                bh = tbh_ [amt|902.55+|] [date|1.viii.96|] (𝕵 [stmt|5|]) 𝕹
              in
                TrxBrk bh (b01 :| [b02,b03,b04,b05,b06,b07])
        t10 = tsimp_ [amt|19.99-|] [date|1997-02-08|] [acct|CarFund|]
                                   (𝕵 [stmt|13|]) (𝕵 [ostmt|P:1|])
                                   (𝕵 [cmt|needle|])
        t11 = tsimp_ [amt|6.17-|]  [date|1997-02-08|] [acct|CarFund|]
                                   (𝕵 [stmt|13|]) (𝕵 [ostmt|P:1|])
                                   (𝕵 [cmt|tapes Mx|])
        t12 = tsimp_ [amt|5.99-|]  [date|1997-02-24|] [acct|CarFund|]
                                   (𝕵 [stmt|12|]) 𝕹 (𝕵 [cmt|slippers|])
        t13 = tsimp_ [amt|0.81+|]  [date|1997-03-12|] [acct|CarFund|]
                                   (𝕵 [stmt|12|]) 𝕹 (𝕵 [cmt|error from12|])
        t14 = tsimp_ [amt|5.49-|]  [date|2020-05-09|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [cmt|fair email Abi|])
        t15 = tsimp_ [amt|5.49-|]  [date|2020-05-09|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [cmt|fair email X|])
        t16 = tsimp_ [amt|9.80-|]  [date|2020-07-01|] [acct|LunchM|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [cmt|bike coffees|])
        t17 = tsimp_ [amt|9.50-|]  [date|2020-07-17|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|R|]) (𝕵 [cmt|ice-cream Wrest|])

        -- remember trx are added in reverse order (i.e., always prepended)
        as = newAcctState & accounts ⊢ fromList [ ([acct|Bills|],[b04,t01])
                                                , ([acct|CarFund|],
                                                   [t13,t12,t11,t10,b06
                                                   ,t08,t06,t03])
                                                , ([acct|Entz|],[t17,t15,t14])
                                                , ([acct|Food|],[b02,t04])
                                                , ([acct|LunchM|],[t16])
                                                , ([acct|Petrol|],[b05])
                                                , ([acct|Save|],
                                                   [b07,b01,t07,t05])
                                                , ([acct|Tithe|],[b03,t02])]
                          & otherAccounts ⊢ fromList [('A',fromList [(𝕹,
                                                                      [t16
                                                                      ,t15
                                                                      ,t14]
                                                                      )])
                                                     ,('M',fromList [])
                                                     ,('P',fromList [(𝕵 1,
                                                                      [t11
                                                                      ,t10]
                                                                      )])
                                                     ,('R',fromList [(𝕹,[t17])])
                                                     ]
      in
        parseT (unlines [ "-- This is a comment"
                        , ""
                        , "Start: CarFund"
                        , "Start: Bills"
                        , "Start: Tithe"
                        , "Start: Food"
                        , "Start: Save"
                        , "Start: Petrol"
                        , "Start: Entz"
                        , "Start: LunchM"
                        -- line 6
                        , "oStart: P"
                        , "oStart: M"
                        , "oStart: A"
                        , "oStart: R"
                        -- line 9
                        , "10.13+\t#D<6.viii.96>A<Bills>X<5>"
                        , "472.50+  #D<6.viii.96>A<Tithe>X<5>"
                        , "28.07-\t#D<6.viii.96>A<CarFund>X<5>"
                        , "21.79-\t#D<6.viii.96>A<Food>X<5>"
                        , "147.89-\t#D<6.VIII.96>A<Save>X<5>"
                        , "6.28+\t#D<8.viii.96>A<CarFund>C<int to 8 Aug>X<5>"
                        , "2.58+\t#D<8.viii.96>A<Save>C<int to 8 Aug>X<5>"
                        , "1.70+\t#D<15.ix.96>A<CarFund>C<error correction>X<6>"
                        , ""
                        -- line 18
                        , "902.55+\t#D<1.viii.96>B<>X<5>"
                        , "#107.53+  #D<1.viii.96>A<Save>C<for Hx>"
                        , "#230+#D<1.viii.96>A<Food>"
                        , "#100+\t#D<1.vii.96>A<Tithe>"
                        , "#35+\t#D<1.viii.96>A<Bills>"
                        , "#160+\t#D<1.viii.96>A<Petrol>"
                        , "#40+\t#D<1.viii.96>A<CarFund>C<lounge decoration>"
                        , "#230.02+\t#D<1.viii.96>A<Save>"
                        , "##"
                        , "19.99-\t#D<8.ii.97>A<CarFund>C<needle>O<P:1>X<13>"
                        , "6.17-\t#D<8.ii.97>A<CarFund>C<tapes Mx>O<P:1>X<13>"
                        , "5.99-\t#D<24.ii.97>A<CarFund>C<slippers>X<12>"
                        , "0.81+\t#D<12.iii.97>A<CarFund>C<error from12>X<12>"
                        , "5.49-\t#D<9.v.20>A<Entz>C<fair email Abi>O<A>"
                        , "5.49-\t#D<9.v.20>A<Entz>C<fair email X>O<A>"
                        , "9.80-\t#D<1.vii.20>A<LunchM>C<bike coffees>O<A>"
                        , "9.50-\t#D<17.vii.20>A<Entz>C<ice-cream Wrest>O<R>"
                        ])
        ([ TSimpleTrx t01
         , TSimpleTrx t02
         , TSimpleTrx t03
         , TSimpleTrx t04
         , TSimpleTrx t05
         , TSimpleTrx t06
         , TSimpleTrx t07
         , TSimpleTrx t08
         , TBrk t09
         , TSimpleTrx t10
         , TSimpleTrx t11
         , TSimpleTrx t12
         , TSimpleTrx t13
         , TSimpleTrx t14
         , TSimpleTrx t15
         , TSimpleTrx t16
         , TSimpleTrx t17]
        , as)
    ]


-- XXX ? test breakdown account inheritance & shadows
-- XXX ? test breakdown date inheritance & shadows
-- XXX ? test breakdown trx inheritance & shadows

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Entry" [ printTests, tParseTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
