{-| An `TEntry` is a Comment, Simple Trx or Broken Down Trx. -}
module Acct.TEntry
  ( TEntry( SheetComment, TSimpleTrx, TBrk )

  , tests )
where

import Base1T

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail( fail ) )
import Data.Function       ( flip )
import Data.String         ( unlines )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing
                                , char, digit, noneOf, oneOf, string )
import Text.Parser.Combinators  ( try )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( oneof, suchThat )

-- tasty-plus --------------------------

import TastyPlus    ( (≟) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- text --------------------------------

import Data.Text  ( intercalate, unwords )

-- textual-plus -------------------

import TextualPlus  ( TextualPlus( textual' )
                    , parseString, propInvertibleString, propInvertibleText )

-- trifecta-plus -----------------------

import TrifectaPlus  ( testParse, testParseE, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), isValid )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( acct )
import Acct.Date        ( dte )
import Acct.OStmt       ( ostmt )
import Acct.Parser      ( wspaces )
import Acct.SComment    ( SComment, scmt )
import Acct.Stmt        ( stmt )
import Acct.StmtIndex   ( NotStmtIndex( notStmtIndex ), stmtIndexGet )
import Acct.TComment    ( tcmt )
import Acct.TrxBrkHead  ( tbh_ )
import Acct.TrxSimp     ( TrxSimp, tsimp_ )
import Acct.TrxBrk      ( TrxBrk, trxBrk )

--------------------------------------------------------------------------------

{- | A comment, account start, other account start, simple or broken-down
     transaction; so, roughly, a "line" of an input file (albeit several lines
     for a broken-down transaction.
-}
data TEntry = SheetComment SComment
            | TSimpleTrx   TrxSimp
            | TBrk TrxBrk
  deriving (Eq,Show)

--------------------

instance Validity TEntry where
  validate (SheetComment t)  = validate t
  validate (TSimpleTrx a)    = validate a
  validate (TBrk a)          = validate a

--------------------

instance GenValid TEntry where
  genValid    = let
                  genEntry   = oneof [ SheetComment ⊳ arbitrary
                                     , TSimpleTrx ⊳ arbitrary
                                     , TBrk ⊳ arbitrary
                                     ]
                in
                  genEntry `suchThat` isValid
  shrinkValid (SheetComment t)  = SheetComment ⊳ shrinkValid t
  shrinkValid (TSimpleTrx a)    = TSimpleTrx ⊳ shrinkValid a
  shrinkValid (TBrk a)          = TBrk ⊳ shrinkValid a

--------------------

instance Arbitrary TEntry where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable TEntry where
  print (SheetComment t) = print t
  print (TSimpleTrx t)   = print t
  print (TBrk t)         = print t

----------

printTests ∷ TestTree
printTests =
  testGroup "print"
    [ testCase "SheetComment" $
      "% a comment" ≟ toText (SheetComment [scmt|% a comment|])
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

instance TextualPlus TEntry where
  textual' = -- e.g., "10.13+\t#D<6.viii.96>A<Bills>X<5>"
             -- or
             -- >   10.13+\t#D<6.iix.96>B<>X<5>
             -- >   #10.00+\t#D<4.vi.96>A<Foo>X<5>
             -- >   #0.13+\t#D<5.i.96>A<Bar>X<5>
             -- >   ##
             trxParse
           ∤ SheetComment ⊳ textual'
           ∤ const (SheetComment [scmt||]) ⊳ wspaces


{- | Like `TextualPlus.parseString`, but throws errors into a `MonadFail` rather
     then returning a `Parsed`.
 -}
parse ∷ ∀ η α . (MonadFail η, CharParsing η, TextualPlus α) ⇒ 𝕊 → η α
parse s =
  case parseString s of
         Parsed    x    → return x
         Malformed [] e → fail e
         Malformed es "CharParsing.satisfy" → fail $ "expected: " ⊕ unlines es
         Malformed es e                     → fail $ unlines es ⊕ "\n'" ⊕ e

{- | This awkward parser is designed to dispatch parsing of a line beginning
     with an amount to the `TBrk` or `TSimpleTrx` parsers as appropriate.

     The first attempt was to use a `try`; but that gave us unsatisfying error
     messages because, e.g., if the first attempt (e.g., a TSimpleTrx) failed
     due to a mal-formed account name; then it would backtrack (discarding the
     error message) and try the other (i.e., a TBrk) - which definitely won't
     work because it hase an A<>.  And so the error message will be confusing.

     Thus we look for either an A< or a B<, and dispatch appropriately.
 -}
trxParse ∷ (MonadFail η, CharParsing η) ⇒ η TEntry
trxParse =
  let
    attr ∷ CharParsing η ⇒ η 𝕊
    attr = (\ c x → c : '<' : x ⊕ ">") ⊳ oneOf "DCXO" ⊵ char '<'
                                       ⋫ many (noneOf "\r\n<>") ⋪ char '>'

    reformat ∷ (MonadFail η, CharParsing η) ⇒
               𝕊 → (ℂ, 𝕊 → η TEntry, 𝕊) → (𝕊 → η TEntry, 𝕊)
    reformat a (i,f,h) = (f,a⊕[i]⊕h)

    -- apply an `TEntry` parser to an input string
    do_parse ∷ (MonadFail η, CharParsing η) ⇒ (𝕊 → η TEntry, 𝕊) → η TEntry
    do_parse (f,s) = f s

    s_trx ∷ (Applicative φ, MonadFail η, CharParsing η) ⇒ φ (𝕊 → η TEntry)
    s_trx = pure $ TSimpleTrx ⩺ parse

    t_brk ∷ (Applicative φ, MonadFail η, CharParsing η) ⇒ φ (𝕊 → η TEntry)
    t_brk = pure $ TBrk ⩺ parse

    s_trx_sfx ∷ CharParsing η ⇒ η 𝕊
    s_trx_sfx = many (noneOf "\r\n")

    t_brk_sfx ∷ CharParsing η ⇒ η 𝕊
    t_brk_sfx = let
                  non_term ∷ CharParsing η ⇒ η 𝕊
                  non_term = ю ⊳ many (try ((:) ⊳ char '#' ⊵ some (noneOf "#"))
                                       ∤ some (noneOf "#"))
                in
                  (\ a b c → a : b ⊕ c) ⊳ char '<' ⊵ non_term ⊵ string "##"

    pfx ∷ (MonadFail η, CharParsing η) ⇒ η 𝕊
    pfx = merge ⊳ many (digit ∤ oneOf ".,") ⊵ oneOf "+-" ⊵ wspaces
                                            ⊵ char '#' ⊵ (many attr)
          where merge ∷ 𝕊 → ℂ → 𝕊 → ℂ → [𝕊] → 𝕊
                merge s c s' c' ss = s ⊕ [c] ⊕ s' ⊕ [c'] ⊕ ю ss

    -- parse an A<…> or a B<…>, along with the suffix of their
    -- expression, and hand them back along with a suitable parser
    op ∷ (Applicative η, CharParsing η) ⇒
         ℂ → η (𝕊 → η TEntry) → η 𝕊 → η (ℂ,(𝕊 → η TEntry),𝕊)
    op c f x = ((,,) ⊳ char c ⊵ f ⊵ x)

    sfx_parse ∷ (MonadFail η, CharParsing η) ⇒ η (ℂ, 𝕊 → η TEntry, 𝕊)
    sfx_parse = op 'A' s_trx s_trx_sfx ∤ op 'B' t_brk t_brk_sfx
  in
    reformat ⊳ pfx ⊵ sfx_parse ≫ do_parse

------------------------------------------------------------

{- | A `TEntry` may be a transaction (simple or broken-down), an Account
     start or an OtherAccount start, or a `TComment`.
 -}
parseTests ∷ TestTree
parseTests =
  let
    testParse'    = flip testParse
    unline        = intercalate "\n"
    testParseE' a = testParseE a (tParse' @TEntry)
  in
    testGroup "parse"
              [ testParse "%  bar "     $ SheetComment [scmt|%  bar |]
              , testParse "  % bar"     $ SheetComment [scmt|  % bar|]
              , testParse ""            $ SheetComment [scmt||]
              , testParse "  "          $ SheetComment [scmt|  |]
              , testParse "200+ #D<2.xi.11>A<Act>"
                          (TSimpleTrx $ tsimp_ 20000 [dte|2011-11-02|]
                                                     [acct|Act|] 𝕹 𝕹 𝕹)
              , let
                  t = tsimp_ 1013 [dte|1996-8-6|] [acct|Bill|] (𝕵 [stmt|5|])
                                  𝕹 𝕹
                in
                  testParse "10.13+ #D<6.viii.96>A<Bill>X<5>" $ TSimpleTrx t

              , let
                  t' = tsimp_ (-628) [dte|1996-8-8|] [acct|CarFund|]
                              (𝕵 [stmt|5|]) 𝕹 (𝕵 [tcmt|int to 8 Aug|])
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

              , testParseE' (unline [ "10.13+ #D<6.viii.96>B<>X<5>"
                                    , "#10+   #D<4.vi.96>X<5>A<Foo>"
                                    , "#0.31+ #D<5.i.96>X<5>A<Bar>"
                                    , "##"
                                    ])
                            (unwords ["breakdown total"
                                     ,"was 10.31+, expected 10.13+"
                                     ])
              , testParseE' "200+ #D<2.xi.11>A<act>"
                            "expected: uppercase letter"

              , let
                  h = tbh_ (-2373843) [dte|2048-2-28|] (𝕵 [stmt|427510|])
                           (𝕵 [ostmt|X|]) (𝕵 [tcmt|éçñ|])
                  t1 = tsimp_ (-2373843) [dte|2041-3-31|] [acct|GX|]
                              (𝕵 [stmt|427510|]) (𝕵 [ostmt|R:370047|]) 𝕹
                  t = TBrk (trxBrk h (t1 :| []))
                  l1 = "23,738.43-\t#D<28.ii.48>B<>X<427510>O<X>C<éçñ>"
                  l2 = "#23,738.43-\t#D<31.iii.41>A<GX>X<427510>O<R:370047>"
                in testParse' t (unline [ l1, l2, "##" ])


              , testProperty "invertibleString" (propInvertibleString @TEntry)
              , testProperty "invertibleText" (propInvertibleText @TEntry)
              ]

----------------------------------------

instance NotStmtIndex TEntry where
  notStmtIndex i (TSimpleTrx t) = stmtIndexGet t ≡ i
  notStmtIndex i (TBrk b)       = stmtIndexGet b ≡ i
  notStmtIndex _ _              = 𝕱

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.TEntry" [ printTests, parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
