{-# LANGUAGE OverloadedLists #-}

{-| Accounts, Statements, OtherStatements; each as (ultimately) a collection of
    transactions: gathered together as a record type e.g., to use in MonadState.

    A note on nomenclature:

    -) `parse*` functions convert input (strings, files) to EntryItemRaw -
       so textual parsing, but no importing nor updating AcctState
    -) `handle*` functions produce `EntryItem`s; thus, they perform imports
       as necessary (or, in some cases, throw if they see imports)
    -) `process*` functions produce `Entry`s; thus, they update and/or return
       `AcctState`s.
-}

module Acct.ParseEntry
  ( processInput, tests )
where

import Base1T  hiding  ( (∈), toList )

-- base --------------------------------

import Control.Monad          ( (<=<) )
import Control.Monad.Fail     ( MonadFail, fail )
import Data.Function          ( flip )
import Data.Functor.Identity  ( Identity( Identity ), runIdentity )
import Data.List              ( length, zip )
import Data.Maybe             ( catMaybes )
import GHC.Exts               ( IsList( toList ) )

-- fpath -------------------------------

import qualified  FPath.Parent

import FPath.AsFilePath        ( filepath )
import FPath.AbsDir            ( root )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.At      ( at, ix )
import Control.Lens.Fold    ( has )
import Control.Lens.Getter  ( use )
import Control.Lens.Setter  ( (%=), (.=) )

-- monadio-plus ------------------------

import MonadIO.FPath  ( pResolveDir )
import MonadIO.Temp   ( testsWithTempfiles )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- mtl ---------------------------------

import Control.Monad.Except  ( runExceptT )
import Control.Monad.State   ( MonadState, runStateT )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char )
import Text.Parser.Combinators  ( eof, sepEndBy )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertFailure )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEqIO, withResource' )
import TastyPluser  ( TestCmp'( testCmp' ) )

-- text --------------------------------

import Data.Text  ( intercalate, isInfixOf, unlines, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus ------------------------

import TextualPlus'  ( textual' )

-- trifecta ----------------------------

import qualified  Text.Trifecta  as  Trifecta
import Text.Trifecta  ( Result( Failure, Success ), parseFromFileEx )

-- trifecta-plus -----------------------

import TrifectaPlus  ( eiText, tname )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account       ( Account, HasAccount( account ), acct )
import Acct.AcctError     ( AcctError, AsAcctProcessingError, AsParseError
                          , AsImportError, ParseError
                          , throwAcctRestartE, throwImportError
                          , throwInvalidAccountE, throwInvalidOAccountE
                          , throwNoImportE, throwOAcctRestartE, throwParseE
                          )
import Acct.AcctMap       ( AcctMap )
import Acct.AcctState     ( AcctState
                          , accounts, newAcctState, otherAccounts, stmts )
import Acct.Amount        ( amt )
import Acct.Annotation    ( Annotation( Annotation ) )
import Acct.Date          ( dte )
import Acct.Entries       ( Entries )
import Acct.EntryItem     ( EntryItem( TAcctStart, TOStmtStart, TTEntry ) )
import Acct.EntryItemRaw  ( EntryItemRaw( EIIEI, TImport ) )
import Acct.EntrySource   ( EntrySource( SourceFile, SourceString ) )
import Acct.OAcctMap      ( OAcctMap, addTrx )
import Acct.OStmt         ( oAcct, ostmt )
import Acct.OStmtIndex    ( ostmtindex )
import Acct.OStmtName     ( OStmtName, ostmtname )
import Acct.SComment      ( scmt )
import Acct.Stmt          ( stmt )
import Acct.StmtIndex     ( stmtindex, stmtIndexGet )
import Acct.StmtEntry     ( StmtEntry( SE_BRK, SE_SIMP ) )
import Acct.TComment      ( tcmt )
import Acct.TEntry        ( TEntry( SheetComment, TBrk, TSimpleTrx ) )
import Acct.TrxBrk        ( inferredStmt, trx, trxBrk )
import Acct.TrxBrkHead    ( tbh_ )
import Acct.TrxSimp       ( TrxSimp, oStmtGetY, parent, tsimp_ )
import Acct.Util          ( (⋰), mcons )

--------------------------------------------------------------------------------

type AnnoES α = Annotation α EntrySource

------------------------------------------------------------

newtype AnnotatedTEntries = AnnotatedTEntries [AnnoES TEntry]

instance Printable AnnotatedTEntries where
  print (AnnotatedTEntries xs) = P.text $ intercalate " ⫽ " (toText ⊳ xs)

------------------------------------------------------------

{-| convert a `MonadError` to a `MonadFail` -}
failME ∷ (MonadFail η, Printable ε) ⇒ ExceptT ε η α → η α
failME f = runExceptT f ≫ either (fail ∘ toString) return

----------------------------------------

{-| convert a `Result` to a `MonadError` -}
resultME ∷ (AsParseError ε, MonadError ε η) ⇒ Result α → η α
resultME (Success a) = return a
resultME (Failure e) = throwParseE e

----------------------------------------

{-| parse a series of entries, with neither recursive importation nor acct state
    updating. -}
parseEntriesRaw ∷ (MonadFail η, CharParsing η) ⇒
                  EntrySource → η [AnnoES EntryItemRaw]
parseEntriesRaw s = let eol = many (char '\r') ⋫ char '\n'
                    in  ((flip Annotation s) ⊳ textual') `sepEndBy` eol ⋪ eof

----------------------------------------

{-| parse an input string to its raw entry items; N.B. no IO -}
parseString ∷ (AsParseError ε, MonadError ε η) ⇒ 𝕊 → η [AnnoES EntryItemRaw]
parseString = resultME ∘ parseString'
              where parseString' ∷ 𝕊 → Result [AnnoES EntryItemRaw]
                    parseString' s =
                      let parser = parseEntriesRaw (SourceString s)
                      in  Trifecta.parseString parser ф s

----------------------------------------

{-| parse a file or string to its raw entry items -}
parseSource ∷ ∀ ε μ . (MonadIO μ, AsParseError ε, MonadError ε μ) ⇒
              EntrySource → μ [AnnoES EntryItemRaw]
parseSource (SourceString t) = parseString t
parseSource s@(SourceFile f)   = let f' = filepath # f
                                     p  = parseEntriesRaw s
                                 in parseFromFileEx p f' ≫ resultME

----------------------------------------

{-| update `AcctState` by creating a new account; fail if it already exists -}
startAcct ∷ ∀ ε η .
            (MonadState AcctState η, AsAcctProcessingError ε, MonadError ε η) ⇒
            AcctMap → Account → η ()
startAcct accts a =
  if has (ix a) accts
  then throwAcctRestartE a
  else accounts ∘ at a .= 𝕵 []

----------------------------------------

{-| update `AcctState` by creating a new other account; fail if it already
    exists -}
startOAcct ∷ ∀ ε η .
             (MonadState AcctState η, AsAcctProcessingError ε, MonadError ε η)⇒
             OAcctMap → OStmtName → η ()
startOAcct oaccts c =
  if has (ix c) oaccts
  then throwOAcctRestartE c
  else otherAccounts ∘ at c .= 𝕵 ф

----------------------------------------

{-| add a `TrxSimp` to its `Account`; throw if no such account exists -}
addToAcct ∷ ∀ ε η .
            (MonadState AcctState η, AsAcctProcessingError ε, MonadError ε η) ⇒
            TrxSimp → η ()
addToAcct t = do
  acctmap ← use accounts
  let a = t ⊣ account
  if has (ix a) acctmap
  then accounts ∘ at a %= 𝕵 ∘ mcons t
  else throwInvalidAccountE a

----------------------------------------

{-| add a `TrxSimp` to its `OStmt` (if it has one); throw if no such account
    exists -}
addToOAcct ∷ ∀ ε η .
             (MonadState AcctState η, AsAcctProcessingError ε, MonadError ε η)⇒
             TrxSimp → η ()
addToOAcct t = do
  oaccts ← use otherAccounts
  case oStmtGetY t of
    𝕹    → return ()
    𝕵 oa →
      let
        toact = oa ⊣ oAcct
      in
        if has (ix toact) oaccts
        then otherAccounts %= addTrx t
        else throwInvalidOAccountE (oa ⊣ oAcct) t

----------------------------------------

{-| add a trx (simp or brk) to its stmt -}
addToStmt ∷ ∀ ε η .
             (MonadState AcctState η, AsAcctProcessingError ε, MonadError ε η) ⇒
             StmtEntry → η ()
addToStmt t = do
  let st = stmtIndexGet t
  stmts ∘ at st %=  𝕵 ∘ (t ⋰)

----------------------------------------

{-| process a `TEntry` by adding it to the `AcctState` as required. -}
processTEntry ∷ ∀ ε η .
                (MonadState AcctState η,AsAcctProcessingError ε,MonadError ε η)⇒
                TEntry → η ()
processTEntry (SheetComment _) = return ()
processTEntry (TSimpleTrx t)   = do
  addToAcct t
  addToOAcct t
  addToStmt (SE_SIMP t)

processTEntry (TBrk t)         = do
  forM_ (trx t) $ \ t' → do
    addToAcct t'
    addToOAcct t'
  case inferredStmt t of
    𝕵 _ → addToStmt (SE_BRK t)
    𝕹   → forM_ (trx t) $ addToStmt ∘ SE_SIMP

----------------------------------------

{-| process an `EntryItem`, by updating the `AcctState`, and returning the
    embedded `TEntry` if there is one -}
processEntry ∷ ∀ ε ρ η .
               (MonadState AcctState η,AsAcctProcessingError ε,MonadError ε η) ⇒
               Annotation EntryItem ρ → η (𝕄 (Annotation TEntry ρ))
processEntry (Annotation (TAcctStart a) _) = do
  accts ← use accounts
  startAcct accts a
  return 𝕹
processEntry (Annotation (TOStmtStart c) _) = do
  oaccts ← use otherAccounts
  startOAcct oaccts c
  return 𝕹
processEntry (Annotation (TTEntry t) s) = do
  processTEntry t
  return $ 𝕵 (Annotation t s)

--------------------

{-| process entries a la `processEntry`; but throw if any import is seen -}
processRawEntryNoImport ∷ (MonadState AcctState η,
                           AsAcctProcessingError ε, MonadError ε η) ⇒
                          AnnoES EntryItemRaw → η (𝕄 (AnnoES TEntry))
processRawEntryNoImport (Annotation (EIIEI ei) s) =processEntry(Annotation ei s)
processRawEntryNoImport (Annotation (TImport _) _)=throwNoImportE

----------------------------------------

{-| Parse an input string to a list of entries; will throw on any import. -}
processStringNoImports ∷ ∀ ε η .
                       (MonadState AcctState η,
                        AsParseError ε,AsAcctProcessingError ε,MonadError ε η) ⇒
                       𝕊 → η AnnotatedTEntries
processStringNoImports s =
  AnnotatedTEntries ⊳ (parseString s ≫ catMaybes⩺ mapM processRawEntryNoImport)

----------------------------------------

{-| convert `EntryItemRaw` to a list of `EntryItem`s, within an Annotation
    context; by enacting any imports (potentially recursively) -}
handleImport ∷ (MonadIO μ,
                AsImportError ε, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
               AnnoES EntryItemRaw → μ (Entries EntryItem EntrySource)
handleImport (Annotation (TImport f) o) = do
  let parseFileRaw ∷ MonadIO μ ⇒ AbsFile → μ (Result [AnnoES EntryItemRaw])
      parseFileRaw g = parseFromFileEx (parseEntriesRaw $ SourceFile g)
                                       (filepath # g)
  f' ← case o of
    SourceFile   s → pResolveDir (s ⊣ FPath.Parent.parent) f
    SourceString _ → pResolveDir root f
  parseFileRaw f' ≫ \ case
    Failure e → throwImportError f' (eiText e)
    Success s → ю ⊳ forM s handleImport

handleImport (Annotation (EIIEI e) s) = return $ fromList [Annotation e s]

----------------------------------------

{-| convert a list of `EntryItemRaw`s to `EntryItem`s; by using `parseImport`
    as required -}
handleImports ∷ (MonadIO μ,
                 AsImportError ε, AsIOError ε, AsFPathError ε, MonadError ε μ)⇒
                [AnnoES EntryItemRaw] → μ (Entries EntryItem EntrySource)
handleImports = ю ⩺ mapM handleImport

----------------------------------------

{-| process a load of raw entry items, reading in imports, and updating
    `AcctState` -}
processEntries' ∷ ∀ ε μ . (MonadIO μ, MonadState AcctState μ,
                           AsImportError ε, AsIOError ε, AsFPathError ε,
                           AsAcctProcessingError ε, MonadError ε μ) ⇒
                  [AnnoES EntryItemRaw] → μ [AnnoES TEntry]
processEntries' es = catMaybes ⊳ (handleImports es ≫ mapM processEntry∘toList)

----------------------------------------

{-| read in some source, including imports, process them all, update
    `AcctState` -}
processInputT ∷ (MonadIO μ, MonadState AcctState μ, AsParseError ε, AsIOError ε,
                 AsImportError ε, AsFPathError ε, AsAcctProcessingError ε,
                 MonadError ε μ) ⇒
                EntrySource → μ [AnnoES TEntry]
processInputT = processEntries' <=< parseSource

----------------------------------------

{-| read in some source, including imports, process them all, produce an
    `AcctState` -}
processInput ∷ ∀ ε μ . (MonadIO μ, AsParseError ε, AsIOError ε, AsImportError ε,
                        AsFPathError ε,AsAcctProcessingError ε,MonadError ε μ) ⇒
               EntrySource → μ ([AnnoES TEntry], AcctState)
processInput = flip runStateT newAcctState ∘ processInputT

----------------------------------------

parseTests ∷ TestTree
parseTests = testGroup "parse" $
  let
    parseT ∷ HasCallStack ⇒ 𝕋 → ([AnnoES TEntry], AcctState) → TestTree
    parseT t (exp_es,exp_as) =
      withResource' (failME $ processInput @AcctError(SourceString $ unpack t))$
        \ es'→ testGroup (tname t)
                 [ assertListEqIO "[Entry]" exp_es (fst ⊳ es')
                 , testCmp' (tname t) exp_as (snd ⊳ es') ]

    parseE ∷ HasCallStack ⇒ 𝕋 → 𝕋 → TestTree
    parseE s x =
      let
        processStringNoImports' = processStringNoImports @AcctError
      in
        case flip runStateT newAcctState $ processStringNoImports' (unpack s) of
          𝕷 e →
            testCase (unpack s) $ assertBool ([fmt|'%t' `isInfixOf` '%T'|] x e)
                                             (x `isInfixOf` toText e)
          𝕽 (es,as) →
            testCase (unpack s) $
              assertFailure $ [fmt|parse of '%t' succeeded (got ('%T','%T'))|]
                              s es as

    emptySCmt = SheetComment [scmt||]
    annot ∷ α → AnnoES α
    annot t = Annotation t (SourceString "")
  in
    [ parseT "" ([ annot emptySCmt ],newAcctState)
    , parseT " % comment0"        (annot ⊳ [SheetComment [scmt| % comment0|]],
                                   newAcctState)
    , parseT "% com\n% ment"
             (annot ⊳ [ SheetComment [scmt|% com|]
                        , SheetComment [scmt|% ment|] ],
              newAcctState)
    , parseT " \r\r\n\t\n"         (annot ⊳ [ SheetComment [scmt| |]
                                              , SheetComment [scmt|	|]
                                              , emptySCmt
                                              ],
                                    newAcctState)
    , parseT "\n\n% comment\n"    (annot ⊳ [ emptySCmt
                                             , emptySCmt
                                             , SheetComment [scmt|% comment|]
                                             , emptySCmt
                                             ],
                                   newAcctState)

    --------------------

    , let
        t = tsimp_ [amt|200+|] [dte|2011-11-02|] [acct|Act|] 𝕹 𝕹 𝕹
      in
        parseT "Start: Act\n\n200+ #D<2.xi.11>A<Act>"
               (annot ⊳ [emptySCmt,TSimpleTrx t],
                newAcctState & accounts ⊢ fromList [([acct|Act|],[t])]
                               & stmts    ⊢ fromList [([stmtindex||],
                                                         [SE_SIMP t])])

    , parseE "201+ #D<2.xi.11>A<act>\n" "expected: uppercase letter"

    --------------------

    , let
        as = newAcctState & accounts ⊢ fromList [([acct|Quux|],[])]
      in
        parseT "% comment\n\nStart: Quux"
               (annot ⊳ [ SheetComment [scmt|% comment|], emptySCmt ], as)

    --------------------

    , let
        as = newAcctState & otherAccounts ⊢ fromList [([ostmtname|P|],
                                                       fromList [])]
      in
        parseT "% moccent\n\noStart: P"
               (annot ⊳ [ SheetComment [scmt|% moccent|], emptySCmt ], as)

    --------------------

    , let
        as = newAcctState & otherAccounts ⊢ fromList [([ostmtname|D|],
                                                       fromList [])]
                          & accounts ⊢ fromList [([acct|Car|],[])]
      in
        parseT "Start: Car\n\noStart: D" (annot ⊳ [emptySCmt],as)

    --------------------

    , let
        t  = tsimp_ [amt|10+|] [dte|2073-01-01|] [acct|Foo|] 𝕹 𝕹 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Foo|],[t])]
                          & stmts    ⊢ fromList [([stmtindex||],[SE_SIMP t])]
      in
        parseT "Start: Foo\n10+ #D<1.i.73>A<Foo>" (annot ⊳ [TSimpleTrx t],as)

    --------------------

    , let
        t     = tsimp_ [amt|10+|] [dte|2073-01-01|] [acct|Act|] 𝕹 𝕹 𝕹
        tsts ∷ [AnnoES EntryItemRaw] → [(TestName,Identity AbsFile → Assertion)]
        tsts exp_es =
             let
               entries f = parseSource @ParseError (SourceFile $ runIdentity f)
               check ∷ ([AnnoES EntryItemRaw] → Assertion) → Identity AbsFile
                     → Assertion
               check c f = ѥ (entries f) ≫ \ case
                       𝕷 _  → assertFailure "parse failed"
                       𝕽 es → c es

             in
             [ ("#Entries", check $ \ es → length (exp_es) ≟ length (toList es))
             , ("Entries", check $ \ es → forM_ (zip (toList es) exp_es)
                                                (\ (e,exp) → e ≟ exp))
             ]
      in
        testsWithTempfiles "parseSource"
                           (Identity ("Start: Act\n10+ #D<1.i.73>A<Act>" ∷ 𝕋))
                           (tsts ([ annot $ EIIEI (TAcctStart [acct|Act|])
                                  , annot $ EIIEI (TTEntry (TSimpleTrx t))]))

    --------------------

    , let
        t     = tsimp_ [amt|10+|] [dte|2073-01-01|] [acct|Act|] 𝕹 𝕹 𝕹
        expAs = newAcctState & accounts ⊢ fromList [([acct|Act|],[t])]
                             & stmts    ⊢ fromList [([stmtindex||],[SE_SIMP t])]
        tsts ∷ ([AnnoES TEntry],AcctState)
             → [(TestName, Identity AbsFile → Assertion)]
        tsts (exp_es,exp_as) =
             let
               entries f = processInput @AcctError (SourceFile $ runIdentity f)
               check ∷ (([AnnoES TEntry],AcctState) → Assertion)
                     → Identity AbsFile → Assertion
               check c f = ѥ (entries f) ≫ \ case
                       𝕷 _  → assertFailure "parse failed"
                       𝕽 es → c es

             in
               [ ("#Entries",
                  check $ \ (es,_) → length (exp_es) ≟ length (toList es) )
               , ("Entries", check $ \ (es,_) → forM_ (zip (toList es) exp_es)
                                                      (\ (e,exp) → e ≟ exp))
               , ("AcctState", check $ \ (_,as) → exp_as ≟ as)
               ]
      in
        testsWithTempfiles "processInput"
                           (Identity ("Start: Act\n10+ #D<1.i.73>A<Act>" ∷ 𝕋))
                           (tsts ([annot $ TSimpleTrx t],expAs))

    --------------------

    , let
        t     = tsimp_ [amt|10+|] [dte|2073-01-01|] [acct|Act|] 𝕹 𝕹 𝕹
        expTs = [annot $ TSimpleTrx t]
        expAs = newAcctState & accounts ⊢ fromList [([acct|Act|],[t])]
                             & stmts    ⊢ fromList [([stmtindex||],[SE_SIMP t])]
        tsts ∷ ([AnnoES TEntry],AcctState)
             → [(TestName, Identity AbsFile → Assertion)]
        tsts (exp_es, exp_as) =
             let
               source f = SourceString $ [fmt|import: %T|] (runIdentity f)
               entries f = processInput @AcctError (source f)
               check ∷ (([AnnoES TEntry],AcctState) → Assertion)
                     → Identity AbsFile → Assertion
               check c f = ѥ (entries f) ≫ \ case
                       𝕷 _  → assertFailure "parse failed"
                       𝕽 es → c es

             in
             [("contents",
               check $ \ (es,as) → do
                      length (toList es) ≟ length (exp_es)
                      forM_ (zip (toList es) exp_es) (\ (e,exp) → e ≟ exp)
                      exp_as @=? as
              )]
      in
        testsWithTempfiles "import"
                           (Identity ("Start: Act\n10+ #D<1.i.73>A<Act>" ∷ 𝕋))
                           (tsts (expTs,expAs))

    --------------------

    , let
        t   = tsimp_ [amt|5+|] [dte|2022-08-10|] [acct|Baz|] (𝕵 [stmt|4|])
                               (𝕵 [ostmt|B|]) 𝕹
        oas = fromList [([ostmtname|B|], fromList [([ostmtindex||],[t])])]
        as  = newAcctState & accounts ⊢ fromList [([acct|Baz|],[t])]
                           & otherAccounts ⊢ oas
                           & stmts ⊢ fromList [([stmtindex|4|],[SE_SIMP t])]
      in
        parseT "Start: Baz\noStart: B\n5+ #D<10.viii.22>O<B>X<4>A<Baz>"
               (annot ⊳ [TSimpleTrx t],as)
    --------------------

    , let
        t   = tsimp_ [amt|8-|] [dte|2022-07-10|] [acct|Baz|] (𝕵 [stmt|4|])
                               (𝕵 [ostmt|B:6|]) 𝕹
        oas = fromList [([ostmtname|B|], fromList[([ostmtindex|6|],[t])])]
        as  = newAcctState & accounts ⊢ fromList [([acct|Baz|],[t])]
                           & otherAccounts ⊢ oas
                           & stmts ⊢ fromList [([stmtindex|4|],[SE_SIMP t])]
      in
        parseT "Start: Baz\noStart: B\n8- #D<10.vii.22>O<B:6>A<Baz>X<4>"
               (annot ⊳ [TSimpleTrx t],as)

    --------------------

    , let
        t  = tsimp_ [amt|20-|] [dte|2069-02-02|] [acct|Bar|] 𝕹 𝕹 𝕹
        as = newAcctState & accounts ⊢ fromList [([acct|Bar|],[t])]
                          & stmts    ⊢ fromList [([stmtindex||],[SE_SIMP t])]
      in
        parseT "\n\r\nStart: Bar\n20- #D<2.2.69>A<Bar>\n\n"
               (annot ⊳ [ emptySCmt, emptySCmt, TSimpleTrx t
                          , emptySCmt, emptySCmt ]
               ,as)

      ------------------------------------------------------

    , let
        th  = tbh_ [amt|122.47+|] [dte|1996-08-01|] (𝕵 [stmt|5|]) 𝕹 𝕹
        t1  = tsimp_ [amt|107.53-|] [dte|1996-08-01|] [acct|Aston|] 𝕹 𝕹
                     (𝕵 [tcmt|for Hx|]) & parent ⊩ th
        t2  = tsimp_ [amt|230+|] [dte|1996-08-01|] [acct|Villa|] 𝕹 𝕹 𝕹
                     & parent ⊩ th
        tb  = trxBrk th (t1 :| [t2])
        as = fromList  [ ([acct|Villa|],[t2]), ([acct|Aston|],[t1]) ]
        sts = fromList [ ([stmtindex|5|],[SE_BRK tb]) ]
        ast = newAcctState & accounts ⊢ as & stmts ⊢ sts
      in
        parseT (unlines [ "Start: Aston"
                        , "Start: Villa"
                        , "122.47+\t#D<1.viii.96>B<>X<5>"
                        , "#107.53-  #C<for Hx>A<Aston>D<1.viii.96>"
                        , "#230+#A<Villa>D<1.viii.96>"
                        , "##"
                        ])
               (annot ⊳ [TBrk tb, emptySCmt],ast)

      ------------------------------------------------------

    , let
        th  = tbh_ [amt|122.47+|] [dte|1996-08-01|] 𝕹 (𝕵 [ostmt|P:1|]) 𝕹
        t1  = tsimp_ [amt|107.53-|] [dte|1996-08-01|] [acct|Paul|] (𝕵 [stmt|5|])
                     𝕹 (𝕵 [tcmt|for Hx|]) & parent ⊩ th
        t2  = tsimp_ [amt|230+|] [dte|1996-08-01|] [acct|Simon|] (𝕵 [stmt|5|])
                                 (𝕵 [ostmt|N|]) 𝕹 & parent ⊩ th
        tb  = trxBrk th (t1 :| [t2])
        as = fromList  [ ([acct|Simon|],[t2]), ([acct|Paul|],[t1]) ]
        oas = fromList [ ([ostmtname|N|], fromList[([ostmtindex||], [t2])])
                       , ([ostmtname|P|], fromList[([ostmtindex|1|], [t1])]) ]
        sts = fromList [ ([stmtindex||],[SE_BRK tb]) ]
        ast = newAcctState & accounts ⊢ as & otherAccounts ⊢ oas & stmts ⊢ sts
      in
        parseT (unlines [ "Start: Paul"
                        , "Start: Simon"
                        , "oStart: N"
                        , "oStart: P"
                        , "122.47+\t#D<1.viii.96>B<>O<P:1>"
                        , "#107.53-  #C<for Hx>A<Paul>D<1.viii.96>X<5>"
                        , "#230+#A<Simon>D<1.viii.96>O<N>X<5>"
                        , "##"
                        ])
               (annot ⊳ [TBrk tb, emptySCmt],ast)

      ------------------------------------------------------

    , let
        th  = tbh_ [amt|122.47+|] [dte|1996-08-01|] (𝕵 [stmt|5|])
                                  (𝕵 [ostmt|P:1|]) 𝕹
        t1  = tsimp_ [amt|107.53-|] [dte|1996-08-01|] [acct|Save|] 𝕹 𝕹
                     (𝕵 [tcmt|for Hx|]) & parent ⊩ th
        t2  = tsimp_ [amt|230+|] [dte|1996-08-01|] [acct|Food|] 𝕹
                                 (𝕵 [ostmt|N|]) 𝕹 & parent ⊩ th
        tb  = trxBrk th (t1 :| [t2])
        as = fromList  [ ([acct|Food|],[t2]), ([acct|Save|],[t1]) ]
        oas = fromList [ ([ostmtname|N|], fromList[([ostmtindex||], [t2])])
                       , ([ostmtname|P|], fromList[([ostmtindex|1|], [t1])]) ]
        sts = fromList [ ([stmtindex|5|],[SE_BRK tb]) ]
        ast = newAcctState & accounts ⊢ as & otherAccounts ⊢ oas & stmts ⊢ sts
      in
        parseT (unlines [ "Start: Save"
                        , "Start: Food"
                        , "oStart: N"
                        , "oStart: P"
                        , "122.47+\t#D<1.viii.96>B<>X<5>O<P:1>"
                        , "#107.53-  #C<for Hx>A<Save>D<1.viii.96>"
                        , "#230+#A<Food>D<1.viii.96>O<N>"
                        , "##"
                        ])
               (annot ⊳ [TBrk tb, emptySCmt],ast)

      ------------------------------------------------------

    , parseE "Start: Foo\n10+ #D<1.i.73>A<Food>" "Not a valid account 'Food'"
    , parseE "x"                     "error: expected"
    , parseE "% comment\nX"         "error: expected"
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
                      , "Start: Foox"
                      , "122.47+\t#D<1.viii.96>B<>X<5>"
                      , "#107.53-  #C<for Hx>A<Save>D<1.viii.96>"
                      , "#230+#A<Fool>D<1.viii.96>"
                      , "##"
                      ])
             "Not a valid account 'Fool'"
    , let
        t01 = tsimp_ [amt|10.13+|]  [dte|1996-08-06|] [acct|Bills|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t02 = tsimp_ [amt|472.50+|] [dte|1996-08-06|]  [acct|Tithe|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t03 = tsimp_ [amt|28.07-|]  [dte|1996-08-06|] [acct|CarFund|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t04 = tsimp_ [amt|21.79-|]  [dte|1996-08-06|] [acct|Food|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t05 = tsimp_ [amt|147.89-|] [dte|1996-08-06|] [acct|Save|]
                                    (𝕵 [stmt|5|]) 𝕹 𝕹
        t06 = tsimp_ [amt|6.28+|]   [dte|1996-08-08|] [acct|CarFund|]
                                    (𝕵 [stmt|5|]) 𝕹 (𝕵 [tcmt|int to 8 Aug|])
        t07 = tsimp_ [amt|2.58+|]   [dte|1996-08-08|] [acct|Save|]
                                    (𝕵 [stmt|5|]) 𝕹 (𝕵 [tcmt|int to 8 Aug|])
        t08 = tsimp_ [amt|1.70+|]   [dte|1996-09-15|] [acct|CarFund|]
                                    (𝕵 [stmt|6|]) 𝕹 (𝕵 [tcmt|error correction|])
        t09h = tbh_ [amt|902.55+|] [dte|1.viii.96|] (𝕵 [stmt|5|]) 𝕹 𝕹
        b01 = tsimp_ [amt|107.53+|] [dte|1.viii.96|] [acct|Save|] 𝕹 𝕹
                                    (𝕵 [tcmt|for Hx|])
                     & parent ⊩ t09h
        b02 = tsimp_ [amt|230+|] [dte|1.viii.96|] [acct|Food|] 𝕹 𝕹 𝕹
                     & parent ⊩ t09h
        b03 = tsimp_ [amt|100+|] [dte|1.vii.96|] [acct|Tithe|] 𝕹 𝕹 𝕹
                     & parent ⊩ t09h
        b04 = tsimp_ [amt|35+|]  [dte|1.viii.96|] [acct|Bills|] 𝕹 𝕹 𝕹
                     & parent ⊩ t09h
        b05 = tsimp_ [amt|160+|] [dte|1.viii.96|] [acct|Petrol|] 𝕹 𝕹 𝕹
                     & parent ⊩ t09h
        b06 = tsimp_ [amt|40+|]  [dte|1.viii.96|] [acct|CarFund|] 𝕹 𝕹
                                 (𝕵 [tcmt|lounge decoration|])
                     & parent ⊩ t09h
        b07 = tsimp_ [amt|230.02+|] [dte|1.viii.96|] [acct|Save|] 𝕹 𝕹 𝕹
                     & parent ⊩ t09h
        t09 = trxBrk t09h (b01 :| [b02,b03,b04,b05,b06,b07])
        t10 = tsimp_ [amt|19.99-|] [dte|1997-02-08|] [acct|CarFund|]
                                   (𝕵 [stmt|13|]) (𝕵 [ostmt|P:1|])
                                   (𝕵 [tcmt|needle|])
        t11 = tsimp_ [amt|6.17-|]  [dte|1997-02-08|] [acct|CarFund|]
                                   (𝕵 [stmt|13|]) (𝕵 [ostmt|P:1|])
                                   (𝕵 [tcmt|tapes Mx|])
        t12 = tsimp_ [amt|5.99-|]  [dte|1997-02-24|] [acct|CarFund|]
                                   (𝕵 [stmt|12|]) 𝕹 (𝕵 [tcmt|slippers|])
        t13 = tsimp_ [amt|0.81+|]  [dte|1997-03-12|] [acct|CarFund|]
                                   (𝕵 [stmt|12|]) 𝕹 (𝕵 [tcmt|error from12|])
        t14 = tsimp_ [amt|5.49-|]  [dte|2020-05-09|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [tcmt|fair email Abi|])
        t15 = tsimp_ [amt|5.49-|]  [dte|2020-05-09|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [tcmt|fair email X|])
        t16 = tsimp_ [amt|9.80-|]  [dte|2020-07-01|] [acct|LunchM|] 𝕹
                                   (𝕵 [ostmt|A|]) (𝕵 [tcmt|bike coffees|])
        t17 = tsimp_ [amt|9.50-|]  [dte|2020-07-17|] [acct|Entz|] 𝕹
                                   (𝕵 [ostmt|R|]) (𝕵 [tcmt|ice-cream Wrest|])

        as  = fromList [ ([acct|Bills|],[b04,t01])
                       , ([acct|CarFund|], [t13,t12,t11,t10,b06 ,t08,t06,t03])
                       , ([acct|Entz|],[t17,t15,t14])
                       , ([acct|Food|],[b02,t04])
                       , ([acct|LunchM|],[t16])
                       , ([acct|Petrol|],[b05])
                       , ([acct|Save|], [b07,b01,t07,t05])
                       , ([acct|Tithe|],[b03,t02])
                       ]

        oas = fromList[([ostmtname|A|],fromList[([ostmtindex||],[t16,t15,t14])])
                      ,([ostmtname|M|],fromList [])
                      ,([ostmtname|P|],fromList [([ostmtindex|1|],[t11,t10])])
                      ,([ostmtname|R|],fromList [([ostmtindex||],[t17])])
                      ]

        sts = fromList [ ([stmtindex||]  , [SE_SIMP t17,SE_SIMP t16,SE_SIMP t15
                                           ,SE_SIMP t14])
                       , ([stmtindex|5|] , [SE_BRK t09
                                           ,SE_SIMP t07,SE_SIMP t06
                                           ,SE_SIMP t05,SE_SIMP t04,SE_SIMP t03
                                           ,SE_SIMP t02,SE_SIMP t01])
                       , ([stmtindex|6|] , [SE_SIMP t08])
                       , ([stmtindex|12|], [SE_SIMP t13,SE_SIMP t12])
                       , ([stmtindex|13|], [SE_SIMP t11,SE_SIMP t10])
                       ]

        -- remember trx are added in reverse order (i.e., always prepended)
        ast = newAcctState & accounts ⊢ as
                           & otherAccounts ⊢ oas
                           & stmts ⊢ sts
      in
        parseT (unlines [ "% This is a comment"
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
        (annot ⊳ [ SheetComment [scmt|% This is a comment|]
                   , SheetComment [scmt||]
                   , TSimpleTrx t01
                   , TSimpleTrx t02
                   , TSimpleTrx t03
                   , TSimpleTrx t04
                   , TSimpleTrx t05
                   , TSimpleTrx t06
                   , TSimpleTrx t07
                   , TSimpleTrx t08
                   , SheetComment [scmt||]
                   , TBrk t09
                   , TSimpleTrx t10
                   , TSimpleTrx t11
                   , TSimpleTrx t12
                   , TSimpleTrx t13
                   , TSimpleTrx t14
                   , TSimpleTrx t15
                   , TSimpleTrx t16
                   , TSimpleTrx t17
                   , SheetComment [scmt||]
                   ]
        , ast)
    ]

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.ParseEntry" [ parseTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
