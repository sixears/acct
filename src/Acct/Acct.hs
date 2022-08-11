{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Acct.Acct
  ( {- main -} )
where

-- import Prelude  ( head, tail )

-- base --------------------------------

-- import Control.Applicative     ( many )
-- import Control.Monad           ( forM_, join, return )
-- import Control.Monad.IO.Class  ( MonadIO, liftIO )
-- import Data.Function           ( ($) )
-- import System.IO               ( IO )

-- base-unicode-symbols ----------------

-- import Data.Function.Unicode  ( (∘) )
-- import Data.Monoid.Unicode    ( (⊕) )
-- import Data.Ord.Unicode       ( (≥) )

-- data-default ------------------------

-- import Data.Default  ( Default )

-- data-textual ------------------------

-- import Data.Textual  ( Printable( print ), toText )

-- fpath -------------------------------

-- import FPath.AsFilePath  ( filepath )
-- import FPath.File        ( File )
-- import FPath.Parseable   ( readM )

-- log-plus ----------------------------

-- import Log  ( Log, ToDoc_, toDoc_ )

-- logging-effect ----------------------

-- import Control.Monad.Log  ( MonadLog, Severity( Informational ) )

-- mockio ------------------------------

-- import MockIO.DoMock       ( HasDoMock, DoMock( DoMock, NoMock )  )
-- import MockIO.IOClass      ( HasIOClass )
-- import MockIO.MockIOClass  ( MockIOClass )

-- mockio-log --------------------------

-- import MockIO.Log      ( mkIOL' )
-- import MockIO.IOClass  ( IOClass( IORead ) )

-- monaderror-io -----------------------

-- import MonadError           ( MonadError, ѥ )
-- import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

-- import qualified  MonadIO.File

-- more-unicode ------------------------

-- import Data.MoreUnicode.Applicative  ( (⋪) )
-- import Data.MoreUnicode.Functor      ( (⊳) )
-- import Data.MoreUnicode.Lens         ( (⫥) )

-- mtl --------------------------------

-- import Control.Monad.Except  ( ExceptT )

-- natural -----------------------------

-- import Natural  ( One, count )

-- optparse-applicative ----------------

-- import Options.Applicative.Builder  ( argument, metavar)
-- import Options.Applicative.Types    ( Parser )

-- parsec ------------------------------

-- import Text.Parsec.Char  ( noneOf, oneOf )

-- parsec-plus -------------------------

-- import ParsecPlus  ( AsParseError, Parsecable( parser ), parsec )

-- prettyprinter -----------------------

-- import Data.Text.Prettyprint.Doc  ( Doc, parens )

-- stdmain -----------------------------

-- import StdMain             ( stdMain'' )
-- import StdMain.StdOptions  ( DryRunLevel )

-- text --------------------------------

-- import Data.Text     ( Text, pack )
-- import Data.Text.IO  ( putStrLn )

-- text-printer ------------------------

-- import qualified  Text.Printer  as  P

-- tfmt --------------------------------

-- import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- import Acct.AcctError   ( AcctError )

--------------------------------------------------------------------------------

{-
data Options = Options { input ∷ File }

parseOpts ∷ Parser Options
parseOpts =
  let
    -- dvorak_help = help "remap to dvorak layout"
  in
    Options ⊳ argument readM (metavar "LAYER-FILE")
-}
{-
main ∷ IO ()
main =
  main_ ≫ \ case
    Left e →
-}
{-
mockParens ∷ ToDoc_ τ ⇒ τ → DoMock → Doc ()
mockParens d DoMock = parens (toDoc_ d)
mockParens d NoMock = toDoc_ d

{- | `mkIOL'`, but for error-based IO (e.g., `MonadIO μ, MonadError ε μ`) -}
mkIOL'E ∷ (MonadIO μ, ToDoc_ τ, MonadError ε μ,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         (DoMock → τ) → μ α → ExceptT ε IO α → DoMock  → μ α
mkIOL'E msg mval io mck =
  join $ mkIOL' Informational IORead msg (return mval) (ѥ io) mck

readFileUTF8 ∷ (MonadIO μ,
                MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                AsIOError ε, MonadError ε μ) ⇒
               File → DoMock → μ Text
readFileUTF8 fn =
  mkIOL'E (mockParens $ [fmtT|read %T|] fn) (return "")
          (MonadIO.File.readFile fn)

readFileUTF8Lenient ∷ (MonadIO μ,
                       MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                       AsIOError ε, MonadError ε μ) ⇒
                      File → DoMock → μ Text
readFileUTF8Lenient fn =
  mkIOL'E (mockParens $ [fmtT|read %T|] fn) (return "")
          (MonadIO.File.readFileUTF8Lenient fn)
-}
{-
{- | Parse a file whose contents are UTF8-encoded text; with lenient decoding
     (see `readFileUTF8Lenient`. -}
parsecFileUTF8L ∷ ∀ χ ω ε μ .
                  (MonadIO μ, Parsecable χ,
                   MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω,
                   AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
                  File → χ → DoMock → μ χ
parsecFileUTF8L fn x mck = do
  t ← readFileUTF8Lenient fn mck
  mkIOL'E (mockParens $ [fmtT|parse %T|] fn) (return x) (parsec (fn ⫥ filepath) t) mck
-}
--------------------------------------------------------------------------------

-- run with, e.g., `:run main -v -v data/joint'`
{-
main ∷ MonadIO μ ⇒ μ ()
main = stdMain'' "parse accounts file" parseOpts main_

main_ ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, MonadError AcctError μ) ⇒
        DryRunLevel One → Options → μ ()
main_ mck opts = do
  let mock = if count mck ≥ 1 then DoMock else NoMock
-}
--  (Transactions ts) ← parsecFileUTF8L (input opts) (Transactions []) mock
--  ts ← parsecFileUTF8L @Transactions (input opts) emptyTrans mock
--  liftIO $ putStrLn ("head ts")
--  liftIO $ putStrLn ("head ts: '" ⊕ toText (head $ tail ts) ⊕ "'")
--  liftIO $ forM_ (unTransactions ts) (putStrLn ∘ toText)
--  liftIO $ putStrLn ("head ts: '" ⊕ toText t ⊕ "'")
{-
  _ ← ѥ (pResolve $ input opts) ≫ \ case
    Left e  → throwError e
    Right f → readAcctFile f mock
-}
{-
  return ()
-}
-- that's all, folks! ----------------------------------------------------------
