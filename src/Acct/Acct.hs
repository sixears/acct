{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Acct.Acct
  ( main )
where

import Base1T  hiding  ( toList )

-- base --------------------------------

import qualified  Data.List           as  List
import qualified  Data.List.NonEmpty  as  NonEmpty

import Data.Foldable       ( sum )
import Data.List           ( filter, reverse, sortOn )
import Data.String         ( unwords )
import GHC.Exts            ( toList )
import Text.Read           ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile )
import FPath.File     ( File )

-- lens --------------------------------

import Control.Lens.At      ( at )
import Control.Lens.Getter  ( view )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock )

-- mockio-log --------------------------

import MockIO.Log  ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO        ( say, warn )
import MonadIO.Base   ( getArgs )
import MonadIO.FPath  ( pResolve )

-- mtl ---------------------------------

import Control.Monad.Writer  ( MonadWriter, execWriterT, tell )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag', help, long, metavar, option
                                    , short )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( readT, textualArgument )

-- parsers -----------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( eof )

-- stdmain -----------------------------

import StdMain  ( LogTIO, stdMain )

-- text --------------------------------

import Data.Text  ( unlines )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account      ( Account )
import Acct.AcctError    ( AcctError, exitCode, throwInconsistentTrxStmtsE
                         , throwNoNonStmtTrxFoundE, throwNoSuchStmtE )
import Acct.AcctState    ( AcctState, accounts, stmts )
import Acct.Amount       ( Amount, amount, aTotal, pretty )
import Acct.Annotation   ( Annotation )
import Acct.Date         ( date )
import Acct.EntrySource  ( EntrySource( SourceFile ) )
import Acct.ParseEntry   ( processInput )
import Acct.OStmtName    ( OStmtName )
import Acct.Stmt         ( Stmt( Stmt ), stmtY )
import Acct.StmtIndex    ( stmtIndex, stmtindex )
import Acct.StmtEntries  ( StmtEntries )
import Acct.StmtEntry    ( StmtEntry( SE_BRK, SE_SIMP ), accts, oAccts )
import Acct.StmtIndex    ( notStmtIndex )
import Acct.TEntry       ( TEntry )
import Acct.TrxBrkHead   ( TrxBrkHead )
import Acct.TrxBrk       ( hd, prettyBrk )
import Acct.TrxSimp      ( TrxSimp, parent )

--------------------------------------------------------------------------------

newtype MaybeN = MaybeN (???? ???)  deriving  (Eq,Show)

instance Printable MaybeN where
  print (MaybeN ????) = "??nothing??"
  print (MaybeN (???? i)) = P.text $ [fmt|%d|] i

instance Textual MaybeN where
  textual = MaybeN ??? ((const ???? ??? eof) ??? (???? ??? read ??? some digit))

data DumpType = DUMP_ACCTS | DUMP_STMTS | DUMP_STMT ??? | OUTPUT_STMT MaybeN
              | TUPTUO_STMT MaybeN | REMOVE_STMT ???
  deriving (Eq,Show)

data Options = Options { input    ??? File
                       , dumpType ??? DumpType
                       }

parseOpts ??? Parser Options
parseOpts =
  let dump_stmt_help   = "print statement transactions"
      output_stmt_help = unwords [ "write statement to stdout suitable for its"
                                 , "own file; use '' for the empty stmt"]
      tuptuo_stmt_help = unwords [ "write stdout all entries not in a given"
                                 , "stmt; suitable for re-reading from a file;"
                                 , "use '' for the empty stmt"
                                 ]
      remove_stmt_help = unwords [ "write statement to its own file;"
                                 , "re-write the main file excluding that stmt"]
  in  Options ??? textualArgument (metavar "ACCOUNTS-FILE")
              ??? (flag' DUMP_STMTS (?? [ short 'x', long "dump-stmts"
                                     , help "dump statement totals" ])
                ??? DUMP_STMT ??? option auto     (?? [ short 'X', long "to-stmt"
                                              , help dump_stmt_help ])
                ??? OUTPUT_STMT ??? option readT  (?? [ short 'S', long "print-stmt"
                                                 , help output_stmt_help
                                                 ])
                ??? TUPTUO_STMT ??? option readT  (?? [ long "tnirp-stmt"
                                                 , help tuptuo_stmt_help
                                                 ])
                ??? REMOVE_STMT ??? option auto   (?? [ short 'R', long "remove-stmt"
                                                 , help remove_stmt_help ])
                ??? pure DUMP_ACCTS
                )

----------------------------------------

main ??? IO()
main = do
  let prog_desc = "read an accounts file, emit totals"
  let do_main ??? DoMock ??? Options ??? LogTIO MockIOClass AcctError Word8
      do_main _ opts = do
        i ??? AbsFile ??? pResolve (input opts)
        ?? @AcctError (processInput $ SourceFile i) ??? \ case
          ???? e ??? warn ([fmtT|error parsing file '%T': %T|] i e) ??? return 10
          ???? (es,as) ??? liftIO $ case dumpType opts of
                       DUMP_STMTS             ??? dump_stmts as
                       DUMP_STMT x            ??? dump_stmt as x
                       OUTPUT_STMT (MaybeN x) ??? output_stmt as x
                       TUPTUO_STMT (MaybeN x) ??? tuptuo_stmt es x
                       DUMP_ACCTS             ??? dump_accts as
  getArgs ??? stdMain prog_desc parseOpts do_main

----------------------------------------

dump_accts ??? AcctState ??? IO Word8
dump_accts as = do
  ms ??? forM (toList $ as ??? accounts) $ \ (a,ts) ??? do
                                         let m = aTotal ts
                                         say $ [fmtT|%20T  %12t|] a (pretty m)
                                         return m
  say $ [fmtT|Total:  %10t|] (pretty $ sum ms)
  return 0

----------------------------------------

stmtFold ??? [(??, Amount, Amount)] ??? (??, StmtEntries) ??? [(??, Amount, Amount)]
stmtFold (x@(_,_,c):xs) (i,toList ??? ts) =
  let new = (i,(aTotal ts),c + (aTotal ts))
  in  new : x : xs
stmtFold [] (i,toList ??? ts) =
  [(i,(aTotal ts),(aTotal ts))]

----------------------------------------

dump_stmt ??? MonadIO ?? ??? AcctState ??? ??? ??? ?? Word8
dump_stmt as n =
  let n'  = stmtIndex (???? $ Stmt n)
      st ??? ???? StmtEntries
      st = as ??? stmts ??? at n'
      sts = foldl stmtFold [] (sortOn fst ??? toList $ as ??? stmts)
  in do
    let prior_stmt = last $ filter (\ (i,_,_) ??? i < n')
                          $ reverse sts
    acc ??? case prior_stmt of
      ???? (i,_,t) ??? do say ([fmtT|Statement %03T  Accumulated %12t|] i (pretty t))
                     return t
      ????         ??? return 0
    case st of
      ????    ??? warn ([fmtT|no such statement found: %d|] n) ??? return 11
      ???? ts ??? liftIO $ do
        let -- if p (which should be the parent of t) has already been printed;
            -- then do nothing; else if p's stmt is t's stmt; then print it
            -- (adding it and its amount to the accumulator); else print t
            -- (again, adding it to the accumulator)
            maybe_print_entry ??? MonadIO ?? ???
                                (Amount,[TrxBrkHead]) ??? TrxSimp ??? TrxBrkHead
                              ??? ?? (Amount,[TrxBrkHead])
            maybe_print_entry (tacc,xs) t p =
                                if p ??? xs
                                then return (tacc,xs)
                                else if p ??? stmtY ??? ???? (Stmt n)
                                     then do say $ pretty p
                                             return (tacc + p???amount, p:xs)
                                     else do say $ pretty t
                                             return (tacc + t???amount, xs)

            -- Print each statement entry, and add its amount to a running
            -- summary total - unless it is a sub-trx of a breakdown whose
            -- header has already been printed.
            print_entry ??? MonadIO ?? ??? (Amount,[TrxBrkHead]) ??? StmtEntry
                                    ??? ?? (Amount,[TrxBrkHead])
            print_entry (tacc,xs) x =
              case x of
                SE_BRK b ??? do
                  say $ pretty b
                  return (tacc + b???amount, b ??? hd : xs)
                SE_SIMP t ???
                  case t ??? parent of
                    ????   ??? do
                      say $ pretty t
                      return (tacc + t???amount, xs)
                    ???? p ??? maybe_print_entry (tacc,xs) t p

        (ts_acc,_) ??? foldM print_entry (0,[]) (sortOn (view date) (toList ts))
        say ([fmtT|Statement %03T  Accumulated %12t|] n' (pretty (acc+ts_acc)))
        return 0

----------------------------------------

{-| Print each statement entry - unless it is a sub-trx of a breakdown whose
    header has already been printed.  The accumulator of TrxBrkHeads that have
    already been printed is passed in as the first argument, and returned.
 -}
textEntry ??? (MonadError AcctError ??, MonadWriter ???? ??) ???
            [TrxBrkHead] ??? StmtEntry ??? ?? [TrxBrkHead]
textEntry xs (SE_BRK  b) = do
  tell $ "\n" ??? prettyBrk b ??? "\n"
  return (b ??? hd : xs)
textEntry xs (SE_SIMP t) =
  case t ??? parent of
    ????   ??? tell (pretty t ??? "\n") ??? return xs
    ???? p ??? throwInconsistentTrxStmtsE t p

----------------------------------------

entriesAccounts ??? StmtEntries ??? NonEmpty Account
entriesAccounts ts =
  NonEmpty.sort ??? NonEmpty.nub $ foldl1 (???) (accts ??? toList ts)

----------------------------------------

entriesOAccounts ??? StmtEntries ??? [OStmtName]
entriesOAccounts ts = List.sort ??? List.nub $ foldl1 (???) (oAccts ??? toList ts)

----------------------------------------

entriesAccountsText ??? MonadWriter ???? ?? ??? StmtEntries ??? ?? ()
entriesAccountsText ts =
  forM_ (entriesAccounts ts) (tell ??? [fmt|ReStart: %T\n|])

----------------------------------------

entriesOAccountsText ??? MonadWriter ???? ?? ??? StmtEntries ??? ?? ()
entriesOAccountsText ts =
  forM_ (entriesOAccounts ts) (tell ??? [fmt|ReoStart: %T\n|])

----------------------------------------

{-| Format text for a statement, suitable for writing to its own file. -}
outputStmt ??? MonadError AcctError ?? ??? AcctState ??? ???? ??? ??? ?? ????
outputStmt as n =
  let st ??? ???? StmtEntries
      st = as ??? stmts ??? at (stmtIndex (Stmt ??? n))
  in do
    case st of
      ????    ??? case n of
                ????   ??? throwNoNonStmtTrxFoundE
                ???? m ??? throwNoSuchStmtE m
      ???? ts ??? do
        let pfx = maybe "" [fmt|## Statement: %d\n\n|] n
        t ??? execWriterT $ do
              entriesAccountsText ts
              entriesOAccountsText ts
              tell "\n"
              foldM textEntry [] (sortOn (view date) (toList ts))
        return $ pfx ??? t

----------------------------------------

{-| Format text for all statements except a given one (or except the
    non-statement trx), suitable for writing a file. -}
tuptuoStmt ??? MonadError AcctError ?? ???
             [Annotation TEntry EntrySource] ??? ???? ??? ??? ?? ????
tuptuoStmt es n = do
  case filter (??? ??? notStmtIndex (stmtIndex (Stmt ??? n))) $ es of
    [] ??? throwNoNonStmtTrxFoundE
    ts ??? return ??? unlines $ toText ??? ts

----------------------------------------

output_stmt ??? MonadIO ?? ??? AcctState ??? ???? ??? ??? ?? Word8
output_stmt as n = case outputStmt as n of
                     ???? e ??? warn (toText e) ??? return (exitCode e)
                     ???? t ??? say t ??? return 0

----------------------------------------

tuptuo_stmt ??? MonadIO ?? ??? [Annotation TEntry EntrySource] ??? ???? ??? ??? ?? Word8
tuptuo_stmt es n = case tuptuoStmt es n of
                     ???? e ??? warn (toText e) ??? return (exitCode e)
                     ???? t ??? say t ??? return 0

----------------------------------------

dump_stmts ??? AcctState ??? IO Word8
dump_stmts as = do
  let sts = foldl stmtFold [] (sortOn fst ??? toList $ as ??? stmts)
  forM_ (reverse sts) $ \ (i,a,t) ???
    if i ??? [stmtindex||]
    then say $ [fmtT|Total: %t|] (pretty t)
    else say ([fmtT|  Statement %03T  This Stmt %12t  Accumulated %12t|]
              i (pretty a) (pretty t))
  return 0

-- that's all, folks! ----------------------------------------------------------
