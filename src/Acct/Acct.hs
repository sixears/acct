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
import Prelude  ( error )

-- base --------------------------------

import Control.Monad  ( foldM_ )
import Data.Foldable  ( sum )
import Data.List      ( filter, reverse, sortOn )
import GHC.Exts       ( toList )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.At      ( at )
import Control.Lens.Getter  ( view )

-- monadio-plus ------------------------

import MonadIO       ( say, warn )
import MonadIO.Base  ( getArgs )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag', help, long, metavar, option
                                    , short )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument )

-- stdmain -----------------------------

import StdMain             ( stdMain )
import StdMain.UsageError  ( UsageIOError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount       ( Amount, amount, aTotal, pretty )
import Acct.AcctState    ( AcctState, accounts, stmts )
import Acct.Date         ( date )
import Acct.Entries      ( parseFile )
import Acct.Stmt         ( Stmt( Stmt ), stmtY )
import Acct.StmtIndex    ( stmtIndex, stmtindex )
import Acct.StmtEntries  ( StmtEntries )
import Acct.StmtEntry    ( StmtEntry( SE_BRK, SE_SIMP ) )
import Acct.TrxBrkHead   ( TrxBrkHead )
import Acct.TrxBrk       ( hd, prettyBrk )
import Acct.TrxSimp      ( TrxSimp, parent )

--------------------------------------------------------------------------------

data DumpType = DUMP_ACCTS | DUMP_STMTS | DUMP_STMT ℕ | OUTPUT_STMT ℕ
  deriving (Eq,Show)

data Options = Options { input    ∷ File
                       , dumpType ∷ DumpType
                       }

parseOpts ∷ Parser Options
parseOpts =
  Options ⊳ textualArgument (metavar "ACCOUNTS-FILE")
          ⊵ (flag' DUMP_STMTS (ю [ short 'x', long "dump-stmts"
                                 , help "dump statement totals" ])
            ∤ DUMP_STMT ⊳ option auto  (ю [ short 'X', long "to-stmt"
                                          , help "print statement transactions"
                                          ])
            ∤ OUTPUT_STMT ⊳ option auto  (ю [ short 'S', long "print-stmt"
                                            , help ("write statement to stdout "
                                                   ⊕"suitable for its own file")
                                            ])
            ∤ pure DUMP_ACCTS
            )

----------------------------------------

main ∷ IO()
main = do
  let prog_desc = "read an accounts file, emit totals"
  let do_main _ opts = liftIO $ do
        let i = input opts
        parseFile i ≫ \ case
          𝕹 → warn ([fmtT|error parsing file '%T'|] i) ⪼ return 10
          𝕵 (_,as) → case dumpType opts of
                       DUMP_STMTS  → dump_stmts as
                       DUMP_STMT x → dump_stmt as x
                       OUTPUT_STMT x → output_stmt as x
                       DUMP_ACCTS  → dump_accts as
  getArgs ≫ stdMain @UsageIOError prog_desc parseOpts do_main

----------------------------------------

dump_accts ∷ AcctState → IO Word8
dump_accts as = do
  ms ← forM (toList $ as ⊣ accounts) $ \ (a,ts) → do
                                         let m = aTotal ts
                                         say $ [fmtT|%20T  %12t|] a (pretty m)
                                         return m
  say $ [fmtT|Total:  %10t|] (pretty $ sum ms)
  return 0

----------------------------------------

stmtFold ∷ [(α, Amount, Amount)] → (α, StmtEntries) → [(α, Amount, Amount)]
stmtFold (x@(_,_,c):xs) (i,toList → ts) =
  let new = (i,(aTotal ts),c + (aTotal ts))
  in  new : x : xs
stmtFold [] (i,toList → ts) =
  [(i,(aTotal ts),(aTotal ts))]

----------------------------------------

dump_stmt ∷ MonadIO μ ⇒ AcctState → ℕ → μ Word8
dump_stmt as n =
  let n'  = stmtIndex (𝕵 $ Stmt n)
      st ∷ 𝕄 StmtEntries
      st = as ⊣ stmts ∘ at n'
      sts = foldl stmtFold [] (sortOn fst ∘ toList $ as ⊣ stmts)
  in do
    let prior_stmt = last $ filter (\ (i,_,_) → i < n')
                          $ reverse sts
    acc ← case prior_stmt of
      𝕵 (i,_,t) → do say ([fmtT|Statement %03T  Accumulated %12t|] i (pretty t))
                     return t
      𝕹         → return 0
    case st of
      𝕹    → warn ([fmtT|no such statement found: %d|] n) ⪼ return 11
      𝕵 ts → liftIO $ do
        let -- if p (which should be the parent of t) has already been printed;
            -- then do nothing; else if p's stmt is t's stmt; then print it
            -- (adding it and its amount to the accumulator); else print t
            -- (again, adding it to the accumulator)
            maybe_print_entry ∷ MonadIO μ ⇒
                                (Amount,[TrxBrkHead]) → TrxSimp → TrxBrkHead
                              → μ (Amount,[TrxBrkHead])
            maybe_print_entry (tacc,xs) t p =
                                if p ∈ xs
                                then return (tacc,xs)
                                else if p ⊣ stmtY ≡ 𝕵 (Stmt n)
                                     then do say $ pretty p
                                             return (tacc + p⊣amount, p:xs)
                                     else do say $ pretty t
                                             return (tacc + t⊣amount, xs)

            -- Print each statement entry, and add its amount to a running
            -- summary total - unless it is a sub-trx of a breakdown whose
            -- header has already been printed.
            print_entry ∷ MonadIO μ ⇒ (Amount,[TrxBrkHead]) → StmtEntry
                                    → μ (Amount,[TrxBrkHead])
            print_entry (tacc,xs) x =
              case x of
                SE_BRK b → do
                  say $ pretty b
                  return (tacc + b⊣amount, b ⊣ hd : xs)
                SE_SIMP t →
                  case t ⊣ parent of
                    𝕹   → do
                      say $ pretty t
                      return (tacc + t⊣amount, xs)
                    𝕵 p → maybe_print_entry (tacc,xs) t p

        (ts_acc,_) ← foldM print_entry (0,[]) (sortOn (view date) (toList ts))
        say ([fmtT|Statement %03T  Accumulated %12t|] n' (pretty (acc+ts_acc)))
        return 0

----------------------------------------

output_stmt ∷ MonadIO μ ⇒ AcctState → ℕ → μ Word8
output_stmt as n =
  let n'  = stmtIndex (𝕵 $ Stmt n)
      st ∷ 𝕄 StmtEntries
      st = as ⊣ stmts ∘ at n'
  in do
    -- XXX output comment for statement
    -- XXX output accounts, o accts
    case st of
      𝕹    → warn ([fmtT|no such statement found: %d|] n) ⪼ return 11
      𝕵 ts → liftIO $ do
        let -- Print each statement entry - unless it is a sub-trx of a
            -- breakdown whose header has already been printed.
            print_entry ∷ MonadIO μ ⇒ [TrxBrkHead] → StmtEntry → μ [TrxBrkHead]
            print_entry xs x =
              case x of
                SE_BRK b → do
                  say $ "\n" ⊕ prettyBrk b ⊕ "\n"
                  return (b ⊣ hd : xs)
                SE_SIMP t →
                  case t ⊣ parent of
                    𝕹   → do
                      say $ pretty t
                      return xs
                    𝕵 p → error $ [fmt|inconsistent trx stmts?\n#%T\n%T|] t p


        foldM_ print_entry [] (sortOn (view date) (toList ts))
        return 0

----------------------------------------

dump_stmts ∷ AcctState → IO Word8
dump_stmts as = do
  let sts = foldl stmtFold [] (sortOn fst ∘ toList $ as ⊣ stmts)
  forM_ (reverse sts) $ \ (i,a,t) →
    if i ≡ [stmtindex||]
    then say $ [fmtT|Total: %t|] (pretty t)
    else say ([fmtT|  Statement %03T  This Stmt %12t  Accumulated %12t|]
              i (pretty a) (pretty t))
  return 0

-- that's all, folks! ----------------------------------------------------------
