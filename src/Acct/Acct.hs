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

import Data.Foldable  ( sum )
import Data.List      ( reverse, sortOn )
import GHC.Exts       ( toList )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.At  ( at )

-- monadio-plus ------------------------

import MonadIO  ( say, warn )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag', fullDesc, help, info, long
                                    , metavar, option, progDesc, short )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount     ( asText, aTotal )
import Acct.AcctState  ( AcctState, accounts, stmts )
import Acct.Entries    ( parseFile )
import Acct.Stmt       ( Stmt( Stmt ) )
import Acct.StmtIndex  ( stmtIndex, stmtindex )
import Acct.TrxSimp    ( TrxSimp )

--------------------------------------------------------------------------------

data DumpType = DUMP_ACCTS | DUMP_STMTS | DUMP_STMT ℕ  deriving  (Eq,Show)

data Options = Options { input ∷ File
                       , dumpType ∷ DumpType
                       }

parseOpts ∷ Parser Options
parseOpts =
  Options ⊳ textualArgument (metavar "ACCOUNTS-FILE")
          ⊵ (flag' DUMP_STMTS (ю [ short 'x', long "dump-stmts"
                                 , help "dump statement totals" ])
            ∤ DUMP_STMT ⊳ option auto  (ю [ short 'X', long "to-stmt"
                                          , help "print total to statement" ])
            ∤ pure DUMP_ACCTS
            )

----------------------------------------

main ∷ IO()
main = do
  let prog_desc = progDesc "read an accounts file, emit totals"
      parser ∷ ParserInfo Options
      parser = info (parseOpts ⊴ helper) (fullDesc ⊕ prog_desc)
  opts ← execParser parser

  let i = input opts
  parseFile i ≫ \ case
    𝕹 → warn $ [fmtT|error parsing file '%T'|] i
    𝕵 (_,as) → case dumpType opts of
                 DUMP_STMTS  → dump_stmts as
                 DUMP_STMT x → dump_stmt as x
                 DUMP_ACCTS  → dump_accts as

----------------------------------------

dump_accts ∷ AcctState → IO()
dump_accts as = do
  ms ← forM (toList $ as ⊣ accounts) $ \ (a,ts) → do
                                         let m = aTotal ts
                                         say $ [fmtT|%20T  %10t|] a (asText m)
                                         return m
  say $ [fmtT|Total:  %10t|] (asText $ sum ms)

----------------------------------------

dump_stmt ∷ AcctState → ℕ → IO()
dump_stmt as n =
  let _fld (x@(_,_,c):xs) (i,ts) =
        let new = (i,(aTotal ts),c + (aTotal ts))
        in  new : x : xs
      _fld [] (i,ts) =
        [(i,(aTotal ts),(aTotal ts))]
      st ∷ 𝕄 [TrxSimp]
      st = as ⊣ stmts ∘ at (stmtIndex ∘ 𝕵 $ Stmt n)
      sts = foldl _fld [] (sortOn fst ∘ toList $ as ⊣ stmts)
  in  forM_ (reverse sts) $ \ (i,a,t) →
    if i ≡ [stmtindex||]
    then say $ [fmtT|Total: %t|] (asText t)
    else say ([fmtT|  Statement %03T  This Stmt %12t  Accumulated %12t|]
              i (asText a) (asText t))

----------------------------------------

dump_stmts ∷ AcctState → IO()
dump_stmts as =
  let _fld (x@(_,_,c):xs) (i,ts) =
        let new = (i,(aTotal ts),c + (aTotal ts))
        in  new : x : xs
      _fld [] (i,ts) =
        [(i,(aTotal ts),(aTotal ts))]
      sts = foldl _fld [] (sortOn fst ∘ toList $ as ⊣ stmts)
  in  forM_ (reverse sts) $ \ (i,a,t) →
    if i ≡ [stmtindex||]
    then say $ [fmtT|Total: %t|] (asText t)
    else say ([fmtT|  Statement %03T  This Stmt %12t  Accumulated %12t|]
              i (asText a) (asText t))

-- that's all, folks! ----------------------------------------------------------
