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

import Debug.Trace     ( trace, traceShow, traceM )
import Base1T  hiding  ( toList )

-- base --------------------------------

import Data.Foldable  ( sum )
import Data.List      ( reverse, sortOn )
import Data.Maybe     ( fromMaybe )
import GHC.Exts       ( toList )
import System.IO      ( putStrLn )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Tuple   ( _3 )

-- monadio-plus ------------------------

import MonadIO  ( say, warn )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( fullDesc, help, info, long, metavar, progDesc, short, switch )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

-- optparse-plus -----------------------

import OptParsePlus  ( textualArgument )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Amount     ( amount, amt, asText )
import Acct.AcctState  ( accounts, stmts )
import Acct.Entries    ( parseFile )
import Acct.StmtIndex  ( stmtindex )

--------------------------------------------------------------------------------

data Options = Options { input ∷ File
                       , dumpStmts ∷ 𝔹
                       }

parseOpts ∷ Parser Options
parseOpts =
  let
    -- dvorak_help = help "remap to dvorak layout"
  in
    Options ⊳ textualArgument (metavar "ACCOUNTS-FILE")
            ⊵ switch (ю [ short 'x', long "dump-stmts"
                        , help "dump statement totals" ])

main ∷ IO()
main = do
  let prog_desc = progDesc "read an accounts file, emit totals"
      parser ∷ ParserInfo Options
      parser = info (parseOpts ⊴ helper) (fullDesc ⊕ prog_desc)
  opts ← execParser parser

  let i = input opts
  parseFile i ≫ \ case
    𝕹 → warn $ [fmtT|error parsing file '%T'|] i
    𝕵 (_,as) → do
      ms ← forM (toList $ as ⊣ accounts) $ \ (a,ts) →
        let m = sum (view amount ⊳ ts)
        in  putStrLn ([fmt|%20T  %10t|] a (asText m)) ⪼ return m
      putStrLn $ [fmt|Total:  %10t|] (asText $ sum ms)

-- XXX only in -x

      let _fld (x@(_,_,c):xs) (i,ts) =
            let new = (i,(sum $ view amount ⊳ ts),c + (sum $ view amount ⊳ ts))
            in  new : x : xs
          _init = [([stmtindex||],0,0)]
          sts = foldl _fld _init (sortOn fst ∘ toList $ as ⊣ stmts)
      forM_ (reverse sts) $ \ (i,a,t) →
        if i ≡ [stmtindex||]
        then putStrLn $ [fmt|Total: %t|] (asText t)
        else putStrLn ([fmt|  Statement %03T  This Stmt %12t  Accumulated %12t|]
                       i (asText a) (asText t))
  say i

-- that's all, folks! ----------------------------------------------------------
