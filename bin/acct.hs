{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative  ( (<*>), (<**>), some )
import Data.Function        ( ($) )
import Data.Functor         ( (<$>) )
import System.IO            ( FilePath, IO, putStrLn )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- diagrams-lib ------------------------

import Diagrams.TwoD.Size  ( mkWidth )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG  ( renderPretty )

-- fpath -------------------------------

import FPath.Parseable  ( readM )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag, fullDesc, help, info, long
                                    , metavar, option, progDesc, short
                                    , strArgument, strOption, value
                                    )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data Options = Options { input ∷ FilePath }

parseOpts ∷ Parser Options
parseOpts =
  let
    -- dvorak_help = help "remap to dvorak layout"
  in
    Options <$> strArgument (metavar "LAYER-FILE")

main ∷ IO ()
main = do
  let prog_desc = progDesc "create an atreus layout svg from layer descriptions"
      parser ∷ ParserInfo Options
      parser = info (parseOpts <**> helper) (fullDesc ⊕ prog_desc)
  opts ← execParser parser
  putStrLn "hello"

-- that's all, folks! ----------------------------------------------------------
