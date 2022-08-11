{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Acct.AcctError
  ( {- AcctError -} )
where

-- base --------------------------------

-- import Control.Exception  ( Exception )
-- import Data.Eq            ( Eq )
-- import Data.Function      ( ($), (&) )
-- import Data.Maybe         ( Maybe( Just, Nothing ) )
-- import Text.Show          ( Show )

-- base-unicode-symbols ----------------

-- import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

-- import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

-- import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathIOError
--                                , _FPIO_IO_ERROR, _FPIO_PATH_ERROR )

-- has-callstack -----------------------

-- import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

-- import Control.Lens.Lens   ( Lens, lens )
-- import Control.Lens.Prism  ( Prism', prism' )

-- monaderror-io -----------------------

-- import MonadError.IO.Error  ( AsIOError( _IOError ) )

-- more-unicode ------------------------

-- import Data.MoreUnicode.Lens  ( (⊣), (⊢) )

-- parsec-plus -------------------------

-- import ParsecPlus  ( AsParseError( _ParseError ), ParseError, Parsecable
--                    , parsec )

-- stdmain -----------------------------

-- import StdMain.UsageError  ( AsUsageError( _UsageError ), UsageError )

--------------------------------------------------------------------------------

{- | Disjunction of all error types in Acct program. -}
{-
data AcctError = ACCT_FPIO_E  FPathIOError
               | ACCT_USAGE_E UsageError
               | ACCT_PARSE_E ParseError
  deriving (Eq, Show)

instance Exception AcctError

_ACCT_FPIO_E ∷ Prism' AcctError FPathIOError
_ACCT_FPIO_E = prism' (\ e → ACCT_FPIO_E e)
                       (\ case ACCT_FPIO_E e → Just e; _ → Nothing)

instance AsFPathError AcctError where
  _FPathError = _ACCT_FPIO_E ∘ _FPIO_PATH_ERROR

instance AsIOError AcctError where
  _IOError = _ACCT_FPIO_E ∘ _FPIO_IO_ERROR

instance AsUsageError AcctError where
  _UsageError = prism' ACCT_USAGE_E (\ case ACCT_USAGE_E e → Just e; _ →Nothing)

instance AsParseError AcctError where
  _ParseError = prism' ACCT_PARSE_E (\ case ACCT_PARSE_E e → Just e; _ →Nothing)

instance Printable AcctError where
  print (ACCT_FPIO_E  e) = print e
  print (ACCT_USAGE_E e) = print e
  print (ACCT_PARSE_E e) = print e

instance HasCallstack AcctError where
  callstack = lens (\ (ACCT_FPIO_E e) → e ⊣ callstack)
                   (\ ae cs →
                      case ae of
                        ACCT_FPIO_E  e → ACCT_FPIO_E  $ e & callstack ⊢ cs
                        ACCT_USAGE_E e → ACCT_USAGE_E $ e & callstack ⊢ cs
                   )
-}

-- that's all, folks! ----------------------------------------------------------
