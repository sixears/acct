module Acct.AcctError
  ( AcctError, AsAcctProcessingError, AsImportError, AsParseError, ParseError
  , exitCode
  , throwAcctRestartE, throwImportError, throwInconsistentTrxStmtsE
  , throwInvalidAccountE, throwInvalidOAccountE, throwNoImportE
  , throwNoNonStmtTrxFoundE, throwNoSuchStmtE, throwOAcctRestartE, throwParseE
  )
where

import Base1T

-- fpath -------------------------------

import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError
                               , _FPathError, _FPIO_IO_ERROR, _FPIO_PATH_ERROR )

-- stdmain -----------------------------

import StdMain.UsageError  ( AsUsageError( _UsageError ), UsageError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta ----------------------------

import Text.Trifecta  ( ErrInfo )

-- trifecta-plus -----------------------

import TrifectaPlus  ( eiText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Acct.Account     ( Account )
import Acct.OStmtName   ( OStmtName )
import Acct.TrxBrkHead  ( TrxBrkHead )
import Acct.TrxSimp     ( TrxSimp )

--------------------------------------------------------------------------------

data ImportError = ImportError AbsFile 𝕋 CallStack  deriving  Show

instance Exception ImportError

----------------------------------------

instance HasCallstack ImportError where
  callstack = lens (\ (ImportError _ _ cs) → cs)
                   (\ (ImportError f t _) cs → ImportError f t cs)

----------------------------------------

instance Printable ImportError where
  print (ImportError f t _) = P.text $ [fmt|ImportError (%T): %t|] f t

------------------------------

class AsImportError ε where
  _ImportError ∷ Prism' ε ImportError

instance AsImportError ImportError where
  _ImportError = id

----------------------------------------

throwImportError ∷ ∀ ε α η . (AsImportError ε, MonadError ε η, HasCallStack) ⇒
                   AbsFile → 𝕋 → η α
throwImportError f e = throwError  ∘ (_ImportError#) $ ImportError f e callStack

------------------------------------------------------------

{- | Disjunction of all error types in Acct program. -}
data AcctProcessingError = AcctRestartE          Account            CallStack
                         | OAcctRestartE         OStmtName          CallStack
                         | InvalidAccountE       Account            CallStack
                         | InvalidOAccountE      OStmtName TrxSimp  CallStack
                         | NoImportE                                CallStack
                         | NoNonStmtTrxFoundE                       CallStack
                         | InconsistentTrxStmtsE TrxSimp TrxBrkHead CallStack
                         | NoSuchStmtE           ℕ                  CallStack
  deriving Show

--------------------

instance Exception AcctProcessingError

instance HasCallstack AcctProcessingError where
  callstack = lens (\ ape → case ape of
                       (AcctRestartE          _   cs) → cs
                       (OAcctRestartE         _   cs) → cs
                       (InvalidAccountE       _   cs) → cs
                       (InvalidOAccountE      _ _ cs) → cs
                       (NoImportE                 cs) → cs
                       (NoNonStmtTrxFoundE        cs) → cs
                       (InconsistentTrxStmtsE _ _ cs) → cs
                       (NoSuchStmtE           _   cs) → cs
                   )
                   (\ ape cs → case ape of
                       AcctRestartE a _       → AcctRestartE       a cs
                       OAcctRestartE o _      → OAcctRestartE      o cs
                       InvalidAccountE a _    → InvalidAccountE    a cs
                       InvalidOAccountE o s _ → InvalidOAccountE o s cs
                       NoImportE _            → NoImportE            cs
                       NoNonStmtTrxFoundE _   → NoNonStmtTrxFoundE   cs
                       InconsistentTrxStmtsE s b _
                                              → InconsistentTrxStmtsE s b cs
                       NoSuchStmtE n _        → NoSuchStmtE        n cs
                   )

--------------------

instance Printable AcctProcessingError where
  print (NoNonStmtTrxFoundE _) = P.text "no non-statement trx found!"
  print (NoSuchStmtE n _)      = P.text $ [fmt|no such statement found: %d|] n
  print (AcctRestartE a _)     =
    P.text $ [fmt|Cannot re-start extant account '%T'|] a
  print (OAcctRestartE a _)    =
    P.text $ [fmt|Cannot re-start extant other account '%T'|] a
  print (InvalidAccountE a _) =
    P.text $ [fmt|Not a valid account '%T'|] a
  print (InvalidOAccountE a t _) =
    P.text $ [fmt|Not a valid other account '%T' (%T)|] a t
  print (NoImportE _) =
    P.text $ [fmt|No import allowed from string|]
  print (InconsistentTrxStmtsE t h _) =
    P.text $ [fmt|inconsistent trx stmts?\n#%T\n%T|] t h

------------------------------------------------------------

class AsAcctProcessingError ε where
  _AcctProcessingError ∷ Prism' ε AcctProcessingError

--------------------

instance AsAcctProcessingError AcctProcessingError where
  _AcctProcessingError = id

----------------------------------------

throwAcctRestartE ∷ ∀ ε α η .
                    (AsAcctProcessingError ε, MonadError ε η, HasCallStack) ⇒
                    Account → η α
throwAcctRestartE a =
  throwError  ∘ (_AcctProcessingError #) $ AcctRestartE a callStack

----------------------------------------

throwOAcctRestartE ∷ ∀ ε α η .
                     (AsAcctProcessingError ε, MonadError ε η, HasCallStack) ⇒
                     OStmtName → η α
throwOAcctRestartE a =
  throwError ∘ (_AcctProcessingError #) $ OAcctRestartE a callStack

----------------------------------------

throwInvalidAccountE ∷ ∀ ε α η .
                       (AsAcctProcessingError ε, MonadError ε η, HasCallStack) ⇒
                       Account → η α
throwInvalidAccountE a =
  throwError ∘ (_AcctProcessingError #) $ InvalidAccountE a callStack

----------------------------------------

throwInvalidOAccountE ∷ ∀ ε α η .
                        (AsAcctProcessingError ε, MonadError ε η, HasCallStack)⇒
                        OStmtName → TrxSimp → η α
throwInvalidOAccountE a t =
  throwError ∘ (_AcctProcessingError #) $ InvalidOAccountE a t callStack

----------------------------------------

throwNoImportE ∷ ∀ ε α η .
                 (AsAcctProcessingError ε, MonadError ε η, HasCallStack) ⇒ η α
throwNoImportE = throwError ∘ (_AcctProcessingError #) $ NoImportE callStack

----------------------------------------

throwNoNonStmtTrxFoundE ∷ ∀ ε α η .
                          (AsAcctProcessingError ε, MonadError ε η,
                           HasCallStack) ⇒
                          η α
throwNoNonStmtTrxFoundE =
  throwError ∘ (_AcctProcessingError #) $ NoNonStmtTrxFoundE callStack

----------------------------------------

throwInconsistentTrxStmtsE ∷ ∀ ε α η .
                             (AsAcctProcessingError ε, MonadError ε η,
                              HasCallStack) ⇒
                             TrxSimp → TrxBrkHead → η α
throwInconsistentTrxStmtsE a t =
  throwError ∘ (_AcctProcessingError #) $ InconsistentTrxStmtsE a t callStack

----------------------------------------

throwNoSuchStmtE ∷ ∀ ε α η .
                   (AsAcctProcessingError ε, MonadError ε η, HasCallStack) ⇒
                   ℕ → η α
throwNoSuchStmtE a =
  throwError ∘ (_AcctProcessingError #) $ NoSuchStmtE a callStack

------------------------------------------------------------

data ParseError = ParseError ErrInfo CallStack deriving  Show

--------------------

instance Exception ParseError
instance HasCallstack ParseError where
  callstack = lens (\ (ParseError _ cs) → cs)
                   (\ (ParseError ei _) cs → ParseError ei cs)

--------------------

instance Printable ParseError where
  print (ParseError e _) = P.text $ eiText e

------------------------------------------------------------

class AsParseError ε where
  _ParseError ∷ Prism' ε ParseError

--------------------

instance AsParseError ParseError where
  _ParseError = id

----------------------------------------

throwParseE ∷ ∀ ε α η . (AsParseError ε, MonadError ε η, HasCallStack) ⇒
              ErrInfo → η α
throwParseE a = throwError  ∘ (_ParseError #) $ ParseError a callStack

------------------------------------------------------------

data AcctError = AE_FPIOE FPathIOError
               | AE_APE   AcctProcessingError
               | AE_PE    ParseError
               | AE_IE    ImportError
               | AE_UE    UsageError
  deriving Show

--------------------

instance Exception AcctError
instance HasCallstack AcctError where
  callstack = let getter (AE_FPIOE e) = e ⊣ callstack
                  getter (AE_APE e)   = e ⊣ callstack
                  getter (AE_IE e)    = e ⊣ callstack
                  getter (AE_PE e)    = e ⊣ callstack
                  getter (AE_UE e)    = e ⊣ callstack
                  setter (AE_FPIOE e) cs = AE_FPIOE (e & callstack ⊢ cs)
                  setter (AE_APE e)   cs = AE_APE   (e & callstack ⊢ cs)
                  setter (AE_PE e)    cs = AE_PE    (e & callstack ⊢ cs)
                  setter (AE_IE e)    cs = AE_IE    (e & callstack ⊢ cs)
                  setter (AE_UE e)    cs = AE_UE    (e & callstack ⊢ cs)
              in  lens getter setter

--------------------

instance Printable AcctError where
  print (AE_FPIOE e) = print e
  print (AE_APE   e) = print e
  print (AE_PE    e) = print e
  print (AE_IE    e) = print e
  print (AE_UE    e) = print e

--------------------

_AE_FPIOE ∷ Prism' AcctError FPathIOError
_AE_FPIOE = prism' (\ e → AE_FPIOE e) (\ case AE_FPIOE e → 𝕵 e; _ → 𝕹)

--------------------

instance AsAcctProcessingError AcctError where
  _AcctProcessingError = prism' AE_APE (\ case AE_APE e → 𝕵 e; _ → 𝕹)

--------------------

instance AsFPathError AcctError where
  _FPathError = _AE_FPIOE ∘ _FPIO_PATH_ERROR

--------------------

instance AsImportError AcctError where
  _ImportError = prism' AE_IE (\ case AE_IE e → 𝕵 e; _ → 𝕹)

--------------------

instance AsIOError AcctError where
  _IOError = _AE_FPIOE ∘ _FPIO_IO_ERROR

--------------------

instance AsParseError AcctError where
  _ParseError = prism' AE_PE (\ case AE_PE e → 𝕵 e; _ → 𝕹)

--------------------

instance AsUsageError AcctError where
  _UsageError = prism' AE_UE (\ case AE_UE e → 𝕵 e; _ → 𝕹)

----------------------------------------

exitCode ∷ AcctError → Word8
exitCode (AE_APE (NoNonStmtTrxFoundE _))        = 10
exitCode (AE_APE (InconsistentTrxStmtsE _ _ _)) = 11
exitCode (AE_APE (NoSuchStmtE _ _))             = 12
exitCode (AE_APE (AcctRestartE _ _))            = 13
exitCode (AE_APE (OAcctRestartE _ _))           = 14
exitCode (AE_APE (InvalidAccountE _ _))         = 15
exitCode (AE_APE (InvalidOAccountE _ _ _))      = 16
exitCode (AE_APE (NoImportE _))                 = 17
exitCode (AE_FPIOE _)                           = 18
exitCode (AE_PE _)                              = 19
exitCode (AE_IE _)                              = 20
exitCode (AE_UE _)                              = 21

-- exitCode _ = 199

-- that's all, folks! ----------------------------------------------------------
