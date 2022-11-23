module TastyPluser
  ( TestCmp'( testCmp' ) )
where

import Base1T

-- base --------------------------------

import GHC.Stack  ( CallStack, HasCallStack, callStack )

--------------------------------------------------------------------------------

class TestCmp' α where
  testCmp' ∷ HasCallStack ⇒ TestName → α → IO α → TestTree

-- that's all, folks! ----------------------------------------------------------
