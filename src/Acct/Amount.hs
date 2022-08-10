{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Acct.Amount
  ( Amount( Amount ), HasAmount( amount ), amt, tests )
where

import Base1T
import Prelude  ( Enum, Integral, Num, Real, (*), quot, rem )

-- base --------------------------------

import Text.Read  ( read )

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- genvalidity -------------------------

import Data.GenValidity  ( GenValid( genValid, shrinkValid ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, digit )
import Text.Parser.Combinators  ( option, try )

-- quasiquoting ------------------------

import QuasiQuoting  ( mkQQExp )

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary, shrink ) )
import Test.QuickCheck.Gen        ( chooseInteger )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), propInvertibleString, propInvertibleText )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( testProperty )

-- template-haskell --------------------

import Language.Haskell.TH.Quote   ( QuasiQuoter )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- trifecta-plus -----------------------

import TrifectaPlus  ( liftTParse', testParse, testParseE, tParse, tParse' )

-- validity ----------------------------

import Data.Validity  ( Validity( validate ), trivialValidation )

--------------------------------------------------------------------------------

data Sign = SIGN_PLUS   -- a credit, paying into the bank
          | SIGN_MINUS  -- a debit, typically a payment to a vendor
  deriving (Eq,Show)

--------------------

instance Printable Sign where
  print SIGN_PLUS  = P.char '+'
  print SIGN_MINUS = P.char '-'

instance Textual Sign where
  textual = try (pure SIGN_MINUS ⋪ char '-') ∤ pure SIGN_PLUS ⋪ char '+'

signmult ∷ Sign → ℤ
signmult SIGN_PLUS  = 1
signmult SIGN_MINUS = -1

------------------------------------------------------------

newtype Amount  = Amount ℤ  deriving (Enum,Eq,Integral,Lift,Num,Ord,Real,Show)

{-| construct an `Amount` from pounds, pence & sign -}
fromPPS ∷ ℕ → Word8 → Sign → Amount
fromPPS l p s =
  Amount ∘ ((signmult s) *) ∘ fromIntegral $ l*100 + fromIntegral p

--------------------

instance Validity Amount where
  validate = trivialValidation

--------------------

instance GenValid Amount where
  genValid    = Amount ⊳ chooseInteger (-50_000_00,50_000_00)
  shrinkValid = pure

--------------------

instance Arbitrary Amount where
  arbitrary = genValid
  shrink = shrinkValid

--------------------

instance Printable Amount where
  print p = P.text $ [fmt|%d.%02d%T|] (p ⊣ pounds) (p ⊣ pence) (p ⊣ sign)

----------

printTests ∷ TestTree
printTests =
  let
    test exp ts = testCase (unpack exp) $ exp ≟ toText ts
  in
    testGroup "print"
      [ test "10.00+" (Amount 1000)
      , test "0.01-" (Amount $ -1)
      , testProperty "invertibleString" (propInvertibleString @Amount)
      , testProperty "invertibleText" (propInvertibleText @Amount)
      ]

--------------------

instance Textual Amount where
  textual = let pnceP = (\ x y → [x,y]) ⊳ (char '.' ⋫ digit) ⊵ digit
                pnce  = option "00" pnceP
                mkval pnds pnc sgn = signmult sgn * read (pnds ⊕ pnc)
             in Amount ⊳ (mkval ⊳ some digit ⊵ pnce ⊵ textual)

parseTests ∷ TestTree
parseTests =
  testGroup "parse"
            [ testParse  "1+" (Amount 100)
            , -- sign missing
              testParseE "1" (tParse @Amount) "unexpected EOF"
            , -- sign missing
              testParseE  "11.01" (tParse @Amount) "unexpected EOF"
            , -- wrong sign loc
              testParseE  "-11.01" (tParse @Amount) "expected: digit"
            , testParse  "11.01-" (Amount (-1101))
            ]

------------------------------------------------------------

class HasAmount α where
  amount ∷ Lens' α Amount
  pounds ∷ Lens' α ℕ
  pounds = lens (\ x → (fromInteger $ abs ( toInteger $ x ⊣ amount)) `quot` 100)
                (\ x p → x & amount ⊢ fromPPS p (x ⊣ pence) (x ⊣ sign))
  pence  ∷ Lens' α Word8
  pence  = lens (\ x → (fromInteger $ abs (toInteger $ x ⊣ amount) `rem` 100))
                (\ x p → x & amount ⊢ fromPPS (x ⊣ pounds) p (x ⊣ sign))
  sign   ∷ Lens' α Sign
  sign   = lens (\ x → if (x ⊣ amount < 0) then SIGN_MINUS else SIGN_PLUS)
                (\ x s → x & amount ⊢ fromPPS (x ⊣ pounds) (x ⊣ pence) s)

instance HasAmount Amount where
  amount = id

hasAmountTests ∷ TestTree
hasAmountTests =
  let
    _1234  = Amount 1234
    _5678  = Amount 5678
    __1234 = Amount (-1234)
    __5678 = Amount (-5678)
  in
    let
      test_get f x exp = testCase (show x) $ exp @=? x ⊣ f
      test_set f x to exp = testCase (show x) $ exp @=? (x & f ⊢ to)
    in
      testGroup "HasAmount" $
        [ testGroup "pounds" $
            [ testGroup "get" $
                [ test_get pounds _1234  12
                , test_get pounds __1234 12
                , test_get pounds _5678  56
                , test_get pounds __5678 56
                ]
            , testGroup "set" $
                [ test_set pounds __1234 21 (Amount (-2134))
                , test_set pounds  _1234 21 (Amount 2134)
                , test_set pounds __5678 21 (Amount (-2178))
                , test_set pounds  _5678 21 (Amount 2178)
                ]
            ]
      , testGroup "pence" $
          [ testGroup "get" $
              [ test_get pence _1234 34
              , test_get pence __1234 34
              , test_get pence _5678 78
              , test_get pence __5678 78
              ]
          , testGroup "set" $
                [ test_set pence __1234 21 (Amount (-1221))
                , test_set pence  _1234 21 (Amount 1221)
                , test_set pence __5678 21 (Amount (-5621))
                , test_set pence  _5678 21 (Amount 5621)
                ]
          ]
      , testGroup "sign" $
          [ testGroup "get" $
              [ test_get sign _1234 SIGN_PLUS
              , test_get sign __1234 SIGN_MINUS
              , test_get sign _5678 SIGN_PLUS
              , test_get sign __5678 SIGN_MINUS
              ]
          , testGroup "set" $
              [ test_set sign  _1234 SIGN_PLUS   _1234
              , test_set sign  _1234 SIGN_MINUS __1234
              , test_set sign __1234 SIGN_PLUS   _1234
              , test_set sign __1234 SIGN_MINUS __1234
              , test_set sign  _5678 SIGN_PLUS   _5678
              , test_set sign  _5678 SIGN_MINUS __5678
              , test_set sign __5678 SIGN_PLUS   _5678
              , test_set sign __5678 SIGN_MINUS __5678
              ]
          ]
      ]

----------------------------------------

{-| QuasiQuoter for `Amount` -}
amt ∷ QuasiQuoter
amt = mkQQExp "Amount" (liftTParse' @Amount tParse')


-- testing infrastructure ------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Acct.Amount" [ printTests, parseTests, hasAmountTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
