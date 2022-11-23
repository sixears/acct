{-| Like `Data.Textual` (from @data-textual@); but with lookahead -}
module TextualPlus'
  ( TextualPlus(..), parse, parseString
  , propInvertibleString, propInvertibleText, propInvertibleUtf8 )
where

import Base1T

-- base --------------------------------

import Control.Applicative  ( Alternative( (<|>), empty ) )
import Control.Monad.Fail   ( MonadFail( fail ) )
import Data.List            ( intercalate, length, reverse, stripPrefix )
import Data.Functor         ( Functor )
import Data.Word            ( Word )

-- bytestring --------------------------

import qualified  Data.ByteString       as  BS
import qualified  Data.ByteString.Lazy  as  BL

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), toUtf8 )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, satisfy, string )
import Text.Parser.Combinators  ( Parsing( notFollowedBy, skipMany, skipSome )
                                , (<?>), eof, try, unexpected )

-- tasty-plus --------------------------

import TastyPlus  ( (≣) )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property )

-- text --------------------------------

import qualified  Data.Text       as  Text
import qualified  Data.Text.Lazy  as  TL

import Data.Text.Lazy.Encoding  ( decodeUtf8 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

data Parser α =
  Parser { runParser ∷ ∀ r .
                       [String] → Word → String
                     → ([String] → Word → String → α → Parsed r)
                     → ([String] → Word → String → String → Parsed r)
                     → Parsed r }

instance Functor Parser where
  fmap f p = Parser $ \ls n i c h →
               runParser p ls n i (\ls' n' i' a → c ls' n' i' (f a)) h
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \ls n i c _ → c ls n i a
  {-# INLINE pure #-}
  p <*> p' = Parser $ \ls n i c h →
               runParser p ls n i
                 (\ls' n' i' f →
                    runParser p' ls' n' i'
                      (\ls'' n'' i'' a → c ls'' n'' i'' (f a)) h)
                 h
  {-# INLINE (<*>) #-}
  p *> p' = Parser $ \ls n i c h →
              runParser p ls n i (\ls' n' i' _ → runParser p' ls' n' i' c h) h
  {-# INLINE (*>) #-}
  p <* p' = Parser $ \ls n i c h →
              runParser p ls n i
                        (\ls' n' i' a →
                           runParser p' ls' n' i'
                                     (\ls'' n'' i'' _ → c ls'' n'' i'' a) h)
                        h
  {-# INLINE (<*) #-}


instance Alternative Parser where
  empty = unexpected "Alternative.empty"
  {-# INLINE empty #-}
  p <|> p' = Parser $ \ls n i c h →
               runParser p ls n i c $ \ls' n' i' e →
                 if n' == n then runParser p' ls n' i' c h
                            else h ls' n' i' e
  {-# INLINE (<|>) #-}

instance Parsing Parser where
  try p = Parser $ \ls n i c h →
            runParser p ls n i c (\ls' _ _ e → h ls' n i e)
  {-# INLINE try #-}
  p <?> l = Parser $ \ls n i c h →
              runParser p (l : ls) n i (\_ n' i' a → c ls n' i' a) h
  {-# INLINE (<?>) #-}
  skipMany p = Parser $ \ls n i c h →
                 runParser p ls n i
                   (\ls' n' i' _ → runParser (skipMany p) ls' n' i' c h)
                   (\ls' n' i' _ → c ls' n' i' ())
  skipSome p = p ⋫ skipMany p
  {-# INLINE skipSome #-}
  unexpected e = Parser $ \ls n i _ h → h ls n i e
  {-# INLINE unexpected #-}
  eof = Parser $ \ls n i c h → case i of
                   [] → c ls n i ()
                   _  → h ls n i "Parsing.eof"
  {-# INLINABLE eof #-}
  notFollowedBy p = Parser $ \ls n i c h →
                      runParser p ls n i
                                (\_ _ _ _ → h ls n i "Parsing.notFollowedBy")
                                (\_ _ _ _ → c ls n i ())
  {-# INLINE notFollowedBy #-}

instance CharParsing Parser where
  satisfy f = Parser $ \ls n i c h → case i of
                         x : xs | f x → c ls n' xs x
                                          where !n' = n + 1
                         _ → h ls n i "CharParsing.satisfy"
  {-# INLINABLE satisfy #-}
  string s = Parser $ \ls n i c h → case stripPrefix s i of
                        Just i' → c ls n' i' s
                                    where !n' = n + fromIntegral (length s)
                        Nothing → h ls n i "CharParsing.string"
  {-# INLINABLE string #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  p >>= f = Parser $ \ls n i c h →
              runParser p ls n i
                        (\ls' n' i' a → runParser (f a) ls' n' i' c h) h
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail Parser where
  fail = unexpected
  {-# INLINE fail #-}

parse ∷ Parser α → String → Parsed α
parse p i = runParser p [] 0 i (\ _  _ _ a → Parsed a)
                               (\ ls _ _ e → Malformed (reverse ls) e)

-- | Parse a 'String' to extract the 'Textual' value.
parseString ∷ TextualPlus α ⇒ String → Parsed α
parseString = parse $ textual' ⋪ eof
{-# INLINE parseString #-}

-- | Parse a 'Text.Text' to extract the 'Textual' value.
parseText ∷ TextualPlus α ⇒ Text.Text → Parsed α
parseText = parseString . Text.unpack
{-# INLINE parseText #-}

parseLazyText ∷ TextualPlus α ⇒ TL.Text → Parsed α
parseLazyText = parseString . TL.unpack
{-# INLINE parseLazyText #-}

-- | Decode and parse a UTF-8 'BS.ByteString' to extract the 'Textual' value.
parseUtf8 ∷ TextualPlus α ⇒ BS.ByteString → Parsed α
parseUtf8 = parseLazyText . decodeUtf8 . BL.fromStrict
{-# INLINE parseUtf8 #-}

------------------------------------------------------------

class TextualPlus α where
  textual' ∷ (MonadFail μ, CharParsing μ) ⇒ μ α

------------------------------------------------------------

newtype P α = P α
  deriving Eq

instance Printable α ⇒ Printable (P (Parsed α)) where
  print (P (Parsed a))       = P.string (toString a)
  print (P (Malformed [] s)) =
    let quote t = "'" <> t <> "'"
     in P.string $ "MALFORMED: " <> quote s
  print (P (Malformed ss s)) =
    let quote     t  = "'" <> t <> "'"
        bracketsp t  = "[ " <> t <> " ]"
        list    ts = bracketsp $ intercalate ", " (quote ⊳ ts)
     in P.string $ "MALFORMED: " <> quote s <> " " <> list ss

--------------------

propInvertibleString ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleString d = P (parseString (toString d)) ≣ P (Parsed d)

--------------------

propInvertibleText ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleText d = P (parseText (toText d)) ≣ P (Parsed d)

--------------------

propInvertibleUtf8 ∷ (Eq α, Printable α, TextualPlus α) ⇒ α → Property
propInvertibleUtf8 d = P (parseUtf8 (toUtf8 d)) ≣ P (Parsed d)

-- that's all, folks! ----------------------------------------------------------
