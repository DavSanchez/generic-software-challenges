{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module without exported functions (i.e. library only). Contains functions for parsing the input values coming from the server to abstract types representing them for later processing. This logic is implemented using parser combinators and the [attoparsec](https://hackage.haskell.org/package/attoparsec) library.
--
-- === Comments
-- Parser combinators are popular in functional programming resources and an ideal way to map inputs to commands defined in the type 'Input'. Support for additional commands would be easy to achieve adding new type constructors and the appropriate parser to the 'inputParser' combinator.
--
-- These parsers are implemented leveraging the [attoparsec](https://hackage.haskell.org/package/attoparsec) library.
module Internal.Input
  ( -- * Type
    Input (..),
    -- * Input parsing function
    parseMaybeInput,
    -- * Parser combinator
    inputParser,
    -- * Parsers
    parseDigits,
    parseTerminate,
    -- * Byte check functions
    digit,
    eol,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString
  ( Parser,
    count,
    maybeResult,
    parse,
    satisfy,
    string,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Functor (($>))
import Data.Word (Word8)

-- | Abstract type representing the valid values entered by the client to the socket server.
data Input
  = -- | Appropriately-formatted number. Impl-dependent but should suffice (64 bits). Otherwise 'Integer' can be used (arbitrary length).
    Number Int
  | -- | Abstract type constructor for the termination sequence.
    Terminate
  deriving stock (Eq, Show)

-- |  Maps input from the client to a value from the 'Input' type. If the data cannot be parsed, 'Nothing' is returned.
parseMaybeInput ::
  -- | Input bytes
  B.ByteString ->
  Maybe Input
parseMaybeInput input = maybeResult $ parse inputParser input

-- |  Parser combinator for the digits and the termination sequence.
inputParser :: Parser Input
inputParser = parseDigits <|> parseTerminate

-- | Parses digits when they come in the required format (with a @0@ padding up to 9 characters and a newline)
parseDigits :: Parser Input
parseDigits = do
  digits <- count 9 digit
  _ <- eol
  pure $ Number ((read . C.unpack . B.pack) digits)

-- | Parses the termination byte string (i.e. "terminate" followed by a newline)
parseTerminate :: Parser Input
parseTerminate = string "terminate" >> eol $> Terminate

-- | Checker for bytes representing digits. The numeric parser 'parseDigits' defined above should satisfy it.
digit :: Parser Word8
digit = satisfy isDigit
  where
    isDigit w = w >= 48 && w <= 57

-- | Checker for bytes representing newline characters. The parsers defined above should satisfy it.
eol :: Parser Word8
eol = satisfy endOfLine
  where
    endOfLine w = w == 13 || w == 10
