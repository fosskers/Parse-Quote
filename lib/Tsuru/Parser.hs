module Tsuru.Parser where

import qualified Data.Attoparsec.ByteString as A
import           Data.Text.Encoding (decodeUtf8)
import           Protolude hiding (ask)
import           Time.Types
import           Tsuru.Types

---

-- | Each `Quote` is parsed from 215 bytes, but can be preceded by "header noise."
quote :: TimeOfDay -> A.Parser Quote
quote pktTime = do
  A.manyTill A.anyWord8 header  -- Not very efficient, but at least the header isn't long.
  ic <- issue
  A.take 12  -- Issue seq.-no, Market Status Type, Total bid quote volume
  bs <- reverse <$> A.count 5 bid
  A.take 7   -- Total ask quote volume
  as <- A.count 5 ask
  A.take 50  -- No. of best bid/ask quotes
  at <- accept
  A.word8 0xff
  pure $ Quote pktTime at ic bs as

header :: A.Parser ()
header = void $ A.string "B6034"

issue :: A.Parser ISIN
issue = ISIN . decodeUtf8 <$> A.take 12

bid :: A.Parser Bid
bid = uncurry Bid <$> pair

ask :: A.Parser Ask
ask = uncurry Ask <$> pair

pair :: A.Parser (Word, Price)
pair = f <$> A.count 5 digit <*> A.count 7 digit
  where f p q = (flatten q, Price $ flatten p)

accept :: A.Parser TimeOfDay
accept = TimeOfDay <$> fmap Hours two <*> fmap Minutes two <*> fmap Seconds two <*> fmap NanoSeconds two
  where two = fromIntegral . flatten <$> A.count 2 digit

digit :: A.Parser Word8
digit = (\n -> n - 0x30) <$> A.satisfy isDigit
  where isDigit w = w >= 0x30 && w <= 0x39

-- | Collapse a series of digits into their true form.
flatten :: [Word8] -> Word
flatten = foldl' (\acc n -> acc * 10 + fromIntegral n) 0
