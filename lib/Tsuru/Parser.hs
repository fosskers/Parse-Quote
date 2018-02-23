module Tsuru.Parser ( quote ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Text.Encoding (decodeUtf8)
import           Protolude hiding (ask)
import           Time.Types
import           Tsuru.Types

---

quote :: A.Parser Quote
quote = do
  header
  ic <- issue
  A.take 12  -- Issue seq.-no, Market Status Type, Total bid quote volume
  bs <- reverse <$> A.count 5 bid
  A.take 7   -- Total ask quote volume
  as <- A.count 5 ask
  A.take 50  -- No. of best bid/ask quotes
  at <- accept
  A.word8 0xff
  pure $ Quote undefined at ic bs as

header :: A.Parser ()
header = void $ A.string "B6034"

issue :: A.Parser ISIN
issue = ISIN . decodeUtf8 <$> A.take 12

bid :: A.Parser Bid
bid = uncurry Bid <$> pair

ask :: A.Parser Ask
ask = uncurry Ask <$> pair

pair :: A.Parser (Word, Price)
pair = f <$> A.take 5 <*> A.take 7
  where f p q = undefined

accept :: A.Parser TimeOfDay
accept = TimeOfDay
  <$> fmap Hours digit
  <*> fmap Minutes digit
  <*> fmap Seconds digit
  <*> fmap NanoSeconds digit

digit :: A.Parser Int64
digit = fromIntegral . (\n -> n - 0x30) <$> A.satisfy isDigit
  where isDigit w = w >= 0x30 && w <= 0x39
