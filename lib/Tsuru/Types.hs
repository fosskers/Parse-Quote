module Tsuru.Types where

import qualified Data.Text as T
import           Protolude
import           Text.Printf (printf)
import           Time.Types

---

-- | ASSUMPTIONS:
--
--  * We want actual numbers, not just the string representations of those numbers.
data Quote = Quote { packetTime :: DateTime
                   , acceptTime :: DateTime
                   , issueCode  :: ISIN
                   , bids       :: [Bid]
                   , asks       :: [Ask] } deriving (Eq, Ord, Show)

-- | International Securities Identification Number
newtype ISIN = ISIN Text deriving (Eq, Ord, Show)

-- TODO The price is likely not a `Word`
data Bid = Bid { bQuant :: Word, bPrice :: Price } deriving (Eq, Ord, Show)

data Ask = Ask { aQuant :: Word, aPrice :: Price } deriving (Eq, Ord, Show)

newtype Price = Price Word deriving (Eq, Ord, Show)

prettyQuote :: Quote -> Text
prettyQuote (Quote pt (DateTime _ at) (ISIN ic) bs as) = T.intercalate " " $ vs <> bs' <> as'
  where vs  = [ prettyDateTime pt, prettyTime at, ic ]
        bs' = map (\(Bid q (Price p)) -> T.pack $ printf "%d@%d" q p) bs
        as' = map (\(Ask q (Price p)) -> T.pack $ printf "%d@%d" q p) as

prettyTime :: TimeOfDay -> Text
prettyTime (TimeOfDay (Hours h) (Minutes m) (Seconds s) (NanoSeconds n)) =
  T.pack $ printf "%02d:%02d:%02d:%02d" h m s (n `div` 10000000)  -- Tens-of-milliseconds

prettyDateTime :: DateTime -> Text
prettyDateTime (DateTime (Date y m d) tod)= T.pack (printf "%04d/%02d/%02d/" y m' d) <> prettyTime tod
  where m' = 1 + fromEnum m
