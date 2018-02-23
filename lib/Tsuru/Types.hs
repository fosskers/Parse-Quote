module Tsuru.Types where

import Protolude
import Time.Types (TimeOfDay)

---

data Quote = Quote { packetTime :: TimeOfDay
                   , acceptTime :: TimeOfDay
                   , issueCode  :: ISIN
                   , bids       :: [Bid]
                   , asks       :: [Ask] } deriving (Eq, Ord, Show)

-- | International Securities Identification Number
newtype ISIN = ISIN Text deriving (Eq, Ord, Show)

-- TODO The price is likely not a `Word`
data Bid = Bid { bQuant :: Word, bPrice :: Price } deriving (Eq, Ord, Show)

data Ask = Ask { aQuant :: Word, aPrice :: Price } deriving (Eq, Ord, Show)

newtype Price = Price Word deriving (Eq, Ord, Show)