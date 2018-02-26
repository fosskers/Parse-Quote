{-# LANGUAGE BangPatterns #-}

module Main ( main ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Hourglass
import qualified Data.PQueue.Prio.Min as Q
import           Network.Pcap
import qualified Network.Pcap.Streaming as NPS
import           Options.Applicative
import           Protolude
import           Streaming
import qualified Streaming.Prelude as S
import           Tsuru.Parser (quote)
import           Tsuru.Types

---

-- | How Quote entries should be ordered in their output.
data Order = AsIs | ByTime deriving (Eq, Show)

data Env = Env { ordering :: Order, path :: FilePath } deriving (Eq, Show)

envP :: Parser Env
envP = Env
  <$> flag AsIs ByTime (short 'r' <> help "Order output by 'quote accept time'")
  <*> argument str (metavar "FILE")

-- TODO TIMEZONES!
ptoq :: NPS.Packet -> Either [Char] Quote
ptoq (NPS.Packet h b) = A.parseOnly (quote timestamp) b
  where timestamp = timeFromElapsedP $ ElapsedP secs nanos
        secs      = Elapsed . Seconds . fromIntegral $ hdrSeconds h
        nanos     = NanoSeconds . (1000 *) . fromIntegral $ hdrUseconds h

-- | All legally parsable `Quote` values.
quotes :: FilePath -> Stream (Of Quote) IO ()
quotes = S.concat . S.map ptoq . NPS.offline

-- | Reorder the values of the `Stream` according to the given `Order`.
reorder :: Order -> Stream (Of Quote) IO () -> Stream (Of Quote) IO ()
reorder AsIs s   = s
reorder ByTime s = do
  mq <- lift $ S.uncons s
  case mq of
    Nothing      -> pure ()  -- The stream was empty before we could try anything.
    Just (q, s') -> go (Q.singleton (acceptTime q) q) s'
  where go !pq strm = do
          mq <- lift $ S.uncons strm
          let (t, q) = Q.findMin pq
          case mq of
            Nothing -> S.each . map snd $ Q.toAscList pq  -- The stream has completed.
            Just (q', strm')
              | are3Apart t (acceptTime q') -> S.yield q *> go (Q.insert (acceptTime q') q' $ Q.deleteMin pq) strm'
              | otherwise -> go (Q.insert (acceptTime q') q' pq) strm'

-- | Did two `Quote`s arrive more than 3 seconds apart?
are3Apart :: DateTime -> DateTime -> Bool
are3Apart q q' = (\(Seconds t) -> abs t > 3) $ timeDiff q q'

work :: Env -> IO ()
work (Env o p) = S.mapM_ putText . S.map prettyQuote . reorder o $ quotes p

main :: IO ()
main = execParser opts >>= work
  where opts = info (envP <**> helper) (fullDesc <> header "Parse Quote Data")
