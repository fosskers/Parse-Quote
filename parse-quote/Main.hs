module Main ( main ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Hourglass
import           Network.Pcap
import qualified Network.Pcap.Streaming as NPS
import           Options.Applicative
import           Protolude
import           Streaming
import qualified Streaming.Prelude as S
import           Tsuru.Parser (quote)
import           Tsuru.Types

---

-- | How should Quote entries be ordered in their output?
data Order = AsIs | ByTime deriving (Eq, Show)

data Env = Env { ordering :: Order, path :: FilePath } deriving (Eq, Show)

envP :: Parser Env
envP = Env
  <$> flag AsIs ByTime (short 'r' <> help "Order output by 'quote accept time'?")
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

work :: Env -> IO ()
work (Env o p) = S.length_ (quotes p) >>= print

main :: IO ()
main = execParser opts >>= work
  where opts = info (envP <**> helper) (fullDesc <> header "Parse Quote Data")
