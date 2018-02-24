module Main ( main ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Hourglass
import           Network.Pcap
import qualified Network.Pcap.Streaming as NPS
import           Options.Applicative
import           Protolude
import           Streaming
import qualified Streaming.Prelude as P
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

ptoq :: NPS.Packet -> Maybe Quote
ptoq (NPS.Packet h b) = either (const Nothing) Just $ A.parseOnly (quote timestamp) b
  where timestamp = dtTime . timeFromElapsedP $ ElapsedP secs nanos
        secs      = Elapsed . Seconds . fromIntegral $ hdrSeconds h
        nanos     = NanoSeconds . fromIntegral $ 1000 * hdrUseconds h


work :: Env -> IO ()
work (Env o p) = P.print . P.take 10 . P.map ptoq $ NPS.offline p

main :: IO ()
main = execParser opts >>= work
  where opts = info (envP <**> helper) (fullDesc <> header "Parse Quote Data")
