module Main ( main ) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import           Protolude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tsuru.Parser

---

main :: IO ()
main = defaultMain suite

assertRight :: Either [Char] b -> Assertion
assertRight t = case t of
  Left e  -> assertFailure e
  Right _ -> assert True

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testCase "flatten" $ flatten [1,2,3,4] @?= 1234
  , testCase "Parse Quote" . assertRight $ A.parseOnly quote quoteT
  ]

quoteT :: BS.ByteString
quoteT = BS.pack bytes

bytes :: [Word8]
bytes =
  [ 0x00, 0x00, 0x00, 0x00, 0x00  -- Header noise that should be properly skipped.
  , 0x42 , 0x36 , 0x30 , 0x33 , 0x34 , 0x4b , 0x52 , 0x34 , 0x32 , 0x30 , 0x31 , 0x46 , 0x33 , 0x32 , 0x37 , 0x30
  , 0x35 , 0x30 , 0x30 , 0x31 , 0x31 , 0x30 , 0x30 , 0x30 , 0x32 , 0x32 , 0x36 , 0x38 , 0x31 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x37 , 0x32 , 0x32 , 0x34
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x39 , 0x34
  , 0x37 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x33 , 0x36 , 0x35 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30
  , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x30 , 0x38
  , 0x35 , 0x39 , 0x35 , 0x39 , 0x39 , 0x37 , 0xff ]
