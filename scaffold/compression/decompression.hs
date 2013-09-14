
module Main ( main ) where

-- Internal Imports
--
import Text.Compression.Simple
import GHC.Word

-- External Imports
--
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Binary                as B

-- Main
--
main :: IO ()
main = do
  input <- BSL.getContents

  let
    (l, fs, cs, b) = metadata input
    table          = zip (map fromIntegral fs) (BSC.unpack cs)
    t              = frequenciesToCoding table

  putStrLn $ take l $ unpackString t b

metadata :: BSL.ByteString -> (Int, [Word32], BSL.ByteString, BS.ByteString)
metadata = B.decode
