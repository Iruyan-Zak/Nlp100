{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as LBS
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Control.Applicative
import Control.Monad
import Data.List (find)
import Text.Regex.Posix
import Data.Maybe (mapMaybe)
main = q20 >>= q21

f21 :: String -> [String]
f21 =  join . map (getAllTextMatches . (=~ "(.*Category.*)")) . lines

-- linerRegex :: String -> Regex
-- linerRegex pattern = mkRegexWithOpts pattern True False

f20 :: LBS.ByteString -> String
f20 = findTextOf "イギリス" . mapMaybe decode . UTF8.lines

q21 :: String -> IO ()
q21 = putStr . unlines . f21

q20 :: IO String
q20 = getGZipContents "input/jawiki-country.json.gz" >>= return . f20

data Article = Article
    { text :: String
    , title :: String
    } deriving (Show, Generic)

instance FromJSON Article

getGZipContents :: String -> IO LBS.ByteString
getGZipContents = fmap decompress . LBS.readFile

findTextOf :: String -> [Article] -> String
findTextOf title' = maybe "" text . find ((title' ==) . title)
