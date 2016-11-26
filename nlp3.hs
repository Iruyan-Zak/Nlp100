{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

import Data.Aeson (FromJSON, decode)
import Codec.Compression.GZip (decompress)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.List (find)
import Data.Maybe (mapMaybe)

import GHC.Generics
import Control.Applicative
import Control.Monad
import Text.Regex.Posix

main = getUKText >>= q22
getUKText = liftA f20 (getGZipContents "input/jawiki-country.json.gz")

-- 戻り読みと先読みを使うと下のパターンになるけど、サポートされてなさそう
-- "(?<=Category:).+(?=\\]\\])"
f22 :: BS.ByteString -> [BS.ByteString]
f22 =  mapMaybe ((`at` 1) . head . (=~ (utf8pack "\\[\\[Category:(.+)\\]\\]"))) . f21

f21 :: BS.ByteString -> [BS.ByteString]
f21 =  filter (=~ (utf8pack "\\[\\[Category:.*\\]\\]")) . BS.lines

f20 :: LBS.ByteString -> BS.ByteString
f20 = utf8pack . findTextOf "イギリス" . mapMaybe decode . LBS.lines

q22 :: BS.ByteString -> IO ()
q22 = putStr . utf8unpack . BS.unlines . f22

q21 :: BS.ByteString -> IO ()
q21 = putStr . utf8unpack . BS.unlines . f21

q20 :: BS.ByteString -> IO ()
q20 = putStrLn . utf8unpack

at :: [a] -> Int -> Maybe a
at list i
    | (i <) $ length list = Just $ list !! i
    | otherwise = Nothing

data Article = Article
    { text :: String
    , title :: String
    } deriving (Show, Generic)

instance FromJSON Article

getGZipContents :: String -> IO LBS.ByteString
getGZipContents = fmap decompress . LBS.readFile

findTextOf :: String -> [Article] -> String
findTextOf title' = maybe "" text . find ((title' ==) . title)

utf8unpack :: BS.ByteString -> String
utf8unpack =  decodeString . BS.unpack

utf8pack :: String -> BS.ByteString
utf8pack = BS.pack . encodeString

