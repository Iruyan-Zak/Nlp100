{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

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

main = getUKText >>= q24
getUKText = liftA f20 (getGZipContents "input/jawiki-country.json.gz")

f25 :: BS.ByteString -> [BS.ByteString]
f25 = mapMaybe (@@1) . matchOf "\\|(.+?)\\|" . basicInfo

basicInfo :: BS.ByteString -> BS.ByteString
basicInfo = BS.concat . reverse . basicInfo' [] True . BS.lines where
    basicInfo' :: [BS.ByteString] -> Bool -> [BS.ByteString] -> [BS.ByteString]
    basicInfo' buf skipping (s:strs)
        | matchOf "\\{\\{基礎情報" s = basicInfo' buf False strs
        | skipping = basicInfo' buf True strs
        | matchOf "^\\}\\}$" s = (utf8pack "|"):buf
        | otherwise = basicInfo' (s:buf) False strs

f24 :: BS.ByteString -> [BS.ByteString]
f24 = mapMaybe (@@ 1) . matchOf "([^ :=][^:=]+\\.(png|PNG|jpg|JPG|jpeg|svg))"

f23 :: BS.ByteString -> [(BS.ByteString, Int)]
f23 = map (\(_:equals:title:_) -> (title, BS.length equals)) . matchOf "^\\=(=+)([^=]+)="

-- 戻り読みを使うと下のパターンになるけど、サポートされてなさそう
-- "(?<=Category:)[^]]+"
f22 :: BS.ByteString -> [BS.ByteString]
f22 = mapMaybe ((@@ 1) . head . matchOf "Category:([^]]+)") . f21

f21 :: BS.ByteString -> [BS.ByteString]
f21 = filter (matchOf "\\[\\[Category:.*\\]\\]") . BS.lines

matchOf :: (RegexContext Text.Regex.Posix.Regex source target) =>
    String -> (source -> target)
matchOf pattern = (=~ (utf8pack pattern))

f20 :: LBS.ByteString -> BS.ByteString
f20 = utf8pack . findTextOf "イギリス" . mapMaybe decode . LBS.lines

q24 :: BS.ByteString -> IO ()
q24 = putStr . utf8unpack . BS.unlines . f24

q23 :: BS.ByteString -> IO ()
q23 = putStr . unlines . map (\(s, i) -> (utf8unpack s ++ "\t" ++ show i)) . f23

q22 :: BS.ByteString -> IO ()
q22 = putStr . utf8unpack . BS.unlines . f22

q21 :: BS.ByteString -> IO ()
q21 = putStr . utf8unpack . BS.unlines . f21

q20 :: BS.ByteString -> IO ()
q20 = putStrLn . utf8unpack

-- Safe (!!) operation
(@@) :: [a] -> Int -> Maybe a
(@@) list i
    | (i <) $ length list = Just $ list !! i
    | otherwise = Nothing
infix 9 @@

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

