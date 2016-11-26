{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as LBS
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Control.Applicative
import Control.Monad
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Article = Article
    { text :: T.Text
    , title :: String
    } deriving (Show, Generic)

instance FromJSON Article

getGZipContents :: String -> IO LBS.ByteString
getGZipContents = fmap decompress . LBS.readFile



main = do
    gz <- getGZipContents "input/jawiki-country.json.gz"
    let
        ukText = findTextOf "イギリス" . L.map decode . UTF8.lines $ gz
        -- head10 = fmap (T.unlines . L.take 10 . T.lines) ukText
    case ukText of
        Just s -> T.writeFile "UK.txt" s
        Nothing -> return ()


findTextOf :: String -> [Maybe Article] -> Maybe T.Text
findTextOf title' = fmap text . join . L.find (titleIs title') where
    titleIs :: String -> Maybe Article -> Bool
    titleIs title' (Just t) | title t == title' = True
    titleIs _ _ = False

