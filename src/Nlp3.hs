{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
module Nlp3 (answers) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

import Data.Aeson (FromJSON, decode)
import Codec.Compression.GZip (decompress)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.List (find, intersperse)
import Data.Maybe (mapMaybe)
import Network.HTTP.Conduit (simpleHttp)

import GHC.Generics
import Control.Applicative
import Control.Monad
import Text.Regex.Posix

answers = map (getUKText >>=) [q20, q21, q22, q23, q24, q25, q26, q27, q28, q29]

getUKText :: IO BS.ByteString
getUKText = return . f20 =<< getGZipContents "input/jawiki-country.json.gz"

f29 :: BS.ByteString -> IO BS.ByteString
f29 = return . extractFlagUrl <=< simpleHttp . flagRequestUrl

f28 :: BS.ByteString -> M.Map BS.ByteString BS.ByteString
f28 = M.map formatInfo . f27

f27 :: BS.ByteString -> M.Map BS.ByteString BS.ByteString
f27 = M.map removeInnerLink . f26

f26 :: BS.ByteString -> M.Map BS.ByteString BS.ByteString
f26 = M.map (removeStress "''" . removeStress "'''" . removeStress "'''''") . f25

f25 :: BS.ByteString -> M.Map BS.ByteString BS.ByteString
f25 = M.fromList . extractInfo . basicInfo

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

f20 :: LBS.ByteString -> BS.ByteString
f20 = utf8pack . findTextOf "イギリス" . mapMaybe decode . LBS.lines

type ByteStringFilter = BS.ByteString -> BS.ByteString

extractFlagUrl :: LBS.ByteString -> BS.ByteString
extractFlagUrl = maybe (u"") id . (@@1) . head . matchOf "\"url\":\"([^\"]+)\"" . LBS.toStrict

flagRequestUrl :: BS.ByteString -> String
flagRequestUrl str = let
    m = f28 str
    (Just filename) = M.lookup (u"国旗画像") m
    filename' = regexReplace " " "%20" filename
    in "https://en.wikipedia.org/w/api.php?action=query&titles=File:"
    ++ (utf8unpack filename')
    ++ "&prop=imageinfo&format=json&iiprop=url"

formatInfo :: ByteStringFilter
formatInfo = removeDots . simplifyLang . removeTags . replaceBrakes

removeDots :: ByteStringFilter
removeDots = regexReplace "^\\*+\\s*" ""

removeTags :: ByteStringFilter
removeTags = regexReplace "<[^.]*>" ""

replaceBrakes :: ByteStringFilter
replaceBrakes = regexReplace "(<br/>)|(<br />)" "\n"

regexReplace :: String -> String -> BS.ByteString -> BS.ByteString
regexReplace pattern repl str =
    BS.concat . Data.List.intersperse (utf8pack repl) $ regexReplace' str $ match str where
    match = matchOf pattern
        :: (BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString))
    regexReplace' s (before, target, after)
        | target == u"" = [s]
        | otherwise = (before :) $ regexReplace' after $ match after

simplifyLang :: ByteStringFilter
simplifyLang str = BS.unwords $ simplifyLang' str $ match str where
    match = matchOf "\\{\\{lang\\|[^|]*\\|([^}]+)\\}\\}"
        :: (BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]))
    simplifyLang' _ (before, _, after, (inner:_)) = (before :) . (inner :) $ simplifyLang' after $ match after
    simplifyLang' s _ = [s]

removeInnerLink :: ByteStringFilter
removeInnerLink str = removeInnerLink' str $ match str where
    match = matchOf "\\[\\[([^]]+)\\]\\]"
        :: (BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]))
    removeInnerLink' _ (before, target, after, (inner:_))
        | matchOf ".*\\|.*\\|.*" inner = BS.concat [before, head $ BS.split '|' $ inner, removeInnerLink after]
        | otherwise = BS.concat [before, last $ BS.split '|' $ inner, removeInnerLink after]
    removeInnerLink' s _ = s
        --  | matchOf ".*\\|.*\\|.*" inner = BS.concat [before, target, removeInnerLink after]

removeStress :: String -> BS.ByteString -> BS.ByteString
removeStress quotes = removeStress' where
    match = matchOf $ quotes ++ "(.*)" ++ quotes
        :: (BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]))
    removeStress' s = removeStress'' s $ match s
    removeStress'' _ (before, _, after, (inner:_)) = removeStress' $ BS.concat [before, inner, after]
    removeStress'' s _ = s

showBSMap :: M.Map BS.ByteString BS.ByteString -> BS.ByteString
showBSMap = BS.unlines . map (\(k,v) -> BS.concat [u"\"", k, u"\": \"", v, u"\""]) . M.toList

extractInfo :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
extractInfo = map (\(_:key:val:_) -> (key :: BS.ByteString, val :: BS.ByteString))
    . mapMaybe ((@@0) . matchOf "([^=]+) = (.*)" . head)
    . (matchOf "(([^[{|]+)|(\\{\\{[^}]+\\}\\})|(\\[\\[[^]]+\\]\\]))+" :: (BS.ByteString -> [[BS.ByteString]]))

basicInfo :: ByteStringFilter
basicInfo = BS.concat . reverse . basicInfo' [] True . BS.lines where
    basicInfo' :: [BS.ByteString] -> Bool -> [BS.ByteString] -> [BS.ByteString]
    basicInfo' buf skipping (s:strs)
        | matchOf "\\{\\{基礎情報" s = basicInfo' buf False strs
        | skipping = basicInfo' buf True strs
        | matchOf "^\\}\\}$" s = (u"|"):buf
        | otherwise = basicInfo' (s:buf) False strs

matchOf :: (RegexContext Text.Regex.Posix.Regex source target) =>
    String -> (source -> target)
matchOf pattern = (=~ (utf8pack pattern))

putBS :: BS.ByteString -> IO ()
putBS = putStr . utf8unpack

putBSLn :: BS.ByteString -> IO ()
putBSLn = putStrLn . utf8unpack

utf8unpack :: BS.ByteString -> String
utf8unpack =  decodeString . BS.unpack

utf8pack :: String -> BS.ByteString
utf8pack = BS.pack . encodeString
u = utf8pack

-- Safe (!!) operation
(@@) :: [a] -> Int -> Maybe a
(@@) list i
    | (i <) $ length list = Just $ list !! i
    | otherwise = Nothing
infix 9 @@

q29 :: BS.ByteString -> IO ()
q29 = putBSLn <=< f29
q28 :: BS.ByteString -> IO ()
q28 = putBS . showBSMap . f28
q27 :: BS.ByteString -> IO ()
q27 = putBS . showBSMap . f27
q26 :: BS.ByteString -> IO ()
q26 = putBS . showBSMap . f26
q25 :: BS.ByteString -> IO ()
q25 = putBS . showBSMap . f25
q24 :: BS.ByteString -> IO ()
q24 = putBS . BS.unlines . f24
q23 :: BS.ByteString -> IO ()
q23 = putStr . unlines . map (\(s, i) -> (utf8unpack s ++ "\t" ++ show i)) . f23
q22 :: BS.ByteString -> IO ()
q22 = putBS . BS.unlines . f22
q21 :: BS.ByteString -> IO ()
q21 = putBS . BS.unlines . f21
q20 :: BS.ByteString -> IO ()
q20 = putBSLn

data Article = Article
    { text :: String
    , title :: String
    } deriving (Show, Generic)

instance FromJSON Article

getGZipContents :: String -> IO LBS.ByteString
getGZipContents = fmap decompress . LBS.readFile

findTextOf :: String -> [Article] -> String
findTextOf title' = maybe "" text . find ((title' ==) . title)

