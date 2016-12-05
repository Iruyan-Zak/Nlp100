module Nlp4 (answers) where

import System.Directory
import Text.MeCab
import Zak.Utf8
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)
import Control.Monad
import Data.List (group, sort)
import Graphics.Gnuplot.Simple

answers = map (morphemeBook >>=) [q30, q31, q32, q33, q34, q35, q36, q37, q38, q39]

morphemeBook :: IO [[Morpheme]]
morphemeBook = return . createMorphemeBook =<< mecabString

f39 :: [[Morpheme]] -> [Int]
f39 = map fst . frequency

q39 :: [[Morpheme]] -> IO ()
q39 = plotList attr . f39 where
    attr =
        [ Custom "logscale" []
        , Title "Zipf's law"
        , XLabel "Index"
        , YLabel "Usage count"
        , Key Nothing
        ]

f38 :: [[Morpheme]] -> [(Int, Int)]
f38 = runlength . map ((*10) . (`div` 10) . fst) . frequency

q38 :: [[Morpheme]] -> IO ()
q38 = plotListStyle attr (defaultStyle { plotType = Boxes }) . f38 where
    attr =
        [ Custom "boxwidth" ["10"]
        , Title "Histogram"
        , XLabel "Usage count"
        , YLabel "Number of kind of words"
        , XRange (0.0, 10**4)
        , YRange (0.0, 50.0)
        , Key Nothing
        ]

f37 :: [(Int, BS.ByteString)] -> String
f37 = unlines . map (\(n,s) -> "\"" ++ utf8unpack s ++ "\" " ++ show n)

q37 :: [[Morpheme]] -> IO ()
q37 morphs = do
    let option =
            [";set title \"Top 10 of most freqently used morpheme\""
            , "set xlabel \"Morphemes\""
            , "set ylabel \"Usage count\""
            , "set yrange [0:1e4]"
            , "unset key"
            ]
        plotCmd = "; plot \"" ++ outfile ++ "\" using 0:2:xtic(1) with boxes; #\\"
        attr = [Custom "boxwidth" ["0.5", "relative", unlines option ++ plotCmd]]
        dat = f37 $ f36 morphs
    writeFile outfile dat
    plotList attr ([] :: [Int])
    where outfile = "output/top10.dat"

f36 :: [[Morpheme]] -> [(Int, BS.ByteString)]
f36 = take 10 . frequency

frequency :: [[Morpheme]] -> [(Int, BS.ByteString)]
frequency = reverse . sort . runlengthF . sort . map base . join

runlength :: (Eq a) => [a] -> [(a, Int)]
runlength = map (\s -> (head s, length s)) . group

runlengthF :: (Eq a) => [a] -> [(Int, a)]
runlengthF = map (\s -> (length s, head s)) . group

f35 :: [[Morpheme]] -> [BS.ByteString]
f35 = join . map (nouns []) where
    nouns [] [] = []
    nouns buf [] = [BS.concat buf]
    nouns buf (m:ms) | isNoun m = nouns (surface m:buf) ms
    nouns [] (_:ms) = nouns [] ms
    nouns buf (_:ms) = (BS.concat buf) : (nouns [] ms)

f34 :: [[Morpheme]] -> [BS.ByteString]
f34 = join . map aofb where
    aofb :: [Morpheme] -> [BS.ByteString]
    aofb (a:o:b:ms)
        | (isNoun a) && (surface o == u"の") && (isNoun b)
            = BS.concat [surface a, surface o, surface b] : (aofb $ b:ms)
        | otherwise = aofb $ o:b:ms
    aofb _ = []

f33 :: [[Morpheme]] -> [BS.ByteString]
f33 = map surface . filter ((u"サ変接続" ==) . pos1) . join

f32 :: [[Morpheme]] -> [BS.ByteString]
f32 = map base . filter isVerb . join

f31 :: [[Morpheme]] -> [BS.ByteString]
f31 = map surface . filter isVerb . join

isNoun = (u"名詞" ==) . pos
isVerb = (u"動詞" ==) . pos

q36 = putStr . unlines . map (\(l,s) -> show l ++ "\t" ++ utf8unpack s) . f36
q35 = BS.putStr . BS.unlines . f35
q34 = BS.putStr . BS.unlines . f34
q33 = BS.putStr . BS.unlines . f33
q32 = BS.putStr . BS.unlines . f32
q31 = BS.putStr . BS.unlines . f31
q30 = putStr . show

mecabString :: IO BS.ByteString
mecabString = do
    exist <- doesFileExist outfile
    if exist then BS.readFile outfile
        else do
            contents <- readFile infile
            mecab <- new2 " -E\\ -F %m\\t%f[6]\\t%f[0]\\t%f[1]\\n"
            parsedStr <- parse mecab $ utf8pack contents
            BS.writeFile outfile parsedStr
            return parsedStr
    where
        infile = "input/neko.txt"
        outfile = "output/neko.txt.mecab"

createMorphemeBook :: BS.ByteString -> [[Morpheme]]
createMorphemeBook =
    intoBook . mapMaybe (listToMorpheme . BS.split '\t') . BS.lines
    where
        intoBook :: [Morpheme] -> [[Morpheme]]
        intoBook [] = []
        intoBook morphs = (first :) $ intoBook second
            where (first, second) = devideAfter (\(Morpheme s _ _ _) -> s == u"。") morphs

devideAfter :: (a -> Bool) -> [a] -> ([a], [a])
devideAfter _ [] = ([], [])
devideAfter pred (x:xs)
    | pred x = ([x], xs)
    | otherwise = (x:first, second) where (first, second) = devideAfter pred xs

data Morpheme = Morpheme
    { surface   :: ByteString
    , base      :: ByteString
    , pos       :: ByteString
    , pos1      :: ByteString
    } deriving(Eq, Ord)

instance Show Morpheme where
    show (Morpheme s b p p1) = utf8unpack $ BS.concat [s, u": ", b, u"/", p, u"/", p1]

listToMorpheme :: [ByteString] -> Maybe Morpheme
listToMorpheme (s:b:p:p1:[]) = Just $ Morpheme s b p p1
listToMorpheme _ = Nothing

