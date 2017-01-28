{-# LANGUAGE FlexibleInstances #-}

module Nlp4 (answers) where

import System.Directory
import Zak.Utf8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (mapMaybe)
import Control.Monad
import Data.List (group, sort, sortBy)
import Data.List.HT (chop)
import Data.Tuple.HT
import Control.Arrow
import Graphics.Gnuplot.Simple

answers = map (morphemeBook >>=) [q30, q31, q32, q33, q34, q35, q36, q37, q38, q39]

morphemeBook :: IO [[Morpheme]]
morphemeBook = buildMorphemeBook <$> BS.readFile "input/neko.txt.mecab"

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

f37 :: [(Int, ByteString)] -> String
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

f36 :: [[Morpheme]] -> [(Int, ByteString)]
f36 = take 10 . frequency

frequency :: [[Morpheme]] -> [(Int, ByteString)]
frequency = sortBy (flip compare) . runlengthF . sort . map base . join

runlength :: (Eq a) => [a] -> [(a, Int)]
runlength = map (head &&& length) . group

runlengthF :: (Eq a) => [a] -> [(Int, a)]
runlengthF = map (length &&& head) . group

f35 :: [[Morpheme]] -> [ByteString]
f35 = join . map (map (BS.concat . map surface) . filter ((2 <=) . length) . chop (not . isNoun))

f34 :: [[Morpheme]] -> [ByteString]
f34 = join . map aofb where
    aofb :: [Morpheme] -> [ByteString]
    aofb (a:o:b:ms)
        | isNoun a && (surface o == u"の") && isNoun b
            = (:) (BS.concat $ map surface [a, o, b]) (aofb $ b:ms)
        | otherwise = aofb $ o:b:ms
    aofb _ = []

f33 :: [[Morpheme]] -> [ByteString]
f33 = map surface . filter ((u"サ変接続" ==) . pos1) . join

f32 :: [[Morpheme]] -> [ByteString]
f32 = map base . filter isVerb . join

f31 :: [[Morpheme]] -> [ByteString]
f31 = map surface . filter isVerb . join

isNoun = (u"名詞" ==) . pos
isVerb = (u"動詞" ==) . pos

q36 = putStr . unlines . map (\(l,s) -> show l ++ "\t" ++ utf8unpack s) . f36
q35 = BS.putStr . BS.unlines . f35
q34 = BS.putStr . BS.unlines . f34
q33 = BS.putStr . BS.unlines . f33
q32 = BS.putStr . BS.unlines . f32
q31 = BS.putStr . BS.unlines . f31
q30 = print . join

buildMorphemeBook :: ByteString -> [[Morpheme]]
buildMorphemeBook =
    map (mapMaybe (toMorpheme . mapSnd (BS.split ',' . BS.tail) . BS.breakSubstring (u "\t"))) . chop (u "EOS" ==) . BS.lines

toMorpheme :: (ByteString, [ByteString]) -> Maybe Morpheme
toMorpheme (s, [p, p1, _, _, _, _, b, _, _]) = Just $ Morpheme s b p p1
toMorpheme _ = Nothing

data Morpheme = Morpheme
    { surface   :: ByteString
    , base      :: ByteString
    , pos       :: ByteString
    , pos1      :: ByteString
    } deriving(Eq, Ord)

instance Show Morpheme where
    show (Morpheme s b p p1) = let
        s' = utf8unpack . escapeEmSpace $ s
        b' = utf8unpack . escapeEmSpace $ b
        p' = utf8unpack p
        p1' = utf8unpack p1
        in concat ["{'base': '", b', "', 'pos': '", p', "', 'pos1': '", p1', "', 'surface': '", s', "'}"] where
            escapeEmSpace s
                | s == u"　" = u"\\u3000"
                | otherwise = s

instance {-# OVERLAPPING #-} Show [Morpheme] where
    show ms = ("[" ++) . drop 3 $ foldr (\a b -> ",\n " ++ show a ++ b) "]" ms

