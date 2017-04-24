module Nlp4 (answers) where

import Zak.Utf8
import Zak.Morph
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.Monad
import Data.List (group, sort, sortBy)
import Data.List.HT (chop)
import Control.Arrow
import Graphics.Gnuplot.Simple

answers = map (morphemeBook >>=) [q30, q31, q32, q33, q34, q35, q36, q37, q38, q39]

morphemeBook :: IO [[Morph]]
morphemeBook = buildMorphBook . chop (u "EOS" ==) . BS.lines
                <$> BS.readFile "input/neko.txt.mecab"

f39 :: [[Morph]] -> [Int]
f39 = map fst . frequency

q39 :: [[Morph]] -> IO ()
q39 = plotList attr . f39 where
    attr =
        [ Custom "logscale" []
        , Title "Zipf's law"
        , XLabel "Index"
        , YLabel "Usage count"
        , Key Nothing
        ]

f38 :: [[Morph]] -> [(Int, Int)]
f38 = runlength . map ((*10) . (`div` 10) . fst) . frequency

q38 :: [[Morph]] -> IO ()
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

q37 :: [[Morph]] -> IO ()
q37 morphs = do
    let option =
            [";set title \"Top 10 of most freqently used Morph\""
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

f36 :: [[Morph]] -> [(Int, ByteString)]
f36 = take 10 . frequency

frequency :: [[Morph]] -> [(Int, ByteString)]
frequency = sortBy (flip compare) . runlengthF . sort . map base . join

runlength :: (Eq a) => [a] -> [(a, Int)]
runlength = map (head &&& length) . group

runlengthF :: (Eq a) => [a] -> [(Int, a)]
runlengthF = map (length &&& head) . group

f35 :: [[Morph]] -> [ByteString]
f35 = join . map (map (BS.concat . map surface) . filter ((2 <=) . length) . chop (not . isNoun))

f34 :: [[Morph]] -> [ByteString]
f34 = join . map aofb where
    aofb :: [Morph] -> [ByteString]
    aofb (a:o:b:ms)
        | isNoun a && (surface o == u"の") && isNoun b
            = (:) (BS.concat $ map surface [a, o, b]) (aofb $ b:ms)
        | otherwise = aofb $ o:b:ms
    aofb _ = []

f33 :: [[Morph]] -> [ByteString]
f33 = map surface . filter ((u"サ変接続" ==) . pos1) . join

f32 :: [[Morph]] -> [ByteString]
f32 = map base . filter isVerb . join

f31 :: [[Morph]] -> [ByteString]
f31 = map surface . filter isVerb . join

q36 = putStr . unlines . map (\(l,s) -> show l ++ "\t" ++ utf8unpack s) . f36
q35 = BS.putStr . BS.unlines . f35
q34 = BS.putStr . BS.unlines . f34
q33 = BS.putStr . BS.unlines . f33
q32 = BS.putStr . BS.unlines . f32
q31 = BS.putStr . BS.unlines . f31
q30 = print . join

