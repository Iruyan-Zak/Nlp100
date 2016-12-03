module Nlp2 (answers) where

import Data.List
import System.Directory
import System.IO.Error
import Control.Exception

answers = map (readFile "input/hightemp.txt" >>=) [q10, q11, q12, q13, q14, q15, q16, q17, q18, q19]

-- cut -f1 | LC_ALL=C sort | uniq -c
f19 :: String -> String
f19 = unlines
    . map (\(s, l) -> (++ ('\t':s)) . show . negate $ l)
    . sort
    . map (\s -> (head s, negate . length $ s))
    . group . sort . lines . head . columns

-- sort -nrk 3 hightemp.txt
f18 :: String -> String
f18 = unlines . map snd . sort
    . map (\(w,s) -> (negate . read . (!!2) $ w :: Float, s))
    . filter ((3<=) . length . fst)
    . map (\s -> (words s, s))
    . lines

-- cut -f1 | LC_ALL=C sort -u
f17 :: String -> String
f17 = unlines . sort . nub . lines . head . columns

-- | split -l に対する実装。
-- | 問題文は split -n l/ を想定している可能性がある。
-- | 行数の分割ポリシーは把握できたが、それを書くともう少し長くなるため今回はここまで。
f16 :: Int -> String -> [(String, String)]
f16 n str = let
    iSplit = f16' 1 $ lines str
    digits = log10N $ length $ iSplit
    paddingZero = replicate (digits-1) '0'
    zeroPadding = tailN digits . (paddingZero++) . show
    in map (\(ind, block) -> (zeroPadding ind, unlines block)) iSplit where
    f16' :: Int -> [String] -> [(Int, [String])]
    f16' _ [] = []
    f16' ind seq = let (h, t) = splitAt n seq in (ind, h):(f16' (ind + 1) t)
    log10N :: Int -> Int
    log10N = log10N' 0 where
        log10N' ans 0 = ans
        log10N' ans n = log10N' (ans+1) (n `div` 10)

-- tail -n
f15 :: Int -> String -> String
f15 n = unlines . tailN n . lines

tailN :: Int -> [a] -> [a]
tailN n = reverse . take n . reverse

-- head -n
f14 :: Int -> String -> String
f14 n = unlines . take n . lines

-- paste str1 str2
f13 :: String -> String -> String
f13 str1 str2 = unlines $ zipWith (\s1 s2 -> s1 ++ '\t':s2) (lines str1) (lines str2)

-- (cut -f1, cut -f2)
f12 :: String -> (String, String)
f12 str = (s1, s2) where (s1:s2:_) = columns str

columns :: String -> [String]
columns = map unlines . transpose . map words . lines

-- expand -t1
f11 :: String -> String
f11 = map expand where
    expand :: Char -> Char
    expand '\t' = ' '
    expand c = c

-- wc -l
f10 :: String -> String
f10 = show . length . lines


q19 = putStr . f19
q18 = putStr . f18
q17 = putStr . f17

q16 input = do
    n <- readLn :: IO Int
    putStrLn $ foldr (\(ind, str) buf -> unlines [ind, str] ++ buf) "" $ f16 n input

q15 input = do
    n <- readLn :: IO Int
    putStr $ f15 n input

q14 input = do
    n <- readLn :: IO Int
    putStr $ f14 n input

q13 input = do
    c1 <- readFile "output/col1.txt"
    c2 <- readFile "output/col2.txt"
    writeFile "output/col.txt" $ f13 c1 c2

q12 input = do
    let (s1, s2) = f12 input
    mkdir_p "output"
    writeFile "output/col1.txt" s1
    writeFile "output/col2.txt" s2

ignoreAlreadyExist :: IOError -> IO()
ignoreAlreadyExist error
    | ioeGetErrorType error == alreadyExistsErrorType = return ()
    | otherwise = throwIO error

mkdir_p :: FilePath -> IO ()
mkdir_p = handle ignoreAlreadyExist . createDirectory

q11 = putStrLn . f11
q10 = putStrLn . f10

