module Nlp1 where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random

answers :: [IO ()]
answers = map putStrLn [q00, q01, q02, q03, q04, q05, q06, q07, q08] ++ [q09]

-- |
-- 09. Typoglycemia
--
-- スペースで区切られた単語列に対して、各単語の先頭と末尾の文字は残し、それ以外の文字の順序をランダムに並び替える。長さが4以下の単語は並び替えない。
--
-- Test is in `test/Spec.hs`.
f09 :: String -> IO String
f09 str = do
    gen <- newStdGen
    return $ foldr1 catWithSpace $ fst . foldr midShuf ([], gen) $ words str

-- |
-- Concat two strings with space.
--
-- >>> catWithSpace "Hello," "world!"
-- "Hello, world!"
--
catWithSpace :: String -> String -> String
catWithSpace str1 str2 = str1 ++ ' ':str2

-- |
-- Shuffle middle of each list which has length 5 or more.
--
midShuf :: RandomGen t => [a] -> ([[a]], t) -> ([[a]], t)
midShuf [] (buf, rand) = (buf, rand)
midShuf element@(c:s) (buf, rand)
    | (4>=) . length $ element = (element:buf, rand)
    | otherwise = (element':buf, rand')
        where middle = init s
              (middle', rand') = shuffle middle rand
              element' = c:middle' ++ [last s]

shuffle :: RandomGen t => [a] -> t -> ([a], t)
shuffle seq rand = shuffle' [] seq rand
    where
        shuffle' buf [] rand = (buf, rand)
        shuffle' buf seq rand = shuffle' (c:buf) s' rand'
            where
                (r, rand') = random rand
                (c, s') = extract seq $ mod r $ length seq

extract :: [a] -> Int -> (a, [a])
extract seq ind =
    let c:s = drop ind seq
        seq' = take ind seq ++ s
    in (c, seq')

-- |
-- 08. 暗号文
-- 入力文字列の各文字を以下のように変換する。
-- - 英小文字 -> (219 - 文字コード)
-- - それ以外 -> そのまま
--
-- prop> all (not . (`elem` ['a'..'z'])) str || str /= cipher str
-- prop> str == (cipher . cipher) str
f08 :: String -> String
f08 str = show (cipher . cipher $ str, cipher str)

cipher :: String -> String
cipher = map cipher'
    where cipher' char
            | isAsciiLower char = chr . (219-) . ord $ char
            | otherwise = char

-- |
-- 07. テンプレートによる文生成
-- 入力 x, y, z に対し、「x時のyはz」という文字列を返す。
--
-- >>> putStr $ f07 12 "気温" 22.4
-- 12時の気温は22.4
f07 :: Int -> String -> Float -> String
f07 a b c = (show a) ++ "時の" ++ b ++ "は" ++ (show c)

-- |
-- 06. 集合
-- 2つの文字列からそれぞれ文字bi-gramを得、それぞれの和集合・積集合・差集合を求める。
-- また、2つの文字列の文字bi-gramに"se"が含まれるかを判定する。
--
-- >>> let str1 = "paraparaparadise"
-- >>> let str2 = "paragraph"
-- >>> let (u,i,d,s1,s2) = f06 str1 str2
-- >>> i `Set.isSubsetOf` u
-- True
-- >>> d `Set.isSubsetOf` (u `Set.difference` i)
-- True
-- >>> s1
-- True
-- >>> s2
-- False
--
f06 :: String -> String -> (Set.Set String, Set.Set String, Set.Set String, Bool, Bool)
f06 str1 str2 = (union, intersection, difference, se_in x, se_in y)
    where
        bigramSet = Set.fromList . ngram 2
        (x, y) = (bigramSet str1, bigramSet str2)
        union = Set.union x y
        intersection = Set.intersection x y
        difference = Set.difference x y
        se_in = Set.member "se"


-- |
-- 05. n-gram
-- 与えられた文字列から単語bi-gramと文字bi-gramを生成する。
--
-- >>> let str = "I am an NLPer"
-- >>> let (w, c) = f05 str
-- >>> c
-- ["I "," a","am","m "," a","an","n "," N","NL","LP","Pe","er"]
-- >>> w
-- [["I","am"],["am","an"],["an","NLPer"]]
--
f05 :: String -> ([[String]], [String])
f05 str = (nwords 2 str, ngram 2 str)

nwords :: Int -> String -> [[String]]
nwords n str = ngram n $ words str

ngram :: Int -> [a] -> [[a]]
ngram n seq =
    let len = length seq
        front = [0..(len-n)]
    in  map (\i -> take n $ drop i seq) front

-- |
-- 04. 元素記号
-- 文字列を単語に分解し、以下の規則によって変換し、得られたも文字列から単語の位置への連想配列を得る。
-- 1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語 -> 先頭の1文字
-- それ以外 -> 先頭の2文字
--
-- >>> let elements = f04 "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
--
-- >>> Map.lookup "H" elements
-- Just 1
--
-- >>> Map.lookup "Ca" elements
-- Just 20
--
f04 :: String -> Map.Map String Int
f04 = Map.fromList . map (\(i, s) -> (take (f04' i) s, i)) . zip [1..] . words
f04' x  | x `elem` [1, 5, 6, 7, 8, 9, 15, 16, 19] = 1
        | otherwise = 2

-- |
-- 03. 円周率
-- 文字列に含まれる各単語の文字数を先頭から出現順に並べたリストを得る。
--
-- >>> f03 "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
-- [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]
--
f03 :: String -> [Int]
f03 = map (length . filter isAlpha) . words

-- |
-- 02. 「パトカー」+「タクシー」=「パタトクカシーー」
-- 2つの文字列に含まれる文字を、先頭から交互に連結する。
--
-- >>> putStr $ f02 "パトカー" "タクシー"
-- パタトクカシーー
--
f02 :: String -> String -> String
f02 = ((foldr (\(a, b) s -> a:b:s) []) .) . zip

-- |
-- 01. 「パタトクカシーー」
-- 文字列の奇数番目の文字を取り出して連結する。
--
-- >>> putStr $ f01 "パタトクカシーー"
-- パトカー
--
f01 :: String -> String
f01 = map snd . filter (\(i, c) -> odd i) . zip [1..]

-- |
-- 00. 文字列の逆順
-- 逆順の文字列を得る。
--
-- >>> f00 "stressed"
-- "desserts"
--
f00 :: String -> String
f00 = reverse

q09 = putStrLn =<< f09 "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."
q08 = f08 "The QUICK brown fox jumps over the *lazy* dog."
q07 = f07 12 "気温" 22.4
q06 = show $ f06 "paraparaparadise" "paragraph"
q05 = show $ f05 "I am an NLPer"
q04 = show $ f04 "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
q03 = show $ f03 "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
q02 = f02 "パトカー" "タクシー"
q01 = f01 "パタトクカシーー"
q00 = f00 "stressed"
