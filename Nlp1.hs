import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random

main = q09 >>= putStrLn

f09 :: String -> IO String
f09 str = do
    gen <- newStdGen
    return $ foldr1 catWithSpace $ fst . foldr midShuf ([], gen) $ words str

catWithSpace :: String -> String -> String
catWithSpace str1 str2 = str1 ++ ' ':str2

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

f08 :: String -> String
f08 str = show (cipher . cipher $ str, cipher str)

cipher :: String -> String
cipher = map cipher'
    where cipher' char
            | isAsciiLower char = chr . (219-) . ord $ char
            | otherwise = char

f07 :: Int -> String -> Float -> String
f07 a b c = (show a) ++ "時の" ++ b ++ "は" ++ (show c)

f06 :: String -> String -> String
f06 str1 str2 = show (union, intersection, difference, se_in x, se_in y)
    where
        bigramSet = Set.fromList . ngram 2
        (x, y) = (bigramSet str1, bigramSet str2)
        union = Set.union x y
        intersection = Set.intersection x y
        difference = Set.difference x y
        se_in = Set.member "se"

f05 :: String -> String
f05 str = show (nwords 2 str, ngram 2 str)

nwords :: Int -> String -> [[String]]
nwords n str = ngram n $ words str

ngram :: Int -> [a] -> [[a]]
ngram n seq =
    let len = length seq
        front = [0..(len-n)]
    in  map (\i -> take n $ drop i seq) front

f04 :: String -> String
f04 = show . Map.fromList . map (\(i, s) -> (take (f04' i) s, i)) . zip [1..] . words
f04' x  | x `elem` [1, 5, 6, 7, 8, 9, 15, 16, 19] = 1
        | otherwise = 2

f03 :: String -> String
f03 = show . map (length . filter isAlpha) . words

f02 :: String -> String -> String
f02 = ((foldr (\(a, b) s -> a:b:s) []) .) . zip

f01 :: String -> String
f01 = map snd . filter (\(i, c) -> odd i) . zip [1..]

f00 :: String -> String
f00 = reverse

q09 = f09 "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."
q08 = f08 "The QUICK brown fox jumps over the *lazy* dog."
q07 = f07 12 "気温" 22.4
q06 = f06 "paraparaparadise" "paragraph"
q05 = f05 "I am an NLPer"
q04 = f04 "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
q03 = f03 "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
q02 = f02 "パトカー" "タクシー"
q01 = f01 "パタトクカシーー"
q00 = f00 "stressed"
