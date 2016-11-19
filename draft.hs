f15 n str = let
    l = lines str
    d = (length l) - n
    in unlines $ drop d l

countLF :: Int -> Char -> Int
countLF n '\n' = n + 1
countLF n c = n

ifmap pred func seq = map (ifmap' pred func) seq
    where ifmap' pred func element
            | pred element = func element
            | otherwise = element

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to-from) . drop from

stringAt n l
    | (n <) . length $ l = l !! n
    | otherwise = ""

concatWith :: Char -> String -> String -> String
concatWith delimeter str1 str2 = str1 ++ (delimeter:str2)

-- f18 = unlines . map unwords . sortBy (flip . comparing (negate . read . (!!2))) . filter ((3<=) . length) . map words . lines
-- f18 = unlines . sortBy (comparing (!!2)) . filter ((3<=) . length) . map words . lines
-- f18 = unlines . snd . sortBy (\(a,_) (b,_) -> compare b a) . ifmap ((3<=) . length . words) (\ l -> ((!!2) . words $ l, l)) . lines
