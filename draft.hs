ifmap pred func seq = map (ifmap' pred func) seq
    where ifmap' pred func element
            | pred element = func element
            | otherwise = element

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to-from) . drop from


