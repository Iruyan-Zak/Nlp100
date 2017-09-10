module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Nlp1.hs" ]

