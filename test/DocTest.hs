module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Nlp1.hs"
               , "src/Nlp2.hs"
               ]

