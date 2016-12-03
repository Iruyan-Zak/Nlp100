module Main where

import qualified Nlp1
import qualified Nlp2
import qualified Nlp3
-- import Nlp4

import System.Environment
import Control.Monad

q = join [Nlp1.answers, Nlp2.answers, Nlp3.answers]

main :: IO ()
main = getArgs >>= (q !!) . read . head

