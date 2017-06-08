import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (transpose, sort)
import Control.Monad
import Data.Traversable (for)
import Debug.Trace

import Nlp1 hiding (answers)


main :: IO ()
main = hspec $ do
    describe "Q9" $ do
        it "doesn't change short strs." $ f09case "I am the man." (\(xs, k) -> all (k ==) xs)
        it "changes long strs." $ f09case "There aren't peoples." (\(xs, k) -> any (k /=) xs)
        it "doesn't changes a kind of characters." $ f09case "There aren't peoples." (\(xs, k) -> all (sort k ==) $ map sort xs)
        it "doesn't move the first character," $ f09case "There aren't peoples." (\(xs, k) -> all (head k ==) $ map head xs)
        it "and the last one." $ f09case "There aren't peoples." (\(xs, k) -> all (last k ==) $ map last xs)


f09case :: String -> (([String], String) -> Bool) -> Expectation
f09case input pred = do
    shortStrs <- replicateM 10 $ words <$> f09 input
    transpose shortStrs `zip` words input `forM_` ((`shouldBe` True) . pred)

