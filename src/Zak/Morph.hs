{-# LANGUAGE FlexibleInstances #-}

module Zak.Morph where

import Zak.Utf8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (mapMaybe)
import Data.Tuple.HT (mapSnd)

data Morph = Morph
    { surface   :: ByteString
    , base      :: ByteString
    , pos       :: ByteString
    , pos1      :: ByteString
    } deriving(Eq, Ord)

instance Show Morph where
    show (Morph s b p p1) = let
        s' = utf8unpack . escapeEmSpace $ s
        b' = utf8unpack . escapeEmSpace $ b
        p' = utf8unpack p
        p1' = utf8unpack p1
        in concat ["{'base': '", b', "', 'pos': '", p', "', 'pos1': '", p1', "', 'surface': '", s', "'}"]
            where
                escapeEmSpace s
                    | s == u"　" = u"\\u3000"
                    | otherwise = s

instance {-# OVERLAPPING #-} Show [Morph] where
    show ms = ("[" ++) . drop 3 $ foldr (\a b -> ",\n " ++ show a ++ b) "]" ms


buildMorphBook :: [[ByteString]] -> [[Morph]]
buildMorphBook = map (mapMaybe lineToMorph)

lineToMorph :: ByteString -> Maybe Morph
lineToMorph = toMorph . getFields where
    getFields :: ByteString -> (ByteString, [ByteString])
    getFields = mapSnd (BS.split ',' . BS.tail) . BS.breakSubstring (u "\t")

    toMorph :: (ByteString, [ByteString]) -> Maybe Morph
    toMorph (s, [p, p1, _, _, _, _, b, _, _]) = Just $ Morph s b p p1
    toMorph _ = Nothing

isNoun :: Morph -> Bool
isNoun = (u"名詞" ==) . pos

isVerb :: Morph -> Bool
isVerb = (u"動詞" ==) . pos

