module Zak.Utf8 (
    utf8unpack,
    utf8pack,
    u,
    BS.ByteString
) where

import qualified Data.ByteString.Char8 as BS
import Codec.Binary.UTF8.String (encodeString, decodeString)

utf8unpack :: BS.ByteString -> String
utf8unpack =  decodeString . BS.unpack

utf8pack :: String -> BS.ByteString
utf8pack = BS.pack . encodeString
u = utf8pack

