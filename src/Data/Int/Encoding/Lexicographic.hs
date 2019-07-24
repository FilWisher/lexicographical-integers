module Data.Int.Encoding.Lexicographic 
    ( encodeMax

    , encode
    , encodehex
    , decode
    , decodehex
    ) where

import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16

import Data.Word

-- | The largest number that is safely encodable using this scheme
encodeMax :: Word64
encodeMax = 17592186044667

test x = (exp, res)
    where
        exp = (log (fromIntegral x) / log 2) - 32
        res = (fromIntegral x :: Double) / (2 ** (exp-11))

encode' :: Word64 -> [Word8]
encode' n =
    if n < fromIntegral max then
        [fromIntegral n]
    else if x < 256 then
        [max, fromIntegral x]
    else if x < 256*256 then
        [ max+1
        , fromIntegral (x `div` 256)
        , fromIntegral (x `mod` 256)
        ]
    else if x < 256*256*256 then
        [ max+2
        , fromIntegral ((x `div` 256) `div` 256)
        , fromIntegral ((x `div` 256) `mod` 256)
        , fromIntegral (x `mod` 256)
        ]
    else if x < 256*256*256*256 then
        [ max+3
        , fromIntegral (((x `div` 256) `div` 256) `div` 256)
        , fromIntegral (((x `div` 256) `div` 256) `mod` 256)
        , fromIntegral ((x `div` 256) `mod` 256)
        , fromIntegral (x `mod` 256)
        ]
    else
        let exp = floor (log (fromIntegral x :: Double) / log 2) - 32 :: Word64
            res = (fromIntegral x) / 2.0 ^^ (fromIntegral exp-11)
        in
            (255:encode' exp) ++ bytesOf (floor res)
    where
        max :: Word8
        max = 251

        bytesOf :: Word64 -> [Word8]
        bytesOf x = go [] 0 1
            where
                go xs 6 _ = xs
                go xs i d = go ((fromIntegral $ (floor ((fromIntegral x) / d)) `mod` 256): xs) (i+1) (d*256)

        x :: Word64
        x = n - fromIntegral max

encode :: Word64 -> BS.ByteString
encode = BS.pack . encode'

encodehex :: Word64 -> BS.ByteString
encodehex = Base16.encode . encode

decode' :: [Word8] -> Either String Word64
decode' [a] | a < 251 = 
    return $ fromIntegral a
decode' [a,b] | a == 251 = 
    return $ 251 + fromIntegral b
decode' [a,b,c] | a == 252 = 
    return $ 251 + 256 * fromIntegral b + fromIntegral c
decode' [a,b,c,d] | a == 253 = 
    return $ 251 
           + (256 * 256 * fromIntegral b) 
           + (256 * fromIntegral c) 
           + fromIntegral d
decode' [a,b,c,d,e] | a == 254 = 
    return $ 251 
           + (256 * 256 * 256 * fromIntegral b) 
           + (256 * 256 * fromIntegral c) 
           + (256 * fromIntegral d) 
           + fromIntegral e
decode' xs@(a:b:c:d:_) | length xs > 5 && a == 255 = do
    if b + 32 < 251 then do
        v <- decode' [b + 32]
        return $ f (v - 11)
    else if a == 255 && b < 251 then
        Right . f . fromIntegral $ b+21
    else if pivot == 3 then
        f  <$> decode' [b, c + 21]
    else if pivot == 4 then
        f <$> decode' [b, c, d + 21]
    else
        Left "Unmatched case"
    where 
        pivot = max 2 (length xs - 6)
        go :: Int -> Word64 -> Word64 -> Word64
        go i m x
            | i < pivot = m
            | otherwise = go (i-1) (m + (x * (fromIntegral $ xs!!i))) (x*256)
        m :: Word64
        m = go (length xs - 1) 0 1

        f :: Word64 -> Word64
        f n = 251 + floor ((fromIntegral m :: Double) / (2.0 ^^ (32 - fromIntegral n)))
decode' _ = Left "Unmatched case"

decode :: BS.ByteString -> Either String Word64
decode = decode' . BS.unpack

decodehex :: BS.ByteString -> Either String Word64
decodehex hex = 
    case Base16.decode hex of
        (bs, stuff) -> if BS.null stuff
            then decode bs
            else Left $ "Not valid hex: " <> B8.unpack stuff

toWord64 :: Word8 -> Word64
toWord64 = fromIntegral
