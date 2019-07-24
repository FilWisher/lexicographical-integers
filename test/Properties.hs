import Test.QuickCheck (quickCheck, choose, Gen, arbitrary, forAll)

import Control.Monad
import Data.Word

import Data.Int.Encoding.Lexicographic

prop_encode_ordering :: Word64 -> Word64 -> Bool
prop_encode_ordering x y
    | x < y && encode x < encode y = True
    | x > y && encode x > encode y = True
    | x == y && encode x == encode y = True
    | otherwise = False

prop_encodehex_ordering :: Word64 -> Word64 -> Bool
prop_encodehex_ordering x y
    | x < y && encodehex x < encodehex y = True
    | x > y && encodehex x > encodehex y = True
    | x == y && encodehex x == encodehex y = True
    | otherwise = False

prop_inverse :: Word64 -> Bool
prop_inverse x =
    case decode (encode x) of
        Left _  -> False
        Right y -> x == y

prop_inverse_hex :: Word64 -> Bool
prop_inverse_hex x =
    case decodehex (encodehex x) of
        Left _  -> False
        Right y -> x == y


main :: IO ()
main = do
    quickCheck $ prop_encode_ordering
    quickCheck $ forAll (choose (0,encodeMax)) prop_inverse
    quickCheck $ prop_encodehex_ordering
    quickCheck $ forAll (choose (0,encodeMax)) prop_inverse_hex
