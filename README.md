# lexicographic-integers

Lexicographic-ordered encoding for integers. Produces `ByteString` and hex
encoded `ByteString`s for integers.

The following invariants always hold:

```
x < y => encode(x) < encode(y)
decode(encode(x)) == x
```

## Example

```
import Data.Int.Encoding.Lexicographic

main :: IO ()
main = do
    if encodehex 21 < encodehex 26
        then print "always true"
        else error "this will never run"
```

## API

 - `encodeMax :: Word64` - The largest safely-encodable number using this encoding scheme
 - `encode :: Word64 -> ByteString` - encode an integer as a ByteString
 - `encodehex :: Word64 -> ByteString` - encode an integer as a hex encoded ByteString
 - `decode :: ByteString -> Either String Word64` - decode a ByteString to an integer
 - `decodehex :: ByteString -> Either String Word64` - decode a hex-encoded ByteString to an integer
