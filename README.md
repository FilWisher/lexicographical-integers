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
