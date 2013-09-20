
----

# Compression

In the scaffold folder `compression` there is a Cabal project that
implements a compression strategy based on
[Huffman Coding](http://en.wikipedia.org/wiki/Huffman_coding).

<div class="center">

  ![Huffman Tree](resources/images/huffmantree.png)

</div>

A Huffman Tree is built recursively by recursively constructing a tree
from the two lowest-scored trees in a list, then adding the new
tree to the list, until there is only one tree left.

~~~{data-language=haskell .nocheck}
data FingerTree k v = L k (Maybe v) | B k (FingerTree k v) (FingerTree k v)

type HuffmanTree = FingerTree (Sum Int) Char

instance Monoid k => Monoid (FingerTree k v)
  where x `mappend` y = B (getKey x `mappend` getKey y) x y
        mempty        = L mempty Nothing

getKey :: FingerTree t t1 -> t
getKey (L k _  ) = k
getKey (B k _ _) = k

step :: [HuffmanTree] -> [HuffmanTree]
step []    = []
step [x]   = [x]
step [x,y] = [ x <> y ]
step xs    = step (take 2 sorted) ++ drop 2 sorted
  where
    sorted = sortBy (comparing getKey) xs

build :: [HuffmanTree] -> HuffmanTree
build = head . fromJust . find ((<= 1) . length) . iterate step
~~~

The basic idea is that in a non-random string, some combinations
of tokens will be more frequent than others, and as-such, common
substrings can be represented with a short-hand encoding to
save space.

The project contains two executables: `compression` and `decompression`.
These accept data on STDIN and output data on STDOUT. `compression` accepts
text on STDIN and outputs binary data. Decompression does the reverse.

```real
The Haskell package "binary" exposes the module "Data.Binary" that
allows for very easy binary serialisation and parsing, with
most existing types already containing a marshelling implementation.
```

If you install these executables by using `cabal install`, then
you will be able to use them in the following manner:

```shell
> # Compress
> compression < lyrics.txt > lyrics.txt.compressed
> # Decompress
> decompression < lyrics.txt.compressed
```

Top down, the compression algorithm works as follows:

```text
* Determine the frequencies of the characters in the input text from STDIN
* Build a Huffman Tree from the frequency table
* Traverse the tree, outputting Bit-String encodings for each character
* Map a lookup of the encoding table over the input text
* Pack the resulting bits into a ByteString
* Attach a header (containing length, and frequency table)
* Output the result to STDOUT
```

With the decompression algorithm performing the following:

```
* Read a ByteString
* Unpack into header (length and frequency table) and body
* Translate frequency-table into encoding table as per compression
* For each encoding match against the start of the compressed body
* Output the character if there is a match
* Process the left-over body
```

```note
Unfortunately, the compression algorithm contains a bug.
```

```instruction
In the compression scaffolding project, locate and fix the bug
that causes the test-suite to fail.

You can run the tests by calling "cabal test" from within
the project folder.
```

~~~{ data-language=haskell .answer .nocheck }
-- Line 131 should be:

treeToBits t = map (second reverse) result
~~~

Although the algorithm itself is very basic, and the use of
strings instead of binary-data is not a common way to perform
compression, the structure  itself is typical of a real-world
Haskell project. The project structures itself around the Cabal toolkit,
and uses several libraries. It exposes a library, several
executables, and a test-suite.

```open
An open question:

How would you implement a more efficient compression algorithm in Haskell?
```
