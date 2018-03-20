---
title: Dealing with multiple CSVs in one file
date: 2018-03-19
author: gwilson
---

== Problem

Have you ever seen a CSV that looked like this?

\begin{code}
"name","age"
John Doe,50
Jane Smith,30
Frank Cooper,45

"item",        "stock", "price"
"skateboard",  "5",     "$10.0"
"fishing rod", "1",     "$500.0"
"basket",      "20",    "$20.0"
\end{code}

Although the above is one file, it is not really one CSV document. It's really
two documents! Each of them has its own header, and a different number of
columns. Unfortunately, they're together in a single file. This will
cause our CSV processing tools some difficulty.

== Solution

sv has been designed to separate the parse and decode phases of ingestion.
Parsing is converting the textual representation into a CSV syntax tree, and
decoding refers to extracting useful data types from the contets of this tree.

Because sv's parser is disentangled from its decoding, you can use the parser
on its own, with other parsers, and then potentially feed the results into the
decoding later.
This lets us deal with this kind of malformed CSV file with ease.

Let's see how to do this. First some imports:

\begin{code}
module Data.Sv.Example.Concat where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Trifecta (TokenParsing, CharParsing, double, parseFromFile, char)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D
\end{code}

and the filepath we'll get that CSV from

\begin{code}
file :: FilePath
file = "csv/concat.csv"
\end{code}

Next we set up our parse options. Here we're using some operators from
[lens](https://hackage.haskell.org/package/lens)
to set `endOnBlankLine` to `True`, but we could use field accessors
instead if we preferred that style.

\begin{code}
opts :: ParseOptions ByteString
opts = defaultParseOptions & endOnBlankLine .~ True

-- we could just as easily do this without lens,
-- using field accessors instead
-- opts = defaultParseOptions { _endOnBlankLine = True }
\end{code}

Setting `endOnBlankLine` to true means that when sv encounters a blank line in
a file, it will treat that line as the end of the CSV. So with this setting,
we can plug two copies of sv's parser (`separatedValues`) together, and they
will parse the file above.
The first parser will parse the first logical document, and end when it hits
the blank line separating the two. Then the second parser will begin, parsing
the second document.

\begin{code}
pairParser :: CharParsing m => m (Sv ByteString, Sv ByteString)
pairParser = (,) <$> separatedValues opts <*> separatedValues opts
\end{code}

Now we'll set up our types and `Decode`s. They're not particularly interesting.
To handle the '$' in cost, we build a trifecta parser and turn it into a `Decode`.

\begin{code}
type Name = Text
type Age = Int
data Person = Person Name Age deriving Show

person :: Decode' ByteString Person
person = Person <$> D.utf8 <*> D.int

type Stock = Int
newtype Cost = Cost Double deriving Show
data Item = Item Name Stock Cost deriving Show

cost :: Decode' ByteString Cost
cost = D.withTrifecta (char '$' *> fmap Cost double)

item :: Decode' ByteString Item
item = Item <$> D.utf8 <*> D.int <*> cost
\end{code}

This is the type to store all our decoded data from both documents

\begin{code}
data PeopleAndItems = PeopleAndItems [Person] [Item] deriving Show
\end{code}

and finally, our main function

\begin{code}
main :: IO ()
main = do
  d <- parseFromFile pairParser file
  case d of
    Nothing -> exitFailure
    Just (s1,s2) -> do
      let result = PeopleAndItems <$> decode person s1 <*> decode item s2
      case result of
        Failure e -> do
          print e
          exitFailure
        Success a -> print a
\end{code}

== Conclusion

By strictly separating the `Parse` and `Decode` phases of CSV ingestion, sv
allows one to extend either of these independently, allowing for more
flexibility in handling certain classes of malformed files.
