---
title: Dealing with malformed CSV data with sv
author: gwilson
date: 0218-03-19
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

It's not really one CSV document. It's really two logical documents sharing one
file.

== Solution

By exposing its parser for you to build on, sv lets you deal with this kind of
file with relative ease.

Let's have a look. First some imports:

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
to set `endOnBlankLine` to `True`, but we could just use field accessors
instead if we wanted to.

\begin{code}
opts :: ParseOptions ByteString
opts = defaultParseOptions & endOnBlankLine .~ True
\end{code}

This setting means that when sv encounters a blank line in a file, it will
treat that line as the end of the CSV. So with this setting, we can plug two
copies of sv's parser together, and they will parse the nasty file above.
The first parser will parse the first logical document, and end when it hits
the blank line separating the two. Then the second parser will begin, parsing
the second document.

\begin{code}
sv2 :: CharParsing m => ParseOptions s -> m (Sv s, Sv s)
sv2 o = (,) <$> separatedValues o <*> separatedValues o

parser :: CharParsing m => m (Sv ByteString, Sv ByteString)
parser = sv2 opts
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

cost :: TokenParsing m => m Cost
cost = char '$' *> fmap Cost double

item :: Decode' ByteString Item
item = Item <$> D.utf8 <*> D.int <*> D.withTrifecta cost
\end{code}

This is the type containing all our decoded data from both logical documents

\begin{code}
data PeopleAndItems = PeopleAndItems [Person] [Item] deriving Show
\end{code}

and our main function

\begin{code}
main :: IO ()
main = do
  d <- parseFromFile parser file
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
allows one to extend either of these independently, allowing for handling of
particularly malformed files.

