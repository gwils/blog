---
title: Transforming CSV with sv
date: 2018-03-20
author: gwilson
---

CSV libraries like [sv](https://hackage.haskell.org/package/sv) are often used
to decode documents into data types we can use in our program, but we can also
use them to transform documents in place.

You can do this to make your CSV easier for some other tool to work with.
Perhaps you have a machine learning pipeline in Python. In that case, you won't
be able to use sv in your program directly, since sv is a Haskell library, and
there is not yet a good way to bridge Python and Haskell code.
But you might still find value in using sv as a preprocessor to clean some
aspects of your data, before it even touches your pipeline.

This blog post will go through an example of using sv along with
[lens](https://hackage.haskell.org/package/lens) to normalise a CSV file that
has inconsistent quoting.

Here's the file:

\begin{code}
(TODO put the file here George)
\end{code}

The problem with this file is of course that its quoting structure is
inconsistent. Let's fix the file. We'll replace all single quotes around fields
with double quotes, and we'll insert quotes where there are none.

We'll start with some imports:

\begin{code}
import Control.Lens
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Sv
import Data.Sv.Print (writeSvToFile)
import Text.Escape (Unescaped (Unescaped))
import Text.Quote (Quote (DoubleQuote))
\end{code}

Next, our input and output files.

\begin{code}
input :: FilePath
input = "csv/requote.csv"

fixed :: FilePath
fixed = "csv/requote.fixed.csv"
\end{code}

As for parsing, sv's default options suit our needs this time.

\begin{code}
opts :: ParseOptions ByteString
opts = defaultParseOptions
\end{code}

Our `main` reads in the file using `parseSvFromFile` and writes the output using
`writeSvToFile`. The interesting part is `fixQuotes`, which we haven't defined yet.

\begin{code}
main :: IO ()
main = do
  svEither <- parseSvFromFile opts input
  case svEither of
    Left _ -> exitFailure
    Right s ->
      let s' :: Sv ByteString
          s' = fixQuotes s
      in  writeSvToFile fixed s'
\end{code}

Finally, here's the interesting part: `fixQuotes`.
This function takes an `Sv`, which is sv's CSV syntax tree, and quotes all
fields in the document with double quotes, without changing any of the fields'
contents. It does this by composing `Traversal`s which are defined on the
structure of the `Sv` type. A `Traversal` is a way of viewing or modifying
many parts of a structure at once.

\begin{code}
fixQuotes :: Sv s -> Sv s
fixQuotes = over headerFields fixQuote . over recordFields fixQuote
  where
    fixQuote :: Field a -> Field a
    fixQuote f = case f of
      Unquoted a -> Quoted DoubleQuote (Unescaped a)
      Quoted _ v -> Quoted DoubleQuote v
    headerFields = traverseHeader . fields
    recordFields = traverseRecords . fields
\end{code}

First we go through the header if there is one, fixing all the fields we find
there. Then we go through the actual records themselves fixing all the fields
we find.
And that's all there is to it. We've made the quoting in the file consistent.

\begin{code}
(TODO put the fixed file here George)
\end{code}

This is just one of the many ways we could transform files with sv. In fact,
they don't even have to be transformations. We could use the lenses, prisms,
and traversals from sv to count the occurences of the letter `x` in our
document, or emit an error if we see CRLF line endings, or many more things.

In this way, you could use sv as a linting or sanitisation tool over your data,
even if there's no Haskell in your application.

== An aside on limitations

We were able to fix pairs of quotes, but at this time sv can't parse a document
with mismatched quote pairs. For example, fields like `'single double"` or
`"No closing` will result in parse failure.

Since a field is allowed to contain newlines, commas, and quotes, it's
difficult to distinguish where a field is supposed to end in the absence
of the proper quote.
