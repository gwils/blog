---
title: Decoding Real-World Data with sv
date: 2018-03-19
authors: gwilson
---

This is an example of using [sv](https://hackage.haskell.org/package/sv) to
parse and decode a real CSV document into a Haskell datatype.

First, we'll get some imports out of the way. We import `Data.Sv` to get many
useful definitons, and we import `Data.Sv.Decode` qualified. This module was
designed to be imported qualified, as it contains definitions which clash with
`Prelude`.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.Species where

import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D

\end{code}

Our test file
[comes from data.gov.au](https://data.qld.gov.au/dataset/conservation-status-of-queensland-wildlife/resource/1c8b4859-31a4-42e7-8e63-b7cf125d4321).
It lists species found in Queensland, Australia along with their conservation
statuses.

\begin{code}
file :: FilePath
file = "csv/species.csv"
\end{code}

The file contains a header row featuring the column names, and then
thousands of rows of data. We will load the file from disk, parse it, and
then decode each row into a Haskell datatype, which will be
collected in a list.
This file is encoded using Windows-1252,
which is a non-ISO extension to latin1 8-bit ASCII. In particular, it is
incompatible with UTF-8.

The parser needs some configuration to parse our file. For example, it
needs to know which separator character we're using - in our case: comma.
It's useful to start from the default options and modify them using lens, but
you could just as easily modify them with record syntax, or build a config from
scratch using the types and values defined in `Data.Sv.Parse.Options`.
For this file, we can just use the default options.

\begin{code}
opts :: ParseOptions ByteString
opts = defaultParseOptions
\end{code}

sv's parser is written using
[parsers](https://hackage.haskell.org/package/parsers), which abstracts
the choice of parsing library. Hence we will now select a parsing library.
Most users can use the default by calling `parseDecode` rather than `parseDecode'`.

The default parsing library used is
[Trifecta](https://hackage.haskell.org/package/trifecta)
because of its very helpful clang-style error messages.
However Trifecta requires UTF-8 compatible input and, as mentioned above, our
file is encoded as Windows-1252.
Hence we're using
[Attoparsec](https://hackage.haskell.org/package/attoparsec),
which has less helpful error messages but can handle this encoding.

Benchmarking has shown that our parser is faster with attoparsec than with
trifecta, so if this is a concern to you, you might like to choose attoparsec
anyway.

\begin{code}
parser :: SvParser ByteString
parser = attoparsecByteString
\end{code}

This is the type we've made for our rows. It was designed by observing
the properties of the file. Many of the fields are simply
textual data, so we've chosen ByteString. We recommend using Text if
your data is ASCII, UTF-8, or UTF-16. We've made data types for the other
fields.
Other fields include the ID which is an int, and several optional fields.
Because they are optional, we have wrapped them in Maybe.
These optional fields are categorical data, meaning that each entry is
from a fixed set of categories or classes.

\begin{code}
data Species =
  Species {
    taxonId :: ID
  , kingdom :: ByteString
  , clazz :: ByteString
  , family :: ByteString
  , scientificName :: ByteString
  , taxonAuthor :: ByteString
  , commonName :: ByteString
  , ncaCode:: Maybe NcaCode
  , epbcCode :: Maybe EpbcCode
  , significant :: Maybe Significance
  , endemicityCode :: Maybe Endemicity
  }
  deriving Show
\end{code}

We newtype the ID so that we don't get it mixed up with any other ints.

\begin{code}
newtype ID = ID Int deriving Show
\end{code}

Next let's make our first decoder. We decode an `ID` by decoding an int and
then mapping the `ID` constructor over it. `int' is a primitive decoder
defined in `Data.Sv.Decode`. Typically more complex decoders are made by
combining these primitive decoders. Decoders are Applicative and
Alternative, giving us access to many derived combinators. There are also
other useful combinators defined in Data.Sv.Decode.

\begin{code}
idDecode :: Decode ByteString ByteString ID
idDecode = ID <$> D.int
\end{code}

A `Decode` has three type parameters. The first of these is the string
type to use for errors. Usually one would use `Text` or `Bytestring`. The second
type parameter is the input string type. We recommend these be the same, as
many decoding operations will want to use chunks of input strings to build
error strings.
In this case, both type paramters are `ByteString`. A type alias `Decode'`
is provided, which has these two type parameters the same. That type alias
will be used for the rest of the post.
The final paramter for a Decode is the type we're decoding into.

Here is a sum type for the different categories defined by the
Nature Conservation Act 1992. It is a simple sum type with only nullary
constructors. These are our categories...

\begin{code}
data NcaCode
  = NExtinct -- PE
  | NCriticallyEndangered -- CE
  | NEndangered -- E
  | NVulnerable -- V
  | NNearThreatened -- NT
  | NSpecialLeastConcern -- SL
  | NLeastConcern -- C
  | NInternational -- I
  | NProhibited -- P
  deriving Show
\end{code}

... and here is our decoder. We use the `Decode` primitive `categorical`,
as it makes specifying categorical data easier and provides specific error
messages when a field does not parse as any known category.

\begin{code}
nca :: Decode' ByteString NcaCode
nca =
  D.categorical [
    (NExtinct, "PE")
  , (NCriticallyEndangered, "CE")
  , (NEndangered, "E")
  , (NVulnerable, "V")
  , (NNearThreatened, "NT")
  , (NSpecialLeastConcern, "SL")
  , (NLeastConcern, "C")
  , (NInternational, "I")
  , (NProhibited, "P")
  ]
\end{code}

The following are similar types and decoders, this time for categories defined
in Environment Protection and Biodiversity Conservation Act 1999, and an
endemicity code. They are defined similarly to NcaCode above and its decoder.

\begin{code}
data EpbcCode
  = EExtinct -- EX
  | EWildExtinct -- WX
  | ECriticallyEndangered -- CE
  | EEndangered -- E
  | EVulnerable -- V
  | EConservationDependent -- CD
  deriving Show

epbc :: Decode' ByteString EpbcCode
epbc =
  D.categorical [
    (EExtinct, "EX")
  , (EWildExtinct, "WX")
  , (ECriticallyEndangered, "CE")
  , (EEndangered, "E")
  , (EVulnerable, "V")
  , (EConservationDependent, "CD")
  ]

data Endemicity
  = QueenslandEndemic -- Q
  | AustralianEndemic -- QA
  | QldAndInternational -- QI
  | AustraliaAndInternational -- QAI
  | QldNativeUndefinedEndemicity -- U
  | NaturalisedFromOverseas -- II
  | NaturalisedFromElsewhereInAus -- IA
  | NaturalisedFromUndefinedOrigin -- IU
  | VagrantAustralian -- VA
  | VagrantInternational -- VI
  | VagrantUndefined -- VU
  deriving Show

endemicity :: Decode' ByteString Endemicity
endemicity =
  D.categorical [
    (QueenslandEndemic, "Q")
  , (AustralianEndemic, "QA")
  , (QldAndInternational, "QI")
  , (AustraliaAndInternational, "QAI")
  , (QldNativeUndefinedEndemicity, "U")
  , (NaturalisedFromOverseas, "II")
  , (NaturalisedFromElsewhereInAus, "IA")
  , (NaturalisedFromUndefinedOrigin, "IU")
  , (VagrantAustralian, "VA")
  , (VagrantInternational, "VI")
  , (VagrantUndefined, "VU")
  ]
\end{code}

Setting up sum types for these codes and using `categorical` requires more
typing than we would otherwise do. It comes with benefits however! If any
string occurs that we did not expect, the decoding will fail with an error
that shows us the errant string! Furthermore, if multiple errors occur,
they will all be collected. This means that we will be shown all of the invalid
categorical data points in our document after one run of the program.


Next we define a boolean indicator of significance. We give it a categorical
decoder.
This categorical decoder allows several different strings to indicate each of
the two values. This helps to deal with data sets with inconsistently labeled
categorical data.

\begin{code}
data Significance
  = Y
  | N
  deriving Show

significance :: Decode' ByteString Significance
significance =
  D.categorical' [
    (Y, ["Y", "y", "yes"])
  , (N, ["N", "n", "no"])
  ]
\end{code}

Now we put it all together! This is the decoder for our Species data type
defined above. We use our `idDecode`, a bunch of bytestrings, and then our
categorical decoders, each wrapped in `orEmpty` since they might be missing.
It's all glued together with Applicative combinators.

\begin{code}
speciesDecoder :: Decode' ByteString Species
speciesDecoder = let s = D.byteString in
  Species <$> idDecode <*> s <*> s <*> s <*> s <*> s <*> s <*>
    D.orEmpty nca <*> D.orEmpty epbc <*> D.orEmpty significance <*> D.orEmpty endemicity
\end{code}

We call `parseDecodeFromFile` to load, parse, and decode our file. Note that we're
passing our parse options in.

\begin{code}
species :: IO (DecodeValidation ByteString [Species])
species = parseDecodeFromFile' parser speciesDecoder opts file
\end{code}

Finally, a main method to put it all together.

\begin{code}
main :: IO ()
main = do
  dv <- species
  case dv of
    Success rows ->
      print (length rows)
    Failure e -> do
      putStrLn "Failed to parse and decode species.csv:"
      print e
      exitFailure
\end{code}

And that's it! We've defined a data type for our rows, built a `Decode` for
that type, and then parsed our CSV file into useful Haskell data types.

