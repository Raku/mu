# Ideas for Perl 6 Google Summer of Code projects

## Rakudo

### Implement missing regex/grammar features

Perl 6 has a fairly comprehensive regex and grammar engine. Rakudo's
implementation is fairly complete, but some features are still missing,
most notably various backtracking control constructs (`::`, `:::`, `<cut>`
`<commit>`) and a few more esoteric features, like `<*foo>` and recursion
into rules associated with submatches (`<~~0>` and the likes).
[S05](http://perlcabal.org/syn/S05.html) has the gory details.

A student interested in regex compilers could implement those missing
features.

Possible mentors: ???

### NFG Strings

The Perl 6 specification contains a feature that hasn't been implemented in
any backend yet: The ability to treat strings not as a collection of bytes or
codepoints, but as a collection of grapheme clusters.

A proposal to implement this is to store a string as a series of integers,
with negative integers being indexes into a table that maps the numbers to
grapheme clusters for those that do not a have an NFC representation.

A student interested in Unicode could implement this in the JVM or the MoarVM
backend.

Possible mentors: moritz, ???

## MoarVM

Your ideas here

## Test suite

### test rosetta code

Implement a testing framework that works with perl6's roast that tests all existing rosetta code examples to verify they still work.

Possible mentors: Coke, moritz, ???

## Ecosystem
