#!/usr/bin/pugs

use Text::Typoifier <transform>;

for =<> -> $line {
  print transform $line;
}
