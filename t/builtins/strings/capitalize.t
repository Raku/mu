#!/usr/bin/pugs

use v6;
use Test;

plan 5;

is capitalize(""),             "",               "capitalize('') works";
is capitalize("puGS Is cOOl!"), "Pugs Is Cool!", "capitalize('...') works";
is "puGS Is cOOl!".capitalize,  "Pugs Is Cool!", "'...'.capitalize works";

{
  $_ = "puGS Is cOOl!";
  is capitalize(), "Pugs Is Cool!", 'capitalize() uses \$_ as default';
}

# Non-ASCII chars:
is capitalize("äöü abcä"), "Äöü abcä", "capitalize() works on non-ASCII chars";
