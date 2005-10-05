#!/usr/bin/pugs

use v6;
use Test;
use XML::SAX;

plan 1;

ok(XML::SAX.load_parsers());
