#!/usr/bin/pugs

use v6;
require Test;
require SHA1;

plan 1;

is(sha1(''), 'da39a3ee5e6b4b0d3255bfef95601890afd80709', "sanity");
