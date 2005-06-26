#!/usr/bin/pugs

use v6;
use Test;

# http://use.perl.org/~autrijus/journal/25365
# Closure composers like anonymous sub, class and module always trumps hash
# dereferences:
# 
#   sub{...}
#   module{...}
#   class{...}

plan 2;

is sub { 42 }(), 'sub {...} works';
is sub{ 42 }(),  'sub{...} works';
