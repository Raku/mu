#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# Dubious: According to S29's definition, neither map nor grep allows for
# mutation.  On the other hand, it seems useful to preserve the bugward
# behaviour.  Marking :todo<unspecced>, pending p6l discussion (see thread
# "[S29] Mutating map and grep" on p6l started by Ingo Blechschmidt
# (http://www.nntp.perl.org/group/perl.perl6.language/22553)) and S29 patch.

{
  my @array = <a b c d>;
  is ~(try { @array.map:{ $_ ~= "c"; $_ ~ "d" } }), "acd bcd ccd dcd",
    'mutating $_ in map works (1)', :todo<unspecced>;
  is ~@array, "ac bc cc dc",
    'mutating $_ in map works (2)', :todo<unspecced>;
}

{
  my @array = <a b c d>;
  is ~(try { @array.grep:{ $_ ~= "c"; 1 } }), "ac bc cc dc",
    'mutating $_ in grep works (1)', :todo<unspecced>;
  is ~@array, "ac bc cc dc",
    'mutating $_ in grep works (2)', :todo<unspecced>;
}

{
  my @array = <a b c d>;
  for @array { $_ ~= "c" }
  is ~@array, "ac bc cc dc",
    'mutating $_ in for works';
}

# Hm... what about @array.sort:{ $_ = ... }? Disallow? (As that would prevent
# many optimizations...)  (and Perl 5 never allowed that anyway)
