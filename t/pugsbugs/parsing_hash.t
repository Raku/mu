#!/usr/bin/pugs

use v6;
use Test;

=kwid

Parse problem with //= and hash 

=cut

plan 3;

# hash {} gets parsed as two items when used with //=

my %hash;
%hash<foo> //= hash {};
%hash<bar> //= hash({});
my $h_ref;
$h_ref  //= hash {};
is(ref %hash<foo>, 'Hash', "Parses as two items");
is(ref %hash<bar>, 'Hash', "Parens do not help");
is(ref $h_ref,     'Hash', "It is not limited to hash values");
