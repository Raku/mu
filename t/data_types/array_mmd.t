#!/usr/bin/pugs

use v6;

# Test.pm doesn't work well with these tests
#use Test;

#plan 4;
say "1..4";

my $here;

multi infix:<..> ( Int $a, Int $b ) { $here++ }
multi push ( Array @a, *@data ) { $here++ }
multi postcircumfix:<[]> ( *@a ) { $here++ }

my @a;
my @b;

$here = 0;
@a = 1..2;
print "not " unless $here; 
say "ok - range operator was redefined";

$here = 0;
push @a, 2;
print "not " unless $here; 
say "ok - push operator was redefined";

$here = 0;
my $x = @a[1];
print "not " unless $here; 
say "ok - slice fetch was redefined";

$here = 0;
@a[1] = 0;
print "not " unless $here; 
say "ok - slice store was redefined";

