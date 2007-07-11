use strict;
use warnings;
use Test::More tests => 6;

use Perlhint::Parse;
use Perlhint::Lookup;

my $parser = Perlhint::Parse->new({filename => 't5/sanity.dat'});
ok(ref $parser, "Perlhint::Parse->new returns a reference");
my $r = $parser->records;
is(ref $r, 'ARRAY', "records() returns an array ref");

is($r->[0]{key}, "foo", "First key parsed successfully");
is($r->[1]{key}, "blubb", "Second key parsed successfully");

is($r->[0]{ex}, "something continued on the next line", 
		"Multi line value without newline");
is($r->[1]{ex}, "something\nwith two lines, containing a newline",
		"Multi line value with newline");
