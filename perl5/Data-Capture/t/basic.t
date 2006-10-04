#!/usr/bin/perl
use strict;
use warnings;
use Test::More tests => 6;

use_ok('Data::Capture');

my $c = Data::Capture->new( { invocant => \'xyz',
			      positional => [\(2, 'fnord', 3)],
			      named => { xy => \3, fnord => \5 } } );

is($$c, 'xyz');

is_deeply( [map {$$_} @$c], [2, 'fnord', 3] );
is(${$c->[1]}, 'fnord');

is(${$c->{xy}}, 3);
is(${$c->{fnord}}, 5);
